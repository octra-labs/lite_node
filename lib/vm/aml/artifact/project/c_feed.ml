(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  values : C_eval.value list;
  raw : string;
}

type spec = {
  name : string;
  typ : C_type.t;
}

type error =
  | Header
  | Bits
  | Size of int * int
  | Form
  | Count of int * int
  | Cap of C_nat.t * C_nat.t
  | Spec of string
  | Lex of C_lex.error
  | Need of C_lex.form * C_lex.form * C_lex.span
  | Name of string * string * C_lex.span
  | Nat of Z.t * C_lex.span
  | Type of string * C_type.t * C_lex.span
  | Inputs of int * int
  | Input of C_term.id * C_type.t
  | Machine of C_term.id * C_type.t

module Key = struct
  type t = C_nat.t * C_nat.t

  let compare (lhs_kind, lhs_id) (rhs_kind, rhs_id) =
    let order = C_nat.compare lhs_kind rhs_kind in
    if order = 0 then C_nat.compare lhs_id rhs_id else order
end

module Keys = Set.Make (Key)

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let spec name typ = { name; typ }

let specs binds =
  let rec loop out = function
    | [] -> Ok (List.rev out)
    | (bind : C_syn.bind) :: rest ->
      begin
        match C_low.typ bind.typ with
        | Ok typ -> loop ({ name = C_syn.name_text bind.name; typ } :: out) rest
        | Error _ -> Error (Spec (C_syn.name_text bind.name))
      end
  in
  loop [] binds

let values feed = feed.values

let shape values =
  let rec walk nodes seen = function
    | [] -> Ok ()
    | _ when nodes >= C_check.max_nodes -> Error Form
    | (depth, _) :: _ when depth > C_check.max_depth -> Error Form
    | (depth, value) :: rest ->
      let next = depth + 1 in
      begin
        match value with
        | C_eval.Unit | C_eval.Bool _ | C_eval.Int _ | C_eval.Bytes _
        | C_eval.Enc _ -> walk (nodes + 1) seen rest
        | C_eval.Vec items ->
          let rest =
            Array.fold_left (fun out item -> (next, item) :: out) rest items
          in
          walk (nodes + 1) seen rest
        | C_eval.Cap (kind, id) ->
          let key = kind, id in
          if Keys.mem key seen then Error (Cap (kind, id))
          else walk (nodes + 1) (Keys.add key seen) rest
        | C_eval.Pair (lhs, rhs) ->
          walk (nodes + 1) seen ((next, lhs) :: (next, rhs) :: rest)
        | C_eval.Inl value | C_eval.Inr value ->
          walk (nodes + 1) seen ((next, value) :: rest)
      end
  in
  walk 0 Keys.empty (List.map (fun value -> 0, value) values)

let bits value =
  let rec loop index out =
    if index < 0 then out
    else loop (index - 1) (Char.equal value.[index] '1' :: out)
  in
  loop (String.length value - 1) []

let chars values =
  let out = Bytes.create (List.length values) in
  let rec loop index = function
    | [] -> Bytes.unsafe_to_string out
    | value :: rest ->
      Bytes.set out index (if value then '1' else '0');
      loop (index + 1) rest
  in
  loop 0 values

let raw values =
  match C_bin.list_code C_bin.value_code values with
  | None -> None
  | Some code ->
    begin
      match C_bin.enc_code code with
      | None -> None
      | Some bits ->
        let raw = "AF1\n" ^ chars bits in
        if String.length raw <= C_rule.local.str then Some raw else None
    end

let make values =
  let count = List.length values in
  if count > C_check.max_inputs then Error (Count (C_check.max_inputs, count))
  else if not (List.for_all (fun value -> Option.is_some (C_bin.enc_value value)) values)
  then Error Form
  else
    let* () = shape values in
    match raw values with
    | Some raw -> Ok { values; raw }
    | None -> Error Form

let encode feed = feed.raw

let decode input =
  let size = String.length input in
  if size > C_rule.local.str then Error (Size (C_rule.local.str, size))
  else if size < 4 || not (String.equal (String.sub input 0 4) "AF1\n") then
    Error Header
  else
    let body = String.sub input 4 (size - 4) in
    if not (String.for_all (fun char -> Char.equal char '0' || Char.equal char '1') body)
    then Error Bits
    else
      match C_bin.dec_code (bits body) with
      | None -> Error Form
      | Some code ->
        begin
          match C_bin.list_get C_bin.value_get code with
          | None -> Error Form
          | Some values ->
            let* feed = make values in
            if String.equal feed.raw input then Ok feed else Error Form
        end

type cur = {
  items : C_lex.item array;
  at : int;
}

let item state = state.items.(state.at)
let form state = C_lex.form (item state).tok
let next state = { state with at = state.at + 1 }

let need wanted state =
  let got = form state in
  if got = wanted then Ok (next state)
  else Error (Need (wanted, got, (item state).span))

let word wanted state =
  match (item state).tok with
  | C_lex.Ident value when String.equal value wanted -> Ok (next state)
  | _ -> Error (Need (C_lex.F_ident, form state, (item state).span))

let name state =
  match (item state).tok with
  | C_lex.Ident value -> Ok (value, next state)
  | _ -> Error (Need (C_lex.F_ident, form state, (item state).span))

let nat state =
  match (item state).tok with
  | C_lex.Nat value ->
    begin
      match C_nat.make value with
      | Some value -> Ok (value, next state)
      | None -> Error (Nat (value, (item state).span))
    end
  | _ -> Error (Need (C_lex.F_nat, form state, (item state).span))

let integer state =
  match (item state).tok with
  | C_lex.Minus ->
    let state = next state in
    begin
      match (item state).tok with
      | C_lex.Nat value -> Ok (Z.neg value, next state)
      | _ -> Error (Need (C_lex.F_nat, form state, (item state).span))
    end
  | C_lex.Nat value -> Ok (value, next state)
  | _ -> Error (Need (C_lex.F_nat, form state, (item state).span))

let rec value typ state =
  let at = (item state).span in
  match typ with
  | C_type.Unit ->
    let* state = need C_lex.F_unit state in
    Ok (C_eval.Unit, state)
  | C_type.Bool ->
    begin
      match (item state).tok with
      | C_lex.True -> Ok (C_eval.Bool true, next state)
      | C_lex.False -> Ok (C_eval.Bool false, next state)
      | _ -> Error (Type ("value", typ, at))
    end
  | C_type.Int ->
    let* number, state = integer state in
    Ok (C_eval.Int number, state)
  | C_type.Num _ ->
    let* number, state = integer state in
    if C_type.admits typ number then Ok (C_eval.Int number, state)
    else Error (Type ("value", typ, at))
  | C_type.Bytes len ->
    begin
      match (item state).tok with
      | C_lex.Hex raw when C_nat.to_int len = String.length raw ->
        Ok (C_eval.Bytes raw, next state)
      | _ -> Error (Type ("value", typ, at))
    end
  | C_type.Vec (len, elem) ->
    let* state = need C_lex.F_vec state in
    let* state = need C_lex.F_lparen state in
    let* items, state = seq (C_nat.to_int len) elem state in
    let* state = need C_lex.F_rparen state in
    Ok (C_eval.Vec (Array.of_list items), state)
  | C_type.Seq (cap, elem) ->
    let* state = need C_lex.F_seq state in
    let* state = need C_lex.F_lparen state in
    let limit = C_nat.to_int cap in
    let rec items count out state =
      match (item state).tok with
      | C_lex.Rparen -> Ok (List.rev out, next state)
      | _ when count = limit -> Error (Type ("value", typ, at))
      | _ ->
        let* value, state = value elem state in
        begin
          match (item state).tok with
          | C_lex.Comma -> more (count + 1) (value :: out) (next state)
          | C_lex.Rparen -> Ok (List.rev (value :: out), next state)
          | _ -> Error (Type ("value", typ, at))
        end
    and more count out state =
      match (item state).tok with
      | C_lex.Rparen -> Error (Type ("value", typ, at))
      | _ -> items count out state
    in
    let* active, state = items 0 [] state in
    begin
      match C_eval.zero elem with
      | None -> Error (Type ("value", typ, at))
      | Some empty ->
        let count = List.length active in
        let values = Array.make limit empty in
        List.iteri (Array.set values) active;
        Ok (C_eval.Pair (C_eval.Int (Z.of_int count), C_eval.Vec values), state)
    end
  | C_type.Cap kind ->
    let* state = need C_lex.F_cap state in
    let* state = need C_lex.F_lbrack state in
    let* actual, state = nat state in
    if not (C_nat.equal kind actual) then Error (Type ("value", typ, at))
    else
      let* state = need C_lex.F_rbrack state in
      let* state = need C_lex.F_lparen state in
      let* id, state = nat state in
      let* state = need C_lex.F_rparen state in
      Ok (C_eval.Cap (kind, id), state)
  | C_type.Enc (key, rem) ->
    let* state = word "enc" state in
    let* state = need C_lex.F_lbrack state in
    let* actual_key, state = nat state in
    let* state = need C_lex.F_comma state in
    let* actual_rem, state = nat state in
    if not (C_nat.equal key actual_key && C_nat.equal rem actual_rem) then
      Error (Type ("value", typ, at))
    else
      let* state = need C_lex.F_rbrack state in
      let* state = need C_lex.F_lparen state in
      let* raw, state = integer state in
      begin
        match C_fp.make raw with
        | None -> Error (Type ("value", typ, at))
        | Some field ->
          let* state = need C_lex.F_rparen state in
          Ok (C_eval.Enc (key, rem, field), state)
      end
  | C_type.Pair (lhs, rhs) ->
    let* state = need C_lex.F_lparen state in
    let* lhs, state = value lhs state in
    let* state = need C_lex.F_comma state in
    let* rhs, state = value rhs state in
    let* state = need C_lex.F_rparen state in
    Ok (C_eval.Pair (lhs, rhs), state)
  | C_type.Sum (lhs, rhs) ->
    begin
      match (item state).tok with
      | C_lex.Good ->
        let* state = need C_lex.F_lparen (next state) in
        let* value, state = value lhs state in
        let* state = need C_lex.F_rparen state in
        Ok (C_eval.Inl value, state)
      | C_lex.Bad ->
        let* state = need C_lex.F_lparen (next state) in
        let* value, state = value rhs state in
        let* state = need C_lex.F_rparen state in
        Ok (C_eval.Inr value, state)
      | _ -> Error (Type ("value", typ, at))
    end

and seq count typ state =
  if count = 0 then Ok ([], state)
  else
    let* first, state = value typ state in
    let rec loop left out state =
      if left = 0 then Ok (List.rev out, state)
      else
        let* state = need C_lex.F_comma state in
        let* item, state = value typ state in
        loop (left - 1) (item :: out) state
    in
    loop (count - 1) [first] state

let parse_value typ source =
  let* items =
    match C_lex.scan source with
    | Ok value -> Ok value
    | Error error -> Error (Lex error)
  in
  let* value, state = value typ { items; at = 0 } in
  let* _ = need C_lex.F_eof state in
  if Result.is_ok (shape [value]) then Ok value else Error Form

let parse specs source =
  let* items =
    match C_lex.scan source with
    | Ok value -> Ok value
    | Error error -> Error (Lex error)
  in
  let state = { items; at = 0 } in
  let* state = word "feed" state in
  let* state = need C_lex.F_lbrace state in
  let rec entries out state = function
    | [] ->
      let* state = need C_lex.F_rbrace state in
      let* _ = need C_lex.F_eof state in
      make (List.rev out)
    | spec :: rest ->
      let* actual, named = name state in
      if not (String.equal actual spec.name) then
        Error (Name (spec.name, actual, (item state).span))
      else
        let* assigned = need C_lex.F_eq named in
        let* item, state = value spec.typ assigned in
        let* state =
          match rest with
          | [] -> Ok state
          | _ :: _ -> need C_lex.F_comma state
        in
        entries (item :: out) state rest
  in
  entries [] state specs

let typed = C_eval.typed

let shaped value = Result.is_ok (shape [value])

let attach binds feed =
  let expected = List.length binds in
  let actual = List.length feed.values in
  if expected <> actual then Error (Inputs (expected, actual))
  else
    let rec loop out binds values =
      match binds, values with
      | [], [] -> Ok (List.rev out)
      | (bind : C_term.bind) :: rest, value :: more ->
        if typed bind.typ value then loop ((bind, value) :: out) rest more
        else Error (Input (bind.id, bind.typ))
      | _, _ -> Error (Inputs (expected, actual))
    in
    loop [] binds feed.values

let rec drop id = function
  | [] -> []
  | (key, _) :: rest when C_nat.equal id key -> drop id rest
  | item :: rest -> item :: drop id rest

let rec sub_get id = function
  | [] -> None
  | (key, value) :: _ when C_nat.equal id key -> Some value
  | _ :: rest -> sub_get id rest

let rec subst values = function
  | C_term.Unit -> C_term.Unit
  | C_term.Bool value -> C_term.Bool value
  | C_term.Int value -> C_term.Int value
  | C_term.Narrow (typ, value) -> C_term.Narrow (typ, value)
  | C_term.Bytes value -> C_term.Bytes value
  | C_term.Vec (typ, items) -> C_term.Vec (typ, List.map (subst values) items)
  | C_term.Seq (cap, typ, items) ->
    C_term.Seq (cap, typ, List.map (subst values) items)
  | C_term.Var id -> Option.value ~default:(C_term.Var id) (sub_get id values)
  | C_term.Let (bind, value, body) ->
    C_term.Let (bind, subst values value, subst (drop bind.id values) body)
  | C_term.If (guard, yes, no) ->
    C_term.If (subst values guard, subst values yes, subst values no)
  | C_term.Pair (lhs, rhs) -> C_term.Pair (subst values lhs, subst values rhs)
  | C_term.Unpair (pair, lhs, rhs, body) ->
    C_term.Unpair (subst values pair, lhs, rhs,
      subst (drop rhs.id (drop lhs.id values)) body)
  | C_term.Fst value -> C_term.Fst (subst values value)
  | C_term.Snd value -> C_term.Snd (subst values value)
  | C_term.Inl (value, rhs) -> C_term.Inl (subst values value, rhs)
  | C_term.Inr (lhs, value) -> C_term.Inr (lhs, subst values value)
  | C_term.Case (value, lhs, yes, rhs, no) ->
    C_term.Case (subst values value, lhs, subst (drop lhs.id values) yes,
      rhs, subst (drop rhs.id values) no)
  | C_term.Act (action, body) -> C_term.Act (action, subst values body)
  | C_term.Add (lhs, rhs) -> C_term.Add (subst values lhs, subst values rhs)
  | C_term.Sub (lhs, rhs) -> C_term.Sub (subst values lhs, subst values rhs)
  | C_term.Mul (lhs, rhs) -> C_term.Mul (subst values lhs, subst values rhs)
  | C_term.Div (lhs, rhs) -> C_term.Div (subst values lhs, subst values rhs)
  | C_term.Mod (lhs, rhs) -> C_term.Mod (subst values lhs, subst values rhs)
  | C_term.Neg value -> C_term.Neg (subst values value)
  | C_term.Abs value -> C_term.Abs (subst values value)
  | C_term.Fit (typ, value) -> C_term.Fit (typ, subst values value)
  | C_term.Wide value -> C_term.Wide (subst values value)
  | C_term.Length value -> C_term.Length (subst values value)
  | C_term.Eq (typ, lhs, rhs) -> C_term.Eq (typ, subst values lhs, subst values rhs)
  | C_term.Cmp (rel, lhs, rhs) ->
    C_term.Cmp (rel, subst values lhs, subst values rhs)
  | C_term.Cat (lhs, rhs) -> C_term.Cat (subst values lhs, subst values rhs)
  | C_term.Take (len, value) -> C_term.Take (len, subst values value)
  | C_term.Drop (len, value) -> C_term.Drop (len, subst values value)
  | C_term.Vcat (lhs, rhs) -> C_term.Vcat (subst values lhs, subst values rhs)
  | C_term.At (index, value) -> C_term.At (index, subst values value)
  | C_term.Uncons value -> C_term.Uncons (subst values value)
  | C_term.Vfold (vector, seed, fold) ->
    C_term.Vfold (subst values vector, subst values seed,
      { fold with body = subst (drop fold.state.id (drop fold.item.id values)) fold.body })
  | C_term.Step (cap, value) -> C_term.Step (subst values cap, subst values value)
  | C_term.Close cap -> C_term.Close (subst values cap)

let rec term_of typ value =
  if not (typed typ value) then None
  else
    match typ, value with
    | C_type.Unit, C_eval.Unit -> Some C_term.Unit
    | C_type.Bool, C_eval.Bool value -> Some (C_term.Bool value)
    | C_type.Int, C_eval.Int value -> Some (C_term.Int value)
    | C_type.Num _, C_eval.Int value -> Some (C_term.Narrow (typ, value))
    | C_type.Bytes _, C_eval.Bytes value -> Some (C_term.Bytes value)
    | C_type.Vec (_, elem), C_eval.Vec values ->
      Option.map (fun items -> C_term.Vec (elem, items))
        (terms elem (Array.to_list values))
    | C_type.Seq (cap, elem), C_eval.Pair (C_eval.Int len, C_eval.Vec values) ->
      let count = Z.to_int len in
      Option.map (fun items -> C_term.Seq (cap, elem, items))
        (terms elem (Array.to_list (Array.sub values 0 count)))
    | C_type.Pair (left_ty, right_ty), C_eval.Pair (left, right) ->
      Option.bind (term_of left_ty left) (fun left_term ->
        Option.map (fun right_term -> C_term.Pair (left_term, right_term))
          (term_of right_ty right))
    | C_type.Sum (left_ty, right_ty), C_eval.Inl value ->
      Option.map (fun term -> C_term.Inl (term, right_ty))
        (term_of left_ty value)
    | C_type.Sum (left_ty, right_ty), C_eval.Inr value ->
      Option.map (fun term -> C_term.Inr (left_ty, term))
        (term_of right_ty value)
    | _ -> None

and terms typ = function
  | [] -> Some []
  | value :: rest ->
    Option.bind (term_of typ value) (fun term ->
      Option.map (fun more -> term :: more) (terms typ rest))

let sub_of pairs =
  let rec loop out = function
    | [] -> Ok out
    | ((bind : C_term.bind), value) :: rest ->
      begin
        match bind.mul, bind.typ, value with
        | C_type.Zero, _, _ -> loop out rest
        | (C_type.One | C_type.Many), _, _ ->
          begin
            match term_of bind.typ value with
            | Some term -> loop ((bind.id, term) :: out) rest
            | None -> Error (Machine (bind.id, bind.typ))
          end
      end
  in
  loop [] pairs

let close binds feed term =
  let* pairs = attach binds feed in
  let* values = sub_of pairs in
  Ok (subst values term)

let span_text span =
  Printf.sprintf "%d:%d" span.C_lex.first.line span.first.col

let text = function
  | Header -> "feed header is not AF1"
  | Bits -> "feed body contains a non-bit octet"
  | Size (max, actual) ->
    Printf.sprintf "feed size max = %d actual = %d" max actual
  | Form -> "feed form is invalid"
  | Count (max, actual) ->
    Printf.sprintf "feed value count max = %d actual = %d" max actual
  | Cap (kind, id) ->
    "feed capability is repeated kind = " ^ C_nat.text kind ^ " id = " ^ C_nat.text id
  | Spec name -> "feed input type is unavailable name = " ^ name
  | Lex error -> C_lex.text error
  | Need (wanted, actual, span) ->
    "feed token expected = " ^ C_lex.form_text wanted ^ " actual = "
    ^ C_lex.form_text actual ^ " at = " ^ span_text span
  | Name (wanted, actual, span) ->
    "feed name expected = " ^ wanted ^ " actual = " ^ actual
    ^ " at = " ^ span_text span
  | Nat (value, span) ->
    "feed natural outside profile = " ^ Z.to_string value ^ " at = " ^ span_text span
  | Type (name, typ, span) ->
    "feed value differs name = " ^ name ^ " type = " ^ C_type.text typ
    ^ " at = " ^ span_text span
  | Inputs (expected, actual) ->
    Printf.sprintf "feed input count expected = %d actual = %d" expected actual
  | Input (id, typ) ->
    "feed input differs id = " ^ C_nat.text id ^ " type = " ^ C_type.text typ
  | Machine (id, typ) ->
    "feed input is outside the machine data profile id = " ^ C_nat.text id
    ^ " type = " ^ C_type.text typ