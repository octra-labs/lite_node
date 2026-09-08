(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type shape =
  | SUnit
  | SAtom
  | SCap of C_nat.t
  | SPair of shape * shape
  | SVec of C_nat.t * shape
  | SSum of shape

type emission =
  | Lowered
  | Specialized

type code =
  | Done
  | Push of C_emit.lit * code
  | Void of code
  | Get of C_term.id * shape * code
  | Plus of code
  | Minus of code
  | Times of code
  | Quot of code
  | Rem of code
  | Negate of code
  | Absolute of code
  | Same of code
  | Different of code
  | Order of C_term.rel * code
  | Join of code
  | Clip of C_nat.t * code
  | Skip of C_nat.t * code
  | Duo of code
  | First of code
  | Second of code
  | Empty of shape * code
  | Cons of code
  | Append of code
  | Pick of C_nat.t * code
  | Unhead of code
  | Left of code
  | Right of code
  | Pack of C_nat.t * C_type.t * code
  | Fit of C_type.t * code
  | Wide of code
  | Close of C_nat.t * code
  | Effect of int * C_eff.atom * code * code
  | Scope of C_term.bind * code * code
  | Scope2 of C_term.bind * C_term.bind * code * code
  | Iter of C_nat.t * C_term.bind * C_term.bind * code * code
  | Iter_seq of C_nat.t * C_term.bind * C_term.bind * code * code
  | Choice of C_term.bind * code * C_term.bind * code * shape * code
  | Fork of shape * code * code * code

type loc =
  | LDone
  | LPush of C_lex.span * loc
  | LVoid of C_lex.span * loc
  | LGet of C_lex.span * loc
  | LPlus of C_lex.span * loc
  | LMinus of C_lex.span * loc
  | LTimes of C_lex.span * loc
  | LQuot of C_lex.span * loc
  | LRem of C_lex.span * loc
  | LNegate of C_lex.span * loc
  | LAbsolute of C_lex.span * loc
  | LSame of C_lex.span * loc
  | LDifferent of C_lex.span * loc
  | LOrder of C_term.rel * C_lex.span * loc
  | LJoin of C_lex.span * loc
  | LClip of C_nat.t * C_lex.span * loc
  | LSkip of C_nat.t * C_lex.span * loc
  | LDuo of C_lex.span * loc
  | LFirst of C_lex.span * loc
  | LSecond of C_lex.span * loc
  | LEmpty of C_lex.span * loc
  | LCons of C_lex.span * loc
  | LAppend of C_lex.span * loc
  | LPick of C_nat.t * C_lex.span * loc
  | LUnhead of C_lex.span * loc
  | LLeft of C_lex.span * loc
  | LRight of C_lex.span * loc
  | LPack of C_lex.span * loc
  | LFit of C_lex.span * loc
  | LWide of C_lex.span * loc
  | LClose of C_lex.span * loc
  | LEffect of C_lex.span * loc * loc
  | LScope of C_lex.span * loc * loc
  | LScope2 of C_lex.span * loc * loc
  | LIter of C_lex.span * loc * loc
  | LChoice of C_lex.span * C_lex.span * C_lex.span * loc * loc * loc
  | LFork of C_lex.span * C_lex.span * C_lex.span * loc * loc * loc

type t = {
  inputs : C_term.bind list;
  code : code;
  loc : loc;
  typ : C_type.t;
  result : C_emit.lit option;
  emission : emission;
  veils : int;
  veil_depth : C_nat.t;
  span : C_lex.span;
}

type error =
  | Source of C_parse.error
  | Feed of C_feed.error
  | Input of C_term.id * C_type.t
  | Inputs of Z.t
  | Layout of Z.t
  | Types of Z.t
  | Effects of C_eff.atom list
  | Term
  | Run of C_eval.error
  | Plan of C_eff.atom list
  | Value of C_type.t * C_eval.value
  | Replay
  | Map

type tree = {
  at : C_lex.span;
  sub : tree list;
}

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let equal left right =
  match left, right with
  | C_emit.Bool lhs, C_emit.Bool rhs -> Bool.equal lhs rhs
  | C_emit.Int lhs, C_emit.Int rhs -> Z.equal lhs rhs
  | C_emit.Bytes lhs, C_emit.Bytes rhs -> String.equal lhs rhs
  | C_emit.Data lhs, C_emit.Data rhs -> C_rval.equal lhs rhs
  | C_emit.Cap (lk, li), C_emit.Cap (rk, ri) ->
    C_nat.equal lk rk && C_nat.equal li ri
  | _ -> false

let atom_equal left right =
  match left, right with
  | C_eff.Read lhs, C_eff.Read rhs
  | C_eff.Write lhs, C_eff.Write rhs
  | C_eff.Emit lhs, C_eff.Emit rhs
  | C_eff.Fail lhs, C_eff.Fail rhs
  | C_eff.Close lhs, C_eff.Close rhs -> C_nat.equal lhs rhs
  | _ -> false

let rec plan_equal left right =
  match left, right with
  | [], [] -> true
  | lhs :: lrest, rhs :: rrest ->
    atom_equal lhs rhs && plan_equal lrest rrest
  | _ -> false

let literal typ value =
  match typ, value with
  | C_type.Bool, C_eval.Bool value -> Some (C_emit.Bool value)
  | C_type.Int, C_eval.Int value -> Some (C_emit.Int value)
  | (C_type.Num _ as typ), C_eval.Int value when C_type.admits typ value ->
    Some (C_emit.Int value)
  | C_type.Bytes size, C_eval.Bytes value
      when Z.equal (C_nat.to_z size) (Z.of_int (String.length value)) ->
      Some (C_emit.Bytes value)
  | C_type.Cap kind, C_eval.Cap (found, id) when C_nat.equal kind found ->
    Some (C_emit.Cap (kind, id))
  | _ ->
    begin
      match C_rval.make typ value with
      | Ok item -> Some (C_emit.Data item)
      | Error _ -> None
    end

let rec atoms_into typ value out =
  match typ, value with
  | C_type.Unit, C_eval.Unit -> Some out
  | C_type.Bool, C_eval.Bool value -> Some (C_emit.Bool value :: out)
  | C_type.Int, C_eval.Int value -> Some (C_emit.Int value :: out)
  | (C_type.Num _ as typ), C_eval.Int value when C_type.admits typ value ->
    Some (C_emit.Int value :: out)
  | C_type.Bytes len, C_eval.Bytes value
      when C_nat.to_int len = String.length value ->
    Some (C_emit.Bytes value :: out)
  | C_type.Cap kind, C_eval.Cap (found, id) when C_nat.equal kind found ->
    Some (C_emit.Cap (kind, id) :: out)
  | C_type.Vec (len, elem), C_eval.Vec values
      when C_nat.to_int len = Array.length values ->
    atoms_array elem values 0 out
  | (C_type.Seq _ as typ), value when C_eval.typed typ value ->
    atoms_into (C_type.repr typ) value out
  | C_type.Pair (lhs, rhs), C_eval.Pair (left, right) ->
    Option.bind (atoms_into lhs left out) (atoms_into rhs right)
  | C_type.Sum (lhs, _), C_eval.Inl value ->
    atoms_into lhs value (C_emit.Bool true :: out)
  | C_type.Sum (_, rhs), C_eval.Inr value ->
    atoms_into rhs value (C_emit.Bool false :: out)
  | _ -> None

and atoms_array typ values index out =
  if index = Array.length values then Some out
  else
    Option.bind (atoms_into typ values.(index) out)
      (atoms_array typ values (index + 1))

let atoms typ value =
  Option.map List.rev (atoms_into typ value [])

let rec value typ input =
  match typ, input with
  | C_type.Unit, _ -> Some (C_eval.Unit, input)
  | C_type.Bool, C_emit.Bool value :: rest -> Some (C_eval.Bool value, rest)
  | C_type.Int, C_emit.Int value :: rest -> Some (C_eval.Int value, rest)
  | (C_type.Num _ as typ), C_emit.Int value :: rest
      when C_type.admits typ value ->
    Some (C_eval.Int value, rest)
  | C_type.Bytes len, C_emit.Bytes raw :: rest
      when C_nat.to_int len = String.length raw ->
    Some (C_eval.Bytes raw, rest)
  | C_type.Cap kind, C_emit.Cap (found, id) :: rest
      when C_nat.equal kind found ->
    Some (C_eval.Cap (kind, id), rest)
  | C_type.Vec (len, elem), _ ->
    Option.map
      (fun (values, rest) -> C_eval.Vec (Array.of_list values), rest)
      (values (C_nat.to_int len) elem input)
  | (C_type.Seq _ as typ), _ ->
    Option.bind (value (C_type.repr typ) input) (fun (item, rest) ->
      if C_eval.typed typ item then Some (item, rest) else None)
  | C_type.Pair (lhs, rhs), _ ->
    Option.bind (value lhs input) (fun (left, rest) ->
      Option.map
        (fun (right, tail) -> C_eval.Pair (left, right), tail)
        (value rhs rest))
  | C_type.Sum (lhs, rhs), C_emit.Bool side :: rest ->
    if side then
      Option.map (fun (item, tail) -> C_eval.Inl item, tail) (value lhs rest)
    else Option.map (fun (item, tail) -> C_eval.Inr item, tail) (value rhs rest)
  | _ -> None

and values count typ input =
  if count = 0 then Some ([], input)
  else
    Option.bind (value typ input) (fun (first, rest) ->
      Option.map (fun (tail, left) -> first :: tail, left)
        (values (count - 1) typ rest))

let rec same_shape left right =
  match left, right with
  | SUnit, SUnit | SAtom, SAtom -> true
  | SCap lhs, SCap rhs -> C_nat.equal lhs rhs
  | SPair (ll, lr), SPair (rl, rr) ->
    same_shape ll rl && same_shape lr rr
  | SVec (ln, le), SVec (rn, re) ->
    C_nat.equal ln rn && same_shape le re
  | SSum lhs, SSum rhs -> same_shape lhs rhs
  | _ -> false

let rec shape_of = function
  | C_type.Unit -> Some SUnit
  | C_type.Bool | C_type.Int | C_type.Num _ | C_type.Bytes _ -> Some SAtom
  | C_type.Cap kind -> Some (SCap kind)
  | C_type.Vec (len, elem) ->
    Option.map (fun item -> SVec (len, item)) (shape_of elem)
  | C_type.Seq (cap, elem) ->
    Option.map (fun item -> SPair (SAtom, SVec (cap, item))) (shape_of elem)
  | C_type.Pair (lhs, rhs) ->
    Option.bind (shape_of lhs) (fun lshape ->
      Option.map (fun rshape -> SPair (lshape, rshape)) (shape_of rhs))
  | C_type.Sum (lhs, rhs) ->
    Option.bind (shape_of lhs) (fun lshape ->
      Option.bind (shape_of rhs) (fun rshape ->
        if same_shape lshape rshape then Some (SSum lshape) else None))
  | C_type.Enc _ -> None

let rec shape_width = function
  | SUnit -> Z.zero
  | SAtom -> Z.one
  | SCap _ -> Z.one
  | SPair (lhs, rhs) -> Z.add (shape_width lhs) (shape_width rhs)
  | SVec (len, elem) -> Z.mul (C_nat.to_z len) (shape_width elem)
  | SSum payload -> Z.succ (shape_width payload)

let rec shape_cells = function
  | SUnit | SAtom | SCap _ -> Z.one
  | SPair (lhs, rhs) ->
    Z.succ (Z.add (shape_cells lhs) (shape_cells rhs))
  | SVec (len, elem) ->
    Z.succ (Z.mul (C_nat.to_z len) (shape_cells elem))
  | SSum payload -> Z.succ (shape_cells payload)

let rec same_shapes left right =
  match left, right with
  | [], [] -> true
  | lhs :: lrest, rhs :: rrest ->
    same_shape lhs rhs && same_shapes lrest rrest
  | _ -> false

let keep_shape tail = function
  | value :: rest when same_shapes rest tail -> Some value
  | _ -> None

let rec flow code stack =
  match code with
  | Done -> Some stack
  | Push (_, rest) -> flow rest (SAtom :: stack)
  | Void rest -> flow rest (SUnit :: stack)
  | Get (_, form, rest) -> flow rest (form :: stack)
  | Plus rest | Minus rest | Times rest | Quot rest | Rem rest
  | Same rest | Different rest | Order (_, rest) | Join rest ->
    begin
      match stack with
      | SAtom :: SAtom :: tail -> flow rest (SAtom :: tail)
      | _ -> None
    end
  | Negate rest | Absolute rest ->
    begin
      match stack with
      | SAtom :: tail -> flow rest (SAtom :: tail)
      | _ -> None
    end
  | Clip (_, rest) | Skip (_, rest) ->
    begin
      match stack with
      | SAtom :: tail -> flow rest (SAtom :: tail)
      | _ -> None
    end
  | Duo rest ->
    begin
      match stack with
      | rhs :: lhs :: tail -> flow rest (SPair (lhs, rhs) :: tail)
      | _ -> None
    end
  | First rest ->
    begin
      match stack with
      | SPair (lhs, _) :: tail -> flow rest (lhs :: tail)
      | _ -> None
    end
  | Second rest ->
    begin
      match stack with
      | SPair (_, rhs) :: tail -> flow rest (rhs :: tail)
      | _ -> None
    end
  | Empty (elem, rest) -> flow rest (SVec (C_nat.zero, elem) :: stack)
  | Cons rest ->
    begin
      match stack with
      | SVec (len, elem) :: item :: tail when same_shape item elem ->
        Option.bind (C_nat.add len C_nat.one) (fun next ->
          flow rest (SVec (next, elem) :: tail))
      | _ -> None
    end
  | Append rest ->
    begin
      match stack with
      | SVec (rn, re) :: SVec (ln, le) :: tail when same_shape le re ->
        Option.bind (C_nat.add ln rn) (fun len ->
          flow rest (SVec (len, le) :: tail))
      | _ -> None
    end
  | Pick (index, rest) ->
    begin
      match stack with
      | SVec (len, elem) :: tail when C_nat.lt index len ->
        flow rest (elem :: tail)
      | _ -> None
    end
  | Unhead rest ->
    begin
      match stack with
      | SVec (len, elem) :: tail ->
        Option.bind (C_nat.sub len C_nat.one) (fun next ->
          flow rest (SPair (elem, SVec (next, elem)) :: tail))
      | _ -> None
    end
  | Left rest | Right rest ->
    begin
      match stack with
      | payload :: tail -> flow rest (SSum payload :: tail)
      | [] -> None
    end
  | Pack (cap, typ, rest) ->
    begin
      match stack, shape_of typ with
      | SVec (len, elem) :: tail, Some found
          when C_nat.le len cap && same_shape elem found ->
        flow rest (SPair (SAtom, SVec (cap, elem)) :: tail)
      | _ -> None
    end
  | Fit (typ, rest) ->
    begin
      match stack, typ with
      | SAtom :: tail, C_type.Num _ when C_type.valid typ ->
        flow rest (SSum SAtom :: tail)
      | _ -> None
    end
  | Wide rest ->
    begin
      match stack with
      | SAtom :: tail -> flow rest (SAtom :: tail)
      | _ -> None
    end
  | Close (kind, rest) ->
    begin
      match stack with
      | SCap found :: tail when C_nat.equal kind found ->
        flow rest (SUnit :: tail)
      | _ -> None
    end
  | Effect (_, _, body, rest) ->
    Option.bind (flow body stack) (flow rest)
  | Scope (_, body, rest) ->
    begin
      match stack with
      | _ :: tail ->
        Option.bind (flow body tail) (fun after ->
          Option.bind (keep_shape tail after) (fun out ->
            flow rest (out :: tail)))
      | [] -> None
    end
  | Scope2 (_, _, body, rest) ->
    begin
      match stack with
      | SPair _ :: tail ->
        Option.bind (flow body tail) (fun after ->
          Option.bind (keep_shape tail after) (fun out ->
            flow rest (out :: tail)))
      | _ -> None
    end
  | Iter (len, item, state, body, rest) ->
    begin
      match item.C_term.mul, state.C_term.mul, stack,
          shape_of item.C_term.typ, shape_of state.C_term.typ with
      | C_type.Zero, _, _, _, _ | _, C_type.Zero, _, _, _ -> None
      | _, _, seed :: SVec (found, elem) :: tail, Some item_shape,
          Some state_shape
          when C_nat.equal len found
            && same_shape item_shape elem
            && same_shape state_shape seed ->
        Option.bind (flow body tail) (fun after ->
          Option.bind (keep_shape tail after) (fun out ->
            if same_shape seed out then flow rest (out :: tail) else None))
      | _ -> None
    end
  | Iter_seq (cap, item, state, body, rest) ->
    begin
      match item.C_term.mul, state.C_term.mul, stack,
          shape_of item.C_term.typ, shape_of state.C_term.typ with
      | C_type.Zero, _, _, _, _ | _, C_type.Zero, _, _, _ -> None
      | _, _, seed :: SPair (SAtom, SVec (found, elem)) :: tail,
          Some item_shape, Some state_shape
          when C_nat.equal cap found
            && same_shape item_shape elem
            && same_shape state_shape seed ->
        Option.bind (flow body tail) (fun after ->
          Option.bind (keep_shape tail after) (fun out ->
            if same_shape seed out then flow rest (out :: tail) else None))
      | _ -> None
    end
  | Choice (left, yes, right, no, form, rest) ->
    begin
      match left.C_term.mul, right.C_term.mul, stack,
          shape_of left.C_term.typ, shape_of right.C_term.typ with
      | C_type.Zero, _, _, _, _ | _, C_type.Zero, _, _, _ -> None
      | _, _, SSum payload :: tail, Some left_shape, Some right_shape
          when same_shape payload left_shape
            && same_shape payload right_shape ->
        Option.bind (flow yes tail) (fun yafter ->
          Option.bind (flow no tail) (fun nafter ->
            Option.bind (keep_shape tail yafter) (fun yout ->
              Option.bind (keep_shape tail nafter) (fun nout ->
                if same_shape form yout && same_shape form nout then
                  flow rest (form :: tail)
                else None))))
      | _ -> None
    end
  | Fork (form, yes, no, rest) ->
    begin
      match stack with
      | SAtom :: tail ->
        Option.bind (flow yes tail) (fun yafter ->
          Option.bind (flow no tail) (fun nafter ->
            Option.bind (keep_shape tail yafter) (fun yout ->
              Option.bind (keep_shape tail nafter) (fun nout ->
                if same_shape form yout && same_shape form nout then
                  flow rest (form :: tail)
                else None))))
      | _ -> None
    end

let one_shape code =
  match flow code [] with
  | Some [value] -> Some value
  | _ -> None

let rec find_bind id = function
  | [] -> None
  | item :: _ when C_nat.equal id item.C_term.id -> Some item
  | _ :: rest -> find_bind id rest

let rec continue code tail =
  match code with
  | Done -> tail
  | Push (value, rest) -> Push (value, continue rest tail)
  | Void rest -> Void (continue rest tail)
  | Get (id, shape, rest) -> Get (id, shape, continue rest tail)
  | Plus rest -> Plus (continue rest tail)
  | Minus rest -> Minus (continue rest tail)
  | Times rest -> Times (continue rest tail)
  | Quot rest -> Quot (continue rest tail)
  | Rem rest -> Rem (continue rest tail)
  | Negate rest -> Negate (continue rest tail)
  | Absolute rest -> Absolute (continue rest tail)
  | Same rest -> Same (continue rest tail)
  | Different rest -> Different (continue rest tail)
  | Order (rel, rest) -> Order (rel, continue rest tail)
  | Join rest -> Join (continue rest tail)
  | Clip (len, rest) -> Clip (len, continue rest tail)
  | Skip (len, rest) -> Skip (len, continue rest tail)
  | Duo rest -> Duo (continue rest tail)
  | First rest -> First (continue rest tail)
  | Second rest -> Second (continue rest tail)
  | Empty (shape, rest) -> Empty (shape, continue rest tail)
  | Cons rest -> Cons (continue rest tail)
  | Append rest -> Append (continue rest tail)
  | Pick (index, rest) -> Pick (index, continue rest tail)
  | Unhead rest -> Unhead (continue rest tail)
  | Left rest -> Left (continue rest tail)
  | Right rest -> Right (continue rest tail)
  | Pack (cap, typ, rest) -> Pack (cap, typ, continue rest tail)
  | Fit (typ, rest) -> Fit (typ, continue rest tail)
  | Wide rest -> Wide (continue rest tail)
  | Close (kind, rest) -> Close (kind, continue rest tail)
  | Effect (index, atom, body, rest) ->
    Effect (index, atom, body, continue rest tail)
  | Scope (bind, body, rest) ->
    Scope (bind, body, continue rest tail)
  | Scope2 (left, right, body, rest) ->
    Scope2 (left, right, body, continue rest tail)
  | Iter (len, item, state, body, rest) ->
    Iter (len, item, state, body, continue rest tail)
  | Iter_seq (cap, item, state, body, rest) ->
    Iter_seq (cap, item, state, body, continue rest tail)
  | Choice (left, yes, right, no, shape, rest) ->
    Choice (left, yes, right, no, shape, continue rest tail)
  | Fork (shape, yes, no, rest) ->
    Fork (shape, yes, no, continue rest tail)

let rec build env term rest =
  match term with
  | C_term.Unit -> Some (Void rest)
  | C_term.Bool value -> Some (Push (C_emit.Bool value, rest))
  | C_term.Int value -> Some (Push (C_emit.Int value, rest))
  | C_term.Narrow (typ, value) ->
    if C_type.admits typ value then Some (Push (C_emit.Int value, rest))
    else None
  | C_term.Bytes value -> Some (Push (C_emit.Bytes value, rest))
  | C_term.Vec (elem, values) ->
    Option.bind (shape_of elem) (fun form ->
      build_vec env form values rest)
  | C_term.Seq (cap, elem, values) ->
    Option.bind (shape_of elem) (fun form ->
      build_vec env form values (Pack (cap, elem, rest)))
  | C_term.Var id ->
    Option.bind (find_bind id env) (fun item ->
      Option.map (fun form -> Get (id, form, rest)) (shape_of item.typ))
  | C_term.Let (item, value, body) ->
    begin
      match item.mul with
      | C_type.Zero -> build (item :: env) body rest
      | C_type.One | C_type.Many ->
        Option.bind (build (item :: env) body Done) (fun body_code ->
          build env value (Scope (item, body_code, rest)))
    end
  | C_term.If (guard, yes, no) ->
    Option.bind (build env yes Done) (fun yes_code ->
      Option.bind (build env no Done) (fun no_code ->
        Option.bind (one_shape yes_code) (fun yes_shape ->
          Option.bind (one_shape no_code) (fun no_shape ->
            if same_shape yes_shape no_shape then
              build env guard (Fork (yes_shape, yes_code, no_code, rest))
            else None))))
  | C_term.Pair (lhs, rhs) ->
    Option.bind (build env rhs (Duo rest)) (fun rhs_code ->
      build env lhs rhs_code)
  | C_term.Unpair (value, lhs, rhs, body) ->
    Option.bind (build (rhs :: lhs :: env) body Done) (fun body_code ->
      build env value (Scope2 (lhs, rhs, body_code, rest)))
  | C_term.Fst value -> build env value (First rest)
  | C_term.Snd value -> build env value (Second rest)
  | C_term.Add (lhs, rhs) ->
    Option.bind (build env rhs (Plus rest)) (fun rhs_code ->
      build env lhs rhs_code)
  | C_term.Sub (lhs, rhs) ->
    Option.bind (build env rhs (Minus rest)) (fun rhs_code ->
      build env lhs rhs_code)
  | C_term.Mul (lhs, rhs) ->
    Option.bind (build env rhs (Times rest)) (fun rhs_code ->
      build env lhs rhs_code)
  | C_term.Div (lhs, rhs) ->
    Option.bind (build env rhs (Quot rest)) (fun rhs_code ->
      build env lhs rhs_code)
  | C_term.Mod (lhs, rhs) ->
    Option.bind (build env rhs (Rem rest)) (fun rhs_code ->
      build env lhs rhs_code)
  | C_term.Neg value -> build env value (Negate rest)
  | C_term.Abs value -> build env value (Absolute rest)
  | C_term.Fit (typ, value) -> build env value (Fit (typ, rest))
  | C_term.Wide value -> build env value (Wide rest)
  | C_term.Length value -> build env value (First rest)
  | C_term.Eq (C_type.Bool,
      C_term.Eq (C_type.Int, lhs, rhs), C_term.Bool false) ->
    Option.bind (build env rhs (Different rest)) (fun rhs_code ->
      build env lhs rhs_code)
  | C_term.Eq (typ, lhs, rhs) ->
    begin
      match shape_of typ with
      | Some SAtom ->
        Option.bind (build env rhs (Same rest)) (fun rhs_code ->
          build env lhs rhs_code)
      | _ -> None
    end
  | C_term.Cmp (rel, lhs, rhs) ->
    Option.bind (build env rhs (Order (rel, rest))) (fun rhs_code ->
      build env lhs rhs_code)
  | C_term.Cat (lhs, rhs) ->
    Option.bind (build env rhs (Join rest)) (fun rhs_code ->
      build env lhs rhs_code)
  | C_term.Take (len, value) -> build env value (Clip (len, rest))
  | C_term.Drop (len, value) -> build env value (Skip (len, rest))
  | C_term.Vcat (lhs, rhs) ->
    Option.bind (build env rhs (Append rest)) (fun rhs_code ->
      build env lhs rhs_code)
  | C_term.At (index, value) -> build env value (Pick (index, rest))
  | C_term.Uncons value -> build env value (Unhead rest)
  | C_term.Act (atom, body) ->
    Option.map (fun body -> Effect (-1, atom, body, rest)) (build env body Done)
  | C_term.Close value ->
    Option.bind (build env value Done) (fun value_code ->
      match one_shape value_code with
      | Some (SCap kind) -> Some (continue value_code (Close (kind, rest)))
      | _ -> None)
  | C_term.Inl (value, right) ->
    Option.bind (shape_of right) (fun right_shape ->
      Option.bind (build env value Done) (fun value_code ->
        match one_shape value_code with
        | Some value_shape when same_shape value_shape right_shape ->
          Some (continue value_code (Left rest))
        | _ -> None))
  | C_term.Inr (left, value) ->
    Option.bind (shape_of left) (fun left_shape ->
      Option.bind (build env value Done) (fun value_code ->
        match one_shape value_code with
        | Some value_shape when same_shape value_shape left_shape ->
          Some (continue value_code (Right rest))
        | _ -> None))
  | C_term.Case (value, left, yes, right, no) ->
    begin
      match left.mul, right.mul with
      | C_type.Zero, _ | _, C_type.Zero -> None
      | C_type.One, _ | C_type.Many, _ ->
        Option.bind (build (left :: env) yes Done) (fun yes_code ->
          Option.bind (build (right :: env) no Done) (fun no_code ->
            match one_shape yes_code, one_shape no_code with
            | Some yes_shape, Some no_shape when same_shape yes_shape no_shape ->
              build env value
                (Choice (left, yes_code, right, no_code, yes_shape, rest))
            | _ -> None))
    end
  | C_term.Vfold (vector, seed, fold) ->
    begin
      match fold.item.mul, fold.state.mul with
      | C_type.Zero, _ | _, C_type.Zero -> None
      | C_type.One, _ | C_type.Many, _ ->
        Option.bind (build env vector Done) (fun vector_code ->
          match one_shape vector_code, shape_of fold.item.typ,
              shape_of fold.state.typ with
          | Some (SVec (len, elem)), Some item_shape, Some state_shape
              when same_shape elem item_shape ->
            Option.bind
              (build (fold.state :: fold.item :: env) fold.body Done)
              (fun body_code ->
                match one_shape body_code with
                | Some body_shape when same_shape state_shape body_shape ->
                  Option.bind
                    (build env seed
                      (Iter (len, fold.item, fold.state, body_code, rest)))
                    (fun seed_code -> Some (continue vector_code seed_code))
                | _ -> None)
          | Some (SPair (SAtom, SVec (cap, elem))), Some item_shape,
              Some state_shape when same_shape elem item_shape ->
            Option.bind
              (build (fold.state :: fold.item :: env) fold.body Done)
              (fun body_code ->
                match one_shape body_code with
                | Some body_shape when same_shape state_shape body_shape ->
                  Option.bind
                    (build env seed
                      (Iter_seq (cap, fold.item, fold.state, body_code, rest)))
                    (fun seed_code -> Some (continue vector_code seed_code))
                | _ -> None)
          | _ -> None)
    end
  | C_term.Step _ -> None

and build_vec env elem values rest =
  match values with
  | [] -> Some (Empty (elem, rest))
  | first :: tail ->
    Option.bind (build_vec env elem tail (Cons rest)) (fun tail_code ->
      build env first tail_code)

let rec index_effects index = function
  | Done -> Done, index
  | Push (value, rest) ->
    let rest, index = index_effects index rest in
    Push (value, rest), index
  | Void rest ->
    let rest, index = index_effects index rest in
    Void rest, index
  | Get (id, shape, rest) ->
    let rest, index = index_effects index rest in
    Get (id, shape, rest), index
  | Plus rest ->
    let rest, index = index_effects index rest in
    Plus rest, index
  | Minus rest ->
    let rest, index = index_effects index rest in
    Minus rest, index
  | Times rest ->
    let rest, index = index_effects index rest in
    Times rest, index
  | Quot rest ->
    let rest, index = index_effects index rest in
    Quot rest, index
  | Rem rest ->
    let rest, index = index_effects index rest in
    Rem rest, index
  | Negate rest ->
    let rest, index = index_effects index rest in
    Negate rest, index
  | Absolute rest ->
    let rest, index = index_effects index rest in
    Absolute rest, index
  | Same rest ->
    let rest, index = index_effects index rest in
    Same rest, index
  | Different rest ->
    let rest, index = index_effects index rest in
    Different rest, index
  | Order (rel, rest) ->
    let rest, index = index_effects index rest in
    Order (rel, rest), index
  | Join rest ->
    let rest, index = index_effects index rest in
    Join rest, index
  | Clip (len, rest) ->
    let rest, index = index_effects index rest in
    Clip (len, rest), index
  | Skip (len, rest) ->
    let rest, index = index_effects index rest in
    Skip (len, rest), index
  | Duo rest ->
    let rest, index = index_effects index rest in
    Duo rest, index
  | First rest ->
    let rest, index = index_effects index rest in
    First rest, index
  | Second rest ->
    let rest, index = index_effects index rest in
    Second rest, index
  | Empty (shape, rest) ->
    let rest, index = index_effects index rest in
    Empty (shape, rest), index
  | Cons rest ->
    let rest, index = index_effects index rest in
    Cons rest, index
  | Append rest ->
    let rest, index = index_effects index rest in
    Append rest, index
  | Pick (at, rest) ->
    let rest, index = index_effects index rest in
    Pick (at, rest), index
  | Unhead rest ->
    let rest, index = index_effects index rest in
    Unhead rest, index
  | Left rest ->
    let rest, index = index_effects index rest in
    Left rest, index
  | Right rest ->
    let rest, index = index_effects index rest in
    Right rest, index
  | Pack (cap, typ, rest) ->
    let rest, index = index_effects index rest in
    Pack (cap, typ, rest), index
  | Fit (typ, rest) ->
    let rest, index = index_effects index rest in
    Fit (typ, rest), index
  | Wide rest ->
    let rest, index = index_effects index rest in
    Wide rest, index
  | Close (kind, rest) ->
    let rest, index = index_effects index rest in
    Close (kind, rest), index
  | Effect (_, atom, body, rest) ->
    let at = index in
    let body, index = index_effects (index + 1) body in
    let rest, index = index_effects index rest in
    Effect (at, atom, body, rest), index
  | Scope (bind, body, rest) ->
    let body, index = index_effects index body in
    let rest, index = index_effects index rest in
    Scope (bind, body, rest), index
  | Scope2 (left, right, body, rest) ->
    let body, index = index_effects index body in
    let rest, index = index_effects index rest in
    Scope2 (left, right, body, rest), index
  | Iter (len, item, state, body, rest) ->
    let body, index = index_effects index body in
    let rest, index = index_effects index rest in
    Iter (len, item, state, body, rest), index
  | Iter_seq (cap, item, state, body, rest) ->
    let body, index = index_effects index body in
    let rest, index = index_effects index rest in
    Iter_seq (cap, item, state, body, rest), index
  | Choice (left, yes, right, no, shape, rest) ->
    let yes, index = index_effects index yes in
    let no, index = index_effects index no in
    let rest, index = index_effects index rest in
    Choice (left, yes, right, no, shape, rest), index
  | Fork (shape, yes, no, rest) ->
    let yes, index = index_effects index yes in
    let no, index = index_effects index no in
    let rest, index = index_effects index rest in
    Fork (shape, yes, no, rest), index

let size_limit = C_eval.max_cost
let size_stop = Z.succ size_limit

let size_add left right =
  Z.min size_stop (Z.add left right)

let size_scale count value =
  Z.min size_stop (Z.mul count value)

let rec code_size = function
  | Done -> Z.one
  | Push (_, rest) | Void rest | Get (_, _, rest) | Plus rest | Minus rest
  | Times rest | Quot rest | Rem rest | Negate rest | Absolute rest | Same rest
  | Different rest
  | Order (_, rest) | Join rest | Clip (_, rest) | Skip (_, rest) | Duo rest
  | First rest | Second rest | Empty (_, rest) | Cons rest | Append rest
  | Pick (_, rest) | Unhead rest | Left rest | Right rest | Pack (_, _, rest)
  | Fit (_, rest) | Wide rest | Close (_, rest) ->
    size_add Z.one (code_size rest)
  | Effect (_, _, body, rest) | Scope (_, body, rest)
  | Scope2 (_, _, body, rest) ->
    size_add Z.one (size_add (code_size body) (code_size rest))
  | Iter (len, _, _, body, rest) ->
    size_add Z.one
      (size_add
        (size_scale (C_nat.to_z len) (code_size body))
        (code_size rest))
  | Iter_seq (cap, _, _, body, rest) ->
    size_add Z.one
      (size_add
        (size_scale (C_nat.to_z cap) (code_size body))
        (code_size rest))
  | Choice (_, yes, _, no, _, rest) | Fork (_, yes, no, rest) ->
    size_add Z.one
      (size_add (code_size yes)
        (size_add (code_size no) (code_size rest)))

let lower_in inputs term =
  match build inputs term Done with
  | Some code when Z.leq (code_size code) size_limit ->
    Some (fst (index_effects 0 code))
  | Some _ | None -> None
let lower term = lower_in [] term

let rec locate at = function
  | Done -> LDone
  | Push (_, rest) -> LPush (at, locate at rest)
  | Void rest -> LVoid (at, locate at rest)
  | Get (_, _, rest) -> LGet (at, locate at rest)
  | Plus rest -> LPlus (at, locate at rest)
  | Minus rest -> LMinus (at, locate at rest)
  | Times rest -> LTimes (at, locate at rest)
  | Quot rest -> LQuot (at, locate at rest)
  | Rem rest -> LRem (at, locate at rest)
  | Negate rest -> LNegate (at, locate at rest)
  | Absolute rest -> LAbsolute (at, locate at rest)
  | Same rest -> LSame (at, locate at rest)
  | Different rest -> LDifferent (at, locate at rest)
  | Order (rel, rest) -> LOrder (rel, at, locate at rest)
  | Join rest -> LJoin (at, locate at rest)
  | Clip (len, rest) -> LClip (len, at, locate at rest)
  | Skip (len, rest) -> LSkip (len, at, locate at rest)
  | Duo rest -> LDuo (at, locate at rest)
  | First rest -> LFirst (at, locate at rest)
  | Second rest -> LSecond (at, locate at rest)
  | Empty (_, rest) -> LEmpty (at, locate at rest)
  | Cons rest -> LCons (at, locate at rest)
  | Append rest -> LAppend (at, locate at rest)
  | Pick (index, rest) -> LPick (index, at, locate at rest)
  | Unhead rest -> LUnhead (at, locate at rest)
  | Left rest -> LLeft (at, locate at rest)
  | Right rest -> LRight (at, locate at rest)
  | Pack (_, _, rest) -> LPack (at, locate at rest)
  | Fit (_, rest) -> LFit (at, locate at rest)
  | Wide rest -> LWide (at, locate at rest)
  | Close (_, rest) -> LClose (at, locate at rest)
  | Effect (_, _, body, rest) ->
    LEffect (at, locate at body, locate at rest)
  | Scope (_, body, rest) -> LScope (at, locate at body, locate at rest)
  | Scope2 (_, _, body, rest) ->
    LScope2 (at, locate at body, locate at rest)
  | Iter (_, _, _, body, rest) ->
    LIter (at, locate at body, locate at rest)
  | Iter_seq (_, _, _, body, rest) ->
    LIter (at, locate at body, locate at rest)
  | Choice (_, yes, _, no, _, rest) ->
    LChoice (at, at, at, locate at yes, locate at no, locate at rest)
  | Fork (_, yes, no, rest) ->
    LFork (at, at, at, locate at yes, locate at no, locate at rest)

let root sub = function
  | Some at :: rest -> Ok ({ at; sub }, rest)
  | None :: _ | [] -> Error Map

let rec tree term marks =
  match term with
  | C_term.Unit | C_term.Bool _ | C_term.Int _ | C_term.Narrow _
  | C_term.Bytes _
  | C_term.Var _ ->
      root [] marks
  | C_term.Vec (_, values) | C_term.Seq (_, _, values) ->
    let* values, marks = trees values marks in
    root values marks
  | C_term.Let (_, value, body) ->
      let* value, marks = tree value marks in
      let* body, marks = tree body marks in
      root [value; body] marks
  | C_term.If (guard, yes, no) ->
      let* guard, marks = tree guard marks in
      let* yes, marks = tree yes marks in
      let* no, marks = tree no marks in
      root [guard; yes; no] marks
  | C_term.Add (left, right) | C_term.Sub (left, right)
  | C_term.Mul (left, right) | C_term.Div (left, right)
  | C_term.Mod (left, right) | C_term.Eq (_, left, right)
  | C_term.Cmp (_, left, right)
  | C_term.Cat (left, right) | C_term.Pair (left, right)
  | C_term.Vcat (left, right) ->
      let* left, marks = tree left marks in
      let* right, marks = tree right marks in
      root [left; right] marks
  | C_term.Unpair (value, _, _, body) ->
    let* value, marks = tree value marks in
    let* body, marks = tree body marks in
    root [value; body] marks
  | C_term.Fst value | C_term.Snd value | C_term.Neg value
  | C_term.Abs value | C_term.Fit (_, value) | C_term.Wide value
  | C_term.Length value | C_term.Take (_, value)
  | C_term.Drop (_, value) | C_term.At (_, value) | C_term.Uncons value
  | C_term.Inl (value, _) | C_term.Inr (_, value) | C_term.Close value ->
      let* value, marks = tree value marks in
      root [value] marks
  | C_term.Case (value, _, yes, _, no) ->
    let* value, marks = tree value marks in
    let* yes, marks = tree yes marks in
    let* no, marks = tree no marks in
    root [value; yes; no] marks
  | C_term.Act (_, body) ->
      let* body, marks = tree body marks in
      root [body] marks
  | C_term.Vfold (vector, seed, fold) ->
    let* vector, marks = tree vector marks in
    let* seed, marks = tree seed marks in
    let* body, marks = tree fold.body marks in
    root [vector; seed; body] marks
  | C_term.Step _ -> Error Map

and trees terms marks =
  match terms with
  | [] -> Ok ([], marks)
  | first :: rest ->
    let* first, marks = tree first marks in
    let* rest, marks = trees rest marks in
    Ok (first :: rest, marks)

let rec spread at = function
  | C_term.Unit | C_term.Bool _ | C_term.Int _ | C_term.Narrow _
  | C_term.Bytes _
  | C_term.Var _ -> { at; sub = [] }
  | C_term.Vec (_, values) | C_term.Seq (_, _, values) ->
    { at; sub = List.map (spread at) values }
  | C_term.Let (_, value, body) ->
    { at; sub = [spread at value; spread at body] }
  | C_term.If (guard, yes, no) ->
    { at; sub = [spread at guard; spread at yes; spread at no] }
  | C_term.Pair (left, right) | C_term.Add (left, right)
  | C_term.Sub (left, right) | C_term.Mul (left, right)
  | C_term.Div (left, right) | C_term.Mod (left, right)
  | C_term.Eq (_, left, right) | C_term.Cmp (_, left, right)
  | C_term.Cat (left, right)
  | C_term.Vcat (left, right) ->
    { at; sub = [spread at left; spread at right] }
  | C_term.Unpair (value, _, _, body) ->
    { at; sub = [spread at value; spread at body] }
  | C_term.Fst value | C_term.Snd value | C_term.Neg value
  | C_term.Abs value | C_term.Fit (_, value) | C_term.Wide value
  | C_term.Length value | C_term.Take (_, value)
  | C_term.Drop (_, value) | C_term.At (_, value) | C_term.Uncons value
  | C_term.Inl (value, _) | C_term.Inr (_, value) | C_term.Close value ->
    { at; sub = [spread at value] }
  | C_term.Case (value, _, yes, _, no) ->
    { at; sub = [spread at value; spread at yes; spread at no] }
  | C_term.Act (_, body) -> { at; sub = [spread at body] }
  | C_term.Vfold (vector, seed, fold) ->
    { at; sub = [spread at vector; spread at seed; spread at fold.body] }
  | C_term.Step (cap, value) ->
    { at; sub = [spread at cap; spread at value] }

let rec retree source target mark =
  let node sub = Some { at = mark.at; sub } in
  match source, target, mark.sub with
  | C_term.Var _, _, [] -> Some (spread mark.at target)
  | C_term.Unit, C_term.Unit, []
  | C_term.Bool _, C_term.Bool _, []
  | C_term.Int _, C_term.Int _, []
  | C_term.Narrow _, C_term.Narrow _, []
  | C_term.Bytes _, C_term.Bytes _, [] -> Some mark
  | C_term.Vec (_, left), C_term.Vec (_, right), marks ->
    Option.bind (retrees left right marks) node
  | C_term.Seq (left_cap, left_typ, left),
      C_term.Seq (right_cap, right_typ, right), marks
      when C_nat.equal left_cap right_cap && C_type.equal left_typ right_typ ->
    Option.bind (retrees left right marks) node
  | C_term.Let (_, left_value, left_body),
      C_term.Let (_, right_value, right_body), [value_mark; body_mark] ->
    Option.bind (retree left_value right_value value_mark) (fun value ->
      Option.bind (retree left_body right_body body_mark) (fun body ->
        node [value; body]))
  | C_term.If (left_guard, left_yes, left_no),
      C_term.If (right_guard, right_yes, right_no),
      [guard_mark; yes_mark; no_mark] ->
    Option.bind (retree left_guard right_guard guard_mark) (fun guard ->
      Option.bind (retree left_yes right_yes yes_mark) (fun yes ->
        Option.bind (retree left_no right_no no_mark) (fun no ->
          node [guard; yes; no])))
  | C_term.Pair (left_first, left_second),
      C_term.Pair (right_first, right_second), [first_mark; second_mark]
  | C_term.Add (left_first, left_second),
      C_term.Add (right_first, right_second), [first_mark; second_mark]
  | C_term.Sub (left_first, left_second),
      C_term.Sub (right_first, right_second), [first_mark; second_mark]
  | C_term.Mul (left_first, left_second),
      C_term.Mul (right_first, right_second), [first_mark; second_mark]
  | C_term.Div (left_first, left_second),
      C_term.Div (right_first, right_second), [first_mark; second_mark]
  | C_term.Mod (left_first, left_second),
      C_term.Mod (right_first, right_second), [first_mark; second_mark]
  | C_term.Eq (_, left_first, left_second),
      C_term.Eq (_, right_first, right_second), [first_mark; second_mark]
  | C_term.Cat (left_first, left_second),
      C_term.Cat (right_first, right_second), [first_mark; second_mark]
  | C_term.Vcat (left_first, left_second),
      C_term.Vcat (right_first, right_second), [first_mark; second_mark] ->
    Option.bind (retree left_first right_first first_mark) (fun first ->
      Option.bind (retree left_second right_second second_mark) (fun second ->
        node [first; second]))
  | C_term.Cmp (left_rel, left_first, left_second),
      C_term.Cmp (right_rel, right_first, right_second),
      [first_mark; second_mark] when left_rel = right_rel ->
    Option.bind (retree left_first right_first first_mark) (fun first ->
      Option.bind (retree left_second right_second second_mark) (fun second ->
        node [first; second]))
  | C_term.Unpair (left_value, _, _, left_body),
      C_term.Unpair (right_value, _, _, right_body), [value_mark; body_mark] ->
    Option.bind (retree left_value right_value value_mark) (fun value ->
      Option.bind (retree left_body right_body body_mark) (fun body ->
        node [value; body]))
  | C_term.Fst left, C_term.Fst right, [child]
  | C_term.Snd left, C_term.Snd right, [child]
  | C_term.Neg left, C_term.Neg right, [child]
  | C_term.Abs left, C_term.Abs right, [child]
  | C_term.Take (_, left), C_term.Take (_, right), [child]
  | C_term.Drop (_, left), C_term.Drop (_, right), [child]
  | C_term.At (_, left), C_term.At (_, right), [child]
  | C_term.Uncons left, C_term.Uncons right, [child] ->
    Option.bind (retree left right child) (fun value -> node [value])
  | C_term.Fit (left_typ, left), C_term.Fit (right_typ, right), [child]
      when C_type.equal left_typ right_typ ->
    Option.bind (retree left right child) (fun value -> node [value])
  | C_term.Wide left, C_term.Wide right, [child]
  | C_term.Length left, C_term.Length right, [child] ->
    Option.bind (retree left right child) (fun value -> node [value])
  | C_term.Inl (left, _), C_term.Inl (right, _), [child]
  | C_term.Inr (_, left), C_term.Inr (_, right), [child] ->
    Option.bind (retree left right child) (fun value -> node [value])
  | C_term.Close left, C_term.Close right, [child] ->
    Option.bind (retree left right child) (fun value -> node [value])
  | C_term.Case (left_value, _, left_yes, _, left_no),
      C_term.Case (right_value, _, right_yes, _, right_no),
      [value_mark; yes_mark; no_mark] ->
    Option.bind (retree left_value right_value value_mark) (fun value ->
      Option.bind (retree left_yes right_yes yes_mark) (fun yes ->
        Option.bind (retree left_no right_no no_mark) (fun no ->
          node [value; yes; no])))
  | C_term.Act (left_atom, left), C_term.Act (right_atom, right), [child]
      when left_atom = right_atom ->
    Option.bind (retree left right child) (fun body -> node [body])
  | C_term.Vfold (left_vector, left_seed, left_fold),
      C_term.Vfold (right_vector, right_seed, right_fold),
      [vector_mark; seed_mark; body_mark] ->
    Option.bind (retree left_vector right_vector vector_mark) (fun vector ->
      Option.bind (retree left_seed right_seed seed_mark) (fun seed ->
        Option.bind
          (retree left_fold.body right_fold.body body_mark)
          (fun body -> node [vector; seed; body])))
  | _ -> None

and retrees sources targets marks =
  match sources, targets, marks with
  | [], [], [] -> Some []
  | source :: sources, target :: targets, mark :: marks ->
    Option.bind (retree source target mark) (fun first ->
      Option.map (fun rest -> first :: rest) (retrees sources targets marks))
  | _ -> None

let rec into_loc term mark rest =
  match term, mark.sub with
  | C_term.Unit, [] -> Some (LVoid (mark.at, rest))
  | (C_term.Bool _ | C_term.Int _ | C_term.Narrow _ | C_term.Bytes _), [] ->
      Some (LPush (mark.at, rest))
  | C_term.Vec (_, values), marks -> loc_vec values marks mark.at rest
  | C_term.Seq (_, _, values), marks ->
    loc_vec values marks mark.at (LPack (mark.at, rest))
  | C_term.Var _, [] -> Some (LGet (mark.at, rest))
  | C_term.Let (item, value, body), [value_mark; body_mark] ->
      begin
        match into_loc body body_mark LDone with
        | None -> None
        | Some body_loc ->
          begin
            match item.mul with
            | C_type.Zero -> into_loc body body_mark rest
            | C_type.One | C_type.Many ->
              into_loc value value_mark (LScope (mark.at, body_loc, rest))
          end
      end
  | C_term.If (guard, yes, no), [guard_mark; yes_mark; no_mark] ->
      begin
        match into_loc yes yes_mark LDone, into_loc no no_mark LDone with
        | Some yes_loc, Some no_loc ->
          into_loc guard guard_mark
            (LFork (mark.at, yes_mark.at, no_mark.at,
              yes_loc, no_loc, rest))
        | _ -> None
      end
  | C_term.Pair (left, right), [left_mark; right_mark] ->
    begin
      match into_loc right right_mark (LDuo (mark.at, rest)) with
      | Some right_loc -> into_loc left left_mark right_loc
      | None -> None
    end
  | C_term.Unpair (value, _, _, body), [value_mark; body_mark] ->
    begin
      match into_loc body body_mark LDone with
      | Some body_loc ->
        into_loc value value_mark (LScope2 (mark.at, body_loc, rest))
      | None -> None
    end
  | C_term.Fst value, [value_mark] ->
    into_loc value value_mark (LFirst (mark.at, rest))
  | C_term.Snd value, [value_mark] ->
    into_loc value value_mark (LSecond (mark.at, rest))
  | C_term.Add (left, right), [left_mark; right_mark] ->
      begin
        match into_loc right right_mark (LPlus (mark.at, rest)) with
        | Some right_loc -> into_loc left left_mark right_loc
        | None -> None
      end
  | C_term.Sub (left, right), [left_mark; right_mark] ->
      begin
        match into_loc right right_mark (LMinus (mark.at, rest)) with
        | Some right_loc -> into_loc left left_mark right_loc
        | None -> None
      end
  | C_term.Mul (left, right), [left_mark; right_mark] ->
      begin
        match into_loc right right_mark (LTimes (mark.at, rest)) with
        | Some right_loc -> into_loc left left_mark right_loc
        | None -> None
      end
  | C_term.Div (left, right), [left_mark; right_mark] ->
      begin
        match into_loc right right_mark (LQuot (mark.at, rest)) with
        | Some right_loc -> into_loc left left_mark right_loc
        | None -> None
      end
  | C_term.Mod (left, right), [left_mark; right_mark] ->
      begin
        match into_loc right right_mark (LRem (mark.at, rest)) with
        | Some right_loc -> into_loc left left_mark right_loc
        | None -> None
      end
  | C_term.Neg value, [value_mark] ->
      into_loc value value_mark (LNegate (mark.at, rest))
  | C_term.Abs value, [value_mark] ->
      into_loc value value_mark (LAbsolute (mark.at, rest))
  | C_term.Fit (_, value), [value_mark] ->
    into_loc value value_mark (LFit (mark.at, rest))
  | C_term.Wide value, [value_mark] ->
    into_loc value value_mark (LWide (mark.at, rest))
  | C_term.Length value, [value_mark] ->
    into_loc value value_mark (LFirst (mark.at, rest))
  | C_term.Eq (C_type.Bool,
      C_term.Eq (C_type.Int, left, right), C_term.Bool false),
      [{ sub = [left_mark; right_mark]; _ }; _] ->
    begin
      match into_loc right right_mark (LDifferent (mark.at, rest)) with
      | Some right_loc -> into_loc left left_mark right_loc
      | None -> None
    end
  | C_term.Eq (_, left, right), [left_mark; right_mark] ->
    begin
      match into_loc right right_mark (LSame (mark.at, rest)) with
      | Some right_loc -> into_loc left left_mark right_loc
      | None -> None
    end
  | C_term.Cmp (rel, left, right), [left_mark; right_mark] ->
    begin
      match into_loc right right_mark (LOrder (rel, mark.at, rest)) with
      | Some right_loc -> into_loc left left_mark right_loc
      | None -> None
    end
  | C_term.Cat (left, right), [left_mark; right_mark] ->
    begin
      match into_loc right right_mark (LJoin (mark.at, rest)) with
      | Some right_loc -> into_loc left left_mark right_loc
      | None -> None
    end
  | C_term.Take (len, value), [value_mark] ->
      into_loc value value_mark (LClip (len, mark.at, rest))
  | C_term.Drop (len, value), [value_mark] ->
      into_loc value value_mark (LSkip (len, mark.at, rest))
  | C_term.Vcat (left, right), [left_mark; right_mark] ->
    begin
      match into_loc right right_mark (LAppend (mark.at, rest)) with
      | Some right_loc -> into_loc left left_mark right_loc
      | None -> None
    end
  | C_term.At (index, value), [value_mark] ->
    into_loc value value_mark (LPick (index, mark.at, rest))
  | C_term.Uncons value, [value_mark] ->
    into_loc value value_mark (LUnhead (mark.at, rest))
  | C_term.Inl (value, _), [value_mark] ->
    into_loc value value_mark (LLeft (mark.at, rest))
  | C_term.Inr (_, value), [value_mark] ->
    into_loc value value_mark (LRight (mark.at, rest))
  | C_term.Close value, [value_mark] ->
    into_loc value value_mark (LClose (mark.at, rest))
  | C_term.Case (value, _, yes, _, no),
      [value_mark; yes_mark; no_mark] ->
    begin
      match into_loc yes yes_mark LDone, into_loc no no_mark LDone with
      | Some yes_loc, Some no_loc ->
        into_loc value value_mark
          (LChoice (mark.at, yes_mark.at, no_mark.at,
            yes_loc, no_loc, rest))
      | _ -> None
    end
  | C_term.Act (_, body), [body_mark] ->
    Option.map
      (fun body_loc -> LEffect (mark.at, body_loc, rest))
      (into_loc body body_mark LDone)
  | C_term.Vfold (vector, seed, fold),
      [vector_mark; seed_mark; body_mark] ->
    begin
      match into_loc fold.body body_mark LDone with
      | Some body_loc ->
        begin
          match into_loc seed seed_mark (LIter (mark.at, body_loc, rest)) with
          | Some seed_loc -> into_loc vector vector_mark seed_loc
          | None -> None
        end
      | None -> None
    end
  | _ -> None

and loc_vec values marks at rest =
  match values, marks with
  | [], [] -> Some (LEmpty (at, rest))
  | first :: tail, first_mark :: tail_marks ->
    Option.bind (loc_vec tail tail_marks at (LCons (at, rest)))
      (fun tail_loc -> into_loc first first_mark tail_loc)
  | _ -> None

let rec same_loc code loc =
  match code, loc with
  | Done, LDone -> true
  | Push (_, rest), LPush (_, lrest)
  | Void rest, LVoid (_, lrest)
  | Plus rest, LPlus (_, lrest)
  | Minus rest, LMinus (_, lrest)
  | Times rest, LTimes (_, lrest)
  | Quot rest, LQuot (_, lrest)
  | Rem rest, LRem (_, lrest)
  | Negate rest, LNegate (_, lrest)
  | Absolute rest, LAbsolute (_, lrest)
  | Same rest, LSame (_, lrest)
  | Different rest, LDifferent (_, lrest)
  | Join rest, LJoin (_, lrest)
  | Duo rest, LDuo (_, lrest)
  | First rest, LFirst (_, lrest)
  | Second rest, LSecond (_, lrest)
  | Cons rest, LCons (_, lrest)
  | Append rest, LAppend (_, lrest)
  | Unhead rest, LUnhead (_, lrest)
  | Left rest, LLeft (_, lrest)
  | Right rest, LRight (_, lrest) -> same_loc rest lrest
  | Pack (_, _, rest), LPack (_, lrest)
  | Fit (_, rest), LFit (_, lrest)
  | Wide rest, LWide (_, lrest) -> same_loc rest lrest
  | Close (_, rest), LClose (_, lrest) -> same_loc rest lrest
  | Effect (_, _, body, rest), LEffect (_, lbody, lrest) ->
    same_loc body lbody && same_loc rest lrest
  | Order (rel, rest), LOrder (found, _, lrest) when rel = found ->
    same_loc rest lrest
  | Get (_, _, rest), LGet (_, lrest)
  | Empty (_, rest), LEmpty (_, lrest) -> same_loc rest lrest
  | Clip (len, rest), LClip (found, _, lrest)
      when C_nat.equal len found ->
    same_loc rest lrest
  | Skip (len, rest), LSkip (found, _, lrest)
      when C_nat.equal len found ->
    same_loc rest lrest
  | Pick (index, rest), LPick (found, _, lrest)
      when C_nat.equal index found ->
    same_loc rest lrest
  | Scope (_, body, rest), LScope (_, lbody, lrest) ->
      same_loc body lbody && same_loc rest lrest
  | Scope2 (_, _, body, rest), LScope2 (_, lbody, lrest) ->
    same_loc body lbody && same_loc rest lrest
  | Iter (_, _, _, body, rest), LIter (_, lbody, lrest) ->
    same_loc body lbody && same_loc rest lrest
  | Iter_seq (_, _, _, body, rest), LIter (_, lbody, lrest) ->
    same_loc body lbody && same_loc rest lrest
  | Choice (_, yes, _, no, _, rest),
      LChoice (_, _, _, lyes, lno, lrest) ->
    same_loc yes lyes && same_loc no lno && same_loc rest lrest
  | Fork (_, yes, no, rest), LFork (_, _, _, lyes, lno, lrest) ->
      same_loc yes lyes && same_loc no lno && same_loc rest lrest
  | _ -> false

type mvalue =
  | VUnit
  | VAtom of C_emit.lit
  | VPair of mvalue * mvalue
  | VVec of shape * mvalue list
  | VSum of bool * mvalue

let rec machine_value typ value =
  match typ, value with
  | C_type.Unit, C_eval.Unit -> Some VUnit
  | C_type.Bool, C_eval.Bool value -> Some (VAtom (C_emit.Bool value))
  | C_type.Int, C_eval.Int value -> Some (VAtom (C_emit.Int value))
  | (C_type.Num _ as typ), C_eval.Int value when C_type.admits typ value ->
    Some (VAtom (C_emit.Int value))
  | C_type.Bytes len, C_eval.Bytes value
      when C_nat.to_int len = String.length value ->
    Some (VAtom (C_emit.Bytes value))
  | C_type.Cap kind, C_eval.Cap (found, id) when C_nat.equal kind found ->
    Some (VAtom (C_emit.Cap (kind, id)))
  | C_type.Vec (len, elem), C_eval.Vec values
      when C_nat.to_int len = Array.length values ->
    Option.bind (shape_of elem) (fun form ->
      Option.map (fun items -> VVec (form, items))
        (machine_values elem (Array.to_list values)))
  | (C_type.Seq _ as typ), value when C_eval.typed typ value ->
    machine_value (C_type.repr typ) value
  | C_type.Pair (lhs, rhs), C_eval.Pair (left, right) ->
    Option.bind (machine_value lhs left) (fun first ->
      Option.map (fun second -> VPair (first, second))
        (machine_value rhs right))
  | C_type.Sum (lhs, rhs), C_eval.Inl item ->
    Option.bind (shape_of lhs) (fun left ->
      Option.bind (shape_of rhs) (fun right ->
        if same_shape left right then
          Option.map (fun value -> VSum (true, value))
            (machine_value lhs item)
        else None))
  | C_type.Sum (lhs, rhs), C_eval.Inr item ->
    Option.bind (shape_of lhs) (fun left ->
      Option.bind (shape_of rhs) (fun right ->
        if same_shape left right then
          Option.map (fun value -> VSum (false, value))
            (machine_value rhs item)
        else None))
  | _ -> None

and machine_values typ = function
  | [] -> Some []
  | value :: rest ->
    Option.bind (machine_value typ value) (fun first ->
      Option.map (fun tail -> first :: tail) (machine_values typ rest))

let machine_zero typ =
  Option.bind (C_eval.zero typ) (machine_value typ)

let rec eval_value = function
  | VUnit -> Some C_eval.Unit
  | VAtom (C_emit.Bool value) -> Some (C_eval.Bool value)
  | VAtom (C_emit.Int value) -> Some (C_eval.Int value)
  | VAtom (C_emit.Bytes value) -> Some (C_eval.Bytes value)
  | VAtom (C_emit.Data value) -> Some (C_rval.value value)
  | VAtom (C_emit.Cap (kind, id)) -> Some (C_eval.Cap (kind, id))
  | VPair (left, right) ->
    Option.bind (eval_value left) (fun left ->
      Option.map (fun right -> C_eval.Pair (left, right)) (eval_value right))
  | VVec (_, values) ->
    let rec walk out = function
      | [] -> Some (C_eval.Vec (Array.of_list (List.rev out)))
      | value :: rest ->
        Option.bind (eval_value value) (fun value -> walk (value :: out) rest)
    in
    walk [] values
  | VSum (true, value) -> Option.map (fun value -> C_eval.Inl value) (eval_value value)
  | VSum (false, value) -> Option.map (fun value -> C_eval.Inr value) (eval_value value)

let rec value_shape = function
  | VUnit -> SUnit
  | VAtom (C_emit.Cap (kind, _)) -> SCap kind
  | VAtom _ -> SAtom
  | VPair (lhs, rhs) -> SPair (value_shape lhs, value_shape rhs)
  | VVec (elem, values) ->
    begin
      match C_nat.of_int (List.length values) with
      | Some len -> SVec (len, elem)
      | None -> SVec (C_nat.zero, elem)
    end
  | VSum (_, payload) -> SSum (value_shape payload)

let rec same_value_shapes left right =
  match left, right with
  | [], [] -> true
  | lhs :: lrest, rhs :: rrest ->
    same_shape (value_shape lhs) (value_shape rhs)
    && same_value_shapes lrest rrest
  | _ -> false

type slot = {
  bind : C_term.bind;
  value : mvalue;
  live : bool;
}

let rec take id left = function
  | [] -> None
  | slot :: right when C_nat.equal id slot.bind.id ->
    begin
      match slot.bind.mul, slot.live with
      | C_type.One, true ->
        let next = { slot with live = false } in
        Some (slot.value, List.rev_append left (next :: right))
      | C_type.Many, _ ->
        Some (slot.value, List.rev_append left (slot :: right))
      | C_type.Zero, _ | C_type.One, false -> None
    end
  | slot :: right -> take id (slot :: left) right

let open_slot bind value env =
  match shape_of bind.C_term.typ with
  | Some form when same_shape form (value_shape value) ->
    begin
      match bind.mul with
      | C_type.One | C_type.Many -> Some ({ bind; value; live = true } :: env)
      | C_type.Zero -> None
    end
  | _ -> None

let close_slot bind = function
  | slot :: rest when C_nat.equal bind.C_term.id slot.bind.id ->
    begin
      match bind.mul, slot.bind.mul, slot.live with
      | C_type.One, C_type.One, false -> Some rest
      | C_type.Many, C_type.Many, _ -> Some rest
      | _ -> None
    end
  | [] | _ :: _ -> None

let size code = Z.to_int (code_size code)

let rec collect_effects row = function
  | Done -> row
  | Push (_, rest)
  | Void rest
  | Get (_, _, rest)
  | Plus rest
  | Minus rest
  | Times rest
  | Quot rest
  | Rem rest
  | Negate rest
  | Absolute rest
  | Same rest
  | Different rest
  | Order (_, rest)
  | Join rest
  | Clip (_, rest)
  | Skip (_, rest)
  | Duo rest
  | First rest
  | Second rest
  | Empty (_, rest)
  | Cons rest
  | Append rest
  | Pick (_, rest)
  | Unhead rest -> collect_effects row rest
  | Left rest | Right rest | Pack (_, _, rest) | Fit (_, rest) | Wide rest ->
    collect_effects row rest
  | Close (kind, rest) -> collect_effects (C_eff.add (C_eff.Close kind) row) rest
  | Effect (_, action, body, rest) ->
    collect_effects (collect_effects (C_eff.add action row) body) rest
  | Scope (_, body, rest) ->
    collect_effects (collect_effects row body) rest
  | Scope2 (_, _, body, rest) ->
    collect_effects (collect_effects row body) rest
  | Iter (_, _, _, body, rest) ->
    collect_effects (collect_effects row body) rest
  | Iter_seq (_, _, _, body, rest) ->
    collect_effects (collect_effects row body) rest
  | Choice (_, yes, _, no, _, rest) ->
    collect_effects
      (collect_effects (collect_effects row yes) no)
      rest
  | Fork (_, yes, no, rest) ->
    collect_effects
      (collect_effects (collect_effects row yes) no)
      rest

let effects code = C_eff.to_list (collect_effects C_eff.empty code)

let rec exec fuel code env stack plan =
  if fuel = 0 then None
  else
    let left = fuel - 1 in
    match code with
    | Done -> Some (stack, env, plan)
    | Push (value, rest) -> exec left rest env (VAtom value :: stack) plan
    | Void rest -> exec left rest env (VUnit :: stack) plan
    | Get (id, form, rest) ->
      Option.bind (take id [] env) (fun (value, next) ->
        if same_shape form (value_shape value) then
          exec left rest next (value :: stack) plan
        else None)
    | Plus rest ->
      begin
        match stack with
        | VAtom (C_emit.Int rhs) :: VAtom (C_emit.Int lhs) :: tail ->
          exec left rest env (VAtom (C_emit.Int (Z.add lhs rhs)) :: tail) plan
        | _ -> None
      end
    | Minus rest ->
      begin
        match stack with
        | VAtom (C_emit.Int rhs) :: VAtom (C_emit.Int lhs) :: tail ->
          exec left rest env (VAtom (C_emit.Int (Z.sub lhs rhs)) :: tail) plan
        | _ -> None
      end
    | Times rest ->
      begin
        match stack with
        | VAtom (C_emit.Int rhs) :: VAtom (C_emit.Int lhs) :: tail ->
          exec left rest env (VAtom (C_emit.Int (Z.mul lhs rhs)) :: tail) plan
        | _ -> None
      end
    | Quot rest ->
      begin
        match stack with
        | VAtom (C_emit.Int rhs) :: VAtom (C_emit.Int lhs) :: tail
            when not (Z.equal rhs Z.zero) ->
          exec left rest env (VAtom (C_emit.Int (Z.div lhs rhs)) :: tail) plan
        | _ -> None
      end
    | Rem rest ->
      begin
        match stack with
        | VAtom (C_emit.Int rhs) :: VAtom (C_emit.Int lhs) :: tail
            when not (Z.equal rhs Z.zero) ->
          exec left rest env (VAtom (C_emit.Int (Z.rem lhs rhs)) :: tail) plan
        | _ -> None
      end
    | Negate rest ->
      begin
        match stack with
        | VAtom (C_emit.Int value) :: tail ->
          exec left rest env (VAtom (C_emit.Int (Z.neg value)) :: tail) plan
        | _ -> None
      end
    | Absolute rest ->
      begin
        match stack with
        | VAtom (C_emit.Int value) :: tail ->
          exec left rest env (VAtom (C_emit.Int (Z.abs value)) :: tail) plan
        | _ -> None
      end
    | Same rest ->
      begin
        match stack with
        | VAtom rhs :: VAtom lhs :: tail ->
          exec left rest env (VAtom (C_emit.Bool (equal lhs rhs)) :: tail) plan
        | _ -> None
      end
    | Different rest ->
      begin
        match stack with
        | VAtom (C_emit.Int rhs) :: VAtom (C_emit.Int lhs) :: tail ->
          exec left rest env (VAtom (C_emit.Bool (not (Z.equal lhs rhs))) :: tail)
            plan
        | _ -> None
      end
    | Order (rel, rest) ->
      begin
        match stack with
        | VAtom (C_emit.Int rhs) :: VAtom (C_emit.Int lhs) :: tail ->
          let value =
            match rel with
            | C_term.Lt -> Z.lt lhs rhs
            | C_term.Le -> Z.leq lhs rhs
            | C_term.Gt -> Z.gt lhs rhs
            | C_term.Ge -> Z.geq lhs rhs
          in
          exec left rest env (VAtom (C_emit.Bool value) :: tail) plan
        | _ -> None
      end
    | Join rest ->
      begin
        match stack with
        | VAtom (C_emit.Bytes rhs) :: VAtom (C_emit.Bytes lhs) :: tail ->
          exec left rest env (VAtom (C_emit.Bytes (lhs ^ rhs)) :: tail) plan
        | _ -> None
      end
    | Clip (len, rest) ->
      begin
        match stack with
        | VAtom (C_emit.Bytes value) :: tail ->
          let count = C_nat.to_int len in
          if count <= String.length value then
            exec left rest env
              (VAtom (C_emit.Bytes (String.sub value 0 count)) :: tail) plan
          else None
        | _ -> None
      end
    | Skip (len, rest) ->
      begin
        match stack with
        | VAtom (C_emit.Bytes value) :: tail ->
          let first = C_nat.to_int len in
          let count = String.length value - first in
          if count >= 0 then
            exec left rest env
              (VAtom (C_emit.Bytes (String.sub value first count)) :: tail) plan
          else None
        | _ -> None
      end
    | Duo rest ->
      begin
        match stack with
        | rhs :: lhs :: tail ->
          exec left rest env (VPair (lhs, rhs) :: tail) plan
        | _ -> None
      end
    | First rest ->
      begin
        match stack with
        | VPair (lhs, _) :: tail -> exec left rest env (lhs :: tail) plan
        | _ -> None
      end
    | Second rest ->
      begin
        match stack with
        | VPair (_, rhs) :: tail -> exec left rest env (rhs :: tail) plan
        | _ -> None
      end
    | Empty (elem, rest) ->
      exec left rest env (VVec (elem, []) :: stack) plan
    | Cons rest ->
      begin
        match stack with
        | VVec (elem, values) :: item :: tail
            when same_shape elem (value_shape item) ->
          exec left rest env (VVec (elem, item :: values) :: tail) plan
        | _ -> None
      end
    | Append rest ->
      begin
        match stack with
        | VVec (re, rhs) :: VVec (le, lhs) :: tail when same_shape le re ->
          exec left rest env (VVec (le, lhs @ rhs) :: tail) plan
        | _ -> None
      end
    | Pick (index, rest) ->
      begin
        match stack with
        | VVec (_, values) :: tail ->
          Option.bind (List.nth_opt values (C_nat.to_int index)) (fun item ->
            exec left rest env (item :: tail) plan)
        | _ -> None
      end
    | Unhead rest ->
      begin
        match stack with
        | VVec (elem, first :: values) :: tail ->
          exec left rest env
            (VPair (first, VVec (elem, values)) :: tail) plan
        | _ -> None
      end
    | Left rest ->
      begin
        match stack with
        | payload :: tail -> exec left rest env (VSum (true, payload) :: tail) plan
        | [] -> None
      end
    | Right rest ->
      begin
        match stack with
        | payload :: tail -> exec left rest env (VSum (false, payload) :: tail) plan
        | [] -> None
      end
    | Pack (cap, typ, rest) ->
      begin
        match stack, shape_of typ, machine_zero typ with
        | VVec (form, values) :: tail, Some expected, Some zero
            when List.length values <= C_nat.to_int cap
              && same_shape form expected
              && same_shape form (value_shape zero) ->
          let rec fill count out =
            if count = 0 then List.rev out
            else fill (count - 1) (zero :: out)
          in
          let count = List.length values in
          let pad = fill (C_nat.to_int cap - count) [] in
          let value =
            VPair (VAtom (C_emit.Int (Z.of_int count)), VVec (form, values @ pad))
          in
          exec left rest env (value :: tail) plan
        | _ -> None
      end
    | Fit (typ, rest) ->
      begin
        match stack with
        | VAtom (C_emit.Int value) :: tail ->
          let side = C_type.admits typ value in
          exec left rest env (VSum (side, VAtom (C_emit.Int value)) :: tail) plan
        | _ -> None
      end
    | Wide rest ->
      begin
        match stack with
        | VAtom (C_emit.Int _) :: _ -> exec left rest env stack plan
        | _ -> None
      end
    | Close (kind, rest) ->
      begin
        match stack with
        | VAtom (C_emit.Cap (found, id)) :: tail when C_nat.equal kind found ->
          let action = C_eval.held (C_eff.Close kind) C_eval.Unit kind id in
          exec left rest env (VUnit :: tail) (action :: plan)
        | _ -> None
      end
    | Scope (bind, body, rest) ->
      begin
        match stack with
        | value :: tail ->
          Option.bind
            (open_slot bind value env)
            (fun opened ->
              Option.bind
                (exec left body opened tail plan)
                (fun (stack, prior, next_plan) ->
                  Option.bind (close_slot bind prior) (fun next ->
                    exec left rest next stack next_plan)))
        | _ -> None
      end
    | Scope2 (lhs_bind, rhs_bind, body, rest) ->
      begin
        match stack with
        | VPair (lhs, rhs) :: tail ->
          Option.bind (open_slot lhs_bind lhs env) (fun first ->
            Option.bind (open_slot rhs_bind rhs first) (fun second ->
              Option.bind (exec left body second tail plan)
                (fun (stack, prior, next_plan) ->
                Option.bind (close_slot rhs_bind prior) (fun last ->
                  Option.bind (close_slot lhs_bind last) (fun next ->
                    exec left rest next stack next_plan)))))
        | _ -> None
      end
    | Iter (len, item_bind, state_bind, body, rest) ->
      begin
        match item_bind.C_term.mul, state_bind.C_term.mul, stack,
            shape_of item_bind.C_term.typ, shape_of state_bind.C_term.typ with
        | C_type.Zero, _, _, _, _ | _, C_type.Zero, _, _, _ -> None
        | _, _, state :: VVec (elem, values) :: tail, Some item_shape,
            Some state_shape
            when List.length values = C_nat.to_int len
              && same_shape item_shape elem
              && same_shape state_shape (value_shape state)
              && List.for_all
                (fun value -> same_shape elem (value_shape value))
                values ->
          exec_fold left body item_bind state_bind values env tail state plan rest
        | _ -> None
      end
    | Iter_seq (cap, item_bind, state_bind, body, rest) ->
      begin
        match item_bind.C_term.mul, state_bind.C_term.mul, stack,
            shape_of item_bind.C_term.typ, shape_of state_bind.C_term.typ with
        | C_type.Zero, _, _, _, _ | _, C_type.Zero, _, _, _ -> None
        | _, _, state :: VPair (VAtom (C_emit.Int len), VVec (elem, values))
            :: tail, Some item_shape, Some state_shape
            when Z.sign len >= 0
              && Z.leq len (C_nat.to_z cap)
              && List.length values = C_nat.to_int cap
              && same_shape item_shape elem
              && same_shape state_shape (value_shape state)
              && List.for_all
                (fun value -> same_shape elem (value_shape value))
                values ->
          let rec prefix count out values =
            if count = 0 then Some (List.rev out)
            else
              match values with
              | value :: rest -> prefix (count - 1) (value :: out) rest
              | [] -> None
          in
          Option.bind (prefix (Z.to_int len) [] values) (fun active ->
            exec_fold left body item_bind state_bind active env tail state plan rest)
        | _ -> None
      end
    | Choice (left_bind, yes, right_bind, no, form, rest) ->
      begin
        match left_bind.C_term.mul, right_bind.C_term.mul, stack with
        | C_type.Zero, _, _ | _, C_type.Zero, _ -> None
        | _, _, VSum (side, payload) :: tail ->
          let bind, branch = if side then left_bind, yes else right_bind, no in
          Option.bind (open_slot bind payload env) (fun opened ->
            Option.bind (exec left branch opened tail plan)
              (fun (after, prior, next_plan) ->
              Option.bind (close_slot bind prior) (fun next ->
                match after with
                | item :: after_tail
                    when same_shape form (value_shape item)
                      && same_value_shapes after_tail tail ->
                  exec left rest next after next_plan
                | _ -> None)))
        | _, _, _ -> None
      end
    | Fork (form, yes, no, rest) ->
      begin
        match stack with
        | VAtom (C_emit.Bool value) :: tail ->
          let branch = if value then yes else no in
          Option.bind (exec left branch env tail plan)
            (fun (after, next, next_plan) ->
            match after with
            | item :: _ when same_shape form (value_shape item) ->
              exec left rest next after next_plan
            | _ -> None)
        | _ -> None
      end
    | Effect (_, atom, body, rest) ->
      Option.bind (exec left body env stack [])
        (fun (after, next, actions) ->
          match after with
          | value :: _ ->
            Option.bind (eval_value value) (fun payload ->
              exec left rest next after
                (actions @ (C_eval.direct atom payload :: plan)))
          | [] -> None)

and exec_fold fuel body item_bind state_bind values env tail state plan rest =
  match values with
  | [] -> exec fuel rest env (state :: tail) plan
  | item :: values ->
    Option.bind (open_slot item_bind item env) (fun first ->
      Option.bind (open_slot state_bind state first) (fun opened ->
        Option.bind (exec fuel body opened tail plan)
          (fun (after, prior, next_plan) ->
          match after with
          | next_state :: after_tail when same_value_shapes after_tail tail ->
            Option.bind (close_slot state_bind prior) (fun last ->
              Option.bind (close_slot item_bind last) (fun next ->
                exec_fold fuel body item_bind state_bind values next tail
                  next_state next_plan rest))
          | _ -> None)))

let replay_actions code =
  match exec (1 + size code) code [] [] [] with
  | Some ([VAtom value], [], actions) -> Some ([value], List.rev actions)
  | Some _ | None -> None

let replay_plan code =
  Option.map
    (fun (value, actions) -> value, C_eval.atoms actions)
    (replay_actions code)

let replay code = Option.map fst (replay_plan code)

let atom typ = function
  | C_emit.Bool value when C_type.equal typ C_type.Bool ->
    Some (VAtom (C_emit.Bool value))
  | C_emit.Int value when C_type.equal typ C_type.Int ->
    Some (VAtom (C_emit.Int value))
  | C_emit.Int value when C_type.admits typ value ->
    Some (VAtom (C_emit.Int value))
  | C_emit.Bytes value ->
    begin
      match typ with
      | C_type.Bytes len
          when Z.equal (C_nat.to_z len) (Z.of_int (String.length value)) ->
        Some (VAtom (C_emit.Bytes value))
      | _ -> None
    end
  | C_emit.Data item when C_type.equal typ (C_rval.typ item) ->
    machine_value typ (C_rval.value item)
  | C_emit.Cap (kind, id) ->
    begin
      match typ with
      | C_type.Cap found when C_nat.equal kind found ->
        Some (VAtom (C_emit.Cap (kind, id)))
      | _ -> None
    end
  | C_emit.Data _ | C_emit.Bool _ | C_emit.Int _ -> None

let rec open_inputs env = function
  | [] -> Some env
  | (bind, value) :: rest ->
    begin
      match atom bind.C_term.typ value, bind.mul with
      | Some _, C_type.Zero -> open_inputs env rest
      | Some value, (C_type.One | C_type.Many) ->
        Option.bind (open_slot bind value env) (fun next ->
          open_inputs next rest)
      | None, _ -> None
    end

let terminal inputs env =
  let state bind =
    List.find_opt
      (fun slot -> C_nat.equal bind.C_term.id slot.bind.C_term.id)
      env
  in
  List.for_all
    (fun bind ->
      match bind.C_term.mul, state bind with
      | C_type.Zero, None -> true
      | C_type.One, Some slot -> not slot.live
      | C_type.Many, Some slot -> slot.live
      | _ -> false)
    inputs

let replay_actions_in code inputs =
  match open_inputs [] inputs with
  | None -> None
  | Some env ->
    begin
      match exec (1 + size code) code env [] [] with
      | Some ([VAtom value], final, actions)
          when terminal (List.map fst inputs) final ->
        Some ([value], List.rev actions)
      | Some _ | None -> None
    end

let replay_plan_in code inputs =
  Option.map
    (fun (value, actions) -> value, C_eval.atoms actions)
    (replay_actions_in code inputs)

let replay_in code inputs = Option.map fst (replay_plan_in code inputs)

let replay_value_in code inputs typ =
  match open_inputs [] inputs with
  | None -> None
  | Some env ->
    begin
      match exec (1 + size code) code env [] [] with
      | Some ([value], final, _)
          when terminal (List.map fst inputs) final ->
        Option.bind (eval_value value) (literal typ)
      | Some _ | None -> None
    end

let source source =
  let* parsed =
    match C_parse.parse source with
    | Ok value -> Ok value
    | Error error -> Error (Source error)
  in
  let* lowered, info =
    match C_parse.compile parsed with
    | Ok value -> Ok value
    | Error error -> Error (Source error)
  in
  Ok (parsed, lowered, info)

let source_tree parsed term =
  match tree term (C_parse.body_marks parsed) with
  | Ok (value, []) -> Ok value
  | Ok (_, _ :: _) | Error _ -> Error Map

let image parsed inputs typ code loc result = {
  inputs;
  code;
  loc;
  typ;
  result;
  emission = Lowered;
  veils = C_parse.veil_count parsed;
  veil_depth = C_parse.veil_depth parsed;
  span = C_parse.body_span parsed;
}

let closed parsed typ result = {
  inputs = [];
  code = Push (result, Done);
  loc = LPush (C_parse.body_span parsed, LDone);
  typ;
  result = Some result;
  emission = Specialized;
  veils = C_parse.veil_count parsed;
  veil_depth = C_parse.veil_depth parsed;
  span = C_parse.body_span parsed;
}

let finish ?marks parsed term (info : C_check.info) (out : C_eval.out) =
  let* result =
    match literal info.C_check.typ out.value with
    | Some value -> Ok value
    | None -> Error (Value (info.typ, out.value))
  in
  match lower term with
  | None ->
    if out.C_eval.plan = [] then Ok (closed parsed info.typ result)
    else Error (Plan out.plan)
  | Some code ->
    begin
      match one_shape code with
      | Some SAtom ->
        let* mark =
          match marks with
          | Some value -> value ()
          | None -> source_tree parsed term
        in
        let* loc =
          match into_loc term mark LDone with
          | Some value when same_loc code value -> Ok value
          | Some _ | None -> Error Map
        in
        begin
          match replay_plan code with
          | Some ([actual], plan)
              when equal actual result && plan_equal plan out.plan ->
            Ok (image parsed [] info.typ code loc (Some result))
          | _ -> Error Replay
        end
      | Some _ ->
        if out.plan = [] then Ok (closed parsed info.typ result)
        else Error (Plan out.plan)
      | None -> Error Term
    end

let open_image parsed lowered (info : C_check.info) =
  let rec measure width cells nodes = function
    | [] -> Ok (width, cells, nodes)
    | bind :: rest ->
      begin
        match shape_of bind.C_term.typ, C_type.nodes bind.typ with
        | Some form, Some count ->
          measure
            (Z.add width (shape_width form))
            (Z.add cells (shape_cells form))
            (Z.add nodes (Z.of_int count))
            rest
        | _, _ -> Error (Input (bind.id, bind.typ))
      end
  in
  let* count, cells, nodes =
    measure Z.zero Z.zero Z.zero lowered.C_low.inputs
  in
  let* out, out_nodes =
    match shape_of info.typ, C_type.nodes info.typ with
    | Some value, Some count -> Ok (value, count)
    | _, _ -> Error Term
  in
  let cells = Z.add cells (shape_cells out) in
  let nodes = Z.add nodes (Z.of_int out_nodes) in
  if Z.gt count (Z.of_int Contract_vm.input_limit) then
    Error (Inputs count)
  else if Z.gt cells (Z.of_int C_check.max_inputs) then
    Error (Layout cells)
  else if Z.gt nodes (Z.of_int C_type.max_nodes) then
    Error (Types nodes)
  else if Z.gt info.res.steps C_eval.max_cost then
    Error (Run (C_eval.Fuel (C_eval.max_cost, info.res.steps)))
  else
  let effects = C_eff.to_list info.eff in
  if not (List.for_all (function C_eff.Close _ -> true | _ -> false) effects)
  then Error (Effects effects)
  else
    match lower_in lowered.inputs lowered.term with
    | None -> Error Term
    | Some code ->
      begin
        match one_shape code with
        | Some actual when same_shape actual out ->
          let* mark = source_tree parsed lowered.term in
          let* loc =
            match into_loc lowered.term mark LDone with
            | Some value when same_loc code value -> Ok value
            | Some _ | None -> Error Map
          in
          Ok (image parsed lowered.inputs info.typ code loc None)
        | Some _ | None -> Error Term
      end

let compile source_text =
  let* parsed, lowered, info = source source_text in
  if lowered.C_low.inputs = [] then
    let* out =
      match C_eval.run lowered.term with
      | Ok value -> Ok value
      | Error error -> Error (Run error)
    in
    finish parsed lowered.term info out
  else open_image parsed lowered info

let compile_feed source_text feed =
  let* parsed, lowered, info = source source_text in
  let* pairs =
    match C_feed.attach lowered.C_low.inputs feed with
    | Ok value -> Ok value
    | Error error -> Error (Feed error)
  in
  let* term =
    match C_feed.close lowered.inputs feed lowered.term with
    | Ok value -> Ok value
    | Error error -> Error (Feed error)
  in
  let marks () =
    let* source_mark = source_tree parsed lowered.term in
    match retree lowered.term term source_mark with
    | Some value -> Ok value
    | None -> Error Map
  in
  let* out =
    match C_eval.run_in pairs lowered.term with
    | Ok value -> Ok value
    | Error error -> Error (Run error)
  in
  finish ~marks parsed term info out

let emission_text = function
  | Lowered -> "lowered"
  | Specialized -> "specialized"

let text = function
  | Source error -> C_parse.text error
  | Feed error -> C_feed.text error
  | Input (id, typ) ->
    "machine input is outside the plain data profile id = " ^ C_nat.text id
    ^ " type = " ^ C_type.text typ
  | Inputs count ->
    "machine input count = " ^ Z.to_string count ^ " maximum = 60"
  | Layout cells ->
    "machine layout cells = " ^ Z.to_string cells ^ " maximum = 4096"
  | Types nodes ->
    "machine type nodes = " ^ Z.to_string nodes ^ " maximum = 4096"
  | Effects effects ->
      "machine effects are not representable effects = "
      ^ C_eff.text (C_eff.of_list effects)
  | Term -> "machine term is outside the runtime data profile"
  | Run error -> C_eval.text error
  | Plan effects ->
      "machine execution plan is not empty effects = "
      ^ C_eff.text (C_eff.of_list effects)
  | Value (typ, value) ->
      "machine value is not representable type = " ^ C_type.text typ
      ^ " value = " ^ C_eval.value_text value
  | Replay -> "machine replay differs from checked evaluation"
  | Map -> "machine source position map differs from lowered term"