(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type dtype = {
  dname : C_syn.name;
  code : Z.t;
  decl : C_data.decl;
}

type shape = {
  sname : C_syn.name;
  code : Z.t;
  decl : C_rec.decl;
}

type fdecl =
  | Mono of C_fun.fn
  | Spec of C_spec.fn
  | Poly of C_poly.fn

type flow_marks =
  | Ret_marks of C_lex.span option list
  | Let_marks of C_lex.span option list * flow_marks * C_lex.span option
  | If_marks of C_lex.span option list * flow_marks * flow_marks
      * C_lex.span option
  | Call_marks of C_syn.name * C_lex.span option list list
      * C_lex.span option list * flow_marks * C_lex.span option

type form_marks = {
  seq : C_lex.span option list;
  root : C_lex.span option;
}

type veil = {
  vname : C_syn.name;
  vinfo : C_fhe.info;
  vparam : C_fhe.param;
  vhost : C_fhe.host;
  vstage : C_hfhe.info option;
  vlink : C_hpar.link option;
}

module Ikey = struct
  type t = C_syn.name * C_poly.actual list * C_nat.t list

  let rec vals left right =
    match left, right with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x :: xs, y :: ys ->
        let order = C_nat.compare x y in
        if order = 0 then vals xs ys else order

  let compare (left, ltypes, lvals) (right, rtypes, rvals) =
    let order = String.compare (C_syn.name_text left) (C_syn.name_text right) in
    if order <> 0 then order
    else
      let order = C_poly.compare_actuals ltypes rtypes in
      if order = 0 then vals lvals rvals else order
end

module Imap = Map.Make (Ikey)

type t = {
  name : C_syn.name;
  sizes : C_idx.env;
  inputs : C_decl.input list;
  input_spans : (string * C_lex.span) list;
  decls : fdecl list;
  fns : C_fun.fn list;
  form_marks : (C_syn.name * form_marks) list;
  body : C_fun.t;
  perms : C_perm.set;
  dtypes : (Z.t * C_data.decl) list;
  rtypes : (Z.t * C_rec.decl) list;
  veils : veil list;
  body_marks : C_lex.span option list;
  body_span : C_lex.span;
  span : C_lex.span;
}

type cause =
  | Lex of C_lex.error
  | Held of C_lex.hold
  | Need of C_lex.form list * C_lex.form
  | Name of string
  | Size_name of string
  | Size_dup of string
  | Idx of C_idx.error
  | Decl of C_decl.error
  | Fun of C_fun.error
  | Inst of C_spec.error
  | Poly_error of C_poly.error
  | Data of C_data.error
  | Rec of C_rec.error
  | Quant of C_quant.error
  | Weave of C_weave.error
  | Braid of C_braid.error
  | Loom of C_loom.error
  | Orbit of C_orbit.error
  | Wake of C_wake.error
  | Rift of C_rift.error
  | Private
  | Fhe of C_fhe.error
  | Hfhe of C_hfhe.error
  | Hpar of C_hpar.error
  | Word of string * string
  | Veil_dup of string
  | Data_name of string
  | Data_dup of string
  | Shape_name of string
  | Shape_dup of string
  | Tag of Z.t
  | Tag_dup of Z.t
  | Mark_nat of Z.t
  | Mark_dup of string
  | Under_nat of Z.t
  | Empty
  | Perm of C_perm.error
  | Low of C_low.error
  | Check of C_check.error * string option
  | Depth of int * int
  | Nodes of int * int

type error = {
  cause : cause;
  span : C_lex.span;
}

type state = {
  items : C_lex.item array;
  at : int;
  nodes : int;
  sizes : C_idx.env;
  data : dtype list;
  shapes : shape list;
  perms : C_perm.set;
  veils : veil list;
  kinds : (C_syn.name * C_poly.kind) list;
  defs : fdecl list;
  inst : C_fun.fn Imap.t;
  fmarks : (C_syn.name * form_marks) list;
  gen : int;
  marks : C_lex.span option list;
  mcount : int;
}

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let item state = state.items.(state.at)
let tok state = (item state).tok
let span state = (item state).span
let next state = { state with at = state.at + 1 }

let rec take_marks left out values =
  if left = 0 then out
  else
    match values with
    | [] -> []
    | value :: rest -> take_marks (left - 1) (value :: out) rest

let marks_since start state =
  take_marks (state.mcount - start) [] state.marks

let top_mark state =
  match state.marks with
  | value :: _ -> value
  | [] -> None

let rec mark_term at term tail =
  match term with
  | C_syn.KUnit | C_syn.KBool _ | C_syn.KInt _ | C_syn.KBytes _
  | C_syn.Var _ -> at :: tail
  | C_syn.KVec (_, values) | C_syn.KSeq (_, _, values) ->
      mark_terms at values (at :: tail)
  | C_syn.Let (_, value, body) | C_syn.Unpair (value, _, _, body) ->
      mark_term at value (mark_term at body (at :: tail))
  | C_syn.If (guard, yes, no) ->
      mark_term at guard
        (mark_term at yes (mark_term at no (at :: tail)))
  | C_syn.Pair (left, right) | C_syn.Add (left, right)
  | C_syn.Sub (left, right) | C_syn.Mul (left, right)
  | C_syn.Div (left, right) | C_syn.Mod (left, right)
  | C_syn.Eq (_, left, right) | C_syn.Cmp (_, left, right)
  | C_syn.Cat (left, right)
  | C_syn.Vcat (left, right) | C_syn.Step (left, right) ->
      mark_term at left (mark_term at right (at :: tail))
  | C_syn.Fst value | C_syn.Snd value | C_syn.Inl (value, _)
  | C_syn.Inr (_, value) | C_syn.Act (_, value) | C_syn.Neg value
  | C_syn.Abs value | C_syn.Fit (_, value) | C_syn.Wide value
  | C_syn.Length value | C_syn.Take (_, value)
  | C_syn.Drop (_, value) | C_syn.At (_, value) | C_syn.Uncons value
  | C_syn.Close value -> mark_term at value (at :: tail)
  | C_syn.Case (value, _, yes, _, no) ->
      mark_term at value
        (mark_term at yes (mark_term at no (at :: tail)))
  | C_syn.Vfold (vector, seed, fold) ->
      mark_term at vector
        (mark_term at seed (mark_term at fold.C_syn.body (at :: tail)))

and mark_terms at terms tail =
  List.fold_left (fun out term -> mark_term at term out) tail (List.rev terms)

type origin = {
  term : C_syn.t;
  spans : C_lex.span list;
}

let add_origin term at origins =
  let rec walk out = function
    | [] -> List.rev ({ term; spans = [at] } :: out)
    | item :: rest when item.term == term ->
        List.rev_append out ({ item with spans = at :: item.spans } :: rest)
    | item :: rest -> walk (item :: out) rest
  in
  walk [] origins

let origin_set whole terms marks =
  let rec walk origins terms marks =
    match terms, marks with
    | [], [] ->
        Some (List.map
          (fun item -> { item with spans = List.rev item.spans }) origins)
    | term :: terms, mark :: marks ->
        let at = Option.value ~default:whole mark in
        walk (add_origin term at origins) terms marks
    | _ -> None
  in
  walk [] terms marks

let take_origin term parent origins =
  let rec walk out = function
    | [] -> parent, List.rev out
    | item :: rest when item.term == term ->
        begin
          match item.spans with
          | [] -> parent, List.rev_append out (item :: rest)
          | [at] -> at, List.rev_append out (item :: rest)
          | at :: tail ->
              let next = { item with spans = tail @ [at] } in
              at, List.rev_append out (next :: rest)
        end
    | item :: rest -> walk (item :: out) rest
  in
  walk [] origins

let rec trace_term parent term (marks, origins) =
  let at, origins = take_origin term parent origins in
  let state = marks, origins in
  let marks, origins =
    match term with
    | C_syn.KUnit | C_syn.KBool _ | C_syn.KInt _ | C_syn.KBytes _
    | C_syn.Var _ -> state
    | C_syn.KVec (_, values) | C_syn.KSeq (_, _, values) ->
        trace_terms at values state
    | C_syn.Let (_, value, body) | C_syn.Unpair (value, _, _, body) ->
        trace_term at body (trace_term at value state)
    | C_syn.If (guard, yes, no) ->
        trace_term at no (trace_term at yes (trace_term at guard state))
    | C_syn.Pair (left, right) | C_syn.Add (left, right)
    | C_syn.Sub (left, right) | C_syn.Mul (left, right)
    | C_syn.Div (left, right) | C_syn.Mod (left, right)
    | C_syn.Eq (_, left, right) | C_syn.Cmp (_, left, right)
    | C_syn.Cat (left, right)
    | C_syn.Vcat (left, right) | C_syn.Step (left, right) ->
        trace_term at right (trace_term at left state)
    | C_syn.Fst value | C_syn.Snd value | C_syn.Inl (value, _)
    | C_syn.Inr (_, value) | C_syn.Act (_, value) | C_syn.Neg value
    | C_syn.Abs value | C_syn.Fit (_, value) | C_syn.Wide value
    | C_syn.Length value | C_syn.Take (_, value) | C_syn.Drop (_, value)
    | C_syn.At (_, value) | C_syn.Uncons value | C_syn.Close value ->
        trace_term at value state
    | C_syn.Case (value, _, yes, _, no) ->
        trace_term at no (trace_term at yes (trace_term at value state))
    | C_syn.Vfold (vector, seed, fold) ->
        trace_term at fold.C_syn.body
          (trace_term at seed (trace_term at vector state))
  in
  at :: marks, origins

and trace_terms at terms state = List.fold_left
  (fun state term -> trace_term at term state) state terms

let origin_marks whole terms marks term =
  match origin_set whole terms marks with
  | None -> mark_term (Some whole) term []
  | Some origins ->
      let marks, _ = trace_term whole term ([], origins) in
      List.rev_map Option.some marks

let rec drop_marks left marks =
  if left = 0 then marks
  else
    match marks with
    | [] -> []
    | _ :: rest -> drop_marks (left - 1) rest

let replace_marks start values state =
  let prior = drop_marks (state.mcount - start) state.marks in
  { state with
    marks = List.rev_append values prior;
    mcount = start + List.length values;
  }

let spread_form marks body =
  { marks with seq = mark_term marks.root body [] }

let fit_form marks body =
  let spread = spread_form marks body in
  if List.length marks.seq = List.length spread.seq
      && List.for_all Option.is_some marks.seq then marks
  else spread

let rec find_marks name = function
  | [] -> None
  | (key, value) :: _ when C_syn.name_equal name key -> Some value
  | _ :: rest -> find_marks name rest

let join_marks left right =
  List.rev_append (List.rev left) right

let bind_marks at actuals body =
  let roots = List.init (List.length actuals) (fun _ -> at) in
  List.fold_left
    (fun out actual -> join_marks actual out)
    (join_marks body roots) (List.rev actuals)

let rec expand_marks fmarks = function
  | Ret_marks marks -> Ok marks
  | Let_marks (value, body, at) ->
      let* body = expand_marks fmarks body in
      Ok (join_marks value (join_marks body [at]))
  | If_marks (guard, yes, no, at) ->
      let* yes = expand_marks fmarks yes in
      let* no = expand_marks fmarks no in
      Ok (join_marks guard (join_marks yes (join_marks no [at])))
  | Call_marks (name, caps, arg, rest, at) ->
      let body =
        match find_marks name fmarks with
        | Some marks -> marks.seq
        | None -> []
      in
      let* rest = expand_marks fmarks rest in
      let value = bind_marks at (caps @ [arg]) body in
      Ok (join_marks value (join_marks rest [at]))

let need forms state =
  match tok state with
  | C_lex.Hold item -> Error { cause = Held item; span = span state }
  | _ -> Error { cause = Need (forms, C_lex.form (tok state)); span = span state }

let take form state =
  if C_lex.form (tok state) = form then Ok (next state)
  else need [form] state

let ident state =
  match tok state with
  | C_lex.Ident value -> Ok (value, next state)
  | _ -> need [C_lex.F_ident] state

let word value state =
  match tok state with
  | C_lex.Ident actual when String.equal value actual -> Ok (next state)
  | C_lex.Ident actual ->
      Error { cause = Word (value, actual); span = span state }
  | _ -> need [C_lex.F_ident] state

let is_word value state =
  match tok state with
  | C_lex.Ident actual -> String.equal value actual
  | _ -> false

let rec held items at =
  if at >= Array.length items then Ok ()
  else
    let item = items.(at) in
    match item.C_lex.tok with
    | C_lex.Hold value -> Error { cause = Held value; span = item.span }
    | _ -> held items (at + 1)

let nat state =
  match tok state with
  | C_lex.Nat value -> Ok (value, next state)
  | _ -> need [C_lex.F_nat] state

let name cause state =
  let first = span state in
  let* value, state = ident state in
  match C_syn.name value with
  | Some value -> Ok (value, state)
  | None -> Error { cause = cause value; span = first }

let rec data_find name = function
  | [] -> None
  | item :: _ when C_syn.name_equal name item.dname -> Some item
  | _ :: rest -> data_find name rest

let rec shape_find name = function
  | [] -> None
  | item :: _ when C_syn.name_equal name item.sname -> Some item
  | _ :: rest -> shape_find name rest

let rec kind_find name = function
  | [] -> None
  | (key, kind) :: _ when C_syn.name_equal name key -> Some kind
  | _ :: rest -> kind_find name rest

let rec form_find name = function
  | [] -> None
  | Mono item :: _ when C_syn.name_equal name item.C_fun.name ->
      Some (Mono item)
  | Spec item :: _ when C_syn.name_equal name item.C_spec.name ->
      Some (Spec item)
  | Poly item :: _ when C_syn.name_equal name (C_poly.name item) ->
      Some (Poly item)
  | _ :: rest -> form_find name rest

let mul state =
  match tok state with
  | C_lex.Erase -> Ok (C_type.Zero, next state)
  | C_lex.Once -> Ok (C_type.One, next state)
  | C_lex.Many -> Ok (C_type.Many, next state)
  | _ -> need [C_lex.F_erase; C_lex.F_once; C_lex.F_many] state

let guard depth state =
  if depth > C_rule.local.tm_depth then
    Error {
      cause = Depth (C_rule.local.tm_depth, depth);
      span = span state;
    }
  else Ok ()

let put ?first state term =
  let nodes = state.nodes + 1 in
  if nodes > C_rule.local.tm_nodes then
    Error {
      cause = Nodes (C_rule.local.tm_nodes, nodes);
      span = span state;
    }
  else
    let mark =
      Option.map
        (fun (first : C_lex.span) ->
          let last = state.items.(state.at - 1).C_lex.span in
          { first with last = last.last })
        first
    in
    Ok (term, {
      state with
      nodes;
      marks = mark :: state.marks;
      mcount = state.mcount + 1;
    })

let put_fun ?first state term =
  let nodes = state.nodes + 1 in
  if nodes > C_rule.local.tm_nodes then
    Error {
      cause = Nodes (C_rule.local.tm_nodes, nodes);
      span = span state;
    }
  else
    let mark =
      Option.map
        (fun (first : C_lex.span) ->
          let last = state.items.(state.at - 1).C_lex.span in
          { first with last = last.last })
        first
    in
    Ok (term, {
      state with
      nodes;
      marks = mark :: state.marks;
      mcount = state.mcount + 1;
    })

let idx_lit value state =
  match C_idx.lit value with
  | Some value -> Ok (value, state)
  | None -> Error { cause = Idx (C_idx.Nat value); span = span state }

let rec idx state = idx_add state

and idx_add state =
  let* left, state = idx_mul state in
  let rec walk left state =
    match tok state with
    | C_lex.Plus ->
        let* right, state = idx_mul (next state) in
        walk (C_idx.add left right) state
    | C_lex.Minus ->
        let* right, state = idx_mul (next state) in
        walk (C_idx.sub left right) state
    | _ -> Ok (left, state)
  in
  walk left state

and idx_mul state =
  let* left, state = idx_atom state in
  let rec walk left state =
    match tok state with
    | C_lex.Star ->
        let* right, state = idx_atom (next state) in
        walk (C_idx.mul left right) state
    | _ -> Ok (left, state)
  in
  walk left state

and idx_atom state =
  match tok state with
  | C_lex.Nat value -> idx_lit value (next state)
  | C_lex.Ident _ ->
      let* value, state = name (fun value -> Size_name value) state in
      Ok (C_idx.var value, state)
  | C_lex.Max ->
      let* state = take C_lex.F_lparen (next state) in
      let* left, state = idx state in
      let* state = take C_lex.F_comma state in
      let* right, state = idx state in
      let* state = take C_lex.F_rparen state in
      Ok (C_idx.max left right, state)
  | C_lex.Lparen ->
      let* value, state = idx (next state) in
      let* state = take C_lex.F_rparen state in
      Ok (value, state)
  | _ -> need [C_lex.F_nat; C_lex.F_ident; C_lex.F_max; C_lex.F_lparen] state

let rec typ state =
  let* left, state = typ_atom state in
  match tok state with
  | C_lex.Star ->
      let* right, state = typ (next state) in
      Ok (C_decl.Pair (left, right), state)
  | _ -> Ok (left, state)

and typ_atom state =
  match tok state with
  | C_lex.Unit -> Ok (C_decl.Unit, next state)
  | C_lex.Bool -> Ok (C_decl.Bool, next state)
  | C_lex.Int -> Ok (C_decl.Int, next state)
  | C_lex.Sint ->
      let* state = take C_lex.F_lbrack (next state) in
      let* bits, state = idx state in
      let* state = take C_lex.F_rbrack state in
      Ok (C_decl.Num (C_type.Signed, bits), state)
  | C_lex.Uint ->
      let* state = take C_lex.F_lbrack (next state) in
      let* bits, state = idx state in
      let* state = take C_lex.F_rbrack state in
      Ok (C_decl.Num (C_type.Unsigned, bits), state)
  | C_lex.Bytes ->
      let* state = take C_lex.F_lbrack (next state) in
      let* len, state = idx state in
      let* state = take C_lex.F_rbrack state in
      Ok (C_decl.Bytes len, state)
  | C_lex.Vec ->
      let* state = take C_lex.F_lbrack (next state) in
      let* len, state = idx state in
      let* state = take C_lex.F_comma state in
      let* elem, state = typ state in
      let* state = take C_lex.F_rbrack state in
      Ok (C_decl.Vec (len, elem), state)
  | C_lex.Seq ->
      let* state = take C_lex.F_lbrack (next state) in
      let* cap, state = idx state in
      let* state = take C_lex.F_comma state in
      let* elem, state = typ state in
      let* state = take C_lex.F_rbrack state in
      Ok (C_decl.Seq (cap, elem), state)
  | C_lex.Cap ->
      let* state = take C_lex.F_lbrack (next state) in
      let* kind, state = nat state in
      let* state = take C_lex.F_rbrack state in
      Ok (C_decl.Cap kind, state)
  | C_lex.Result ->
      let* state = take C_lex.F_lbrack (next state) in
      let* good, state = typ state in
      let* state = take C_lex.F_comma state in
      let* bad, state = typ state in
      let* state = take C_lex.F_rbrack state in
      Ok (C_decl.Result (good, bad), state)
  | C_lex.Ident _ ->
      let mark = span state in
      let* name, state = name (fun value -> Name value) state in
      begin
        match kind_find name state.kinds with
        | Some _ -> Ok (C_decl.Var name, state)
        | None ->
            begin
              match data_find name state.data with
              | Some item -> Ok (C_decl.Exact (C_data.typ item.decl), state)
              | None ->
                  begin
                    match shape_find name state.shapes with
                    | Some item -> Ok (C_decl.Exact (C_rec.typ item.decl), state)
                    | None ->
                        Error { cause = Data_name (C_syn.name_text name);
                          span = mark }
                  end
            end
      end
  | C_lex.Lparen ->
      let* value, state = typ (next state) in
      let* state = take C_lex.F_rparen state in
      Ok (value, state)
  | _ ->
      need [C_lex.F_unit; C_lex.F_bool; C_lex.F_int; C_lex.F_sint;
        C_lex.F_uint; C_lex.F_bytes; C_lex.F_vec; C_lex.F_seq;
        C_lex.F_cap; C_lex.F_result; C_lex.F_ident;
        C_lex.F_lparen] state

let bind state =
  let first = span state in
  let* mode, state = mul state in
  let* name, state = name (fun value -> Name value) state in
  let* state = take C_lex.F_colon state in
  let* raw, state = typ state in
  match C_decl.typ_in state.sizes raw with
  | Ok typ -> Ok (C_syn.bind name mode typ, state)
  | Error error -> Error { cause = Decl error; span = first }

let raw_bind state =
  let* mode, state = mul state in
  let* name, state = name (fun value -> Name value) state in
  let* state = take C_lex.F_colon state in
  let* typ, state = typ state in
  Ok (C_raw.bind name mode typ, state)

let raw_cause = function
  | C_raw.Idx error -> Idx error
  | C_raw.Decl error -> Decl error
  | C_raw.Data error -> Data error
  | C_raw.Rec error -> Rec error
  | C_raw.Quant_error error -> Quant error
  | C_raw.Weave_error error -> Weave error
  | C_raw.Braid_error error -> Braid error
  | C_raw.Loom_error error -> Loom error
  | C_raw.Orbit_error error -> Orbit error
  | C_raw.Wake_error error -> Wake error
  | C_raw.Rift_error error -> Rift error
  | C_raw.Fresh -> Private
  | C_raw.Depth (limit, actual) -> Depth (limit, actual)
  | C_raw.Nodes (limit, actual) -> Nodes (limit, actual)

let spec_bind state =
  let* mode, state = mul state in
  let* name, state = name (fun value -> Name value) state in
  let* state = take C_lex.F_colon state in
  let* typ, state = typ state in
  Ok (C_spec.bind name mode typ, state)

let input state =
  let mark = span state in
  let* mode, state = mul state in
  let* name, state = ident state in
  let* state = take C_lex.F_colon state in
  let* typ, state = typ state in
  match C_decl.typ_in state.sizes typ with
  | Ok _ -> Ok ((C_decl.input name mode typ, name, mark), state)
  | Error error -> Error { cause = Decl error; span = mark }

let rec unary depth make state =
  let first = span state in
  let* state = take C_lex.F_lparen (next state) in
  let* value, state = raw_expr (depth + 1) state in
  let* state = take C_lex.F_rparen state in
  put ~first state (make value)

and binary depth make state =
  let first = span state in
  let* state = take C_lex.F_lparen (next state) in
  let* left, state = raw_expr (depth + 1) state in
  let* state = take C_lex.F_comma state in
  let* right, state = raw_expr (depth + 1) state in
  let* state = take C_lex.F_rparen state in
  put ~first state (make left right)

and typed_unary depth make state =
  let first = span state in
  let* state = take C_lex.F_lbrack (next state) in
  let* raw, state = typ state in
  let* state = take C_lex.F_rbrack state in
  let* state = take C_lex.F_lparen state in
  let* value, state = raw_expr (depth + 1) state in
  let* state = take C_lex.F_rparen state in
  put ~first state (make raw value)

and typed_binary depth make state =
  let first = span state in
  let* state = take C_lex.F_lbrack (next state) in
  let* raw, state = typ state in
  let* state = take C_lex.F_rbrack state in
  let* state = take C_lex.F_lparen state in
  let* left, state = raw_expr (depth + 1) state in
  let* state = take C_lex.F_comma state in
  let* right, state = raw_expr (depth + 1) state in
  let* state = take C_lex.F_rparen state in
  put ~first state (make raw left right)

and indexed_unary depth make state =
  let first = span state in
  let* state = take C_lex.F_lbrack (next state) in
  let* index, state = nat state in
  let* state = take C_lex.F_rbrack state in
  let* state = take C_lex.F_lparen state in
  let* value, state = raw_expr (depth + 1) state in
  let* state = take C_lex.F_rparen state in
  put ~first state (make index value)

and effect depth make state =
  indexed_unary depth (fun kind value -> C_raw.Act (make kind, value)) state

and vector depth state =
  let first = span state in
  let* state = take C_lex.F_lbrack (next state) in
  let* raw, state = typ state in
  let* state = take C_lex.F_rbrack state in
  let* state = take C_lex.F_lparen state in
  let rec items out state =
    match tok state with
    | C_lex.Rparen -> Ok (List.rev out, next state)
    | _ ->
        let* value, state = raw_expr (depth + 1) state in
        begin
          match tok state with
          | C_lex.Comma -> items (value :: out) (next state)
          | C_lex.Rparen -> Ok (List.rev (value :: out), next state)
          | _ -> need [C_lex.F_comma; C_lex.F_rparen] state
        end
  in
  let* values, state = items [] state in
  put ~first state (C_raw.KVec (raw, values))

and sequence depth state =
  let first = span state in
  let* state = take C_lex.F_lbrack (next state) in
  let* cap, state = idx state in
  let* state = take C_lex.F_comma state in
  let* raw, state = typ state in
  let* state = take C_lex.F_rbrack state in
  let* state = take C_lex.F_lparen state in
  let rec items out state =
    match tok state with
    | C_lex.Rparen -> Ok (List.rev out, next state)
    | _ ->
        let* value, state = raw_expr (depth + 1) state in
        begin
          match tok state with
          | C_lex.Comma -> more (value :: out) (next state)
          | C_lex.Rparen -> Ok (List.rev (value :: out), next state)
          | _ -> need [C_lex.F_comma; C_lex.F_rparen] state
        end
  and more out state =
    match tok state with
    | C_lex.Rparen -> need [C_lex.F_nat; C_lex.F_ident; C_lex.F_lparen] state
    | _ -> items out state
  in
  let* values, state = items [] state in
  put ~first state (C_raw.KSeq (cap, raw, values))

and let_term depth state =
  let first = span state in
  let* bind, state = raw_bind (next state) in
  let* state = take C_lex.F_eq state in
  let* value, state = raw_expr (depth + 1) state in
  let* state = take C_lex.F_in state in
  let* body, state = raw_expr (depth + 1) state in
  put ~first state (C_raw.Let (bind, value, body))

and split depth state =
  let first = span state in
  let* value, state = raw_expr (depth + 1) (next state) in
  let* state = take C_lex.F_as state in
  match tok state with
  | C_lex.Ident _ -> shape_split depth first value state
  | _ -> pair_split depth first value state

and pair_split depth first value state =
  let* left, state = raw_bind state in
  let* state = take C_lex.F_comma state in
  let* right, state = raw_bind state in
  let* state = take C_lex.F_in state in
  let* body, state = raw_expr (depth + 1) state in
  put ~first state (C_raw.Unpair (value, left, right, body))

and shape_split depth first value state =
  let mark = span state in
  let* shape_name, state = name (fun value -> Name value) state in
  let* item =
    match shape_find shape_name state.shapes with
    | Some value -> Ok value
    | None ->
        Error { cause = Shape_name (C_syn.name_text shape_name); span = mark }
  in
  let* state = take C_lex.F_lbrace state in
  let rec fields out state =
    let* field_name, state = name (fun value -> Name value) state in
    let* state = take C_lex.F_arrow state in
    let* field_bind, state = raw_bind state in
    let out = C_raw.pick field_name field_bind :: out in
    match tok state with
    | C_lex.Comma -> fields out (next state)
    | C_lex.Rbrace -> Ok (List.rev out, next state)
    | _ -> need [C_lex.F_comma; C_lex.F_rbrace] state
  in
  let* fields, state = fields [] state in
  let* state = take C_lex.F_in state in
  let* body, state = raw_expr (depth + 1) state in
  put ~first state (C_raw.Rsplit (item.decl, value, fields, body))

and if_term depth state =
  let first = span state in
  let* cond, state = raw_expr (depth + 1) (next state) in
  let* state = take C_lex.F_then state in
  let* yes, state = raw_expr (depth + 1) state in
  let* state = take C_lex.F_else state in
  let* no, state = raw_expr (depth + 1) state in
  put ~first state (C_raw.If (cond, yes, no))

and case_term depth state =
  let* value, state = raw_expr (depth + 1) (next state) in
  match tok state with
  | C_lex.Of -> result_case depth value (next state)
  | C_lex.As -> data_case depth value (next state)
  | _ -> need [C_lex.F_of; C_lex.F_as] state

and result_case depth value state =
  let* state = take C_lex.F_ok state in
  let* left, state = raw_bind state in
  let* state = take C_lex.F_arrow state in
  let* yes, state = raw_expr (depth + 1) state in
  let* state = take C_lex.F_bar state in
  let* state = take C_lex.F_err state in
  let* right, state = raw_bind state in
  let* state = take C_lex.F_arrow state in
  let* no, state = raw_expr (depth + 1) state in
  put state (C_raw.Case (value, left, yes, right, no))

and data_case depth value state =
  let mark = span state in
  let* data_name, state = name (fun value -> Name value) state in
  let* item =
    match data_find data_name state.data with
    | Some value -> Ok value
    | None ->
        Error { cause = Data_name (C_syn.name_text data_name); span = mark }
  in
  let* state = take C_lex.F_of state in
  let* state = take C_lex.F_lbrace state in
  let rec arms out state =
    let* ctor_name, state = name (fun value -> Name value) state in
    let* payload, state = raw_bind state in
    let* state = take C_lex.F_arrow state in
    let* body, state = raw_expr (depth + 1) state in
    let out = C_raw.arm ctor_name payload body :: out in
    match tok state with
    | C_lex.Bar -> arms out (next state)
    | C_lex.Rbrace -> Ok (List.rev out, next state)
    | _ -> need [C_lex.F_bar; C_lex.F_rbrace] state
  in
  let* arms, state = arms [] state in
  put state (C_raw.Dcase (item.decl, value, arms))

and fold depth state =
  let* source, state = raw_expr (depth + 1) (next state) in
  let* state = take C_lex.F_from state in
  let* init, state = raw_expr (depth + 1) state in
  let* state = take C_lex.F_with state in
  let* item, state = raw_bind state in
  let* state = take C_lex.F_comma state in
  let* acc, state = raw_bind state in
  let* state = take C_lex.F_arrow state in
  let* body, state = raw_expr (depth + 1) state in
  put state (C_raw.Vfold (source, init, C_raw.fold item acc body))

and quant mode depth state =
  let* source, state = raw_expr (depth + 1) (next state) in
  let* state = take C_lex.F_with state in
  let* item, state = raw_bind state in
  let* state = take C_lex.F_arrow state in
  let* pred, state = raw_expr (depth + 1) state in
  put state (C_raw.Quant (mode, source, item, pred))

and weave depth state =
  let* state = take C_lex.F_lbrack (next state) in
  let* raw_count, state = idx state in
  let* state = take C_lex.F_comma state in
  let* raw_out, state = typ state in
  let* state = take C_lex.F_rbrack state in
  let* source, state = raw_expr (depth + 1) state in
  let* state = take C_lex.F_with state in
  let* item, state = raw_bind state in
  let* state = take C_lex.F_arrow state in
  let* body, state = raw_expr (depth + 1) state in
  put state (C_raw.Weave (raw_count, raw_out, source, item, body))

and braid depth state =
  let* state = take C_lex.F_lbrack (next state) in
  let* raw_count, state = idx state in
  let* state = take C_lex.F_comma state in
  let* raw_out, state = typ state in
  let* state = take C_lex.F_rbrack state in
  let* left, state = raw_expr (depth + 1) state in
  let* state = take C_lex.F_comma state in
  let* right, state = raw_expr (depth + 1) state in
  let* state = take C_lex.F_with state in
  let* first, state = raw_bind state in
  let* state = take C_lex.F_comma state in
  let* second, state = raw_bind state in
  let* state = take C_lex.F_arrow state in
  let* body, state = raw_expr (depth + 1) state in
  put state (C_raw.Braid
    (raw_count, raw_out, left, right, first, second, body))

and loom depth state =
  let* state = take C_lex.F_lbrack (next state) in
  let* raw_count, state = idx state in
  let* state = take C_lex.F_comma state in
  let* raw_out, state = typ state in
  let* state = take C_lex.F_rbrack state in
  let* state = take C_lex.F_with state in
  let* item, state = raw_bind state in
  let* state = take C_lex.F_arrow state in
  let* body, state = raw_expr (depth + 1) state in
  put state (C_raw.Loom (raw_count, raw_out, item, body))

and orbit depth state =
  let* state = take C_lex.F_lbrack (next state) in
  let* raw_count, state = idx state in
  let* turns, state =
    match tok state with
    | C_lex.Comma ->
        let* turns, state = raw_expr (depth + 1) (next state) in
        let* state = take C_lex.F_rbrack state in
        Ok (Some turns, state)
    | C_lex.Rbrack -> Ok (None, next state)
    | _ -> need [C_lex.F_comma; C_lex.F_rbrack] state
  in
  let* state = take C_lex.F_from state in
  let* seed, state = raw_expr (depth + 1) state in
  let* state = take C_lex.F_with state in
  let* item, state = raw_bind state in
  let* state = take C_lex.F_arrow state in
  let* body, state = raw_expr (depth + 1) state in
  match turns with
  | None -> put state (C_raw.Orbit (raw_count, seed, item, body))
  | Some turns -> put state (C_raw.Orbit_to
      (raw_count, turns, seed, item, body))

and wake depth state =
  let* state = take C_lex.F_lbrack (next state) in
  let* raw_count, state = idx state in
  let* state = take C_lex.F_rbrack state in
  let* state = take C_lex.F_from state in
  let* seed, state = raw_expr (depth + 1) state in
  let* state = take C_lex.F_with state in
  let* item, state = raw_bind state in
  let* state = take C_lex.F_arrow state in
  let* body, state = raw_expr (depth + 1) state in
  put state (C_raw.Wake (raw_count, seed, item, body))

and rift depth state =
  let* state = take C_lex.F_lbrack (next state) in
  let* raw_cut, state = idx state in
  let* state = take C_lex.F_comma state in
  let* raw_rest, state = idx state in
  let* state = take C_lex.F_comma state in
  let* raw_elem, state = typ state in
  let* state = take C_lex.F_rbrack state in
  let* state = take C_lex.F_lparen state in
  let* source, state = raw_expr (depth + 1) state in
  let* state = take C_lex.F_rparen state in
  put state (C_raw.Rift (raw_cut, raw_rest, raw_elem, source))

and make_term depth state =
  let mark = span state in
  let* type_name, state = name (fun value -> Name value) (next state) in
  match data_find type_name state.data, shape_find type_name state.shapes with
  | Some item, _ -> data_make depth item state
  | None, Some item -> shape_make depth item state
  | None, None ->
      Error { cause = Data_name (C_syn.name_text type_name); span = mark }

and data_make depth item state =
  let* ctor_name, state = name (fun value -> Name value) state in
  let* state = take C_lex.F_lparen state in
  let* payload, state = raw_expr (depth + 1) state in
  let* state = take C_lex.F_rparen state in
  put state (C_raw.Dmake (item.decl, ctor_name, payload))

and shape_make depth item state =
  let* state = take C_lex.F_lbrace state in
  let rec fields out state =
    let* field_name, state = name (fun value -> Name value) state in
    let* state = take C_lex.F_eq state in
    let* value, state = raw_expr (depth + 1) state in
    let out = C_raw.item field_name value :: out in
    match tok state with
    | C_lex.Comma -> fields out (next state)
    | C_lex.Rbrace -> Ok (List.rev out, next state)
    | _ -> need [C_lex.F_comma; C_lex.F_rbrace] state
  in
  let* fields, state = fields [] state in
  put state (C_raw.Rmake (item.decl, fields))

and atom depth state =
  let* () = guard depth state in
  let first = span state in
  match tok state with
  | C_lex.Nat value -> put ~first (next state) (C_raw.KInt value)
  | C_lex.Minus ->
      begin
        match tok (next state) with
        | C_lex.Nat value ->
            put ~first (next (next state)) (C_raw.KInt (Z.neg value))
        | _ ->
            let* value, state = atom (depth + 1) (next state) in
            put ~first state (C_raw.Neg value)
      end
  | C_lex.Hex value -> put ~first (next state) (C_raw.KBytes value)
  | C_lex.True -> put ~first (next state) (C_raw.KBool true)
  | C_lex.False -> put ~first (next state) (C_raw.KBool false)
  | C_lex.Unit -> put ~first (next state) C_raw.KUnit
  | C_lex.Ident _ ->
      let* name, state = name (fun value -> Name value) state in
      begin
        match tok state with
        | C_lex.Lparen -> direct_call depth first name state
        | _ -> put ~first state (C_raw.Var name)
      end
  | C_lex.Lparen ->
      let state = next state in
      begin
        match tok state with
        | C_lex.Rparen -> put ~first (next state) C_raw.KUnit
        | _ ->
            let* left, state = raw_expr (depth + 1) state in
            begin
              match tok state with
              | C_lex.Comma ->
                  let* right, state = raw_expr (depth + 1) (next state) in
                  let* state = take C_lex.F_rparen state in
                  put ~first state (C_raw.Pair (left, right))
              | C_lex.Rparen -> Ok (left, next state)
              | _ -> need [C_lex.F_comma; C_lex.F_rparen] state
            end
      end
  | C_lex.Vec -> vector depth state
  | C_lex.Seq -> sequence depth state
  | C_lex.Make -> make_term depth state
  | C_lex.Fst -> unary depth (fun value -> C_raw.Fst value) state
  | C_lex.Snd -> unary depth (fun value -> C_raw.Snd value) state
  | C_lex.Uncons -> unary depth (fun value -> C_raw.Uncons value) state
  | C_lex.Abs -> unary depth (fun value -> C_raw.Abs value) state
  | C_lex.Fit ->
      typed_unary depth (fun typ value -> C_raw.Fit (typ, value)) state
  | C_lex.Wide -> unary depth (fun value -> C_raw.Wide value) state
  | C_lex.Length -> unary depth (fun value -> C_raw.Length value) state
  | C_lex.Cat -> binary depth (fun left right -> C_raw.Cat (left, right)) state
  | C_lex.Vcat -> binary depth (fun left right -> C_raw.Vcat (left, right)) state
  | C_lex.Step -> binary depth (fun cap value -> C_raw.Step (cap, value)) state
  | C_lex.Equal ->
      typed_binary depth (fun typ left right -> C_raw.Eq (typ, left, right)) state
  | C_lex.Good -> typed_unary depth (fun bad value -> C_raw.Inl (value, bad)) state
  | C_lex.Bad -> typed_unary depth (fun good value -> C_raw.Inr (good, value)) state
  | C_lex.Take -> indexed_unary depth (fun at value -> C_raw.Take (at, value)) state
  | C_lex.Drop -> indexed_unary depth (fun at value -> C_raw.Drop (at, value)) state
  | C_lex.At -> indexed_unary depth (fun at value -> C_raw.At (at, value)) state
  | C_lex.Read -> effect depth (fun kind -> C_syn.ARead kind) state
  | C_lex.Write -> effect depth (fun kind -> C_syn.AWrite kind) state
  | C_lex.Emit -> effect depth (fun kind -> C_syn.AEmit kind) state
  | C_lex.Fail -> effect depth (fun kind -> C_syn.AFail kind) state
  | C_lex.Close ->
      begin
        match tok (next state) with
        | C_lex.Lbrack -> effect depth (fun kind -> C_syn.AClose kind) state
        | _ -> unary depth (fun value -> C_raw.Close value) state
      end
  | _ ->
      need [C_lex.F_nat; C_lex.F_hex; C_lex.F_true; C_lex.F_false;
        C_lex.F_unit; C_lex.F_ident; C_lex.F_lparen; C_lex.F_vec;
        C_lex.F_seq; C_lex.F_make;
        C_lex.F_fst; C_lex.F_snd; C_lex.F_equal; C_lex.F_ok;
        C_lex.F_err; C_lex.F_read; C_lex.F_write; C_lex.F_emit;
        C_lex.F_fail; C_lex.F_cat; C_lex.F_take; C_lex.F_drop;
        C_lex.F_vcat; C_lex.F_at; C_lex.F_uncons; C_lex.F_step;
        C_lex.F_close; C_lex.F_abs; C_lex.F_fit; C_lex.F_wide;
        C_lex.F_length] state

and direct_call depth first name state =
  let* fn =
    match form_find name state.defs with
    | Some (Mono item) when C_fun.direct item -> Ok item
    | Some (Mono _) | Some (Spec _) | Some (Poly _) ->
        Error {
          cause = Fun (C_fun.Direct (C_syn.name_text name));
          span = first;
        }
    | None ->
        Error {
          cause = Fun (C_fun.Fn (C_syn.name_text name));
          span = first;
        }
  in
  let rec args out state =
    match tok state with
    | C_lex.Rparen -> Ok (List.rev out, next state)
    | _ ->
        let* value, state = raw_expr (depth + 1) state in
        begin
          match tok state with
          | C_lex.Comma -> args (value :: out) (next state)
          | C_lex.Rparen -> Ok (List.rev (value :: out), next state)
          | _ -> need [C_lex.F_comma; C_lex.F_rparen] state
        end
  in
  let rec elab out = function
    | [] -> Ok (List.rev out)
    | value :: rest ->
        let* value =
          match C_raw.elab state.sizes value with
          | Ok value -> Ok value
          | Error error -> Error { cause = raw_cause error; span = first }
        in
        elab (value :: out) rest
  in
  let* values, state = args [] (next state) in
  let* values = elab [] values in
  let* value =
    match C_fun.apply fn values with
    | Ok value -> Ok value
    | Error error -> Error { cause = Fun error; span = first }
  in
  let* id =
    match C_nat.of_int state.nodes with
    | Some value -> Ok value
    | None ->
        Error {
          cause = Nodes (C_rule.local.tm_nodes, state.nodes);
          span = first;
        }
  in
  let result =
    C_syn.bind (C_syn.dslot id) C_type.Many fn.C_fun.arr.out
  in
  let value = C_syn.Let (result, value, C_syn.Var result.name) in
  put ~first state (C_raw.of_syn value)

and product depth state =
  let first = span state in
  let* left, state = atom depth state in
  let rec walk left state =
    match tok state with
    | C_lex.Star ->
        let* right, state = atom (depth + 1) (next state) in
        let* left, state = put ~first state (C_raw.Mul (left, right)) in
        walk left state
    | C_lex.Slash ->
        let* right, state = atom (depth + 1) (next state) in
        let* left, state = put ~first state (C_raw.Div (left, right)) in
        walk left state
    | C_lex.Percent ->
        let* right, state = atom (depth + 1) (next state) in
        let* left, state = put ~first state (C_raw.Mod (left, right)) in
        walk left state
    | _ -> Ok (left, state)
  in
  walk left state

and add depth state =
  let first = span state in
  let* left, state = product depth state in
  let rec walk left state =
    match tok state with
    | C_lex.Plus ->
        let* right, state = product (depth + 1) (next state) in
        let* left, state = put ~first state (C_raw.Add (left, right)) in
        walk left state
    | C_lex.Minus ->
        let* right, state = product (depth + 1) (next state) in
        let* left, state = put ~first state (C_raw.Sub (left, right)) in
        walk left state
    | _ -> Ok (left, state)
  in
  walk left state

and order depth state =
  let first = span state in
  let* left, state = add depth state in
  let rel =
    match tok state with
    | C_lex.EqEq -> Some `Eq
    | C_lex.Ne -> Some `Ne
    | C_lex.Lt -> Some (`Cmp C_syn.Lt)
    | C_lex.Le -> Some (`Cmp C_syn.Le)
    | C_lex.Gt -> Some (`Cmp C_syn.Gt)
    | C_lex.Ge -> Some (`Cmp C_syn.Ge)
    | _ -> None
  in
  match rel with
  | None -> Ok (left, state)
  | Some `Eq ->
      let* right, state = add (depth + 1) (next state) in
      put ~first state (C_raw.Eq (C_decl.Int, left, right))
  | Some `Ne ->
      let* right, state = add (depth + 1) (next state) in
      let* same, state =
        put ~first state (C_raw.Eq (C_decl.Int, left, right))
      in
      put ~first state (C_raw.Eq (C_decl.Bool, same, C_raw.KBool false))
  | Some (`Cmp rel) ->
      let* right, state = add (depth + 1) (next state) in
      put ~first state (C_raw.Cmp (rel, left, right))

and raw_expr depth state =
  let* () = guard depth state in
  match tok state with
  | C_lex.Let -> let_term depth state
  | C_lex.Split -> split depth state
  | C_lex.If -> if_term depth state
  | C_lex.Case -> case_term depth state
  | C_lex.Fold -> fold depth state
  | C_lex.Every -> quant C_quant.Every depth state
  | C_lex.Any -> quant C_quant.Any depth state
  | C_lex.Count -> quant C_quant.Count depth state
  | C_lex.Total -> quant C_quant.Sum depth state
  | C_lex.Weave -> weave depth state
  | C_lex.Braid -> braid depth state
  | C_lex.Loom -> loom depth state
  | C_lex.Orbit -> orbit depth state
  | C_lex.Wake -> wake depth state
  | C_lex.Rift -> rift depth state
  | _ -> order depth state

let expr depth state =
  let mark = span state in
  let start = state.mcount in
  let* raw, state = raw_expr depth state in
  match C_raw.elab_trace state.sizes raw with
  | Ok (term, terms) ->
      let last = state.items.(state.at - 1).C_lex.span in
      let whole = { mark with last = last.last } in
      let marks = origin_marks whole terms (marks_since start state) term in
      Ok (term, replace_marks start marks state)
  | Error error -> Error { cause = raw_cause error; span = mark }

let rec flow depth state =
  let* () = guard depth state in
  match tok state with
  | C_lex.Let -> flow_let depth state
  | C_lex.If -> flow_if depth state
  | C_lex.Use -> use_term depth state
  | _ ->
      let start = state.mcount in
      let* term, state = expr depth state in
      Ok ((C_fun.Ret term, Ret_marks (marks_since start state)), state)

and flow_let depth state =
  let first = span state in
  let* result, state = bind (next state) in
  let* state = take C_lex.F_eq state in
  let value_start = state.mcount in
  let* value, state = expr (depth + 1) state in
  let value_marks = marks_since value_start state in
  let* state = take C_lex.F_in state in
  let* (body, body_marks), state = flow (depth + 1) state in
  let* term, state = put_fun ~first state (C_fun.Let (result, value, body)) in
  Ok ((term, Let_marks (value_marks, body_marks, top_mark state)), state)

and flow_if depth state =
  let first = span state in
  let guard_start = state.mcount in
  let* cond, state = expr (depth + 1) (next state) in
  let guard_marks = marks_since guard_start state in
  let* state = take C_lex.F_then state in
  let* (yes, yes_marks), state = flow (depth + 1) state in
  let* state = take C_lex.F_else state in
  let* (no, no_marks), state = flow (depth + 1) state in
  let* term, state = put_fun ~first state (C_fun.If (cond, yes, no)) in
  Ok ((term, If_marks (guard_marks, yes_marks, no_marks, top_mark state)), state)

and use_term depth state =
  let first = span state in
  let* fn_name, state = name (fun value -> Name value) (next state) in
  let found = form_find fn_name state.defs in
  let* type_args, state =
    match found with
    | Some (Poly _) ->
        let* state = take C_lex.F_kind state in
        let* state = take C_lex.F_lbrack state in
        let rec args out state =
          let* value, state = typ state in
          match tok state with
          | C_lex.Comma -> args (value :: out) (next state)
          | C_lex.Rbrack -> Ok (List.rev (value :: out), next state)
          | _ -> need [C_lex.F_comma; C_lex.F_rbrack] state
        in
        args [] state
    | _ -> Ok ([], state)
  in
  let* spec_args, state =
    match found with
    | Some (Spec _) ->
        let* state = take C_lex.F_size state in
        let* state = take C_lex.F_lbrack state in
        let rec args out state =
          let* value, state = idx state in
          match tok state with
          | C_lex.Comma -> args (value :: out) (next state)
          | C_lex.Rbrack -> Ok (List.rev (value :: out), next state)
          | _ -> need [C_lex.F_comma; C_lex.F_rbrack] state
        in
        args [] state
    | Some (Poly _) when tok state = C_lex.Size ->
        let* state = take C_lex.F_size state in
        let* state = take C_lex.F_lbrack state in
        let rec args out state =
          let* value, state = idx state in
          match tok state with
          | C_lex.Comma -> args (value :: out) (next state)
          | C_lex.Rbrack -> Ok (List.rev (value :: out), next state)
          | _ -> need [C_lex.F_comma; C_lex.F_rbrack] state
        in
        args [] state
    | Some (Poly _) | Some (Mono _) | None -> Ok ([], state)
  in
  let* state = take C_lex.F_lbrack state in
  let rec caps out state =
    match tok state with
    | C_lex.Rbrack -> Ok (List.rev out, next state)
    | _ ->
        let start = state.mcount in
        let* value, state = expr (depth + 1) state in
        let item = value, marks_since start state in
        begin
          match tok state with
          | C_lex.Comma -> caps (item :: out) (next state)
          | C_lex.Rbrack -> Ok (List.rev (item :: out), next state)
          | _ -> need [C_lex.F_comma; C_lex.F_rbrack] state
        end
  in
  let* cap_items, state = caps [] state in
  let caps = List.map fst cap_items in
  let cap_marks = List.map snd cap_items in
  let* state = take C_lex.F_lparen state in
  let arg_start = state.mcount in
  let* arg, state = expr (depth + 1) state in
  let arg_marks = marks_since arg_start state in
  let* state = take C_lex.F_rparen state in
  let* state = take C_lex.F_as state in
  let* result, state = bind state in
  let* call_name, state =
    match found with
    | Some (Spec item) ->
        begin
          match C_spec.args state.sizes spec_args with
          | Error error -> Error { cause = Inst error; span = span state }
          | Ok vals ->
              let key = fn_name, [], vals in
              begin
                match Imap.find_opt key state.inst with
                | Some fn -> Ok (fn.C_fun.name, state)
                | None ->
                    if state.gen >= C_check.max_inputs then
                      Error { cause = Fun (C_fun.Fns
                        (C_check.max_inputs, state.gen + 1)); span = span state }
                    else
                      begin
                        match C_nat.of_int state.gen with
                        | None ->
                            Error { cause = Fun (C_fun.Fns
                              (C_check.max_inputs, state.gen + 1));
                              span = span state }
                        | Some id ->
                            let fresh = C_syn.dslot id in
                            begin
                              match C_spec.mono state.sizes fresh item vals with
                              | Error error ->
                                  Error { cause = Inst error; span = span state }
                              | Ok fn ->
                                  let inst = Imap.add key fn state.inst in
                                  let fmarks =
                                    match find_marks fn_name state.fmarks with
                                    | Some marks ->
                                        (fresh, spread_form marks fn.C_fun.body)
                                        :: state.fmarks
                                    | None -> state.fmarks
                                  in
                                  Ok (fresh, { state with inst; fmarks;
                                    gen = state.gen + 1 })
                            end
                      end
              end
        end
    | Some (Poly item) ->
        begin
          match C_poly.actuals state.sizes (C_poly.pars item) type_args with
          | Error error -> Error { cause = Poly_error error; span = span state }
          | Ok types ->
              begin
                match C_spec.args state.sizes spec_args with
                | Error error -> Error { cause = Inst error; span = span state }
                | Ok vals ->
                    let key = fn_name, types, vals in
                    begin
                      match Imap.find_opt key state.inst with
                      | Some fn -> Ok (fn.C_fun.name, state)
                      | None ->
                          if state.gen >= C_check.max_inputs then
                            Error { cause = Fun (C_fun.Fns
                              (C_check.max_inputs, state.gen + 1));
                              span = span state }
                          else
                            begin
                              match C_nat.of_int state.gen with
                              | None ->
                                  Error { cause = Fun (C_fun.Fns
                                    (C_check.max_inputs, state.gen + 1));
                                    span = span state }
                              | Some id ->
                                  let fresh = C_syn.dslot id in
                                  begin
                                    match C_poly.mono state.sizes fresh item
                                        types vals with
                                    | Error error -> Error {
                                        cause = Poly_error error;
                                        span = span state }
                                    | Ok fn ->
                                        let inst = Imap.add key fn state.inst in
                                        let fmarks =
                                          match find_marks fn_name state.fmarks with
                                          | Some marks ->
                                              (fresh,
                                                spread_form marks fn.C_fun.body)
                                              :: state.fmarks
                                          | None -> state.fmarks
                                        in
                                        Ok (fresh, { state with inst; fmarks;
                                          gen = state.gen + 1 })
                                  end
                            end
                    end
              end
        end
    | Some (Mono _) | None -> Ok (fn_name, state)
  in
  let* state = take C_lex.F_in state in
  let* (rest, rest_marks), state = flow (depth + 1) state in
  let* term, state =
    put_fun ~first state (C_fun.Call (result, call_name, caps, arg, rest))
  in
  Ok ((term, Call_marks (call_name, cap_marks, arg_marks, rest_marks,
    top_mark state)), state)

let captures state =
  let* state = take C_lex.F_lbrack state in
  let rec walk out state =
    match tok state with
    | C_lex.Rbrack -> Ok (List.rev out, next state)
    | _ ->
        let* item, state = bind state in
        begin
          match tok state with
          | C_lex.Comma -> walk (item :: out) (next state)
          | C_lex.Rbrack -> Ok (List.rev (item :: out), next state)
          | _ -> need [C_lex.F_comma; C_lex.F_rbrack] state
        end
  in
  walk [] state

let spec_caps state =
  let* state = take C_lex.F_lbrack state in
  let rec walk out state =
    match tok state with
    | C_lex.Rbrack -> Ok (List.rev out, next state)
    | _ ->
        let* item, state = spec_bind state in
        begin
          match tok state with
          | C_lex.Comma -> walk (item :: out) (next state)
          | C_lex.Rbrack -> Ok (List.rev (item :: out), next state)
          | _ -> need [C_lex.F_comma; C_lex.F_rbrack] state
        end
  in
  walk [] state

let form_pars state =
  let* state = take C_lex.F_lbrack (next state) in
  let rec walk out state =
    let* item, state = name (fun value -> Size_name value) state in
    match tok state with
    | C_lex.Comma -> walk (item :: out) (next state)
    | C_lex.Rbrack -> Ok (List.rev (item :: out), next state)
    | _ -> need [C_lex.F_comma; C_lex.F_rbrack] state
  in
  walk [] state

let form_laws state =
  let* state = take C_lex.F_lbrack (next state) in
  let rec walk out state =
    let* left, state = idx state in
    let* rel, state =
      match tok state with
      | C_lex.Eq -> Ok (C_idx.Eq, next state)
      | C_lex.Le -> Ok (C_idx.Le, next state)
      | _ -> need [C_lex.F_eq; C_lex.F_le] state
    in
    let* right, state = idx state in
    let item = C_law.make rel left right in
    match tok state with
    | C_lex.Comma -> walk (item :: out) (next state)
    | C_lex.Rbrack ->
        begin
          match C_law.set (List.rev (item :: out)) with
          | Ok laws -> Ok (laws, next state)
          | Error error -> Error {
              cause = Inst (C_spec.Law error);
              span = span state;
            }
        end
    | _ -> need [C_lex.F_comma; C_lex.F_rbrack] state
  in
  walk [] state

let form_kinds state =
  let* state = take C_lex.F_lbrack (next state) in
  let rec walk pars env state =
    let* name, state = name (fun value -> Name value) state in
    let* state = take C_lex.F_colon state in
    let* kind, state =
      match tok state with
      | C_lex.Data -> Ok (C_poly.Data, next state)
      | C_lex.Res -> Ok (C_poly.Res, next state)
      | _ -> need [C_lex.F_data; C_lex.F_res] state
    in
    let pars = C_poly.par name kind :: pars in
    let env = (name, kind) :: env in
    match tok state with
    | C_lex.Comma -> walk pars env (next state)
    | C_lex.Rbrack -> Ok ((List.rev pars, List.rev env), next state)
    | _ -> need [C_lex.F_comma; C_lex.F_rbrack] state
  in
  walk [] [] state

let mark_atom state =
  let make =
    match tok state with
    | C_lex.Read -> Some (fun kind -> C_eff.Read kind)
    | C_lex.Write -> Some (fun kind -> C_eff.Write kind)
    | C_lex.Emit -> Some (fun kind -> C_eff.Emit kind)
    | C_lex.Fail -> Some (fun kind -> C_eff.Fail kind)
    | C_lex.Close -> Some (fun kind -> C_eff.Close kind)
    | _ -> None
  in
  match make with
  | None ->
      need [C_lex.F_read; C_lex.F_write; C_lex.F_emit; C_lex.F_fail;
        C_lex.F_close] state
  | Some make ->
      let* state = take C_lex.F_lbrack (next state) in
      let mark = span state in
      let* raw, state = nat state in
      let* kind =
        match C_nat.make raw with
        | Some kind -> Ok kind
        | None -> Error { cause = Mark_nat raw; span = mark }
      in
      let* state = take C_lex.F_rbrack state in
      Ok (make kind, state)

let marks state =
  let* state = take C_lex.F_marks state in
  let* state = take C_lex.F_lbrace state in
  let rec walk row state =
    match tok state with
    | C_lex.Rbrace -> Ok (row, next state)
    | _ ->
        let mark = span state in
        let* atom, state = mark_atom state in
        let next_row = C_eff.add atom row in
        if C_eff.equal next_row row then
          Error { cause = Mark_dup (C_eff.atom_text atom); span = mark }
        else
          match tok state with
          | C_lex.Comma -> walk next_row (next state)
          | C_lex.Rbrace -> Ok (next_row, next state)
          | _ -> need [C_lex.F_comma; C_lex.F_rbrace] state
  in
  walk C_eff.empty state

let axis form state =
  let* state = take form state in
  let* state = take C_lex.F_lbrack state in
  let mark = span state in
  let* raw, state = nat state in
  let* value =
    match C_nat.make raw with
    | Some value -> Ok (C_nat.to_z value)
    | None -> Error { cause = Under_nat raw; span = mark }
  in
  let* state = take C_lex.F_rbrack state in
  Ok (value, state)

let under state =
  let* state = take C_lex.F_under state in
  let* state = take C_lex.F_lbrace state in
  let* steps, state = axis C_lex.F_steps state in
  let* state = take C_lex.F_comma state in
  let* depth, state = axis C_lex.F_depth state in
  let* state = take C_lex.F_comma state in
  let* work, state = axis C_lex.F_work state in
  let* state = take C_lex.F_rbrace state in
  Ok (C_limit.make3 ~steps ~depth ~work, state)

let form state =
  let* fn_name, state = name (fun value -> Name value) (next state) in
  let* (kinds, kind_env), state =
    match tok state with
    | C_lex.Kind -> form_kinds state
    | _ -> Ok (([], []), state)
  in
  let* pars, state =
    match tok state with
    | C_lex.Size -> form_pars state
    | _ -> Ok ([], state)
  in
  let* laws, state =
    match pars, tok state with
    | _ :: _, C_lex.Law -> form_laws state
    | _, _ -> Ok (C_law.empty, state)
  in
  let state = { state with kinds = kind_env } in
  let* head, state =
    match kinds, pars with
    | [], [] ->
      let* caps, state = captures state in
      let* state = take C_lex.F_lparen state in
      let* arg, state = bind state in
      Ok (`Mono (caps, arg), state)
    | _, _ ->
      let* caps, state = spec_caps state in
      let* state = take C_lex.F_lparen state in
      let* arg, state = spec_bind state in
      Ok (`Spec (caps, arg), state)
  in
  let* state = take C_lex.F_rparen state in
  let* state = take C_lex.F_thin state in
  let* state = take C_lex.F_lbrack state in
  let* mode, state = mul state in
  let* state = take C_lex.F_rbrack state in
  let mark = span state in
  let* raw_out, state = typ state in
  let* eff, state = marks state in
  let* lim, state =
    match tok state with
    | C_lex.Under ->
        let* value, state = under state in
        Ok (Some value, state)
    | _ -> Ok (None, state)
  in
  let* state = take C_lex.F_eq state in
  let body_first = span state in
  let mark_start = state.mcount in
  let* body, state = raw_expr 0 state in
  let body_last = state.items.(state.at - 1).C_lex.span in
  let fn_marks = {
    seq = marks_since mark_start state;
    root = Some { body_first with last = body_last.last };
  } in
  let state = { state with kinds = [] } in
  match head with
  | `Mono (caps, arg) ->
      let* body =
        match C_raw.elab state.sizes body with
        | Ok value -> Ok value
        | Error error -> Error { cause = raw_cause error; span = mark }
      in
      let* out =
        match C_decl.typ_in state.sizes raw_out with
        | Ok value -> Ok value
        | Error error -> Error { cause = Decl error; span = mark }
      in
      let arr = C_fun.marks (C_fun.arr mode caps arg out) eff in
      let arr = Option.fold ~none:arr ~some:(C_fun.under arr) lim in
      Ok (Mono (C_fun.fn fn_name arr body), fn_marks, state)
  | `Spec (caps, arg) ->
      let arr = C_spec.marks (C_spec.arr mode caps arg raw_out) eff in
      let arr = Option.fold ~none:arr ~some:(C_spec.under arr) lim in
      begin
        match C_spec.fn fn_name pars arr body with
        | Ok item ->
            let item = C_spec.require laws item in
            begin
              match kinds with
              | [] -> Ok (Spec item, fn_marks, state)
              | _ ->
                  begin
                    match C_poly.fn kinds item with
                    | Ok item -> Ok (Poly item, fn_marks, state)
                    | Error error ->
                        Error { cause = Poly_error error; span = mark }
                  end
              end
        | Error error -> Error { cause = Inst error; span = mark }
      end

let bits mark value =
  let limit = Z.shift_left Z.one C_data.tag_len in
  if Z.sign value < 0 || Z.geq value limit then
    Error { cause = Tag value; span = mark }
  else Ok (List.init C_data.tag_len (fun bit -> Z.testbit value bit))

let permission state =
  let mark = span state in
  let* state = take C_lex.F_cap (next state) in
  let* state = take C_lex.F_lbrack state in
  let* kind, state = nat state in
  let* state = take C_lex.F_rbrack state in
  let* state = take C_lex.F_eq state in
  let* perm, state =
    match tok state with
    | C_lex.Step ->
        let state = next state in
        begin
          match tok state with
          | C_lex.Plus ->
              let* state = take C_lex.F_close (next state) in
              Ok (C_perm.Both, state)
          | _ -> Ok (C_perm.Step, state)
        end
    | C_lex.Close -> Ok (C_perm.Close, next state)
    | _ -> need [C_lex.F_step; C_lex.F_close] state
  in
  match C_perm.add state.perms kind perm with
  | Ok perms -> Ok { state with perms }
  | Error error -> Error { cause = Perm error; span = mark }

let data_ctor state =
  let* ctor_name, state = name (fun value -> Name value) state in
  let* state = take C_lex.F_lparen state in
  let mark = span state in
  let* raw, state = typ state in
  let* arg =
    match C_decl.typ_in state.sizes raw with
    | Ok value -> Ok value
    | Error error -> Error { cause = Decl error; span = mark }
  in
  let* state = take C_lex.F_rparen state in
  Ok (C_data.ctor ctor_name arg, state)

let shape_field state =
  let* field_name, state = name (fun value -> Name value) state in
  let* state = take C_lex.F_colon state in
  let mark = span state in
  let* raw, state = typ state in
  let* field_type =
    match C_decl.typ_in state.sizes raw with
    | Ok value -> Ok value
    | Error error -> Error { cause = Decl error; span = mark }
  in
  Ok (C_rec.field field_name field_type, state)

let tag_used code state =
  List.exists (fun (item : dtype) -> Z.equal code item.code) state.data
  || List.exists (fun (item : shape) -> Z.equal code item.code) state.shapes

let data_decl state =
  let mark = span state in
  let* data_name, state = name (fun value -> Name value) (next state) in
  let text = C_syn.name_text data_name in
  if List.exists (fun item -> C_syn.name_equal data_name item.dname) state.data
    || Option.is_some (shape_find data_name state.shapes) then
    Error { cause = Data_dup text; span = mark }
  else
    let* state = take C_lex.F_tag state in
    let tag_mark = span state in
    let* code, state = nat state in
    let* tag = bits tag_mark code in
    if tag_used code state then
      Error { cause = Tag_dup code; span = tag_mark }
    else
      let* state = take C_lex.F_eq state in
      let rec ctors out state =
        let* ctor, state = data_ctor state in
        let out = ctor :: out in
        match tok state with
        | C_lex.Bar -> ctors out (next state)
        | _ -> Ok (List.rev out, state)
      in
      let* ctors, state = ctors [] state in
      begin
        match C_data.decl tag data_name ctors with
        | Error error -> Error { cause = Data error; span = mark }
        | Ok decl ->
            let item = { dname = data_name; code; decl } in
            Ok { state with data = item :: state.data }
      end

let shape_decl state =
  let mark = span state in
  let* shape_name, state = name (fun value -> Name value) (next state) in
  let text = C_syn.name_text shape_name in
  if Option.is_some (data_find shape_name state.data)
    || Option.is_some (shape_find shape_name state.shapes) then
    Error { cause = Shape_dup text; span = mark }
  else
    let* state = take C_lex.F_tag state in
    let tag_mark = span state in
    let* code, state = nat state in
    let* tag = bits tag_mark code in
    if tag_used code state then
      Error { cause = Tag_dup code; span = tag_mark }
    else
      let* state = take C_lex.F_eq state in
      let* state = take C_lex.F_lbrace state in
      let rec fields out state =
        let* field, state = shape_field state in
        let out = field :: out in
        match tok state with
        | C_lex.Comma -> fields out (next state)
        | C_lex.Rbrace -> Ok (List.rev out, next state)
        | _ -> need [C_lex.F_comma; C_lex.F_rbrace] state
      in
      let* fields, state = fields [] state in
      begin
        match C_rec.decl tag shape_name fields with
        | Error error -> Error { cause = Rec error; span = mark }
        | Ok decl ->
            let item = { sname = shape_name; code; decl } in
            Ok { state with shapes = item :: state.shapes }
      end

let put_size mark sizes name term =
  match C_idx.put sizes name term with
  | Ok value -> Ok value
  | Error (C_idx.Dup text) -> Error { cause = Size_dup text; span = mark }
  | Error error -> Error { cause = Idx error; span = mark }

let law sizes state =
  let mark = span state in
  let* left, state = idx (next state) in
  let* rel, state =
    match tok state with
    | C_lex.Eq -> Ok (C_idx.Eq, next state)
    | C_lex.Le -> Ok (C_idx.Le, next state)
    | _ -> need [C_lex.F_eq; C_lex.F_le] state
  in
  let* right, state = idx state in
  match C_idx.hold sizes rel left right with
  | Ok () -> Ok state
  | Error error -> Error { cause = Idx error; span = mark }

let index_in sizes state =
  let mark = span state in
  let* term, state = idx state in
  match C_idx.eval sizes term with
  | Ok value -> Ok (C_nat.to_z value, state)
  | Error error -> Error { cause = Idx error; span = mark }

let vword_idx text sizes state =
  let* state = word text state in
  let* state = take C_lex.F_lbrack state in
  let* value, state = index_in sizes state in
  let* state = take C_lex.F_rbrack state in
  Ok (value, state)

let vform_idx form sizes state =
  let* state = take form state in
  let* state = take C_lex.F_lbrack state in
  let* value, state = index_in sizes state in
  let* state = take C_lex.F_rbrack state in
  Ok (value, state)

let rec vexpr depth state = vadd depth state

and vadd depth state =
  let* left, state = vmul depth state in
  let rec walk left state =
    match tok state with
    | C_lex.Plus ->
        let* right, state = vmul (depth + 1) (next state) in
        let* term, state = put state (C_fhe.add left right) in
        walk term state
    | _ -> Ok (left, state)
  in
  walk left state

and vmul depth state =
  let* left, state = vatom depth state in
  let rec walk left state =
    match tok state with
    | C_lex.Star ->
        let* right, state = vatom (depth + 1) (next state) in
        let* term, state = put state (C_fhe.mul left right) in
        walk term state
    | _ -> Ok (left, state)
  in
  walk left state

and vatom depth state =
  let* () = guard depth state in
  match tok state with
  | C_lex.Ident value when String.equal value "renew" ->
      let* state = take C_lex.F_lparen (next state) in
      let* term, state = vexpr (depth + 1) state in
      let* state = take C_lex.F_rparen state in
      put state (C_fhe.recrypt term)
  | C_lex.Ident value when String.equal value "trim" ->
      let* state = take C_lex.F_lbrack (next state) in
      let* rem, state = index_in state.sizes state in
      let* state = take C_lex.F_rbrack state in
      let* state = take C_lex.F_lparen state in
      let* term, state = vexpr (depth + 1) state in
      let* state = take C_lex.F_rparen state in
      put state (C_fhe.trim rem term)
  | C_lex.Ident _ ->
      let* name, state = name (fun value -> Name value) state in
      put state (C_fhe.var name)
  | C_lex.Lparen ->
      let* term, state = vexpr (depth + 1) (next state) in
      let* state = take C_lex.F_rparen state in
      Ok (term, state)
  | _ -> need [C_lex.F_ident; C_lex.F_lparen] state

let veil_state input_name state =
  let* state = word "state" state in
  let* state = take C_lex.F_lbrace state in
  let* terms, state = vword_idx "terms" state.sizes state in
  let* state = take C_lex.F_comma state in
  let* slots, state = vword_idx "slots" state.sizes state in
  let* state = take C_lex.F_comma state in
  let* query, state = vword_idx "query" state.sizes state in
  let* state = take C_lex.F_rbrace state in
  Ok (C_hfhe.input input_name ~terms ~slots ~query, state)

let veil_input staged state =
  let* input_name, state = name (fun value -> Name value) (next state) in
  let* state = take C_lex.F_colon state in
  let* state = word "enc" state in
  let* state = take C_lex.F_lbrack state in
  let* key, state = index_in state.sizes state in
  let* state = take C_lex.F_comma state in
  let* rem, state = index_in state.sizes state in
  let* state = take C_lex.F_rbrack state in
  let* hinput, state =
    if staged then
      let* input, state = veil_state input_name state in
      Ok (Some input, state)
    else Ok (None, state)
  in
  Ok (C_fhe.input input_name ~key ~rem, hinput, state)

let veil_param key state =
  let mark = span state in
  let* state = word "param" state in
  let* state = take C_lex.F_lbrack state in
  let* id, state = index_in state.sizes state in
  let* state = take C_lex.F_rbrack state in
  let* room, state = vword_idx "room" state.sizes state in
  match C_fhe.param ~id ~key ~cap:room with
  | Ok value -> Ok (value, state)
  | Error error -> Error { cause = Fhe error; span = mark }

let veil_decl state =
  let mark = span state in
  let* state = word "veil" state in
  let* veil_name, state = name (fun value -> Name value) state in
  let text = C_syn.name_text veil_name in
  if List.exists (fun item -> C_syn.name_equal veil_name item.vname) state.veils
  then Error { cause = Veil_dup text; span = mark }
  else
    let* key, state = vword_idx "key" state.sizes state in
    let* full, state = vform_idx C_lex.F_depth state.sizes state in
    let* state = take C_lex.F_under state in
    let* state = take C_lex.F_lbrace state in
    let* add, state = vword_idx "add" state.sizes state in
    let* state = take C_lex.F_comma state in
    let* mul, state = vword_idx "mul" state.sizes state in
    let* state = take C_lex.F_comma state in
    let* recrypt, state = vword_idx "renew" state.sizes state in
    let* state = take C_lex.F_rbrace state in
    let* staged, state =
      match tok state with
      | C_lex.Ident _ when is_word "stage" state ->
          let* state = word "stage" state in
          let* state = take C_lex.F_lbrack state in
          let* state = word "prod" state in
          let* state = take C_lex.F_rbrack state in
          Ok (true, state)
      | _ -> Ok (false, state)
    in
    let* state = take C_lex.F_lbrace state in
    let rec items params inputs hinputs state =
      match tok state with
      | C_lex.Ident _ when is_word "param" state ->
          let* param, state = veil_param key state in
          items (param :: params) inputs hinputs state
      | C_lex.Input ->
          let* input, hinput, state = veil_input staged state in
          let hinputs = Option.fold ~none:hinputs
            ~some:(fun value -> value :: hinputs) hinput in
          items params (input :: inputs) hinputs state
      | C_lex.Term ->
          Ok (List.rev params, List.rev inputs, List.rev hinputs, next state)
      | _ -> need [C_lex.F_ident; C_lex.F_input; C_lex.F_term] state
    in
    let* params, inputs, hinputs, state = items [] [] [] state in
    let* term, state = vexpr 0 state in
    let* state = take C_lex.F_rbrace state in
    let* cfg =
      match C_fhe.cfg ~depth:full ~add ~mul ~recrypt with
      | Ok value -> Ok value
      | Error error -> Error { cause = Fhe error; span = mark }
    in
    let* profile =
      match C_fhe.profile [key, cfg] with
      | Ok value -> Ok value
      | Error error -> Error { cause = Fhe error; span = mark }
    in
    let* env =
      match C_fhe.env profile inputs with
      | Ok value -> Ok value
      | Error error -> Error { cause = Fhe error; span = mark }
    in
    let* catalog =
      match C_fhe.catalog params with
      | Ok value -> Ok value
      | Error error -> Error { cause = Fhe error; span = mark }
    in
    let* vinfo, vparam, vstage, vlink =
      if staged then
        begin
          match C_hpar.check_cat C_hpar.prod_cat catalog profile env hinputs term with
          | Ok link ->
              Ok (link.fhe, link.param, Some link.hfhe, Some link)
          | Error error -> Error { cause = Hpar error; span = mark }
        end
      else
        let* vinfo =
          match C_fhe.check profile env term with
          | Ok value -> Ok value
          | Error error -> Error { cause = Fhe error; span = mark }
        in
        let* vparam =
          match C_fhe.params catalog vinfo with
          | Ok value -> Ok value
          | Error error -> Error { cause = Fhe error; span = mark }
        in
        Ok (vinfo, vparam, None, None)
    in
    let* vhost =
      match C_fhe.host profile env term with
      | Ok value -> Ok value
      | Error error -> Error { cause = Fhe error; span = mark }
    in
    Ok { state with
      veils = { vname = veil_name; vinfo; vparam; vhost; vstage; vlink }
        :: state.veils }

let heads state =
  let rec walk sizes inputs state =
    match tok state with
    | C_lex.Size ->
        let mark = span state in
        let* name, state = name (fun value -> Size_name value) (next state) in
        let* state = take C_lex.F_eq state in
        let* raw, state = nat state in
        let* term, state = idx_lit raw state in
        let* sizes = put_size mark sizes name term in
        walk sizes inputs { state with sizes }
    | C_lex.Measure ->
        let mark = span state in
        let* name, state = name (fun value -> Size_name value) (next state) in
        let* state = take C_lex.F_eq state in
        let* term, state = idx state in
        let* sizes = put_size mark sizes name term in
        walk sizes inputs { state with sizes }
    | C_lex.Law ->
        let* state = law sizes state in
        walk sizes inputs state
    | C_lex.Input ->
        let* input, state = input (next state) in
        walk sizes (input :: inputs) state
    | C_lex.Ident _ when is_word "veil" state ->
        Ok (sizes, List.rev inputs, state)
    | C_lex.Data | C_lex.Shape | C_lex.Permit | C_lex.Form | C_lex.Term ->
        Ok (sizes, List.rev inputs, state)
    | _ ->
        need [C_lex.F_size; C_lex.F_measure; C_lex.F_law; C_lex.F_input;
          C_lex.F_data; C_lex.F_shape; C_lex.F_permit; C_lex.F_form;
          C_lex.F_ident; C_lex.F_term] state
  in
  walk C_idx.empty [] state

let decls inputs state =
  let rec walk out state =
    match tok state with
    | C_lex.Data ->
        let* state = data_decl state in
        walk out state
    | C_lex.Shape ->
        let* state = shape_decl state in
        walk out state
    | C_lex.Input ->
        let* item, state = input (next state) in
        walk (item :: out) state
    | C_lex.Permit ->
        let* state = permission state in
        walk out state
    | C_lex.Ident _ when is_word "veil" state ->
        let* state = veil_decl state in
        walk out state
    | C_lex.Form | C_lex.Term -> Ok (List.rev out, state)
    | _ ->
        need [C_lex.F_data; C_lex.F_shape; C_lex.F_input; C_lex.F_permit;
          C_lex.F_ident; C_lex.F_form; C_lex.F_term] state
  in
  walk (List.rev inputs) state

let forms state =
  let rec walk seen out state =
    match tok state with
    | C_lex.Form ->
        let mark = span state in
        let* item, marks, state = form state in
        let name =
          match item with
          | Mono value -> value.C_fun.name
          | Spec value -> value.C_spec.name
          | Poly value -> C_poly.name value
        in
        if List.exists (C_syn.name_equal name) seen then
          Error { cause = Fun (C_fun.Dup (C_syn.name_text name)); span = mark }
        else
          let out =
            match item with
            | Mono value -> value :: out
            | Spec _ | Poly _ -> out
          in
          let marks =
            match item with
            | Mono value -> fit_form marks value.C_fun.body
            | Spec _ | Poly _ -> marks
          in
          walk (name :: seen) out { state with
            defs = item :: state.defs;
            fmarks = (name, marks) :: state.fmarks;
          }
    | C_lex.Term -> Ok (List.rev out, next state)
    | _ -> need [C_lex.F_form; C_lex.F_term] state
  in
  walk [] [] state

let named_input_span spans occurrence name =
  let rec walk seen = function
    | [] -> None
    | (key, at) :: rest when String.equal key name ->
      if seen = occurrence then Some at else walk (seen + 1) rest
    | _ :: rest -> walk seen rest
  in
  walk 0 spans

let decl_span default spans error =
  let found =
    match error with
    | C_decl.Dup name -> named_input_span spans 1 name
    | C_decl.Name name | C_decl.Mode (name, _) ->
      named_input_span spans 0 name
    | C_decl.Nat _ | C_decl.Depth _ | C_decl.Nodes _ | C_decl.Count _
    | C_decl.Idx _ | C_decl.Low _ -> None
  in
  Option.value ~default found

let parse src =
  match C_lex.scan src with
  | Error error -> Error { cause = Lex error; span = error.span }
  | Ok items ->
      let* () = held items 0 in
      let state = {
        items;
        at = 0;
        nodes = 0;
        sizes = C_idx.empty;
        data = [];
        shapes = [];
        perms = C_perm.empty;
        veils = [];
        kinds = [];
        defs = [];
        inst = Imap.empty;
        fmarks = [];
        gen = 0;
        marks = [];
        mcount = 0;
      } in
      let start = span state in
      let* state = take C_lex.F_program state in
      let* name, state = name (fun value -> Name value) state in
      let* state = take C_lex.F_lbrace state in
      let* state =
        match tok state with
        | C_lex.Rbrace -> Error { cause = Empty; span = span state }
        | _ -> Ok state
      in
      let* sizes, inputs, state = heads state in
      let state = { state with sizes } in
      let* inputs, state = decls inputs state in
      let input_spans = List.map (fun (_, name, at) -> name, at) inputs in
      let inputs = List.map (fun (input, _, _) -> input) inputs in
      let* fns, state = forms state in
      let body_first = span state in
      let* (body, flow_marks), state = flow 0 state in
      let* body_marks =
        match expand_marks state.fmarks flow_marks with
        | Ok value -> Ok value
        | Error error -> Error { cause = Fun error; span = body_first }
      in
      let body_last = state.items.(state.at - 1).C_lex.span in
      let body_span = { body_first with last = body_last.last } in
      let fns = fns @ List.map snd (Imap.bindings state.inst) in
      let last = span state in
      let* state = take C_lex.F_rbrace state in
      let* _ = take C_lex.F_eof state in
      begin
        match C_decl.binds_in sizes inputs with
        | Error error ->
          Error { cause = Decl error;
            span = decl_span body_span input_spans error }
        | Ok _ ->
            Ok { name; sizes; inputs; input_spans;
              decls = List.rev state.defs; fns;
              form_marks = state.fmarks; body;
              perms = state.perms;
              dtypes = List.rev_map
                (fun (item : dtype) -> item.code, item.decl) state.data;
              rtypes = List.rev_map
                (fun (item : shape) -> item.code, item.decl) state.shapes;
              veils = List.rev state.veils;
              body_marks;
              body_span;
              span = { start with last = last.last } }
      end

let name (program : t) = program.name
let binds (program : t) = C_decl.binds_in program.sizes program.inputs
let perms (program : t) = program.perms
let dtypes (program : t) = program.dtypes
let rtypes (program : t) = program.rtypes
let decls (program : t) = program.decls
let forms (program : t) = program.fns
let body (program : t) = program.body
let body_marks (program : t) = program.body_marks
let body_span (program : t) = program.body_span
let veils (program : t) = program.veils
let veil_name item = item.vname
let veil_info item = item.vinfo
let veil_param item = item.vparam
let veil_host item = item.vhost
let veil_stage item = item.vstage
let veil_link item = item.vlink

let vpeak (program : t) =
  List.fold_left
    (fun peak item ->
      if C_nat.compare item.vinfo.peak peak > 0 then item.vinfo.peak else peak)
    C_nat.zero program.veils

let veil_count (program : t) = List.length program.veils
let veil_depth program = vpeak program

let res (program : t) (core : C_limit.t) =
  C_limit.max core (C_limit.level (C_nat.to_z (vpeak program)))

let lower_base (program : t) =
  match C_decl.binds_in program.sizes program.inputs with
  | Error error -> Error { cause = Decl error;
      span = decl_span program.body_span program.input_spans error }
  | Ok inputs ->
      begin
        match C_fun.lower inputs program.fns program.body with
        | Ok value -> Ok value
        | Error (C_fun.Low error) -> Error { cause = Low error; span = program.body_span }
        | Error error -> Error { cause = Fun error; span = program.body_span }
      end

let rec term_order term tail =
  match term with
  | C_term.Unit | C_term.Bool _ | C_term.Int _ | C_term.Narrow _
  | C_term.Bytes _
  | C_term.Var _ -> term :: tail
  | C_term.Vec (_, values) | C_term.Seq (_, _, values) ->
    List.fold_left
      (fun out value -> term_order value out)
      (term :: tail) (List.rev values)
  | C_term.Let (_, value, body)
  | C_term.Unpair (value, _, _, body)
  | C_term.Pair (value, body)
  | C_term.Add (value, body)
  | C_term.Sub (value, body)
  | C_term.Mul (value, body)
  | C_term.Div (value, body)
  | C_term.Mod (value, body)
  | C_term.Eq (_, value, body)
  | C_term.Cmp (_, value, body)
  | C_term.Cat (value, body)
  | C_term.Vcat (value, body)
  | C_term.Step (value, body) ->
    term_order value (term_order body (term :: tail))
  | C_term.If (guard, yes, no) ->
    term_order guard
      (term_order yes (term_order no (term :: tail)))
  | C_term.Fst value | C_term.Snd value | C_term.Inl (value, _)
  | C_term.Inr (_, value) | C_term.Act (_, value) | C_term.Neg value
  | C_term.Abs value | C_term.Fit (_, value) | C_term.Wide value
  | C_term.Length value | C_term.Take (_, value) | C_term.Drop (_, value)
  | C_term.At (_, value) | C_term.Uncons value | C_term.Close value ->
    term_order value (term :: tail)
  | C_term.Case (value, _, yes, _, no) ->
    term_order value
      (term_order yes (term_order no (term :: tail)))
  | C_term.Vfold (vector, seed, fold) ->
    term_order vector
      (term_order seed (term_order fold.body (term :: tail)))

let marked_failure_span default marks term failure =
  match failure.C_check.term with
  | None -> default
  | Some failed ->
    let rec find terms marks =
      match terms, marks with
      | term :: _, Some at :: _ when term == failed -> at
      | _ :: terms, _ :: marks -> find terms marks
      | _ -> default
    in
    find (term_order term []) marks

let failure_span program term failure =
  marked_failure_span program.body_span program.body_marks term failure

let form_mark program name =
  match find_marks name program.form_marks with
  | Some marks -> marks
  | None -> { seq = []; root = Some program.body_span }

let find_name id names =
  List.find_map
    (fun (key, name) -> if C_nat.equal id key then Some name else None)
    names

let check_cause names error =
  match error with
  | C_check.Used id | C_check.Unused id ->
    Check (error, find_name id names)
  | _ -> Check (error, None)

let form_names fn =
  let inputs = fn.C_fun.arr.caps @ [fn.arr.arg] in
  Option.value ~default:[] (C_low.names inputs fn.body)

let form_error_span program fn error =
  let marks = form_mark program fn.C_fun.name in
  let root = Option.value ~default:program.body_span marks.root in
  match error with
  | C_fun.Check _ ->
    let values = fn.arr.caps @ [fn.arr.arg] in
    begin
      match C_low.prog values fn.body with
      | Error _ -> root
      | Ok lowered ->
        begin
          match C_check.check_in_located lowered.inputs lowered.term with
          | Ok _ -> root
          | Error failure ->
            marked_failure_span root marks.seq lowered.term failure
        end
    end
  | _ -> root

let rec locate_form_error program prior = function
  | [] -> Error prior
  | fn :: rest ->
    begin
      match C_fun.def fn with
      | Ok () -> locate_form_error program prior rest
      | Error error ->
        let cause =
          match error with
          | C_fun.Check error -> check_cause (form_names fn) error
          | _ -> Fun error
        in
        Error {
          cause;
          span = form_error_span program fn error;
        }
    end

let lower program =
  match lower_base program with
  | Error ({ cause = Fun _; _ } as error) ->
    locate_form_error program error program.fns
  | result -> result

let compile (program : t) =
  let* program_low = lower program in
  let names =
    match binds program with
    | Error _ -> []
    | Ok inputs ->
      begin
        match C_fun.names inputs program.fns program.body with
        | Ok values -> values
        | Error _ -> []
      end
  in
  match C_check.check_in_located program_low.inputs program_low.term with
  | Error failure ->
    Error {
      cause = check_cause names failure.error;
      span = failure_span program program_low.term failure;
    }
  | Ok info ->
    begin
      match C_perm.check_info program.perms info with
      | Ok info -> Ok (program_low, { info with res = res program info.res })
      | Error (C_perm.Check error) ->
        Error { cause = check_cause names error; span = program.body_span }
      | Error error -> Error { cause = Perm error; span = program.body_span }
    end

let check program =
  let* _, info = compile program in
  Ok info

let source src =
  let* program = parse src in
  let* program, _ = compile program in
  Ok program

let expected forms =
  forms
  |> List.map C_lex.form_text
  |> String.concat " or "

let text error =
  match error.cause with
  | Lex error -> C_lex.text error
  | cause ->
      let detail =
        match cause with
        | Lex _ -> ""
        | Held item -> "source form held by profile = " ^ C_lex.hold_text item
        | Need (forms, actual) ->
            "expected = " ^ expected forms ^ " actual = " ^ C_lex.form_text actual
        | Name name -> "invalid name = " ^ name
        | Size_name name -> "invalid size name = " ^ name
        | Size_dup name -> "duplicate size name = " ^ name
        | Idx error -> C_idx.text error
        | Decl error -> C_decl.text error
        | Fun error -> C_fun.text error
        | Inst error -> C_spec.text error
        | Poly_error error -> C_poly.text error
        | Data error -> C_data.text error
        | Rec error -> C_rec.text error
        | Quant error -> C_quant.text error
        | Weave error -> C_weave.text error
        | Braid error -> C_braid.text error
        | Loom error -> C_loom.text error
        | Orbit error -> C_orbit.text error
        | Wake error -> C_wake.text error
        | Rift error -> C_rift.text error
        | Private -> "private name space exhausted"
        | Fhe error -> C_fhe.text error
        | Hfhe error -> C_hfhe.text error
        | Hpar error -> C_hpar.text error
        | Word (expected, actual) ->
            "expected word = " ^ expected ^ " actual = " ^ actual
        | Veil_dup name -> "duplicate veil = " ^ name
        | Data_name name -> "unknown data type = " ^ name
        | Data_dup name -> "duplicate data type = " ^ name
        | Shape_name name -> "unknown shape type = " ^ name
        | Shape_dup name -> "duplicate shape type = " ^ name
        | Tag value -> "type tag outside " ^ string_of_int C_data.tag_len
            ^ "-bit range = " ^ Z.to_string value
        | Tag_dup value -> "duplicate type tag = " ^ Z.to_string value
        | Mark_nat value -> "function mark outside profile = " ^ Z.to_string value
        | Mark_dup value -> "duplicate function mark = " ^ value
        | Under_nat value ->
            "function under outside profile = " ^ Z.to_string value
        | Empty -> "program body is empty"
        | Perm error -> C_perm.text error
        | Low error -> C_low.text error
        | Check (error, name) ->
            C_check.text_named (fun _ -> name) error
        | Depth (limit, actual) ->
            Printf.sprintf "syntax depth limit = %d actual = %d" limit actual
        | Nodes (limit, actual) ->
            Printf.sprintf "syntax nodes limit = %d actual = %d" limit actual
      in
      C_text.clip
        (Printf.sprintf "line = %d col = %d %s"
          error.span.first.line error.span.first.col detail)