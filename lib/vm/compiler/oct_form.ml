(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Oct_lang

module Names = Set.Make (String)

type core = {
  term : C_syn.t;
  typ : C_syn.typ;
  nodes : int;
  depth : int;
  calls : int;
}

type checked = {
  fn : C_fun.fn;
  nodes : int;
  arity : int;
  calls : int;
  direct : bool;
}

type entry =
  | Form of form_def
  | Pure of func_def

type body =
  | Expr of expr
  | Block of func_def

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let located line column reason =
  Printf.sprintf "line %d column %d: %s" line column reason

let fn_error value reason =
  located (Oct_types.block_line value.fn_body) 1 reason

let find_name names id =
  List.find_map
    (fun (key, name) -> if C_nat.equal id key then Some name else None)
    names

let check_text inputs term error =
  let names = Option.value ~default:[] (C_low.names inputs term) in
  C_check.text_named (find_name names) error

let function_text fn = function
  | C_fun.Check error ->
    let inputs = fn.C_fun.arr.caps @ [fn.arr.arg] in
    check_text inputs fn.body error
  | error -> C_fun.text error

let syn_name line column value =
  match C_syn.name value with
  | Some found -> Ok found
  | None -> Error (located line column ("name is invalid = " ^ value))

let rec typ line column = function
  | TVoid -> Ok C_syn.TUnit
  | TInt -> Ok C_syn.TInt
  | TBool -> Ok C_syn.TBool
  | TBytes32 -> Ok (C_syn.TBytes (Z.of_int 32))
  | TU64 -> Ok (C_syn.TNum (C_type.Unsigned, Z.of_int 64))
  | TU128 -> Ok (C_syn.TNum (C_type.Unsigned, Z.of_int 128))
  | TU256 -> Ok (C_syn.TNum (C_type.Unsigned, Z.of_int 256))
  | TTuple [left; right] ->
    let* left = typ line column left in
    let* right = typ line column right in
    Ok (C_syn.TPair (left, right))
  | value ->
    Error
      (located line column
        ("direct form type is unsupported = " ^ typ_to_string value))

let mul = function
  | Once -> C_type.One
  | Many -> C_type.Many

let syn_atom = function
  | C_eff.Read kind -> C_syn.ARead (C_nat.to_z kind)
  | C_eff.Write kind -> C_syn.AWrite (C_nat.to_z kind)
  | C_eff.Emit kind -> C_syn.AEmit (C_nat.to_z kind)
  | C_eff.Fail kind -> C_syn.AFail (C_nat.to_z kind)
  | C_eff.Close kind -> C_syn.AClose (C_nat.to_z kind)

let same expected actual reason =
  if expected = actual then Ok () else Error reason

let within_limit (value : core) =
  if value.nodes <= C_check.max_nodes then Ok value
  else
    Error
      ("direct form node limit = " ^ string_of_int C_check.max_nodes
        ^ " actual = " ^ string_of_int value.nodes)

let node term typ nodes depth calls =
  within_limit { term; typ; nodes; depth; calls }

let finite term typ calls =
  match C_fin.stat term with
  | Ok value ->
    within_limit { term; typ; nodes = value.nodes; depth = value.depth; calls }
  | Error (C_fin.Nodes actual) ->
    Error
      ("direct form node limit = " ^ string_of_int C_check.max_nodes
        ^ " actual = " ^ Z.to_string actual)
  | Error (C_fin.Depth actual) ->
    Error
      ("direct form depth limit = " ^ string_of_int C_check.max_depth
        ^ " actual = " ^ string_of_int actual)

let max_depth values =
  List.fold_left
    (fun depth (value : core) -> Int.max depth value.depth)
    0 values

let max_calls values =
  List.fold_left
    (fun calls (value : core) -> Int.max calls value.calls)
    0 values

let find_local value env =
  List.find_opt (fun (name, _, _) -> String.equal name value) env

let declared program name =
  List.exists (fun value -> String.equal value.fm_name name) program.forms
  || List.exists (fun value -> String.equal value.fn_name name) program.funcs

let find_entry program name =
  match List.find_opt (fun value -> String.equal value.fm_name name) program.forms with
  | Some value -> Ok (Form value)
  | None ->
    begin
      match List.find_opt (fun value -> String.equal value.fn_name name) program.funcs with
      | Some value
          when value.fn_pure && not value.fn_payable
            && not value.fn_nonreentrant
            && List.for_all (fun param -> Option.is_none param.p_refine)
                value.fn_params -> Ok (Pure value)
      | Some value ->
        Error
          (fn_error value
            ("function cannot enter a direct form = " ^ value.fn_name))
      | None -> Error ("direct form function is absent = " ^ name)
    end

let form_params value = value.fm_params

let pure_params value =
  List.map
    (fun param ->
      {
        fp_name = param.p_name;
        fp_typ = param.p_typ;
        fp_mult = Many;
      })
    value.fn_params

let split_params = function
  | [] -> None
  | values ->
    let rec walk out = function
      | [last] -> Some (List.rev out, last)
      | item :: rest -> walk (item :: out) rest
      | [] -> None
    in
    walk [] values

let bind line column value =
  let* name = syn_name line column value.fp_name in
  let* typ = typ line column value.fp_typ in
  Ok (C_syn.bind name (mul value.fp_mult) typ)

let rec binds line column out = function
  | [] -> Ok (List.rev out)
  | value :: rest ->
    let* value = bind line column value in
    binds line column (value :: out) rest

let rec build program trail cache name =
  if List.exists (String.equal name) trail then
    Error ("direct form recursion is forbidden = " ^ name)
  else
    match List.assoc_opt name cache with
    | Some value ->
      let actual = List.length trail + value.calls in
      if actual <= Contract_vm.call_depth_max then Ok (value, cache)
      else
        Error
          ("direct form call depth max = "
            ^ string_of_int Contract_vm.call_depth_max
            ^ " actual = " ^ string_of_int actual)
    | None when List.length trail >= Contract_vm.call_depth_max ->
      Error
        ("direct form call depth max = "
          ^ string_of_int Contract_vm.call_depth_max
          ^ " actual = " ^ string_of_int (List.length trail + 1))
    | None ->
    let* source = find_entry program name in
    let* line, column, params, ret, mode, marks, lim, body =
      match source with
      | Form value ->
        Ok
          (value.fm_line, value.fm_column, form_params value, value.fm_ret,
            value.fm_mult, value.fm_marks, value.fm_lim, Expr value.fm_body)
      | Pure value ->
        Ok
          (Oct_types.block_line value.fn_body, 1, pure_params value,
            value.fn_ret, Many, [], None, Block value)
    in
    let* caps, arg, values =
      match split_params params with
      | Some (caps, arg) ->
        let* caps = binds line column [] caps in
        let* arg = bind line column arg in
        Ok (caps, arg, caps @ [arg])
      | None ->
        let arg = C_syn.bind (C_syn.slot C_nat.zero) C_type.Many C_syn.TUnit in
        Ok ([], arg, [])
    in
    let* out = typ line column ret in
    let env =
      List.map
        (fun item -> C_syn.name_text item.C_syn.name, item.name, item.typ)
        values
    in
    let* body, cache =
      match body with
      | Expr value -> expr program (name :: trail) cache env value
      | Block value -> flow program (name :: trail) cache env value
    in
    let* () =
      same out body.typ
        (located line column ("direct form result type differs = " ^ name))
    in
    let arr = C_fun.arr (mul mode) caps arg out in
    let arr = C_fun.marks arr (C_eff.of_list (List.map (fun item -> item.mk_atom) marks)) in
    let arr = Option.fold ~none:arr ~some:(C_fun.under arr) lim in
    let* fn_name = syn_name line column name in
    let fn = C_fun.fn fn_name arr body.term in
    let* () =
      match C_fun.def fn with
      | Ok () -> Ok ()
      | Error error -> Error (located line column (function_text fn error))
    in
    let item = {
      fn;
      nodes = body.nodes;
      arity = List.length params;
      calls = body.calls + 1;
      direct = C_fun.direct fn;
    } in
    Ok (item, (name, item) :: cache)

and flow program trail cache env owner =
  let rec unwrap line column = function
    | SLocated (next_line, next_column, value) ->
      unwrap next_line next_column value
    | value -> line, column, value
  in
  let rec walk cache env = function
    | [item] ->
      let line, column, item = unwrap (Oct_types.block_line owner.fn_body) 1 item in
      begin
        match item with
        | SReturn (Some value) -> expr program trail cache env value
        | SIf (guard, yes, Some no) ->
          let* guard, cache = expr program trail cache env guard in
          let* yes, cache = walk cache env yes in
          let* no, cache = walk cache env no in
          let* () =
            same C_syn.TBool guard.typ
              (located line column "direct pure function guard requires bool")
          in
          let* () =
            same yes.typ no.typ
              (located line column "direct pure function branch type differs")
          in
          let* value =
            node (C_syn.If (guard.term, yes.term, no.term)) yes.typ
              (guard.nodes + yes.nodes + no.nodes + 1)
              (max_depth [guard; yes; no])
              (max_calls [guard; yes; no])
          in
          Ok (value, cache)
        | _ ->
          Error
            (located line column
              ("direct pure function body is unsupported = " ^ owner.fn_name))
      end
    | item :: rest ->
      let line, column, item = unwrap (Oct_types.block_line owner.fn_body) 1 item in
      begin
        match item with
        | SLet (name, declared, input) ->
          let* input, cache = expr program trail cache env input in
          let* local =
            match declared with
            | None -> Ok input.typ
            | Some declared ->
              let* declared = typ line column declared in
              let* () =
                same declared input.typ
                  (located line column
                    "direct pure function local type differs")
              in
              Ok declared
          in
          let* key = syn_name line column name in
          let bind = C_syn.bind key C_type.Many local in
          let next = (name, key, local) :: env in
          let* body, cache = walk cache next rest in
          let* value =
            node (C_syn.Let (bind, input.term, body.term)) body.typ
              (input.nodes + body.nodes + 1)
              (Int.max input.depth body.depth)
              (Int.max input.calls body.calls)
          in
          Ok (value, cache)
        | SIf (guard, yes, None) ->
          let* guard, cache = expr program trail cache env guard in
          let* yes, cache = walk cache env yes in
          let* no, cache = walk cache env rest in
          let* () =
            same C_syn.TBool guard.typ
              (located line column "direct pure function guard requires bool")
          in
          let* () =
            same yes.typ no.typ
              (located line column "direct pure function branch type differs")
          in
          let* value =
            node (C_syn.If (guard.term, yes.term, no.term)) yes.typ
              (guard.nodes + yes.nodes + no.nodes + 1)
              (max_depth [guard; yes; no])
              (max_calls [guard; yes; no])
          in
          Ok (value, cache)
        | _ ->
          Error
            (located line column
              ("direct pure function body is unsupported = " ^ owner.fn_name))
      end
    | [] ->
      Error
        (fn_error owner
          ("direct pure function body is empty = " ^ owner.fn_name))
  in
  walk cache env owner.fn_body

and expr program trail cache env value =
  let unary make expected result input =
    let* input, cache = expr program trail cache env input in
    let* () = same expected input.typ result in
    let* value =
      node (make input.term) expected (input.nodes + 1) input.depth input.calls
    in
    Ok (value, cache)
  in
  let binary make expected result left right =
    let* left, cache = expr program trail cache env left in
    let* right, cache = expr program trail cache env right in
    let* () = same expected left.typ result in
    let* () = same expected right.typ result in
    let* value =
      node (make left.term right.term) expected
        (left.nodes + right.nodes + 1) (Int.max left.depth right.depth)
        (Int.max left.calls right.calls)
    in
    Ok (value, cache)
  in
  let compare rel left right =
    let* left, cache = expr program trail cache env left in
    let* right, cache = expr program trail cache env right in
    let reason = "direct form comparison requires int" in
    let* () = same C_syn.TInt left.typ reason in
    let* () = same C_syn.TInt right.typ reason in
    let* value =
      node (C_syn.Cmp (rel, left.term, right.term)) C_syn.TBool
        (left.nodes + right.nodes + 1) (Int.max left.depth right.depth)
        (Int.max left.calls right.calls)
    in
    Ok (value, cache)
  in
  let rec args out cache = function
    | [] -> Ok (List.rev out, cache)
    | value :: rest ->
      let* value, cache = expr program trail cache env value in
      args (value :: out) cache rest
  in
  match value with
  | EInt value ->
    Ok ({ term = C_syn.KInt value; typ = C_syn.TInt;
      nodes = 1; depth = 0; calls = 0 }, cache)
  | EBool value ->
    Ok ({ term = C_syn.KBool value; typ = C_syn.TBool;
      nodes = 1; depth = 0; calls = 0 }, cache)
  | EString _ -> Error "direct form text requires finite bytes"
  | ECaller -> Error "direct form context requires function wrapper = caller"
  | EOrigin -> Error "direct form context requires function wrapper = origin"
  | ESelfAddr ->
    Error "direct form context requires function wrapper = self_addr"
  | EEpoch -> Error "direct form context requires function wrapper = epoch"
  | EEpochTime ->
    Error "direct form context requires function wrapper = epoch_time"
  | EValue -> Error "direct form context requires function wrapper = value"
  | EBalance _ ->
    Error "direct form context requires function wrapper = balance"
  | ETreeHash ->
    Error "direct form context requires function wrapper = tree_hash"
  | ENodeId -> Error "direct form context requires function wrapper = node_id"
  | ETxHash -> Error "direct form context requires function wrapper = tx_hash"
  | EField _
  | EIndex _
  | EStoragePath _
  | EFieldProp _
  | EIndexField _ -> Error "direct form state requires a proved effect"
  | EArray _ -> Error "direct form list requires a finite sequence"
  | ETuple [left; right] ->
    let* left, cache = expr program trail cache env left in
    let* right, cache = expr program trail cache env right in
    let typ = C_syn.TPair (left.typ, right.typ) in
    let* value =
      node (C_syn.Pair (left.term, right.term)) typ
        (left.nodes + right.nodes + 1) (Int.max left.depth right.depth)
        (Int.max left.calls right.calls)
    in
    Ok (value, cache)
  | EEqual (declared, left, right) ->
    let* declared = typ 0 1 declared in
    let* left, cache = expr program trail cache env left in
    let* right, cache = expr program trail cache env right in
    let* () = same declared left.typ "direct form equality left type differs" in
    let* () = same declared right.typ "direct form equality right type differs" in
    let* value =
      node (C_syn.Eq (declared, left.term, right.term)) C_syn.TBool
        (left.nodes + right.nodes + 1) (Int.max left.depth right.depth)
        (Int.max left.calls right.calls)
    in
    Ok (value, cache)
  | ELet (name, mode, declared, input, body) ->
    let* input, cache = expr program trail cache env input in
    let* declared = typ 0 1 declared in
    let* () =
      same declared input.typ "direct form local type differs"
    in
    let* key = syn_name 0 1 name in
    let bind = C_syn.bind key (mul mode) declared in
    let* body, cache =
      expr program trail cache ((name, key, declared) :: env) body
    in
    let term = C_syn.Let (bind, input.term, body.term) in
    let* value = finite term body.typ (Int.max input.calls body.calls) in
    Ok (value, cache)
  | ESplit (pair, (left_name, left_mode, left_type),
      (right_name, right_mode, right_type), body) ->
    let* pair, cache = expr program trail cache env pair in
    let* left_type = typ 0 1 left_type in
    let* right_type = typ 0 1 right_type in
    let* () =
      same (C_syn.TPair (left_type, right_type)) pair.typ
        "direct form split type differs"
    in
    let* left_key = syn_name 0 1 left_name in
    let* right_key = syn_name 0 1 right_name in
    let left = C_syn.bind left_key (mul left_mode) left_type in
    let right = C_syn.bind right_key (mul right_mode) right_type in
    let next =
      (right_name, right_key, right_type)
      :: (left_name, left_key, left_type)
      :: env
    in
    let* body, cache = expr program trail cache next body in
    let term = C_syn.Unpair (pair.term, left, right, body.term) in
    let* value = finite term body.typ (Int.max pair.calls body.calls) in
    Ok (value, cache)
  | EOrbit (count, turns, seed, (name, mode, declared), body) ->
    let* seed, cache = expr program trail cache env seed in
    let* declared = typ 0 1 declared in
    let* () = same declared seed.typ "direct form orbit seed type differs" in
    let* key = syn_name 0 1 name in
    let bind = C_syn.bind key (mul mode) declared in
    let* body, cache =
      expr program trail cache ((name, key, declared) :: env) body
    in
    let* term, count_value, cache =
      match turns with
      | None ->
        begin
          match C_orbit.make count seed.term bind body.term with
          | Ok term -> Ok (term, None, cache)
          | Error error -> Error (C_orbit.text error)
        end
      | Some turns ->
        let* turns, cache = expr program trail cache env turns in
        let* () = same C_syn.TInt turns.typ "direct form orbit count requires int" in
        begin
          match C_orbit.upto count turns.term seed.term bind body.term with
          | Ok term -> Ok (term, Some turns, cache)
          | Error error -> Error (C_orbit.text error)
        end
    in
    let calls =
      Option.fold ~none:(Int.max seed.calls body.calls)
        ~some:(fun turns -> max_calls [turns; seed; body]) count_value
    in
    let* value = finite term declared calls in
    Ok (value, cache)
  | EVar value ->
    begin
      match find_local value env with
      | Some (_, name, typ) ->
        Ok ({ term = C_syn.Var name; typ;
          nodes = 1; depth = 0; calls = 0 }, cache)
      | None -> Error ("direct form variable is absent = " ^ value)
    end
  | EBinop (Add, left, right) ->
    binary (fun left right -> C_syn.Add (left, right)) C_syn.TInt
      "direct form addition requires int" left right
  | EBinop (Sub, left, right) ->
    binary (fun left right -> C_syn.Sub (left, right)) C_syn.TInt
      "direct form subtraction requires int" left right
  | EBinop (Mul, left, right) ->
    binary (fun left right -> C_syn.Mul (left, right)) C_syn.TInt
      "direct form multiplication requires int" left right
  | EBinop (Div, left, right) ->
    binary (fun left right -> C_syn.Div (left, right)) C_syn.TInt
      "direct form division requires int" left right
  | EBinop (Mod, left, right) ->
    binary (fun left right -> C_syn.Mod (left, right)) C_syn.TInt
      "direct form remainder requires int" left right
  | EBinop (Lt, left, right) -> compare C_syn.Lt left right
  | EBinop (Le, left, right) -> compare C_syn.Le left right
  | EBinop (Gt, left, right) -> compare C_syn.Gt left right
  | EBinop (Ge, left, right) -> compare C_syn.Ge left right
  | EBinop ((Eq | Neq as op), left, right) ->
    let* left, cache = expr program trail cache env left in
    let* right, cache = expr program trail cache env right in
    let* () = same left.typ right.typ "direct form equality type differs" in
    let equal = C_syn.Eq (left.typ, left.term, right.term) in
    let term =
      if op = Eq then equal
      else C_syn.Eq (C_syn.TBool, equal, C_syn.KBool false)
    in
    let extra = if op = Eq then 1 else 3 in
    let* value =
      node term C_syn.TBool (left.nodes + right.nodes + extra)
        (Int.max left.depth right.depth)
        (Int.max left.calls right.calls)
    in
    Ok (value, cache)
  | EBinop (And, left, right) ->
    let* left, cache = expr program trail cache env left in
    let* right, cache = expr program trail cache env right in
    let reason = "direct form conjunction requires bool" in
    let* () = same C_syn.TBool left.typ reason in
    let* () = same C_syn.TBool right.typ reason in
    let term = C_syn.If (left.term, right.term, C_syn.KBool false) in
    let* value =
      node term C_syn.TBool (left.nodes + right.nodes + 2)
        (Int.max left.depth right.depth)
        (Int.max left.calls right.calls)
    in
    Ok (value, cache)
  | EBinop (Or, left, right) ->
    let* left, cache = expr program trail cache env left in
    let* right, cache = expr program trail cache env right in
    let reason = "direct form disjunction requires bool" in
    let* () = same C_syn.TBool left.typ reason in
    let* () = same C_syn.TBool right.typ reason in
    let term = C_syn.If (left.term, C_syn.KBool true, right.term) in
    let* value =
      node term C_syn.TBool (left.nodes + right.nodes + 2)
        (Int.max left.depth right.depth)
        (Int.max left.calls right.calls)
    in
    Ok (value, cache)
  | EUnop (Neg, input) ->
    unary (fun value -> C_syn.Neg value) C_syn.TInt
      "direct form negation requires int" input
  | EUnop (Not, input) ->
    unary
      (fun value -> C_syn.Eq (C_syn.TBool, value, C_syn.KBool false))
      C_syn.TBool "direct form negation requires bool" input
  | ETernary (guard, yes, no) ->
    let* guard, cache = expr program trail cache env guard in
    let* yes, cache = expr program trail cache env yes in
    let* no, cache = expr program trail cache env no in
    let* () = same C_syn.TBool guard.typ "direct form guard requires bool" in
    let* () = same yes.typ no.typ "direct form branch type differs" in
    let* value =
      node (C_syn.If (guard.term, yes.term, no.term)) yes.typ
        (guard.nodes + yes.nodes + no.nodes + 1)
        (max_depth [guard; yes; no])
        (max_calls [guard; yes; no])
    in
    Ok (value, cache)
  | EAction (atom, input) ->
    let* input, cache = expr program trail cache env input in
    let* value =
      node (C_syn.Act (syn_atom atom, input.term)) input.typ
        (input.nodes + 1) input.depth input.calls
    in
    Ok (value, cache)
  | ECall ("abs", [input]) when not (declared program "abs") ->
    unary (fun value -> C_syn.Abs value) C_syn.TInt
      "direct form absolute value requires int" input
  | ECall ("abs", values) when not (declared program "abs") ->
    Error
      ("function arity name = abs expected = 1 actual = "
        ^ string_of_int (List.length values))
  | ECall (name, values) ->
    let* values, cache = args [] cache values in
    let* target, cache = build program trail cache name in
    let* () =
      if target.direct then Ok ()
      else Error ("form requires explicit use = " ^ name)
    in
    let actual = List.length values in
    let* () =
      if actual = target.arity then Ok ()
      else
        Error
          ("function arity name = " ^ name ^ " expected = "
            ^ string_of_int target.arity ^ " actual = " ^ string_of_int actual)
    in
    let values =
      if target.arity = 0 then
        [{ term = C_syn.KUnit; typ = C_syn.TUnit;
          nodes = 1; depth = 0; calls = 0 }]
      else values
    in
    let nodes =
      List.fold_left
        (fun count (value : core) -> count + value.nodes)
        target.nodes values
      + List.length values
    in
    let* () =
      if nodes <= C_check.max_nodes then Ok ()
      else
        Error
          ("direct form node limit = " ^ string_of_int C_check.max_nodes
            ^ " actual = " ^ string_of_int nodes)
    in
    let* term =
      match C_fun.apply target.fn (List.map (fun value -> value.term) values) with
      | Ok value -> Ok value
      | Error error -> Error (C_fun.text error)
    in
    Ok ({
      term;
      typ = target.fn.C_fun.arr.out;
      nodes;
      depth = max_depth values;
      calls = Int.max target.calls (max_calls values);
    }, cache)
  | EUse value ->
    let* () =
      if List.exists
          (fun form -> String.equal form.fm_name value.ux_name)
          program.forms
      then Ok ()
      else Error ("form is absent = " ^ value.ux_name)
    in
    let* caps, cache = args [] cache value.ux_caps in
    let* arg, cache = expr program trail cache env value.ux_arg in
    let values = caps @ [arg] in
    let* target, cache = build program trail cache value.ux_name in
    let actual = List.length values in
    let* () =
      if actual = target.arity then Ok ()
      else
        Error
          ("function arity name = " ^ value.ux_name ^ " expected = "
            ^ string_of_int target.arity ^ " actual = " ^ string_of_int actual)
    in
    let* out = typ 0 1 value.ux_typ in
    let* () =
      same target.fn.C_fun.arr.out out
        ("use result type differs = " ^ value.ux_name)
    in
    let* name = syn_name 0 1 value.ux_bind in
    let bind = C_syn.bind name (mul value.ux_mult) out in
    let next = (value.ux_bind, name, out) :: env in
    let* body, cache = expr program trail cache next value.ux_body in
    let nodes =
      List.fold_left
        (fun count (item : core) -> count + item.nodes)
        (target.nodes + body.nodes + List.length values + 1)
        values
    in
    let* () =
      if nodes <= C_check.max_nodes then Ok ()
      else
        Error
          ("direct form node limit = " ^ string_of_int C_check.max_nodes
            ^ " actual = " ^ string_of_int nodes)
    in
    let* used =
      match C_fun.apply target.fn (List.map (fun item -> item.term) values) with
      | Ok term -> Ok term
      | Error error -> Error (C_fun.text error)
    in
    let* result =
      node (C_syn.Let (bind, used, body.term)) body.typ nodes
        (Int.max body.depth (max_depth values))
        (Int.max target.calls (Int.max body.calls (max_calls values)))
    in
    Ok (result, cache)
  | _ -> Error "direct form expression is unsupported"

let put name typ values =
  (name, typ) :: List.filter (fun (found, _) -> not (String.equal name found)) values

let rec core_inputs line column binds env = function
  | [] -> Ok (List.rev binds, env)
  | (name, host) :: rest ->
    begin
      match typ line column host with
      | Error _ -> core_inputs line column binds env rest
      | Ok core ->
        let* key = syn_name line column name in
        let bind = C_syn.bind key C_type.Many core in
        core_inputs line column (bind :: binds) ((name, key, core) :: env) rest
    end

let strict program line column values term =
  let* binds, env = core_inputs line column [] [] values in
  let* value, _ = expr program [] [] env term in
  Ok (binds, value)

let rec host_typ = function
  | C_syn.TInt -> Some TInt
  | C_syn.TBool -> Some TBool
  | C_syn.TBytes size when Z.equal size (Z.of_int 32) -> Some TBytes32
  | C_syn.TNum (C_type.Unsigned, bits) when Z.equal bits (Z.of_int 64) ->
    Some TU64
  | C_syn.TNum (C_type.Unsigned, bits) when Z.equal bits (Z.of_int 128) ->
    Some TU128
  | C_syn.TNum (C_type.Unsigned, bits) when Z.equal bits (Z.of_int 256) ->
    Some TU256
  | C_syn.TPair (left, right) ->
    Option.bind (host_typ left) (fun left ->
      Option.map (fun right -> TTuple [left; right]) (host_typ right))
  | _ -> None

let rec host_infer program values = function
  | EInt _ -> Some TInt
  | EBool _ -> Some TBool
  | EString _ -> Some TString
  | ECaller | EOrigin | ESelfAddr -> Some TAddress
  | EEpoch | EEpochTime | EValue | EBalance _ -> Some TInt
  | ETreeHash | ENodeId | ETxHash -> Some TString
  | EVar name ->
    begin
      match List.assoc_opt name values with
      | Some value -> Some value
      | None ->
        Option.map
          (fun value -> value.c_typ)
          (List.find_opt
            (fun value -> String.equal value.c_name name)
            program.consts)
    end
  | EField name ->
    Option.map
      (fun value -> value.sf_typ)
      (List.find_opt
        (fun value -> String.equal value.sf_name name)
        program.state)
  | EIndex (name, _) ->
    begin
      match
        List.find_opt
          (fun value -> String.equal value.sf_name name)
          program.state
      with
      | Some { sf_typ = TMap (_, value); _ }
      | Some { sf_typ = TList value; _ } -> Some value
      | Some _ | None -> None
    end
  | EBinop ((Eq | Neq | Lt | Gt | Le | Ge | And | Or), _, _) -> Some TBool
  | EBinop (Add, left, right) ->
    begin
      match host_infer program values left, host_infer program values right with
      | Some left, Some right when Oct_types.text left || Oct_types.text right ->
        Some TString
      | Some left, Some right
          when Oct_types.numeric left && Oct_types.numeric right ->
        Some (Oct_types.numeric_result left right)
      | Some _, Some _ | Some _, None | None, Some _ | None, None -> None
    end
  | EBinop ((Sub | Mul | Div | Mod), left, right) ->
    begin
      match host_infer program values left, host_infer program values right with
      | Some left, Some right
          when Oct_types.numeric left && Oct_types.numeric right ->
        Some (Oct_types.numeric_result left right)
      | Some _, Some _ | Some _, None | None, Some _ | None, None -> None
    end
  | EUnop (_, value) -> host_infer program values value
  | ECall (name, _) ->
    begin
      match List.find_opt (fun value -> String.equal value.fn_name name) program.funcs with
      | Some value -> Some value.fn_ret
      | None ->
        Option.map
          (fun value -> value.fm_ret)
          (List.find_opt
            (fun value -> String.equal value.fm_name name)
            program.forms)
    end
  | EArray [] -> Some (TList TVoid)
  | EArray (value :: _) ->
    Option.map (fun typ -> TList typ) (host_infer program values value)
  | ETuple items ->
    let rec tuple out = function
      | [] -> Some (TTuple (List.rev out))
      | value :: rest ->
        Option.bind
          (host_infer program values value)
          (fun typ -> tuple (typ :: out) rest)
    in
    tuple [] items
  | EEqual _ -> Some TBool
  | ELet (name, _, declared, input, body) ->
    begin
      match host_infer program values input with
      | Some actual when Oct_types.compatible declared actual ->
        host_infer program ((name, declared) :: values) body
      | Some _ | None -> None
    end
  | ESplit (_, (left, _, left_typ), (right, _, right_typ), body) ->
    host_infer program
      ((right, right_typ) :: (left, left_typ) :: values)
      body
  | EOrbit (_, turns, seed, (name, _, declared), body) ->
    begin
      let turns_ok =
        Option.fold ~none:true
          ~some:(fun value -> host_infer program values value = Some TInt)
          turns
      in
      match host_infer program values seed with
      | Some actual when turns_ok && Oct_types.compatible declared actual ->
        begin
          match host_infer program ((name, declared) :: values) body with
          | Some actual when Oct_types.compatible declared actual -> Some declared
          | Some _ | None -> None
        end
      | Some _ | None -> None
    end
  | ETernary (_, yes, no) ->
    begin
      match host_infer program values yes, host_infer program values no with
      | Some yes, Some no when Oct_types.compatible yes no -> Some yes
      | Some _, Some _ | Some _, None | None, Some _ | None, None -> None
    end
  | EAction (_, value) -> host_infer program values value
  | EUse value ->
    host_infer program
      ((value.ux_bind, value.ux_typ) :: values)
      value.ux_body
  | EStoragePath _
  | EFieldProp _
  | EIndexField _
  | EEnumVariant _ -> None

let infer program line column values term =
  match strict program line column values term with
  | Ok (_, value) -> host_typ value.typ
  | Error _ -> host_infer program values term

let check_term program line column values term =
  let* binds, value = strict program line column values term in
  let* program =
    match C_low.prog binds value.term with
    | Ok value -> Ok value
    | Error error -> Error (located line column (C_low.text error))
  in
  match C_check.check_in program.inputs program.term with
  | Ok _ -> Ok ()
  | Error error ->
    Error (located line column (check_text binds value.term error))

let rec check_expr program line column values = function
  | (EUse _ | EEqual _ | ELet _ | ESplit _ | EOrbit _) as term ->
    check_term program line column values term
  | EAction _ -> Error "effect atoms are available only in form bodies"
  | EIndex (_, keys)
  | ECall (_, keys)
  | EArray keys
  | ETuple keys -> check_exprs program line column values keys
  | EBinop (_, left, right) ->
    let* () = check_expr program line column values left in
    check_expr program line column values right
  | EUnop (_, value)
  | EBalance value -> check_expr program line column values value
  | EStoragePath (_, keys, _)
  | EIndexField (_, keys, _) -> check_exprs program line column values keys
  | ETernary (guard, yes, no) ->
    let* () = check_expr program line column values guard in
    let* () = check_expr program line column values yes in
    check_expr program line column values no
  | EInt _
  | EBool _
  | EString _
  | EVar _
  | EField _
  | ECaller
  | EOrigin
  | ESelfAddr
  | EEpoch
  | EEpochTime
  | EValue
  | ETreeHash
  | ENodeId
  | ETxHash
  | EFieldProp _
  | EEnumVariant _ -> Ok ()

and check_exprs program line column values = function
  | [] -> Ok ()
  | value :: rest ->
    let* () = check_expr program line column values value in
    check_exprs program line column values rest

let rec check_stmts program line column values = function
  | [] -> Ok values
  | statement :: rest ->
    let* values = check_stmt program line column values statement in
    check_stmts program line column values rest

and check_stmt program line column values = function
  | SLocated (line, column, statement) ->
    check_stmt program line column values statement
  | SLet (name, declared, value) ->
    let* () = check_expr program line column values value in
    let local =
      match declared with
      | Some value -> Some value
      | None -> infer program line column values value
    in
    Ok (Option.fold ~none:values ~some:(fun typ -> put name typ values) local)
  | SLetTuple (_, value) ->
    let* () = check_expr program line column values value in
    Ok values
  | SAssign (_, value)
  | SFieldSet (_, value)
  | SAssert value
  | SExpr value ->
    let* () = check_expr program line column values value in
    Ok values
  | SIndexSet (_, keys, value)
  | SStoragePathSet (_, keys, _, value)
  | SIndexFieldSet (_, keys, _, value) ->
    let* () = check_exprs program line column values keys in
    let* () = check_expr program line column values value in
    Ok values
  | SIndexUpdate (_, keys, _, value)
  | SStoragePathUpdate (_, keys, _, _, value) ->
    let* () = check_exprs program line column values keys in
    let* () = check_expr program line column values value in
    Ok values
  | SReturn value ->
    let* () =
      match value with
      | Some value -> check_expr program line column values value
      | None -> Ok ()
    in
    Ok values
  | SRequire (guard, message) ->
    let* () = check_expr program line column values guard in
    let* () = check_expr program line column values message in
    Ok values
  | SEmit (_, args)
  | SFieldCall (_, _, args)
  | SRevertError (_, args) ->
    let* () = check_exprs program line column values args in
    Ok values
  | SIf (guard, yes, no) ->
    let* () = check_expr program line column values guard in
    let* _ = check_stmts program line column values yes in
    let* _ =
      match no with
      | Some body -> check_stmts program line column values body
      | None -> Ok values
    in
    Ok values
  | SWhile (guard, body) ->
    let* () = check_expr program line column values guard in
    let* _ = check_stmts program line column values body in
    Ok values
  | SFor (name, first, last, body) ->
    let* () = check_expr program line column values first in
    let* () = check_expr program line column values last in
    let* _ = check_stmts program line column (put name TInt values) body in
    Ok values
  | SForEach (_, _, body) ->
    let* _ = check_stmts program line column values body in
    Ok values
  | SMatch (value, arms) ->
    let* () = check_expr program line column values value in
    let rec check = function
      | [] -> Ok ()
      | (_, _, body) :: rest ->
        let* _ = check_stmts program line column values body in
        check rest
    in
    let* () = check arms in
    Ok values

let check_func program value =
  let values = List.map (fun param -> param.p_name, param.p_typ) value.fn_params in
  let line = Oct_types.block_line value.fn_body in
  let* _ = check_stmts program line 1 values value.fn_body in
  Ok ()

let lower_param value =
  { p_name = value.fp_name; p_typ = value.fp_typ; p_refine = None }

let view_atom public = function
  | C_eff.Write _ | C_eff.Close _ -> false
  | C_eff.Emit _ -> not public
  | C_eff.Read _ | C_eff.Fail _ -> true

let lower value =
  {
    fn_name = value.fm_name;
    fn_params = List.map lower_param (form_params value);
    fn_ret = value.fm_ret;
    fn_view =
      List.for_all (fun item -> view_atom value.fm_public item.mk_atom)
        value.fm_marks;
    fn_pure = value.fm_marks = [];
    fn_payable = false;
    fn_nonreentrant = false;
    fn_vis = if value.fm_public then Public else Internal;
    fn_body = [SLocated (value.fm_line, value.fm_column,
      SReturn (Some value.fm_body))];
  }

let evidence program value checked =
  let rec bindings out = function
    | [] -> Ok (List.rev out)
    | item :: rest ->
      let* target =
        match item.mk_atom with
        | C_eff.Read _ | C_eff.Write _ -> Ok (Aml_effect.State item.mk_target)
        | C_eff.Emit _ -> Ok (Aml_effect.Event item.mk_target)
        | C_eff.Fail _ -> Ok (Aml_effect.Fault item.mk_target)
        | C_eff.Close _ ->
          Error
            (located value.fm_line value.fm_column
              "direct form close target is unsupported")
      in
      bindings (Aml_effect.bind item.mk_atom target :: out) rest
  in
  let* bindings = bindings [] value.fm_marks in
  let* entries =
    match Aml_effect.make program bindings with
    | Ok found -> Ok found
    | Error error ->
      Error (located value.fm_line value.fm_column (Aml_effect.text error))
  in
  let params = checked.fn.C_fun.arr.caps @ [checked.fn.arr.arg] in
  let* core =
    match C_low.prog params checked.fn.body with
    | Ok found -> Ok found
    | Error error ->
      Error (located value.fm_line value.fm_column (C_low.text error))
  in
  let* sealed =
    match Aml_effect.seal entries core.inputs core.term with
    | Ok found -> Ok found
    | Error error ->
      Error (located value.fm_line value.fm_column (Aml_effect.text error))
  in
  match Aml_evidence.make sealed with
  | Ok found -> Ok found
  | Error error ->
    Error (located value.fm_line value.fm_column (Aml_evidence.text error))

let unique program =
  let rec walk seen = function
    | [] -> Ok ()
    | name :: _ when List.exists (String.equal name) seen ->
      Error ("callable name is repeated = " ^ name)
    | name :: rest -> walk (name :: seen) rest
  in
  let funcs = List.map (fun value -> value.fn_name) program.funcs in
  let forms = List.map (fun value -> value.fm_name) program.forms in
  walk [] (funcs @ forms)

let add_call program name calls =
  if List.exists (fun value -> String.equal value.fn_name name) program.funcs then
    Names.add name calls
  else calls

let rec refs_expr program (forms, calls) = function
  | EUse value ->
    let found = Names.add value.ux_name forms, calls in
    let found = refs_exprs program found value.ux_caps in
    let found = refs_expr program found value.ux_arg in
    refs_expr program found value.ux_body
  | ECall (name, values) ->
    refs_exprs program (forms, add_call program name calls) values
  | EAction (_, value)
  | EUnop (_, value)
  | EBalance value -> refs_expr program (forms, calls) value
  | EIndex (_, values)
  | EArray values
  | ETuple values
  | EStoragePath (_, values, _)
  | EIndexField (_, values, _) -> refs_exprs program (forms, calls) values
  | EEqual (_, left, right)
  | EBinop (_, left, right) ->
    refs_expr program (refs_expr program (forms, calls) left) right
  | ETernary (guard, yes, no) ->
    let found = refs_expr program (forms, calls) guard in
    let found = refs_expr program found yes in
    refs_expr program found no
  | ELet (_, _, _, value, body) ->
    refs_expr program (refs_expr program (forms, calls) value) body
  | ESplit (value, _, _, body) ->
    refs_expr program (refs_expr program (forms, calls) value) body
  | EOrbit (_, turns, seed, _, body) ->
    let found =
      Option.fold ~none:(forms, calls)
        ~some:(refs_expr program (forms, calls)) turns
    in
    let found = refs_expr program found seed in
    refs_expr program found body
  | EInt _
  | EBool _
  | EString _
  | EVar _
  | EField _
  | ECaller
  | EOrigin
  | ESelfAddr
  | EEpoch
  | EEpochTime
  | EValue
  | ETreeHash
  | ENodeId
  | ETxHash
  | EFieldProp _
  | EEnumVariant _ -> forms, calls

and refs_exprs program found = function
  | [] -> found
  | value :: rest ->
    refs_exprs program (refs_expr program found value) rest

let rec refs_stmt program found = function
  | SLocated (_, _, value) -> refs_stmt program found value
  | SLet (_, _, value)
  | SAssign (_, value)
  | SFieldSet (_, value)
  | SAssert value
  | SExpr value -> refs_expr program found value
  | SLetTuple (_, value) -> refs_expr program found value
  | SIndexSet (_, keys, value)
  | SStoragePathSet (_, keys, _, value)
  | SIndexFieldSet (_, keys, _, value) ->
    refs_expr program (refs_exprs program found keys) value
  | SIndexUpdate (_, keys, _, value)
  | SStoragePathUpdate (_, keys, _, _, value) ->
    refs_expr program (refs_exprs program found keys) value
  | SReturn None -> found
  | SReturn (Some value) -> refs_expr program found value
  | SRequire (guard, message) ->
    refs_expr program (refs_expr program found guard) message
  | SEmit (_, values)
  | SFieldCall (_, _, values)
  | SRevertError (_, values) -> refs_exprs program found values
  | SIf (guard, yes, no) ->
    let found = refs_expr program found guard in
    let found = refs_stmts program found yes in
    Option.fold ~none:found ~some:(refs_stmts program found) no
  | SWhile (guard, body) ->
    refs_stmts program (refs_expr program found guard) body
  | SFor (_, first, last, body) ->
    let found = refs_expr program found first in
    refs_stmts program (refs_expr program found last) body
  | SForEach (_, _, body) -> refs_stmts program found body
  | SMatch (value, arms) ->
    List.fold_left
      (fun found (_, _, body) -> refs_stmts program found body)
      (refs_expr program found value) arms

and refs_stmts program found = function
  | [] -> found
  | value :: rest ->
    refs_stmts program (refs_stmt program found value) rest

let row_forms name rows =
  match List.find_opt (fun (found, _, _) -> String.equal name found) rows with
  | Some (_, forms, _) -> forms
  | None -> Names.empty

let rec close_rows rows =
  let next =
    List.map
      (fun (name, forms, calls) ->
        let forms =
          Names.fold
            (fun called out -> Names.union out (row_forms called rows))
            calls forms
        in
        name, forms, calls)
      rows
  in
  if
    List.for_all2
      (fun (_, left, _) (_, right, _) -> Names.equal left right)
      rows next
  then next
  else close_rows next

let reach_rows program =
  program.funcs
  |> List.map
    (fun value ->
      let forms, calls =
        refs_stmts program (Names.empty, Names.empty) value.fn_body
      in
      value.fn_name, forms, calls)
  |> close_rows

let expand_rows rows (forms, calls) =
  Names.fold
    (fun name out -> Names.union out (row_forms name rows))
    calls forms

let first_form program forms select =
  List.find_opt
    (fun value ->
      Names.mem value.fm_name forms && List.exists select value.fm_marks)
    program.forms

let any_mark _ = true

let write_mark item =
  match item.mk_atom with
  | C_eff.Write _ | C_eff.Close _ -> true
  | C_eff.Read _ | C_eff.Emit _ | C_eff.Fail _ -> false

let check_mode program rows value =
  let forms = row_forms value.fn_name rows in
  if value.fn_pure then
    match first_form program forms any_mark with
    | Some form ->
      Error
        (fn_error value
          ("pure function form marks are not empty = " ^ form.fm_name))
    | None -> Ok ()
  else if value.fn_view then
    match first_form program forms write_mark with
    | Some form ->
      Error
        (fn_error value
          ("view function form writes storage = " ^ form.fm_name))
    | None -> Ok ()
  else Ok ()

let check_modes program rows =
  let rec funcs = function
    | [] -> Ok ()
    | value :: rest ->
      let* () = check_mode program rows value in
      funcs rest
  in
  let static name expr =
    let forms =
      refs_expr program (Names.empty, Names.empty) expr
      |> expand_rows rows
    in
    match first_form program forms any_mark with
    | Some form -> Error (name ^ " form marks are not empty = " ^ form.fm_name)
    | None -> Ok ()
  in
  let rec consts = function
    | [] -> Ok ()
    | value :: rest ->
      let* () = static "constant" value.c_value in
      consts rest
  in
  let rec invariants = function
    | [] -> Ok ()
    | value :: rest ->
      let* () = static "invariant" value.inv_expr in
      invariants rest
  in
  let* () = funcs program.funcs in
  let* () = consts program.consts in
  invariants program.invariants_decl

let link program =
  if program.forms <> [] && program.declaration <> ProgramDecl then
    Error "form declarations require Program"
  else
    let forms = program.forms in
    let* () = unique program in
    let marked_pair =
      List.find_opt
        (fun value ->
          value.fm_public
          && value.fm_marks <> []
          && match value.fm_ret with TTuple _ -> true | _ -> false)
        forms
    in
    let* () =
      match marked_pair with
      | Some value ->
        Error
          (located value.fm_line value.fm_column
            "public main effect result has no proved ABI = tuple")
      | None -> Ok ()
    in
    let rec check cache = function
      | [] -> Ok cache
      | value :: rest ->
        let* _, cache = build program [] cache value.fm_name in
        check cache rest
    in
    let* cache = check [] forms in
    let* () =
      match program.ctor with
      | Some value -> check_func program value
      | None -> Ok ()
    in
    let rec consts = function
      | [] -> Ok ()
      | value :: rest ->
        let* () = check_expr program 1 1 [] value.c_value in
        consts rest
    in
    let* () = consts program.consts in
    let rec invariants = function
      | [] -> Ok ()
      | value :: rest ->
        let* () = check_expr program 1 1 [] value.inv_expr in
        invariants rest
    in
    let* () = invariants program.invariants_decl in
    let rec funcs = function
      | [] -> Ok ()
      | value :: rest ->
        let* () = check_func program value in
        funcs rest
    in
    let* () = funcs program.funcs in
    let rows = reach_rows program in
    let* () = check_modes program rows in
    let direct =
      cache
      |> List.filter_map
        (fun (name, value) -> if value.direct then Some name else None)
      |> List.sort_uniq String.compare
    in
    let rec calls out = function
      | [] -> Ok (List.rev out)
      | value :: rest when value.fm_marks = [] -> calls out rest
      | value :: rest ->
        begin
          match List.assoc_opt value.fm_name cache with
          | None -> Error ("direct form is absent = " ^ value.fm_name)
          | Some checked ->
            let* evidence = evidence program value checked in
            calls ((value.fm_name, evidence) :: out) rest
        end
    in
    let* calls = calls [] forms in
    Ok
      ({ program with funcs = program.funcs @ List.map lower forms },
        direct, calls)