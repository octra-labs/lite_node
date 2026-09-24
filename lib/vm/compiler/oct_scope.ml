(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Oct_lang

module Names = Set.Make (String)

type env = {
  values : Names.t;
  calls : Names.t;
}

let add name env = { env with values = Names.add name env.values }

let add_many names env = List.fold_left (fun acc name -> add name acc) env names

let local name env = Names.mem name env.values

let callable name env = Names.mem name env.calls

let rec expr env = function
  | EVar "epoch" when not (local "epoch" env) -> EEpoch
  | EVar "epoch_time" when not (local "epoch_time" env) -> EEpochTime
  | EVar "value" when not (local "value" env) -> EValue
  | ECall ("balance", [address]) when not (callable "balance" env) ->
    EBalance (expr env address)
  | EIndex (name, keys) -> EIndex (name, exprs env keys)
  | (EBinop _ | EUnop _) as value ->
    let rec descend frames = function
      | EBinop (op, left, right) -> descend ((`Right (op, left)) :: frames) right
      | EUnop (op, value) -> descend ((`Unary op) :: frames) value
      | value -> finish frames (expr env value)
    and finish frames value =
      match frames with
      | [] -> value
      | `Unary op :: rest -> finish rest (EUnop (op, value))
      | `Right (op, left) :: rest -> descend ((`Left (op, value)) :: rest) left
      | `Left (op, right) :: rest -> finish rest (EBinop (op, value, right))
    in
    descend [] value
  | EBalance address -> EBalance (expr env address)
  | ECall (name, args) -> ECall (name, exprs env args)
  | EArray values -> EArray (exprs env values)
  | ETuple values -> ETuple (exprs env values)
  | EStoragePath (name, keys, path) ->
    EStoragePath (name, exprs env keys, path)
  | EIndexField (name, keys, field) ->
    EIndexField (name, exprs env keys, field)
  | ETernary (cond, yes, no) ->
    ETernary (expr env cond, expr env yes, expr env no)
  | EUse value ->
    let next = add value.ux_bind env in
    EUse {
      value with
      ux_caps = exprs env value.ux_caps;
      ux_arg = expr env value.ux_arg;
      ux_body = expr next value.ux_body;
    }
  | value -> value

and exprs env values = List.rev (List.rev_map (expr env) values)

let rec block env body = fst (statements env body)

and statement env = function
  | SLocated (line, column, value) ->
    let resolved, next = statement env value in
    SLocated (line, column, resolved), next
  | SLet (name, typ, value) ->
    SLet (name, typ, expr env value), add name env
  | SLetTuple (names, value) ->
    SLetTuple (names, expr env value), add_many names env
  | SAssign (name, value) -> SAssign (name, expr env value), env
  | SFieldSet (name, value) -> SFieldSet (name, expr env value), env
  | SIndexSet (name, keys, value) ->
    SIndexSet (name, exprs env keys, expr env value), env
  | SIndexUpdate (name, keys, op, value) ->
    SIndexUpdate (name, exprs env keys, op, expr env value), env
  | SReturn value -> SReturn (Option.map (expr env) value), env
  | SAssert value -> SAssert (expr env value), env
  | SRequire (cond, message) ->
    SRequire (expr env cond, expr env message), env
  | SEmit (name, values) -> SEmit (name, exprs env values), env
  | SIf (cond, yes, no) ->
    SIf (expr env cond, block env yes, Option.map (block env) no), env
  | SWhile (cond, body) -> SWhile (expr env cond, block env body), env
  | SFor (name, first, last, body) ->
    SFor (name, expr env first, expr env last, block (add name env) body), env
  | SForEach (name, field, body) ->
    SForEach (name, field, block (add name env) body), env
  | SFieldCall (field, name, args) ->
    SFieldCall (field, name, exprs env args), env
  | SStoragePathSet (name, keys, path, value) ->
    SStoragePathSet
      (name, exprs env keys, path, expr env value), env
  | SStoragePathUpdate (name, keys, path, op, value) ->
    SStoragePathUpdate
      (name, exprs env keys, path, op, expr env value), env
  | SIndexFieldSet (name, keys, field, value) ->
    SIndexFieldSet
      (name, exprs env keys, field, expr env value), env
  | SMatch (value, arms) ->
    let arm (name, variant, body) = name, variant, block env body in
    SMatch (expr env value, List.map arm arms), env
  | SExpr value -> SExpr (expr env value), env
  | SRevertError (name, values) ->
    SRevertError (name, exprs env values), env

and statements env = function
  | [] -> [], env
  | item :: rest ->
    let item, next = statement env item in
    let rest, final = statements next rest in
    item :: rest, final

let callable_names program =
  let funcs = List.map (fun value -> value.fn_name) program.funcs in
  let forms = List.map (fun value -> value.fm_name) program.forms in
  let methods =
    program.interfaces
    |> List.concat_map (fun value -> value.if_methods)
    |> List.map (fun value -> value.im_name)
  in
  Names.of_list (funcs @ forms @ methods)

let value_names program =
  program.consts
  |> List.map (fun value -> value.c_name)
  |> Names.of_list

let resolve_func root value =
  let names = List.map (fun param -> param.p_name) value.fn_params in
  { value with fn_body = block (add_many names root) value.fn_body }

let resolve_form root value =
  let names = List.map (fun param -> param.fp_name) value.fm_params in
  { value with fm_body = expr (add_many names root) value.fm_body }

let resolve program =
  let root = {
    values = value_names program;
    calls = callable_names program;
  } in
  let const value = { value with c_value = expr root value.c_value } in
  let invariant value = { value with inv_expr = expr root value.inv_expr } in
  {
    program with
    consts = List.map const program.consts;
    invariants_decl = List.map invariant program.invariants_decl;
    ctor = Option.map (resolve_func root) program.ctor;
    funcs = List.map (resolve_func root) program.funcs;
    forms = List.map (resolve_form root) program.forms;
  }