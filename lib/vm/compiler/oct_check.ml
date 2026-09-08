(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Oct_lang

exception Invalid of string

type env = {
  ast : contract;
  locals : (string * typ) list;
  ret : typ;
  active : string list;
}

let invalid message =
  raise (Invalid message)

let require context expected actual =
  if not (Oct_types.compatible expected actual) then
    invalid
      (Printf.sprintf
        "%s expected = %s actual = %s"
        context
        (typ_to_string expected)
        (typ_to_string actual))

let find_local env name =
  List.assoc_opt name env.locals

let find_state env name =
  List.find_opt (fun field -> String.equal field.sf_name name) env.ast.state

let find_const env name =
  List.find_opt (fun item -> String.equal item.c_name name) env.ast.consts

let find_func env name =
  List.find_opt (fun item -> String.equal item.fn_name name) env.ast.funcs

let find_struct env name =
  List.find_opt (fun item -> String.equal item.sd_name name) env.ast.structs

let find_enum env name =
  List.find_opt (fun item -> String.equal item.en_name name) env.ast.enums

let find_event env name =
  List.find_opt (fun item -> String.equal item.ev_name name) env.ast.events

let map_value typ =
  let rec dig = function
    | TMap (_, value) -> dig value
    | value -> value
  in
  dig typ

let rec struct_path env typ = function
  | [] -> Some typ
  | name :: rest ->
    begin
      match typ with
      | TStruct struct_name ->
        begin
          match find_struct env struct_name with
          | Some item ->
            begin
              match List.assoc_opt name item.sd_fields with
              | Some next -> struct_path env next rest
              | None -> None
            end
          | None -> None
        end
      | _ -> None
    end

let length_path env typ path =
  match List.rev path with
  | "length" :: rest ->
    begin
      match struct_path env typ (List.rev rest) with
      | Some (TList _) | Some (TMap _) -> Some TInt
      | Some _ | None -> None
    end
  | _ -> None

let rec expr env = function
  | EInt _ -> TInt
  | EBool _ -> TBool
  | EString _ -> TString
  | ECaller | EOrigin | ESelfAddr -> TAddress
  | EEpoch -> Option.value (find_local env "epoch") ~default:TInt
  | EEpochTime -> Option.value (find_local env "epoch_time") ~default:TInt
  | EValue -> Option.value (find_local env "value") ~default:TInt
  | EBalance value ->
    require "balance address type differs" TAddress (expr env value);
    TInt
  | ETreeHash | ENodeId | ETxHash -> TString
  | EVar name ->
    begin
      match find_local env name with
      | Some typ -> typ
      | None ->
        begin
          match find_const env name with
          | Some _ when List.exists (String.equal name) env.active ->
            invalid ("constant dependency is cyclic name = " ^ name)
          | Some item when item.c_typ = TVoid ->
            expr { env with active = name :: env.active } item.c_value
          | Some item -> item.c_typ
          | None ->
            if List.mem name ["epoch"; "epoch_time"; "value"] then TInt
            else invalid ("variable is undefined name = " ^ name)
        end
    end
  | EField name ->
    begin
      match find_state env name with
      | Some field -> field.sf_typ
      | None -> invalid ("state field is undefined name = " ^ name)
    end
  | EIndex (name, keys) -> state_index env name keys
  | EBinop (op, left, right) -> binary env op left right
  | EUnop (Neg, value) ->
    let typ = expr env value in
    if Oct_types.numeric typ then typ
    else invalid ("negation operand type differs actual = " ^ typ_to_string typ)
  | EUnop (Not, value) ->
    require "logical negation type differs" TBool (expr env value);
    TBool
  | ECall (name, args) -> call env name args
  | EArray values ->
    begin
      match List.map (expr env) values with
      | [] -> TList TVoid
      | first :: rest ->
        List.iter (require "list item type differs" first) rest;
        TList first
    end
  | ETuple values -> TTuple (List.map (expr env) values)
  | EStoragePath (field, keys, path) -> storage_path env field keys path
  | EFieldProp (field, name) -> storage_path env field [] [name]
  | EIndexField (field, keys, name) -> storage_path env field keys [name]
  | EEnumVariant (name, variant) ->
    begin
      match find_enum env name with
      | Some item when List.exists (String.equal variant) item.en_variants ->
        TEnum name
      | Some _ -> invalid ("enum variant is undefined name = " ^ name ^ "." ^ variant)
      | None -> invalid ("enum is undefined name = " ^ name)
    end
  | ETernary (guard, yes, no) ->
    require "ternary condition type differs" TBool (expr env guard);
    let expected = expr env yes in
    require "ternary branch type differs" expected (expr env no);
    expected
  | EEqual (declared, left, right) ->
    require "equality left type differs" declared (expr env left);
    require "equality right type differs" declared (expr env right);
    TBool
  | ELet (name, _, declared, input, body) ->
    require "local initializer type differs" declared (expr env input);
    expr { env with locals = (name, declared) :: env.locals } body
  | ESplit (pair, (left, _, left_type), (right, _, right_type), body) ->
    require "split type differs" (TTuple [left_type; right_type]) (expr env pair);
    expr
      { env with
        locals = (right, right_type) :: (left, left_type) :: env.locals }
      body
  | EOrbit (_, turns, seed, (name, _, declared), body) ->
    Option.iter
      (fun value ->
        require "orbit count type differs" TInt (expr env value))
      turns;
    require "orbit seed type differs" declared (expr env seed);
    require
      "orbit body type differs"
      declared
      (expr { env with locals = (name, declared) :: env.locals } body);
    declared
  | EAction _ | EUse _ ->
    invalid "direct form expression requires AML compilation"

and binary env op left right =
  let left_type = expr env left in
  let right_type = expr env right in
  match op with
  | Add when Oct_types.text left_type || Oct_types.text right_type -> TString
  | Add | Sub | Mul | Div | Mod ->
    if Oct_types.numeric left_type && Oct_types.numeric right_type then
      Oct_types.numeric_result left_type right_type
    else
      invalid
        (Printf.sprintf
          "arithmetic operand type differs left = %s right = %s"
          (typ_to_string left_type)
          (typ_to_string right_type))
  | Eq | Neq ->
    let zero = function
      | EInt value -> Z.equal value Z.zero
      | _ -> false
    in
    if Oct_types.compatible left_type right_type
       || Oct_types.opaque left_type && zero right
       || Oct_types.opaque right_type && zero left
    then TBool
    else
      invalid
        (Printf.sprintf
          "equality operand type differs left = %s right = %s"
          (typ_to_string left_type)
          (typ_to_string right_type))
  | Lt | Gt | Le | Ge ->
    if Oct_types.numeric left_type && Oct_types.numeric right_type then TBool
    else
      invalid
        (Printf.sprintf
          "comparison operand type differs left = %s right = %s"
          (typ_to_string left_type)
          (typ_to_string right_type))
  | And | Or ->
    require "logical left operand type differs" TBool left_type;
    require "logical right operand type differs" TBool right_type;
    TBool

and state_index env name keys =
  match find_state env name with
  | Some field -> index env field.sf_typ keys
  | None -> invalid ("state field is undefined name = " ^ name)

and index env typ keys =
  match keys, typ with
  | [], value -> value
  | key :: rest, TMap (key_type, value_type) ->
    require "map key type differs" key_type (expr env key);
    index env value_type rest
  | _ :: _, _ -> invalid "indexed value is not a map"

and storage_path env field keys path =
  let base = state_index env field keys in
  match length_path env base path with
  | Some typ -> typ
  | None ->
    begin
      match struct_path env base path with
      | Some typ -> typ
      | None ->
        invalid
          ("storage path is undefined name = "
           ^ field
           ^ "."
           ^ String.concat "." path)
    end

and call env name args =
  let types = List.map (expr env) args in
  match name with
  | "concat" | "to_string" | "fhe_ser" | "fhe_ser_pk"
  | "substr" | "sha256" | "keccak256"
  | "digest_sha256" | "digest_keccak256" | "current_tx_hash"
  | "blob_store" | "blob_load" | "join" | "replace" -> TString
  | "len" | "index_of" | "bit_and" | "bit_or" | "bit_xor"
  | "bit_shl" | "bit_shr" | "min" | "max" | "abs" | "to_int"
  | "parse_ints" | "mget" | "pow" | "vecdot" | "vecdot_fp"
  | "vecdot_q16" | "argmax_fp" | "argmax_q16" | "exp_lut"
  | "exp_q16" | "split" -> TInt
  | "fhe_load_pk" | "fhe_deser_pk" -> TPubKey
  | "fhe_add" | "fhe_sub" | "fhe_mul" | "fhe_scale" | "fhe_div_const"
  | "fhe_add_const" | "fhe_sub_const" | "fhe_deser" -> TCipher
  | "fhe_commit" | "fhe_pedersen" | "pedersen_add" | "pedersen_sub"
  | "pedersen_identity" -> TBytes
  | "circle_balance_state_ref"
  | "circle_balance_status"
  | "circle_balance_last_workflow"
  | "circle_register_state_ref"
  | "circle_register_status"
  | "circle_register_last_workflow"
  | "circle_object_state_ref"
  | "circle_object_status"
  | "circle_object_last_transition"
  | "circle_object_member_ref_at"
  | "circle_object_member_state_ref"
  | "circle_object_member_kind"
  | "circle_object_member_class"
  | "circle_object_member_codec"
  | "circle_object_member_status"
  | "circle_object_delivery_key_id"
  | "circle_object_transition_mode"
  | "circle_object_required_proof_kind"
  | "circle_state_class"
  | "circle_state_codec"
  | "circle_state_schema_hash"
  | "circle_state_subject_addr"
  | "circle_state_hfhe_profile"
  | "circle_state_delivery_key_id"
  | "circle_balance_cell_ciphertext_commitment"
  | "circle_balance_cell_amount_commitment"
  | "circle_balance_cell_proof_kind"
  | "circle_balance_cell_proof_receipt_hash"
  | "circle_register_cell_ciphertext_commitment"
  | "circle_register_cell_proof_kind"
  | "circle_register_cell_proof_receipt_hash" -> TString
  | "circle_balance_version"
  | "circle_register_version"
  | "circle_object_version"
  | "circle_object_member_count"
  | "circle_object_member_quorum"
  | "circle_state_activate_after"
  | "circle_state_expire_after"
  | "circle_balance_bind"
  | "circle_register_bind"
  | "circle_object_bind"
  | "circle_object_transition_apply" -> TInt
  | "circle_state_mutable"
  | "circle_state_tombstone"
  | "circle_state_revoked"
  | "circle_object_tombstone"
  | "circle_object_revoked"
  | "circle_object_has_member"
  | "circle_object_allow_detach"
  | "circle_object_allow_root_state_rotation"
  | "circle_state_describe"
  | "circle_state_publish"
  | "circle_state_release"
  | "circle_state_retire"
  | "circle_state_tombstone_apply"
  | "circle_state_restore"
  | "circle_state_revoke_apply"
  | "circle_state_reinstate"
  | "circle_object_policy_define"
  | "circle_object_policy_release"
  | "circle_object_policy_retire"
  | "circle_object_tombstone_apply"
  | "circle_object_restore"
  | "circle_object_revoke_apply"
  | "circle_object_reinstate"
  | "circle_balance_cell_materialize"
  | "circle_register_cell_materialize"
  | "circle_object_member_attach"
  | "circle_object_member_detach"
  | "circle_object_transition_record"
  | "circle_balance_workflow_record"
  | "circle_register_workflow_record" -> TBool
  | "call" -> TString
  | "deploy" | "circle_spawn" -> TAddress
  | "some" ->
    begin
      match types with
      | [typ] -> TOption typ
      | _ -> invalid "some argument count differs expected = 1"
    end
  | "none" ->
    if types = [] then TOption TVoid
    else invalid "none argument count differs expected = 0"
  | "unwrap" ->
    begin
      match types with
      | [TOption typ] -> typ
      | [_] -> invalid "unwrap argument type differs"
      | _ -> invalid "unwrap argument count differs expected = 1"
    end
  | "fhe_verify_zero" | "fhe_verify_range" | "fhe_verify_bound"
  | "groth16_verify_bn254" | "is_address" | "assert_address"
  | "starts_with" | "is_hex" | "ed25519_ok" | "sig_ok_ed25519"
  | "is_some_opt" | "transfer" | "checkpoint" | "rollback" | "commit"
  | "mset" | "matmul" | "softmax" | "softmax_q16" | "layernorm"
  | "layernorm_q16" | "relu" | "rmsnorm" | "rmsnorm_q16" | "silu"
  | "silu_q16" | "elemwise_mul" | "load_int8" | "load_int8_b64"
  | "residual_add" | "rope_apply" | "rope_apply_q16" | "matmul_q16"
  | "shift_round" | "matmul_fp" | "rmsnorm_fp" | "silu_fp"
  | "elemwise_mul_fp" | "residual_add_fp" | "rope_apply_fp"
  | "load_int8_fp" | "attention_kv_fp" | "attention_kv_q16"
  | "append_vec_fp" | "load_int8_q16" | "append_vec_q16" -> TBool
  | _ -> custom_call env name types

and custom_call env name types =
  match find_func env name with
  | None -> invalid ("function is undefined name = " ^ name)
  | Some fn ->
    if List.length fn.fn_params <> List.length types then
      invalid
        (Printf.sprintf
          "function argument count differs function = %s expected = %d actual = %d"
          name
          (List.length fn.fn_params)
          (List.length types));
    List.iter2
      (fun param actual ->
        require
          ("function argument type differs function = " ^ name)
          param.p_typ
          actual)
      fn.fn_params
      types;
    fn.fn_ret

let add_local env name typ =
  { env with locals = (name, typ) :: env.locals }

let rec block env statements =
  List.fold_left statement env statements

and statement env = function
  | SLocated (_, _, value) -> statement env value
  | SLet (name, annotation, value) ->
    let actual = expr env value in
    let typ = Option.value annotation ~default:actual in
    require "local initializer type differs" typ actual;
    add_local env name typ
  | SLetTuple (names, value) ->
    begin
      match expr env value with
      | TTuple types when List.length names = List.length types ->
        List.fold_left2 add_local env names types
      | TTuple types ->
        invalid
          (Printf.sprintf
            "tuple binding count differs expected = %d actual = %d"
            (List.length names)
            (List.length types))
      | actual ->
        invalid ("tuple binding type differs actual = " ^ typ_to_string actual)
    end
  | SAssign (name, value) ->
    begin
      match find_local env name with
      | Some typ ->
        require "assignment type differs" typ (expr env value);
        env
      | None -> invalid ("variable is undefined name = " ^ name)
    end
  | SFieldSet (name, value) ->
    begin
      match find_state env name with
      | Some field ->
        require "state assignment type differs" field.sf_typ (expr env value);
        env
      | None -> invalid ("state field is undefined name = " ^ name)
    end
  | SIndexSet (name, keys, value) ->
    let expected = state_index env name keys in
    require "indexed assignment type differs" expected (expr env value);
    env
  | SIndexUpdate (name, keys, op, value) ->
    let expected = state_index env name keys in
    require "indexed update type differs" expected
      (binary env op (EIndex (name, keys)) value);
    env
  | SReturn (Some value) ->
    if env.ret = TVoid then invalid "void function cannot return a value";
    require "return type differs" env.ret (expr env value);
    env
  | SReturn None ->
    if env.ret <> TVoid then
      invalid ("return value is required type = " ^ typ_to_string env.ret);
    env
  | SAssert value ->
    require "assert condition type differs" TBool (expr env value);
    env
  | SRequire (guard, message) ->
    require "require condition type differs" TBool (expr env guard);
    ignore (expr env message);
    env
  | SEmit (name, values) ->
    let types = List.map (expr env) values in
    begin
      match find_event env name with
      | None -> ()
      | Some event ->
        let expected = List.map (fun (_, typ, _) -> typ) event.ev_fields in
        if List.length expected <> List.length types then
          invalid ("event argument count differs event = " ^ name);
        List.iter2 (require ("event argument type differs event = " ^ name)) expected types
    end;
    env
  | SIf (guard, yes, no) ->
    require "if condition type differs" TBool (expr env guard);
    ignore (block env yes);
    Option.iter (fun body -> ignore (block env body)) no;
    env
  | SWhile (guard, body) ->
    require "while condition type differs" TBool (expr env guard);
    ignore (block env body);
    env
  | SFor (name, first, last, body) ->
    require "range start type differs" TInt (expr env first);
    require "range end type differs" TInt (expr env last);
    ignore (block (add_local env name TInt) body);
    env
  | SFieldCall (name, method_name, values) ->
    check_field_call env name method_name values;
    env
  | SStoragePathSet (name, keys, path, value) ->
    let expected = storage_path env name keys path in
    require "storage path assignment type differs" expected (expr env value);
    env
  | SStoragePathUpdate (name, keys, path, op, value) ->
    let expected = storage_path env name keys path in
    require "storage path update type differs" expected
      (binary env op (EStoragePath (name, keys, path)) value);
    env
  | SIndexFieldSet (name, keys, field, value) ->
    let expected = storage_path env name keys [field] in
    require "storage path assignment type differs" expected (expr env value);
    env
  | SForEach (name, field, body) ->
    begin
      match find_state env field with
      | Some { sf_typ = TList typ; _ } ->
        ignore (block (add_local env name typ) body)
      | Some _ -> invalid ("field is not a list name = " ^ field)
      | None -> invalid ("state field is undefined name = " ^ field)
    end;
    env
  | SMatch (value, arms) ->
    ignore (expr env value);
    List.iter (fun (_, _, body) -> ignore (block env body)) arms;
    env
  | SExpr value ->
    ignore (expr env value);
    env
  | SRevertError (_, values) ->
    List.iter (fun value -> ignore (expr env value)) values;
    env

and check_field_call env name method_name values =
  match find_state env name with
  | Some { sf_typ = TList item_type; _ } ->
    begin
      match method_name, values with
      | "push", [value] ->
        require "list item type differs" item_type (expr env value)
      | "delete", [value] ->
        require "list index type differs" TInt (expr env value)
      | "len", [] | "pop", [] -> ()
      | "push", _ | "delete", _ | "len", _ | "pop", _ ->
        invalid ("list method argument count differs method = " ^ method_name)
      | _ -> invalid ("list method is undefined name = " ^ method_name)
    end
  | Some _ -> invalid ("field is not a list name = " ^ name)
  | None -> invalid ("state field is undefined name = " ^ name)

let check_const ast item =
  let env = { ast; locals = []; ret = TVoid; active = [item.c_name] } in
  let actual = expr env item.c_value in
  if item.c_typ <> TVoid then
    require "constant type differs" item.c_typ actual

let check_func ast fn =
  if fn.fn_ret <> TVoid && not (Oct_types.block_returns fn.fn_body) then
    invalid
      (Printf.sprintf
        "function return is not total function = %s type = %s"
        fn.fn_name
        (typ_to_string fn.fn_ret));
  let locals = List.map (fun param -> param.p_name, param.p_typ) fn.fn_params in
  ignore (block { ast; locals; ret = fn.fn_ret; active = [] } fn.fn_body)

let check_ctor ast fn =
  let locals = List.map (fun param -> param.p_name, param.p_typ) fn.fn_params in
  ignore (block { ast; locals; ret = TVoid; active = [] } fn.fn_body)

let check ast =
  try
    List.iter (check_const ast) ast.consts;
    Option.iter (check_ctor ast) ast.ctor;
    List.iter (check_func ast) ast.funcs;
    Ok ()
  with Invalid message -> Error message