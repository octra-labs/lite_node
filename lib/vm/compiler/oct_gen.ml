(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Oct_lang

exception GenError of string * int

let gerr line msg = raise (GenError (msg, line))

type env = {
  structs : struct_def list;
  enums : enum_def list;
  consts : const_def list;
  state : state_field list;
  events : event_def list;
  errors : error_def list;
  funcs : func_def list;
  func_labels : (string, int) Hashtbl.t;
  mutable locals : (string * int * typ) list;
  mutable base_reg : int;
  mutable next_reg : int;
  mutable next_label : int;
  mutable code : Contract_vm.instr list;
  mutable line : int;
  has_payable : bool;
  mutable in_nonreentrant : bool;
  mutable fn_is_view : bool;
  mutable fn_is_pure : bool;
  declaration : declaration;
}

let make_env declaration structs enums consts state events errors funcs = {
  structs; enums; consts; state; events; errors; funcs;
  func_labels = Hashtbl.create 16;
  locals = [];
  base_reg = 1; next_reg = 1; next_label = 10000;
  code = []; line = 0;
  has_payable = List.exists (fun f -> f.fn_payable) funcs;
  in_nonreentrant = false;
  fn_is_view = false;
  fn_is_pure = false;
  declaration;
}

let check_no_storage_write env =
  if env.fn_is_pure then gerr env.line "pure function cannot write storage";
  if env.fn_is_view then gerr env.line "view function cannot write storage"

let check_no_emit env =
  if env.fn_is_pure then gerr env.line "pure function cannot emit events"

let check_no_transfer env =
  if env.fn_is_pure then gerr env.line "pure function cannot transfer";
  if env.fn_is_view then gerr env.line "view function cannot transfer"

let check_no_call env =
  if env.fn_is_pure then gerr env.line "pure function cannot call external contracts"

let alloc_reg env =
  let r = env.next_reg in
  if r > 63 then gerr env.line "too many local variables (max 63 registers)";
  env.next_reg <- r + 1;
  r

let alloc_label env =
  let l = env.next_label in
  env.next_label <- l + 1;
  l

let emit env instr =
  env.code <- instr :: env.code

let emit_bytes32_check env r =
  let len_r = alloc_reg env in
  let expected_r = alloc_reg env in
  let cmp_r = alloc_reg env in
  emit env (Contract_vm.STRLEN (len_r, r));
  emit env (Contract_vm.LDI (expected_r, VInt (Z.of_int 32)));
  emit env (Contract_vm.EQ (cmp_r, len_r, expected_r));
  let ok_label = alloc_label env in
  emit env (Contract_vm.JIF (cmp_r, ok_label));
  emit env Contract_vm.REVERT;
  emit env (Contract_vm.JDEST ok_label)

let emit_unsigned_range_check env r max_z =
  let zero_r = alloc_reg env in
  let max_r = alloc_reg env in
  let cmp_r = alloc_reg env in
  let fail_label = alloc_label env in
  let ok_label = alloc_label env in
  emit env (Contract_vm.LDI (zero_r, VInt Z.zero));
  emit env (Contract_vm.LDI (max_r, VInt max_z));
  emit env (Contract_vm.LT (cmp_r, r, zero_r));
  emit env (Contract_vm.JIF (cmp_r, fail_label));
  emit env (Contract_vm.GT (cmp_r, r, max_r));
  emit env (Contract_vm.JIF (cmp_r, fail_label));
  emit env (Contract_vm.JMP ok_label);
  emit env (Contract_vm.JDEST fail_label);
  emit env Contract_vm.REVERT;
  emit env (Contract_vm.JDEST ok_label)

let emit_type_check env r typ =
  match typ with
  | TBytes32 -> emit_bytes32_check env r
  | TU64 -> emit_unsigned_range_check env r Contract_vm.max_u64
  | TU128 -> emit_unsigned_range_check env r Contract_vm.max_u128
  | TU256 -> emit_unsigned_range_check env r Contract_vm.max_u256
  | _ -> ()

let emit_result_type_check env r typ =
  match typ with
  | TU64 | TU128 | TU256 -> emit_type_check env r typ
  | _ -> ()

let find_state env name =
  List.find_opt (fun sf -> sf.sf_name = name) env.state

let find_local env name =
  List.find_opt (fun (n, _, _) -> n = name) env.locals

let find_event env name =
  List.find_opt (fun ev -> ev.ev_name = name) env.events

let find_const env name =
  List.find_opt (fun c -> c.c_name = name) env.consts

let find_struct env name =
  List.find_opt (fun sd -> sd.sd_name = name) env.structs

let find_enum env name =
  List.find_opt (fun ed -> ed.en_name = name) env.enums

let find_error env name =
  List.find_opt (fun ed -> ed.err_name = name) env.errors

let is_int_storage env t =
  t = TInt || t = TU64 || t = TU128 || t = TU256 || (match t with
    | TStruct name -> find_enum env name <> None
    | TEnum _ -> true
    | _ -> false)

let typed_static_storage env = function
  | TInt
  | TBool
  | TString
  | TAddress
  | TBytes
  | TBytes32
  | TU64
  | TU128
  | TU256 -> env.declaration = ProgramDecl
  | TCipher
  | TPubKey
  | TMap _
  | TList _
  | TStruct _
  | TEnum _
  | TOption _
  | TTuple _
  | TVoid -> false

let resolve_enum_variant env enum_name variant_name =
  match find_enum env enum_name with
  | Some ed ->
    let rec go idx = function
      | [] -> gerr env.line (Printf.sprintf "unknown variant: %s.%s" enum_name variant_name)
      | v :: _ when v = variant_name -> idx
      | _ :: rest -> go (idx + 1) rest
    in
    go 0 ed.en_variants
  | None -> gerr env.line (Printf.sprintf "unknown enum: %s" enum_name)

let storage_key_for_field name = name

let map_value_type t =
  let rec dig = function TMap (_, v) -> dig v | t -> t in
  dig t

let resolve_struct_field_type env state_field sf_name =
  match find_state env state_field with
  | Some sf ->
    let vt = map_value_type sf.sf_typ in
    (match vt with
     | TStruct sname ->
       (match find_struct env sname with
        | Some sd -> List.assoc_opt sf_name sd.sd_fields
        | None -> None)
     | _ -> None)
  | None -> None

let rec resolve_struct_path_type env typ path =
  match path with
  | [] -> Some typ
  | seg :: rest ->
    (match typ with
     | TStruct sname ->
       (match find_struct env sname with
        | Some sd ->
          (match List.assoc_opt seg sd.sd_fields with
           | Some next_typ -> resolve_struct_path_type env next_typ rest
           | None -> None)
        | None -> None)
     | _ -> None)

let resolve_storage_path_base_type env field keys =
  match find_state env field with
  | Some sf ->
    if keys = [] then Some sf.sf_typ else Some (map_value_type sf.sf_typ)
  | None -> None

let resolve_storage_length_prefix env field keys path =
  match List.rev path with
  | "length" :: rev_prefix ->
    let prefix = List.rev rev_prefix in
    (match resolve_storage_path_base_type env field keys with
     | Some base_typ ->
       (match resolve_struct_path_type env base_typ prefix with
        | Some (TList _)
        | Some (TMap _) -> Some prefix
        | _ -> None)
     | None -> None)
  | _ -> None

let resolve_storage_path_type env field keys path =
  match resolve_storage_length_prefix env field keys path with
  | Some _ -> Some TInt
  | None ->
    (match resolve_storage_path_base_type env field keys with
     | Some base_typ -> resolve_struct_path_type env base_typ path
     | None -> None)

let gen_storage_key env field_name keys =
  let kr = alloc_reg env in
  emit env (Contract_vm.LDI (kr, VString (field_name ^ ":")));
  let need_sep = List.length keys > 1 in
  let sep = if need_sep then begin
    let s = alloc_reg env in
    emit env (Contract_vm.LDI (s, VString ":"));
    s
  end else 0 in
  List.iteri (fun i k_reg ->
    if i > 0 then emit env (Contract_vm.CONCAT (kr, kr, sep));
    emit env (Contract_vm.CONCAT (kr, kr, k_reg))
  ) keys;
  kr

let gen_int_from_storage env r =
  let zero = alloc_reg env in
  emit env (Contract_vm.LDI (zero, VInt Z.zero));
  emit env (Contract_vm.ADD (r, zero, r));
  r

let gen_bool_from_storage env r =
  let true_r = alloc_reg env in
  let one_r = alloc_reg env in
  let yes_r = alloc_reg env in
  let is_true_r = alloc_reg env in
  let is_one_r = alloc_reg env in
  let is_yes_r = alloc_reg env in
  let true_l = alloc_label env in
  let end_l = alloc_label env in
  emit env (Contract_vm.LDI (true_r, VString "true"));
  emit env (Contract_vm.LDI (one_r, VString "1"));
  emit env (Contract_vm.LDI (yes_r, VString "yes"));
  emit env (Contract_vm.EQ (is_true_r, r, true_r));
  emit env (Contract_vm.JIF (is_true_r, true_l));
  emit env (Contract_vm.EQ (is_one_r, r, one_r));
  emit env (Contract_vm.JIF (is_one_r, true_l));
  emit env (Contract_vm.EQ (is_yes_r, r, yes_r));
  emit env (Contract_vm.JIF (is_yes_r, true_l));
  emit env (Contract_vm.LDI (r, VBool false));
  emit env (Contract_vm.JMP end_l);
  emit env (Contract_vm.JDEST true_l);
  emit env (Contract_vm.LDI (r, VBool true));
  emit env (Contract_vm.JDEST end_l);
  r

let storage_path_to_string field path =
  match path with
  | [] -> field
  | _ -> field ^ "." ^ String.concat "." path

let gen_storage_path_prefix_key env field key_regs path =
  let kr =
    if key_regs = [] then begin
      let r = alloc_reg env in
      emit env (Contract_vm.LDI (r, VString field));
      r
    end else gen_storage_key env field key_regs
  in
  List.iter (fun segment ->
    let sr = alloc_reg env in
    emit env (Contract_vm.LDI (sr, VString (":" ^ segment)));
    emit env (Contract_vm.CONCAT (kr, kr, sr))
  ) path;
  kr

let gen_storage_loaded_value env r typ =
  if is_int_storage env typ then gen_int_from_storage env r
  else if typ = TBool then gen_bool_from_storage env r
  else r

let gen_dynamic_key env prefix key_r suffix =
  let prefix_r = alloc_reg env in
  let full_r = alloc_reg env in
  let suffix_r = alloc_reg env in
  emit env (Contract_vm.LDI (prefix_r, VString prefix));
  emit env (Contract_vm.MOV (full_r, prefix_r));
  emit env (Contract_vm.CONCAT (full_r, full_r, key_r));
  emit env (Contract_vm.LDI (suffix_r, VString suffix));
  emit env (Contract_vm.CONCAT (full_r, full_r, suffix_r));
  full_r

let gen_dynamic_string_load env prefix key_r suffix =
  let kr = gen_dynamic_key env prefix key_r suffix in
  let rd = alloc_reg env in
  emit env (Contract_vm.SLOADK (rd, kr));
  rd

let gen_dynamic_int_load env prefix key_r suffix =
  let rd = gen_dynamic_string_load env prefix key_r suffix in
  gen_int_from_storage env rd

let gen_dynamic_store env prefix key_r suffix value_r =
  let kr = gen_dynamic_key env prefix key_r suffix in
  emit env (Contract_vm.SSTOREK (kr, value_r))

let gen_dynamic_delete env prefix key_r suffix =
  let kr = gen_dynamic_key env prefix key_r suffix in
  emit env (Contract_vm.SDELK kr)

let gen_dynamic_store_optional_string env prefix key_r suffix value_r =
  let empty_r = alloc_reg env in
  let is_empty_r = alloc_reg env in
  let delete_l = alloc_label env in
  let end_l = alloc_label env in
  emit env (Contract_vm.LDI (empty_r, VString ""));
  emit env (Contract_vm.EQ (is_empty_r, value_r, empty_r));
  emit env (Contract_vm.JIF (is_empty_r, delete_l));
  gen_dynamic_store env prefix key_r suffix value_r;
  emit env (Contract_vm.JMP end_l);
  emit env (Contract_vm.JDEST delete_l);
  gen_dynamic_delete env prefix key_r suffix;
  emit env (Contract_vm.JDEST end_l)

let gen_joined_key env left_r right_r =
  let combined_r = alloc_reg env in
  let sep_r = alloc_reg env in
  emit env (Contract_vm.MOV (combined_r, left_r));
  emit env (Contract_vm.LDI (sep_r, VString ":"));
  emit env (Contract_vm.CONCAT (combined_r, combined_r, sep_r));
  emit env (Contract_vm.CONCAT (combined_r, combined_r, right_r));
  combined_r

let gen_balance_binding_load env suffix subject_r =
  match suffix with
  | "version" ->
    gen_dynamic_int_load env "balance_binding:" subject_r ":version"
  | "current_state_ref"
  | "status"
  | "last_workflow_ref" ->
    gen_dynamic_string_load env "balance_binding:" subject_r (":" ^ suffix)
  | _ ->
    gerr env.line ("unsupported balance binding field: " ^ suffix)

let gen_register_binding_load env suffix register_r =
  match suffix with
  | "version" ->
    gen_dynamic_int_load env "register_binding:" register_r ":version"
  | "current_state_ref"
  | "status"
  | "last_workflow_ref" ->
    gen_dynamic_string_load env "register_binding:" register_r (":" ^ suffix)
  | _ ->
    gerr env.line ("unsupported register binding field: " ^ suffix)

let gen_balance_binding_bind env rd subject_r state_ref_r workflow_ref_r status_r =
  check_no_storage_write env;
  let version_r =
    gen_dynamic_int_load env "balance_binding:" subject_r ":version" in
  let one_r = alloc_reg env in
  emit env (Contract_vm.LDI (one_r, VInt Z.one));
  emit env (Contract_vm.ADD (version_r, version_r, one_r));
  gen_dynamic_store env "balance_binding:" subject_r ":current_state_ref" state_ref_r;
  gen_dynamic_store env "balance_binding:" subject_r ":version" version_r;
  gen_dynamic_store env "balance_binding:" subject_r ":status" status_r;
  gen_dynamic_store env "balance_binding:" subject_r ":last_workflow_ref" workflow_ref_r;
  emit env (Contract_vm.MOV (rd, version_r))

let gen_register_binding_bind env rd register_r state_ref_r workflow_ref_r status_r =
  check_no_storage_write env;
  let version_r =
    gen_dynamic_int_load env "register_binding:" register_r ":version" in
  let one_r = alloc_reg env in
  emit env (Contract_vm.LDI (one_r, VInt Z.one));
  emit env (Contract_vm.ADD (version_r, version_r, one_r));
  gen_dynamic_store env "register_binding:" register_r ":current_state_ref" state_ref_r;
  gen_dynamic_store env "register_binding:" register_r ":version" version_r;
  gen_dynamic_store env "register_binding:" register_r ":status" status_r;
  gen_dynamic_store env "register_binding:" register_r ":last_workflow_ref" workflow_ref_r;
  emit env (Contract_vm.MOV (rd, version_r))

let gen_balance_workflow_record env rd workflow_r flow_kind_r debit_subject_r credit_subject_r
    debit_state_r credit_state_r amount_commitment_r proof_kind_r proof_receipt_hash_r
    status_r intent_id_r =
  check_no_storage_write env;
  gen_dynamic_store env "balance_workflow:" workflow_r ":flow_kind" flow_kind_r;
  gen_dynamic_store env "balance_workflow:" workflow_r ":debit_subject_addr" debit_subject_r;
  gen_dynamic_store env "balance_workflow:" workflow_r ":credit_subject_addr" credit_subject_r;
  gen_dynamic_store env "balance_workflow:" workflow_r ":debit_state_ref" debit_state_r;
  gen_dynamic_store env "balance_workflow:" workflow_r ":credit_state_ref" credit_state_r;
  gen_dynamic_store env "balance_workflow:" workflow_r ":amount_commitment" amount_commitment_r;
  gen_dynamic_store env "balance_workflow:" workflow_r ":proof_kind" proof_kind_r;
  gen_dynamic_store env "balance_workflow:" workflow_r ":proof_receipt_hash" proof_receipt_hash_r;
  gen_dynamic_store env "balance_workflow:" workflow_r ":status" status_r;
  gen_dynamic_store env "balance_workflow:" workflow_r ":intent_id" intent_id_r;
  emit env (Contract_vm.LDI (rd, VBool true))

let gen_register_workflow_record env rd workflow_r register_ref_r previous_state_r
    next_state_r workflow_kind_r proof_kind_r proof_receipt_hash_r status_r intent_id_r =
  check_no_storage_write env;
  gen_dynamic_store env "register_workflow:" workflow_r ":register_ref" register_ref_r;
  gen_dynamic_store env "register_workflow:" workflow_r ":previous_state_ref" previous_state_r;
  gen_dynamic_store env "register_workflow:" workflow_r ":next_state_ref" next_state_r;
  gen_dynamic_store env "register_workflow:" workflow_r ":workflow_kind" workflow_kind_r;
  gen_dynamic_store env "register_workflow:" workflow_r ":proof_kind" proof_kind_r;
  gen_dynamic_store env "register_workflow:" workflow_r ":proof_receipt_hash" proof_receipt_hash_r;
  gen_dynamic_store env "register_workflow:" workflow_r ":status" status_r;
  gen_dynamic_store env "register_workflow:" workflow_r ":intent_id" intent_id_r;
  emit env (Contract_vm.LDI (rd, VBool true))

let gen_state_path_key env state_ref_r =
  let path_key_r = alloc_reg env in
  emit env (Contract_vm.STATE_PATH_KEY (path_key_r, state_ref_r));
  path_key_r

let gen_state_descriptor_load env suffix state_ref_r =
  let path_key_r = gen_state_path_key env state_ref_r in
  match suffix with
  | "mutable_state" ->
    let rd = gen_dynamic_string_load env "state_descriptor:" path_key_r ":mutable_state" in
    gen_bool_from_storage env rd
  | "state_class"
  | "codec"
  | "schema_hash"
  | "subject_addr"
  | "hfhe_profile" ->
    gen_dynamic_string_load env "state_descriptor:" path_key_r (":" ^ suffix)
  | _ ->
    gerr env.line ("unsupported state descriptor field: " ^ suffix)

let gen_state_policy_load env suffix state_ref_r =
  let path_key_r = gen_state_path_key env state_ref_r in
  match suffix with
  | "activate_after_epoch"
  | "expire_after_epoch" ->
    gen_dynamic_int_load env "state_policy:" path_key_r (":" ^ suffix)
  | "tombstone"
  | "revoked" ->
    let rd = gen_dynamic_string_load env "state_policy:" path_key_r (":" ^ suffix) in
    gen_bool_from_storage env rd
  | "delivery_key_id" ->
    gen_dynamic_string_load env "state_policy:" path_key_r ":delivery_key_id"
  | _ ->
    gerr env.line ("unsupported state policy field: " ^ suffix)

let gen_balance_cell_load env suffix state_ref_r =
  let path_key_r = gen_state_path_key env state_ref_r in
  match suffix with
  | "ciphertext_commitment"
  | "amount_commitment"
  | "proof_kind"
  | "proof_receipt_hash" ->
    gen_dynamic_string_load env "balance_cell:" path_key_r (":" ^ suffix)
  | _ ->
    gerr env.line ("unsupported balance cell field: " ^ suffix)

let gen_register_cell_load env suffix state_ref_r =
  let path_key_r = gen_state_path_key env state_ref_r in
  match suffix with
  | "ciphertext_commitment"
  | "proof_kind"
  | "proof_receipt_hash" ->
    gen_dynamic_string_load env "register_cell:" path_key_r (":" ^ suffix)
  | _ ->
    gerr env.line ("unsupported register cell field: " ^ suffix)

let gen_object_binding_load env suffix object_ref_r =
  match suffix with
  | "version" ->
    gen_dynamic_int_load env "object_binding:" object_ref_r ":version"
  | "current_state_ref"
  | "status"
  | "last_transition_ref" ->
    gen_dynamic_string_load env "object_binding:" object_ref_r (":" ^ suffix)
  | _ ->
    gerr env.line ("unsupported object binding field: " ^ suffix)

let gen_object_member_load env suffix object_ref_r member_ref_r =
  let member_key_r = gen_joined_key env object_ref_r member_ref_r in
  match suffix with
  | "state_ref"
  | "member_kind"
  | "state_class"
  | "codec"
  | "status" ->
    gen_dynamic_string_load env "object_member:" member_key_r (":" ^ suffix)
  | _ ->
    gerr env.line ("unsupported object member field: " ^ suffix)

let gen_object_policy_load env suffix object_ref_r =
  match suffix with
  | "activate_after_epoch"
  | "expire_after_epoch"
  | "member_quorum" ->
    gen_dynamic_int_load env "object_policy:" object_ref_r (":" ^ suffix)
  | "tombstone"
  | "revoked"
  | "allow_detach"
  | "allow_root_state_rotation" ->
    let rd = gen_dynamic_string_load env "object_policy:" object_ref_r (":" ^ suffix) in
    gen_bool_from_storage env rd
  | "delivery_key_id"
  | "transition_mode"
  | "required_proof_kind" ->
    gen_dynamic_string_load env "object_policy:" object_ref_r (":" ^ suffix)
  | _ ->
    gerr env.line ("unsupported object policy field: " ^ suffix)

let gen_state_describe env rd state_ref_r state_class_r codec_r schema_hash_r
    subject_addr_r hfhe_profile_r mutable_state_r =
  check_no_storage_write env;
  let path_key_r = gen_state_path_key env state_ref_r in
  gen_dynamic_store env "state_descriptor:" path_key_r ":state_class" state_class_r;
  gen_dynamic_store env "state_descriptor:" path_key_r ":codec" codec_r;
  gen_dynamic_store_optional_string env "state_descriptor:" path_key_r ":schema_hash" schema_hash_r;
  gen_dynamic_store_optional_string env "state_descriptor:" path_key_r ":subject_addr" subject_addr_r;
  gen_dynamic_store env "state_descriptor:" path_key_r ":hfhe_profile" hfhe_profile_r;
  gen_dynamic_store env "state_descriptor:" path_key_r ":mutable_state" mutable_state_r;
  emit env (Contract_vm.LDI (rd, VBool true))

let gen_state_publish env rd state_ref_r delivery_key_r activate_after_r expire_after_r =
  check_no_storage_write env;
  let path_key_r = gen_state_path_key env state_ref_r in
  let false_r = alloc_reg env in
  emit env (Contract_vm.LDI (false_r, VBool false));
  gen_dynamic_store_optional_string env "state_policy:" path_key_r ":delivery_key_id" delivery_key_r;
  gen_dynamic_store env "state_policy:" path_key_r ":activate_after_epoch" activate_after_r;
  gen_dynamic_store env "state_policy:" path_key_r ":expire_after_epoch" expire_after_r;
  gen_dynamic_store env "state_policy:" path_key_r ":tombstone" false_r;
  gen_dynamic_store env "state_policy:" path_key_r ":revoked" false_r;
  emit env (Contract_vm.LDI (rd, VBool true))

let gen_state_release env rd state_ref_r =
  check_no_storage_write env;
  let path_key_r = gen_state_path_key env state_ref_r in
  let false_r = alloc_reg env in
  emit env (Contract_vm.LDI (false_r, VBool false));
  gen_dynamic_delete env "state_policy:" path_key_r ":activate_after_epoch";
  gen_dynamic_delete env "state_policy:" path_key_r ":expire_after_epoch";
  gen_dynamic_store env "state_policy:" path_key_r ":tombstone" false_r;
  gen_dynamic_store env "state_policy:" path_key_r ":revoked" false_r;
  emit env (Contract_vm.LDI (rd, VBool true))

let gen_state_retire env rd state_ref_r expire_after_r =
  check_no_storage_write env;
  let path_key_r = gen_state_path_key env state_ref_r in
  gen_dynamic_store env "state_policy:" path_key_r ":expire_after_epoch" expire_after_r;
  emit env (Contract_vm.LDI (rd, VBool true))

let gen_state_tombstone_apply env rd state_ref_r =
  check_no_storage_write env;
  let path_key_r = gen_state_path_key env state_ref_r in
  let true_r = alloc_reg env in
  emit env (Contract_vm.LDI (true_r, VBool true));
  gen_dynamic_store env "state_policy:" path_key_r ":tombstone" true_r;
  emit env (Contract_vm.LDI (rd, VBool true))

let gen_state_restore env rd state_ref_r =
  check_no_storage_write env;
  let path_key_r = gen_state_path_key env state_ref_r in
  let false_r = alloc_reg env in
  emit env (Contract_vm.LDI (false_r, VBool false));
  gen_dynamic_store env "state_policy:" path_key_r ":tombstone" false_r;
  emit env (Contract_vm.LDI (rd, VBool true))

let gen_state_revoke_apply env rd state_ref_r =
  check_no_storage_write env;
  let path_key_r = gen_state_path_key env state_ref_r in
  let true_r = alloc_reg env in
  emit env (Contract_vm.LDI (true_r, VBool true));
  gen_dynamic_store env "state_policy:" path_key_r ":revoked" true_r;
  emit env (Contract_vm.LDI (rd, VBool true))

let gen_state_reinstate env rd state_ref_r =
  check_no_storage_write env;
  let path_key_r = gen_state_path_key env state_ref_r in
  let false_r = alloc_reg env in
  emit env (Contract_vm.LDI (false_r, VBool false));
  gen_dynamic_store env "state_policy:" path_key_r ":revoked" false_r;
  emit env (Contract_vm.LDI (rd, VBool true))

let gen_balance_cell_materialize env rd state_ref_r ciphertext_commitment_r
    amount_commitment_r proof_kind_r proof_receipt_hash_r =
  check_no_storage_write env;
  let path_key_r = gen_state_path_key env state_ref_r in
  gen_dynamic_store env "balance_cell:" path_key_r ":ciphertext_commitment" ciphertext_commitment_r;
  gen_dynamic_store env "balance_cell:" path_key_r ":amount_commitment" amount_commitment_r;
  gen_dynamic_store env "balance_cell:" path_key_r ":proof_kind" proof_kind_r;
  gen_dynamic_store_optional_string env "balance_cell:" path_key_r ":proof_receipt_hash" proof_receipt_hash_r;
  emit env (Contract_vm.LDI (rd, VBool true))

let gen_register_cell_materialize env rd state_ref_r ciphertext_commitment_r
    proof_kind_r proof_receipt_hash_r =
  check_no_storage_write env;
  let path_key_r = gen_state_path_key env state_ref_r in
  gen_dynamic_store env "register_cell:" path_key_r ":ciphertext_commitment" ciphertext_commitment_r;
  gen_dynamic_store env "register_cell:" path_key_r ":proof_kind" proof_kind_r;
  gen_dynamic_store_optional_string env "register_cell:" path_key_r ":proof_receipt_hash" proof_receipt_hash_r;
  emit env (Contract_vm.LDI (rd, VBool true))

let gen_object_bind env rd object_ref_r state_ref_r transition_ref_r status_r =
  check_no_storage_write env;
  let version_r =
    gen_dynamic_int_load env "object_binding:" object_ref_r ":version" in
  let one_r = alloc_reg env in
  emit env (Contract_vm.LDI (one_r, VInt Z.one));
  emit env (Contract_vm.ADD (version_r, version_r, one_r));
  gen_dynamic_store env "object_binding:" object_ref_r ":current_state_ref" state_ref_r;
  gen_dynamic_store env "object_binding:" object_ref_r ":version" version_r;
  gen_dynamic_store env "object_binding:" object_ref_r ":status" status_r;
  gen_dynamic_store env "object_binding:" object_ref_r ":last_transition_ref" transition_ref_r;
  emit env (Contract_vm.MOV (rd, version_r))

let gen_object_member_attach env rd object_ref_r member_ref_r state_ref_r member_kind_r
    state_class_r codec_r status_r =
  check_no_storage_write env;
  let member_key_r = gen_joined_key env object_ref_r member_ref_r in
  gen_dynamic_store env "object_member:" member_key_r ":state_ref" state_ref_r;
  gen_dynamic_store env "object_member:" member_key_r ":member_kind" member_kind_r;
  gen_dynamic_store env "object_member:" member_key_r ":state_class" state_class_r;
  gen_dynamic_store env "object_member:" member_key_r ":codec" codec_r;
  gen_dynamic_store env "object_member:" member_key_r ":status" status_r;
  emit env (Contract_vm.LDI (rd, VBool true))

let gen_object_member_detach env rd object_ref_r member_ref_r =
  check_no_storage_write env;
  let member_key_r = gen_joined_key env object_ref_r member_ref_r in
  gen_dynamic_delete env "object_member:" member_key_r ":state_ref";
  gen_dynamic_delete env "object_member:" member_key_r ":member_kind";
  gen_dynamic_delete env "object_member:" member_key_r ":state_class";
  gen_dynamic_delete env "object_member:" member_key_r ":codec";
  gen_dynamic_delete env "object_member:" member_key_r ":status";
  emit env (Contract_vm.LDI (rd, VBool true))

let gen_object_transition_record env rd transition_ref_r object_ref_r previous_state_ref_r
    next_state_ref_r touched_members_hash_r proof_kind_r proof_receipt_hash_r
    status_r intent_id_r =
  check_no_storage_write env;
  gen_dynamic_store env "object_transition:" transition_ref_r ":object_ref" object_ref_r;
  gen_dynamic_store env "object_transition:" transition_ref_r ":previous_state_ref" previous_state_ref_r;
  gen_dynamic_store env "object_transition:" transition_ref_r ":next_state_ref" next_state_ref_r;
  gen_dynamic_store env "object_transition:" transition_ref_r ":touched_members_hash" touched_members_hash_r;
  gen_dynamic_store env "object_transition:" transition_ref_r ":proof_kind" proof_kind_r;
  gen_dynamic_store_optional_string env "object_transition:" transition_ref_r ":proof_receipt_hash" proof_receipt_hash_r;
  gen_dynamic_store env "object_transition:" transition_ref_r ":status" status_r;
  gen_dynamic_store env "object_transition:" transition_ref_r ":intent_id" intent_id_r;
  emit env (Contract_vm.LDI (rd, VBool true))

let gen_object_policy_define env rd object_ref_r delivery_key_r activate_after_r
    expire_after_r transition_mode_r required_proof_kind_r member_quorum_r
    allow_detach_r allow_root_state_rotation_r =
  check_no_storage_write env;
  let false_r = alloc_reg env in
  emit env (Contract_vm.LDI (false_r, VBool false));
  gen_dynamic_store_optional_string env "object_policy:" object_ref_r ":delivery_key_id" delivery_key_r;
  gen_dynamic_store env "object_policy:" object_ref_r ":activate_after_epoch" activate_after_r;
  gen_dynamic_store env "object_policy:" object_ref_r ":expire_after_epoch" expire_after_r;
  gen_dynamic_store env "object_policy:" object_ref_r ":transition_mode" transition_mode_r;
  gen_dynamic_store env "object_policy:" object_ref_r ":required_proof_kind" required_proof_kind_r;
  gen_dynamic_store env "object_policy:" object_ref_r ":member_quorum" member_quorum_r;
  gen_dynamic_store env "object_policy:" object_ref_r ":allow_detach" allow_detach_r;
  gen_dynamic_store env "object_policy:" object_ref_r ":allow_root_state_rotation" allow_root_state_rotation_r;
  gen_dynamic_store env "object_policy:" object_ref_r ":tombstone" false_r;
  gen_dynamic_store env "object_policy:" object_ref_r ":revoked" false_r;
  emit env (Contract_vm.LDI (rd, VBool true))

let gen_object_policy_release env rd object_ref_r =
  check_no_storage_write env;
  let false_r = alloc_reg env in
  emit env (Contract_vm.LDI (false_r, VBool false));
  gen_dynamic_delete env "object_policy:" object_ref_r ":delivery_key_id";
  gen_dynamic_delete env "object_policy:" object_ref_r ":activate_after_epoch";
  gen_dynamic_delete env "object_policy:" object_ref_r ":expire_after_epoch";
  gen_dynamic_store env "object_policy:" object_ref_r ":tombstone" false_r;
  gen_dynamic_store env "object_policy:" object_ref_r ":revoked" false_r;
  emit env (Contract_vm.LDI (rd, VBool true))

let gen_object_policy_retire env rd object_ref_r expire_after_r =
  check_no_storage_write env;
  gen_dynamic_store env "object_policy:" object_ref_r ":expire_after_epoch" expire_after_r;
  emit env (Contract_vm.LDI (rd, VBool true))

let gen_object_policy_tombstone env rd object_ref_r =
  check_no_storage_write env;
  let true_r = alloc_reg env in
  emit env (Contract_vm.LDI (true_r, VBool true));
  gen_dynamic_store env "object_policy:" object_ref_r ":tombstone" true_r;
  emit env (Contract_vm.LDI (rd, VBool true))

let gen_object_policy_restore env rd object_ref_r =
  check_no_storage_write env;
  let false_r = alloc_reg env in
  emit env (Contract_vm.LDI (false_r, VBool false));
  gen_dynamic_store env "object_policy:" object_ref_r ":tombstone" false_r;
  emit env (Contract_vm.LDI (rd, VBool true))

let gen_object_policy_revoke env rd object_ref_r =
  check_no_storage_write env;
  let true_r = alloc_reg env in
  emit env (Contract_vm.LDI (true_r, VBool true));
  gen_dynamic_store env "object_policy:" object_ref_r ":revoked" true_r;
  emit env (Contract_vm.LDI (rd, VBool true))

let gen_object_policy_reinstate env rd object_ref_r =
  check_no_storage_write env;
  let false_r = alloc_reg env in
  emit env (Contract_vm.LDI (false_r, VBool false));
  gen_dynamic_store env "object_policy:" object_ref_r ":revoked" false_r;
  emit env (Contract_vm.LDI (rd, VBool true))

let gen_object_transition_apply env rd transition_ref_r object_ref_r previous_state_ref_r
    next_state_ref_r member_bundle_r touched_members_hash_r proof_kind_r
    proof_receipt_hash_r status_r intent_id_r =
  check_no_storage_write env;
  emit env
    (Contract_vm.OBJECT_TRANSITION_APPLY
       ( rd,
         transition_ref_r,
         object_ref_r,
         previous_state_ref_r,
         next_state_ref_r,
         member_bundle_r,
         touched_members_hash_r,
         proof_kind_r,
         proof_receipt_hash_r,
         status_r,
         intent_id_r ))

let rec typ_of_expr env = function
  | EInt _ -> TInt
  | EBool _ -> TBool
  | EString _ -> TString
  | ECaller | EOrigin | ESelfAddr -> TAddress
  | EEpoch | EEpochTime | EValue -> TInt
  | EBalance _ -> TInt
  | ETreeHash | ENodeId | ETxHash -> TString
  | EVar name ->
    (match find_const env name with
     | Some c -> c.c_typ
     | None ->
       (match find_local env name with
        | Some (_, _, t) -> t
        | None -> gerr env.line (Printf.sprintf "undefined variable: %s" name)))
  | EField name ->
    (match find_state env name with
     | Some sf -> sf.sf_typ
     | None -> gerr env.line (Printf.sprintf "undefined field: %s" name))
  | EIndex (name, _keys) ->
    (match find_state env name with
     | Some sf -> map_value_type sf.sf_typ
     | None -> gerr env.line (Printf.sprintf "undefined field: %s" name))
  | EBinop (op, a, b) ->
    (match op with
     | Add ->
       let ta = typ_of_expr env a in
       let tb = typ_of_expr env b in
       if ta = TString || tb = TString || ta = TAddress || tb = TAddress then TString
       else if ta = TU256 || tb = TU256 then TU256
       else if ta = TU128 || tb = TU128 then TU128
       else if ta = TU64 || tb = TU64 then TU64
       else TInt
     | Sub | Mul | Div | Mod ->
       let ta = typ_of_expr env a and tb = typ_of_expr env b in
       if ta = TU256 || tb = TU256 then TU256
       else if ta = TU128 || tb = TU128 then TU128
       else if ta = TU64 || tb = TU64 then TU64
       else TInt
     | Eq | Neq | Lt | Gt | Le | Ge | And | Or -> TBool)
  | EUnop (Neg, _) -> TInt
  | EUnop (Not, _) -> TBool
  | ECall (name, call_args) ->
    (match name with
     | "concat" | "to_string" | "fhe_ser" | "fhe_ser_pk"
     | "substr" | "sha256" | "keccak256"
     | "digest_sha256" | "digest_keccak256" | "current_tx_hash"
     | "blob_store" | "blob_load" | "join" | "replace" -> TString
     | "len" | "index_of" | "bit_and" | "bit_or" | "bit_xor"
     | "bit_shl" | "bit_shr" -> TInt
     | "fhe_load_pk" | "fhe_deser_pk" -> TPubKey
     | "fhe_add" | "fhe_sub" | "fhe_mul" | "fhe_scale" | "fhe_div_const"
     | "fhe_add_const" | "fhe_sub_const" | "fhe_deser" -> TCipher
     | "fhe_verify_zero" | "fhe_verify_range" | "fhe_verify_bound"
     | "groth16_verify_bn254"
     | "is_address" | "assert_address" | "starts_with" | "is_hex"
     | "ed25519_ok" | "sig_ok_ed25519"
     | "is_some_opt" -> TBool
     | "fhe_commit" | "fhe_pedersen" -> TBytes
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
     | "min" | "max" | "abs" | "to_int" | "parse_ints" | "mget" | "pow"
     | "vecdot" | "vecdot_fp" | "vecdot_q16" | "argmax_fp" | "argmax_q16" | "exp_lut" | "exp_q16"
     | "unwrap" | "split" -> TInt
     | "some" -> (match call_args with [e] -> typ_of_expr env e | _ -> TInt)
     | "none" -> TString
     | "transfer" | "checkpoint" | "rollback" | "commit" | "mset"
     | "matmul" | "softmax" | "softmax_q16" | "layernorm" | "layernorm_q16"
     | "relu" | "rmsnorm" | "rmsnorm_q16" | "silu" | "silu_q16" | "elemwise_mul"
     | "load_int8" | "load_int8_b64" | "residual_add" | "rope_apply" | "rope_apply_q16"
     | "matmul_q16" | "shift_round"
     | "matmul_fp" | "rmsnorm_fp" | "silu_fp" | "elemwise_mul_fp"
     | "residual_add_fp" | "rope_apply_fp" | "load_int8_fp"
     | "attention_kv_fp" | "attention_kv_q16" | "append_vec_fp"
     | "load_int8_q16" | "append_vec_q16" -> TBool
     | "call" -> TString
     | "deploy" | "circle_spawn" -> TAddress
     | _ ->
       (match List.find_opt (fun f -> f.fn_name = name) env.funcs with
        | Some f -> f.fn_ret
        | None -> gerr env.line (Printf.sprintf "unknown function: %s" name)))
  | EStoragePath (field, keys, path) ->
    (match resolve_storage_path_type env field keys path with
     | Some t -> t
     | None -> gerr env.line (Printf.sprintf "unknown storage path: %s" (storage_path_to_string field path)))
  | EFieldProp (field, prop) ->
    (match resolve_storage_path_type env field [] [prop] with
     | Some t -> t
     | None -> gerr env.line (Printf.sprintf "unknown storage path: %s" (storage_path_to_string field [prop])))
  | EIndexField (field, _keys, sf) ->
    (match resolve_storage_path_type env field _keys [sf] with
     | Some t -> t
     | None -> gerr env.line (Printf.sprintf "unknown storage path: %s" (storage_path_to_string field [sf])))
  | EEnumVariant _ -> TInt
  | EArray _ | ETuple _ -> TString
  | ETernary (_, then_e, _) -> typ_of_expr env then_e

let gen_some_key_field name = name ^ "_some"

let gen_some_key_index env field_name key_regs =
  let kr = gen_storage_key env field_name key_regs in
  let suffix = alloc_reg env in
  emit env (Contract_vm.LDI (suffix, VString "_some"));
  emit env (Contract_vm.CONCAT (kr, kr, suffix));
  kr

let rec gen_is_some env args =
  match args with
  | [EField name] ->
    let rd = alloc_reg env in
    let r = alloc_reg env in
    emit env (Contract_vm.SLOAD (r, gen_some_key_field name));
    let tr = alloc_reg env in
    emit env (Contract_vm.LDI (tr, VString "true"));
    emit env (Contract_vm.EQ (rd, r, tr));
    rd
  | [EIndex (name, keys)] ->
    let key_regs = List.map (gen_expr env) keys in
    let kr = gen_some_key_index env name key_regs in
    let rd = alloc_reg env in
    emit env (Contract_vm.SLOADK (rd, kr));
    let tr = alloc_reg env in
    emit env (Contract_vm.LDI (tr, VString "true"));
    emit env (Contract_vm.EQ (rd, rd, tr));
    rd
  | _ -> gerr env.line "is_some: argument must be self.field or self.map[key]"

and gen_unwrap env args =
  match args with
  | [EField name] ->
    let some_r = alloc_reg env in
    emit env (Contract_vm.SLOAD (some_r, gen_some_key_field name));
    let tr = alloc_reg env in
    emit env (Contract_vm.LDI (tr, VString "true"));
    let cmp = alloc_reg env in
    emit env (Contract_vm.EQ (cmp, some_r, tr));
    emit env (Contract_vm.ASSERT cmp);
    let rd = alloc_reg env in
    emit env (Contract_vm.SLOAD (rd, storage_key_for_field name));
    (match find_state env name with
     | Some sf ->
       let inner_t = (match sf.sf_typ with TOption t -> t | t -> t) in
       if is_int_storage env inner_t then gen_int_from_storage env rd
       else rd
     | None -> rd)
  | [EIndex (name, keys)] ->
    let key_regs = List.map (gen_expr env) keys in
    let some_kr = gen_some_key_index env name key_regs in
    let some_r = alloc_reg env in
    emit env (Contract_vm.SLOADK (some_r, some_kr));
    let tr = alloc_reg env in
    emit env (Contract_vm.LDI (tr, VString "true"));
    let cmp = alloc_reg env in
    emit env (Contract_vm.EQ (cmp, some_r, tr));
    emit env (Contract_vm.ASSERT cmp);
    let val_key_regs = List.map (gen_expr env) keys in
    let val_kr = gen_storage_key env name val_key_regs in
    let rd = alloc_reg env in
    emit env (Contract_vm.SLOADK (rd, val_kr));
    (match find_state env name with
     | Some sf ->
       let inner_t = (match map_value_type sf.sf_typ with TOption t -> t | t -> t) in
       if is_int_storage env inner_t then gen_int_from_storage env rd
       else rd
     | None -> rd)
  | _ -> gerr env.line "unwrap: argument must be self.field or self.map[key]"

and emit_split_builtin env rd str_r delim_r =
  let count_r = alloc_reg env in
  emit env (Contract_vm.LDI (count_r, VInt Z.zero));
  let remaining_r = alloc_reg env in
  emit env (Contract_vm.MOV (remaining_r, str_r));
  let base_slot = alloc_reg env in
  emit env (Contract_vm.LDI (base_slot, VInt (Z.of_int 100)));
  let test_l = alloc_label env in
  let loop_l = alloc_label env in
  let done_l = alloc_label env in
  let delim_len = alloc_reg env in
  emit env (Contract_vm.STRLEN (delim_len, delim_r));
  emit env (Contract_vm.JDEST test_l);
  let pos = alloc_reg env in
  emit env (Contract_vm.INDEXOF (pos, remaining_r, delim_r));
  let neg1 = alloc_reg env in
  emit env (Contract_vm.LDI (neg1, VInt (Z.of_int (-1))));
  let found = alloc_reg env in
  emit env (Contract_vm.NEQ (found, pos, neg1));
  emit env (Contract_vm.JIF (found, loop_l));
  let slot_r = alloc_reg env in
  emit env (Contract_vm.ADD (slot_r, base_slot, count_r));
  emit env (Contract_vm.MSTORER (slot_r, remaining_r));
  let one2 = alloc_reg env in
  emit env (Contract_vm.LDI (one2, VInt Z.one));
  emit env (Contract_vm.ADD (count_r, count_r, one2));
  emit env (Contract_vm.JMP done_l);
  emit env (Contract_vm.JDEST loop_l);
  let part_r = alloc_reg env in
  let zero2 = alloc_reg env in
  emit env (Contract_vm.LDI (zero2, VInt Z.zero));
  emit env (Contract_vm.SUBSTR (part_r, remaining_r, zero2, pos));
  let slot_r2 = alloc_reg env in
  emit env (Contract_vm.ADD (slot_r2, base_slot, count_r));
  emit env (Contract_vm.MSTORER (slot_r2, part_r));
  let one3 = alloc_reg env in
  emit env (Contract_vm.LDI (one3, VInt Z.one));
  emit env (Contract_vm.ADD (count_r, count_r, one3));
  let next_start = alloc_reg env in
  emit env (Contract_vm.ADD (next_start, pos, delim_len));
  let total_len = alloc_reg env in
  emit env (Contract_vm.STRLEN (total_len, remaining_r));
  let rem_len = alloc_reg env in
  emit env (Contract_vm.SUB (rem_len, total_len, next_start));
  emit env (Contract_vm.SUBSTR (remaining_r, remaining_r, next_start, rem_len));
  emit env (Contract_vm.JMP test_l);
  emit env (Contract_vm.JDEST done_l);
  emit env (Contract_vm.MOV (rd, count_r))

and emit_join_builtin env rd n_r delim_r =
  emit env (Contract_vm.LDI (rd, VString ""));
  let i_r = alloc_reg env in
  emit env (Contract_vm.LDI (i_r, VInt Z.zero));
  let base_slot = alloc_reg env in
  emit env (Contract_vm.LDI (base_slot, VInt (Z.of_int 100)));
  let test_l = alloc_label env in
  let loop_l = alloc_label env in
  emit env (Contract_vm.JMP test_l);
  emit env (Contract_vm.JDEST loop_l);
  let zero3 = alloc_reg env in
  emit env (Contract_vm.LDI (zero3, VInt Z.zero));
  let is_first = alloc_reg env in
  emit env (Contract_vm.EQ (is_first, i_r, zero3));
  let skip_delim = alloc_label env in
  emit env (Contract_vm.JIF (is_first, skip_delim));
  emit env (Contract_vm.CONCAT (rd, rd, delim_r));
  emit env (Contract_vm.JDEST skip_delim);
  let slot_r = alloc_reg env in
  emit env (Contract_vm.ADD (slot_r, base_slot, i_r));
  let part_r = alloc_reg env in
  emit env (Contract_vm.MLOADR (part_r, slot_r));
  emit env (Contract_vm.CONCAT (rd, rd, part_r));
  let one4 = alloc_reg env in
  emit env (Contract_vm.LDI (one4, VInt Z.one));
  emit env (Contract_vm.ADD (i_r, i_r, one4));
  emit env (Contract_vm.JDEST test_l);
  let cmp = alloc_reg env in
  emit env (Contract_vm.LT (cmp, i_r, n_r));
  emit env (Contract_vm.JIF (cmp, loop_l))

and emit_replace_builtin env rd str_r old_r new_r =
  let pos_r = alloc_reg env in
  emit env (Contract_vm.INDEXOF (pos_r, str_r, old_r));
  let neg1 = alloc_reg env in
  emit env (Contract_vm.LDI (neg1, VInt (Z.of_int (-1))));
  let found = alloc_reg env in
  emit env (Contract_vm.NEQ (found, pos_r, neg1));
  let do_replace = alloc_label env in
  let done_l = alloc_label env in
  emit env (Contract_vm.JIF (found, do_replace));
  emit env (Contract_vm.MOV (rd, str_r));
  emit env (Contract_vm.JMP done_l);
  emit env (Contract_vm.JDEST do_replace);
  let zero_r = alloc_reg env in
  emit env (Contract_vm.LDI (zero_r, VInt Z.zero));
  let before_r = alloc_reg env in
  emit env (Contract_vm.SUBSTR (before_r, str_r, zero_r, pos_r));
  let old_len = alloc_reg env in
  emit env (Contract_vm.STRLEN (old_len, old_r));
  let after_start = alloc_reg env in
  emit env (Contract_vm.ADD (after_start, pos_r, old_len));
  let total_len = alloc_reg env in
  emit env (Contract_vm.STRLEN (total_len, str_r));
  let after_len = alloc_reg env in
  emit env (Contract_vm.SUB (after_len, total_len, after_start));
  let after_r = alloc_reg env in
  emit env (Contract_vm.SUBSTR (after_r, str_r, after_start, after_len));
  emit env (Contract_vm.CONCAT (rd, before_r, new_r));
  emit env (Contract_vm.CONCAT (rd, rd, after_r));
  emit env (Contract_vm.JDEST done_l)

and emit_replace_all_builtin env rd str_r old_r new_r =
  emit env (Contract_vm.MOV (rd, str_r));
  let loop_l = alloc_label env in
  let unused_replace_label = alloc_label env in
  let done_l = alloc_label env in
  emit env (Contract_vm.JDEST loop_l);
  let pos_r = alloc_reg env in
  emit env (Contract_vm.INDEXOF (pos_r, rd, old_r));
  let neg1 = alloc_reg env in
  emit env (Contract_vm.LDI (neg1, VInt (Z.of_int (-1))));
  let not_found = alloc_reg env in
  emit env (Contract_vm.EQ (not_found, pos_r, neg1));
  emit env (Contract_vm.JIF (not_found, done_l));
  let zero_r = alloc_reg env in
  emit env (Contract_vm.LDI (zero_r, VInt Z.zero));
  let before_r = alloc_reg env in
  emit env (Contract_vm.SUBSTR (before_r, rd, zero_r, pos_r));
  let old_len = alloc_reg env in
  emit env (Contract_vm.STRLEN (old_len, old_r));
  let after_start = alloc_reg env in
  emit env (Contract_vm.ADD (after_start, pos_r, old_len));
  let total_len = alloc_reg env in
  emit env (Contract_vm.STRLEN (total_len, rd));
  let after_len = alloc_reg env in
  emit env (Contract_vm.SUB (after_len, total_len, after_start));
  let after_r = alloc_reg env in
  emit env (Contract_vm.SUBSTR (after_r, rd, after_start, after_len));
  emit env (Contract_vm.CONCAT (rd, before_r, new_r));
  emit env (Contract_vm.CONCAT (rd, rd, after_r));
  emit env (Contract_vm.JMP loop_l);
  ignore unused_replace_label;
  emit env (Contract_vm.JDEST done_l)

and gen_builtin env name args =
  match name with
  | "some" ->
    (match args with
     | [e] -> gen_expr env e
     | _ -> gerr env.line "Some: need exactly 1 argument")
  | "none" ->
    let rd = alloc_reg env in
    emit env (Contract_vm.LDI (rd, VString ""));
    rd
  | "is_some_opt" -> gen_is_some env args
  | "unwrap" -> gen_unwrap env args
  | "call" when (match args with
                 | [_; _; (EArray _ | ETuple _)] -> true
                 | _ -> false) ->
    let unpacked = match args with
      | [t; m; EArray elems] -> t :: m :: elems
      | [t; m; ETuple elems] -> t :: m :: elems
      | _ -> args
    in
    gen_builtin env name unpacked
  | _ ->
  let arg_regs = List.map (gen_expr env) args in
  let nargs = List.length arg_regs in
  let nth n =
    if n < nargs then List.nth arg_regs n
    else gerr env.line (Printf.sprintf "%s: missing argument %d" name (n + 1))
  in
  let rd = alloc_reg env in
  (match name with
   | "concat" -> emit env (Contract_vm.CONCAT (rd, nth 0, nth 1))
   | "to_string" ->
     emit env (Contract_vm.LDI (rd, VString ""));
     emit env (Contract_vm.CONCAT (rd, rd, nth 0))
   | "len" -> emit env (Contract_vm.STRLEN (rd, nth 0))
   | "fhe_load_pk" -> emit env (Contract_vm.FHE_LOAD_PK (rd, nth 0))
   | "fhe_add" -> emit env (Contract_vm.FHE_ADD (rd, nth 0, nth 1, nth 2))
   | "fhe_sub" -> emit env (Contract_vm.FHE_SUB (rd, nth 0, nth 1, nth 2))
   | "fhe_mul" -> emit env (Contract_vm.FHE_MUL (rd, nth 0, nth 1, nth 2))
   | "fhe_scale" -> emit env (Contract_vm.FHE_SCALE (rd, nth 0, nth 1, nth 2))
   | "fhe_div_const" -> emit env (Contract_vm.FHE_DIV_CONST (rd, nth 0, nth 1, nth 2))
   | "fhe_add_const" -> emit env (Contract_vm.FHE_ADD_CONST (rd, nth 0, nth 1, nth 2))
   | "fhe_sub_const" -> emit env (Contract_vm.FHE_SUB_CONST (rd, nth 0, nth 1, nth 2))
   | "fhe_verify_zero" -> emit env (Contract_vm.FHE_VERIFY_ZERO (rd, nth 0, nth 1, nth 2))
   | "fhe_verify_range" -> emit env (Contract_vm.FHE_VERIFY_RANGE (rd, nth 0, nth 1, nth 2))
   | "groth16_verify_bn254" -> emit env (Contract_vm.GROTH16_VERIFY_BN254 (rd, nth 0, nth 1, nth 2))
   | "fhe_verify_bound" -> emit env (Contract_vm.FHE_VERIFY_BOUND (rd, nth 0, nth 1, nth 2, nth 3))
   | "fhe_commit" -> emit env (Contract_vm.FHE_COMMIT (rd, nth 0, nth 1))
   | "fhe_pedersen" -> emit env (Contract_vm.FHE_PEDERSEN (rd, nth 0, nth 1))
   | "fhe_ser" -> emit env (Contract_vm.FHE_SER (rd, nth 0))
   | "fhe_deser" -> emit env (Contract_vm.FHE_DESER (rd, nth 0))
   | "fhe_ser_pk" -> emit env (Contract_vm.FHE_SER_PK (rd, nth 0))
   | "fhe_deser_pk" -> emit env (Contract_vm.FHE_DESER_PK (rd, nth 0))
   | "circle_balance_state_ref" ->
     emit env (Contract_vm.MOV (rd, gen_balance_binding_load env "current_state_ref" (nth 0)))
   | "circle_balance_version" ->
     emit env (Contract_vm.MOV (rd, gen_balance_binding_load env "version" (nth 0)))
   | "circle_balance_status" ->
     emit env (Contract_vm.MOV (rd, gen_balance_binding_load env "status" (nth 0)))
   | "circle_balance_last_workflow" ->
     emit env (Contract_vm.MOV (rd, gen_balance_binding_load env "last_workflow_ref" (nth 0)))
   | "circle_register_state_ref" ->
     emit env (Contract_vm.MOV (rd, gen_register_binding_load env "current_state_ref" (nth 0)))
   | "circle_register_version" ->
     emit env (Contract_vm.MOV (rd, gen_register_binding_load env "version" (nth 0)))
   | "circle_register_status" ->
     emit env (Contract_vm.MOV (rd, gen_register_binding_load env "status" (nth 0)))
   | "circle_register_last_workflow" ->
     emit env (Contract_vm.MOV (rd, gen_register_binding_load env "last_workflow_ref" (nth 0)))
   | "circle_object_state_ref" ->
     emit env (Contract_vm.MOV (rd, gen_object_binding_load env "current_state_ref" (nth 0)))
   | "circle_object_version" ->
     emit env (Contract_vm.MOV (rd, gen_object_binding_load env "version" (nth 0)))
   | "circle_object_status" ->
     emit env (Contract_vm.MOV (rd, gen_object_binding_load env "status" (nth 0)))
   | "circle_object_last_transition" ->
     emit env (Contract_vm.MOV (rd, gen_object_binding_load env "last_transition_ref" (nth 0)))
   | "circle_object_member_count" ->
     emit env (Contract_vm.OBJECT_MEMBER_COUNT (rd, nth 0))
   | "circle_object_member_ref_at" ->
     emit env (Contract_vm.OBJECT_MEMBER_REF_AT (rd, nth 0, nth 1))
   | "circle_object_member_state_ref" ->
     emit env (Contract_vm.MOV (rd, gen_object_member_load env "state_ref" (nth 0) (nth 1)))
   | "circle_object_member_kind" ->
     emit env (Contract_vm.MOV (rd, gen_object_member_load env "member_kind" (nth 0) (nth 1)))
   | "circle_object_member_class" ->
     emit env (Contract_vm.MOV (rd, gen_object_member_load env "state_class" (nth 0) (nth 1)))
   | "circle_object_member_codec" ->
     emit env (Contract_vm.MOV (rd, gen_object_member_load env "codec" (nth 0) (nth 1)))
   | "circle_object_member_status" ->
     emit env (Contract_vm.MOV (rd, gen_object_member_load env "status" (nth 0) (nth 1)))
   | "circle_object_delivery_key_id" ->
     emit env (Contract_vm.MOV (rd, gen_object_policy_load env "delivery_key_id" (nth 0)))
   | "circle_object_activate_after" ->
     emit env (Contract_vm.MOV (rd, gen_object_policy_load env "activate_after_epoch" (nth 0)))
   | "circle_object_expire_after" ->
     emit env (Contract_vm.MOV (rd, gen_object_policy_load env "expire_after_epoch" (nth 0)))
   | "circle_object_tombstone" ->
     emit env (Contract_vm.MOV (rd, gen_object_policy_load env "tombstone" (nth 0)))
   | "circle_object_revoked" ->
     emit env (Contract_vm.MOV (rd, gen_object_policy_load env "revoked" (nth 0)))
   | "circle_object_has_member" ->
     emit env (Contract_vm.OBJECT_HAS_MEMBER (rd, nth 0, nth 1))
   | "circle_object_transition_mode" ->
     emit env (Contract_vm.MOV (rd, gen_object_policy_load env "transition_mode" (nth 0)))
   | "circle_object_required_proof_kind" ->
     emit env (Contract_vm.MOV (rd, gen_object_policy_load env "required_proof_kind" (nth 0)))
   | "circle_object_member_quorum" ->
     emit env (Contract_vm.MOV (rd, gen_object_policy_load env "member_quorum" (nth 0)))
   | "circle_object_allow_detach" ->
     emit env (Contract_vm.MOV (rd, gen_object_policy_load env "allow_detach" (nth 0)))
   | "circle_object_allow_root_state_rotation" ->
     emit env (Contract_vm.MOV (rd, gen_object_policy_load env "allow_root_state_rotation" (nth 0)))
   | "circle_state_class" ->
     emit env (Contract_vm.MOV (rd, gen_state_descriptor_load env "state_class" (nth 0)))
   | "circle_state_codec" ->
     emit env (Contract_vm.MOV (rd, gen_state_descriptor_load env "codec" (nth 0)))
   | "circle_state_schema_hash" ->
     emit env (Contract_vm.MOV (rd, gen_state_descriptor_load env "schema_hash" (nth 0)))
   | "circle_state_subject_addr" ->
     emit env (Contract_vm.MOV (rd, gen_state_descriptor_load env "subject_addr" (nth 0)))
   | "circle_state_hfhe_profile" ->
     emit env (Contract_vm.MOV (rd, gen_state_descriptor_load env "hfhe_profile" (nth 0)))
   | "circle_state_mutable" ->
     emit env (Contract_vm.MOV (rd, gen_state_descriptor_load env "mutable_state" (nth 0)))
   | "circle_state_delivery_key_id" ->
     emit env (Contract_vm.MOV (rd, gen_state_policy_load env "delivery_key_id" (nth 0)))
   | "circle_state_activate_after" ->
     emit env (Contract_vm.MOV (rd, gen_state_policy_load env "activate_after_epoch" (nth 0)))
   | "circle_state_expire_after" ->
     emit env (Contract_vm.MOV (rd, gen_state_policy_load env "expire_after_epoch" (nth 0)))
   | "circle_state_tombstone" ->
     emit env (Contract_vm.MOV (rd, gen_state_policy_load env "tombstone" (nth 0)))
   | "circle_state_revoked" ->
     emit env (Contract_vm.MOV (rd, gen_state_policy_load env "revoked" (nth 0)))
   | "circle_balance_cell_ciphertext_commitment" ->
     emit env (Contract_vm.MOV (rd, gen_balance_cell_load env "ciphertext_commitment" (nth 0)))
   | "circle_balance_cell_amount_commitment" ->
     emit env (Contract_vm.MOV (rd, gen_balance_cell_load env "amount_commitment" (nth 0)))
   | "circle_balance_cell_proof_kind" ->
     emit env (Contract_vm.MOV (rd, gen_balance_cell_load env "proof_kind" (nth 0)))
   | "circle_balance_cell_proof_receipt_hash" ->
     emit env (Contract_vm.MOV (rd, gen_balance_cell_load env "proof_receipt_hash" (nth 0)))
   | "circle_register_cell_ciphertext_commitment" ->
     emit env (Contract_vm.MOV (rd, gen_register_cell_load env "ciphertext_commitment" (nth 0)))
   | "circle_register_cell_proof_kind" ->
     emit env (Contract_vm.MOV (rd, gen_register_cell_load env "proof_kind" (nth 0)))
   | "circle_register_cell_proof_receipt_hash" ->
     emit env (Contract_vm.MOV (rd, gen_register_cell_load env "proof_receipt_hash" (nth 0)))
   | "circle_state_describe" ->
     gen_state_describe env rd
       (nth 0) (nth 1) (nth 2) (nth 3) (nth 4) (nth 5) (nth 6)
   | "circle_state_publish" ->
     gen_state_publish env rd
       (nth 0) (nth 1) (nth 2) (nth 3)
   | "circle_state_release" ->
     gen_state_release env rd (nth 0)
   | "circle_state_retire" ->
     gen_state_retire env rd (nth 0) (nth 1)
   | "circle_state_tombstone_apply" ->
     gen_state_tombstone_apply env rd (nth 0)
   | "circle_state_restore" ->
     gen_state_restore env rd (nth 0)
   | "circle_state_revoke_apply" ->
     gen_state_revoke_apply env rd (nth 0)
   | "circle_state_reinstate" ->
     gen_state_reinstate env rd (nth 0)
   | "circle_balance_cell_materialize" ->
     gen_balance_cell_materialize env rd
       (nth 0) (nth 1) (nth 2) (nth 3) (nth 4)
   | "circle_register_cell_materialize" ->
     gen_register_cell_materialize env rd
       (nth 0) (nth 1) (nth 2) (nth 3)
   | "circle_object_bind" ->
     gen_object_bind env rd
       (nth 0) (nth 1) (nth 2) (nth 3)
   | "circle_object_member_attach" ->
     gen_object_member_attach env rd
       (nth 0) (nth 1) (nth 2) (nth 3) (nth 4) (nth 5) (nth 6)
   | "circle_object_member_detach" ->
     gen_object_member_detach env rd
       (nth 0) (nth 1)
   | "circle_object_transition_record" ->
     gen_object_transition_record env rd
       (nth 0) (nth 1) (nth 2) (nth 3) (nth 4) (nth 5) (nth 6) (nth 7) (nth 8)
   | "circle_object_policy_define" ->
     gen_object_policy_define env rd
       (nth 0) (nth 1) (nth 2) (nth 3) (nth 4) (nth 5) (nth 6) (nth 7) (nth 8)
   | "circle_object_policy_release" ->
     gen_object_policy_release env rd (nth 0)
   | "circle_object_policy_retire" ->
     gen_object_policy_retire env rd (nth 0) (nth 1)
   | "circle_object_tombstone_apply" ->
     gen_object_policy_tombstone env rd (nth 0)
   | "circle_object_restore" ->
     gen_object_policy_restore env rd (nth 0)
   | "circle_object_revoke_apply" ->
     gen_object_policy_revoke env rd (nth 0)
   | "circle_object_reinstate" ->
     gen_object_policy_reinstate env rd (nth 0)
   | "circle_object_transition_apply" ->
     gen_object_transition_apply env rd
       (nth 0) (nth 1) (nth 2) (nth 3) (nth 4) (nth 5) (nth 6) (nth 7) (nth 8) (nth 9)
   | "circle_balance_bind" ->
     gen_balance_binding_bind env rd (nth 0) (nth 1) (nth 2) (nth 3)
   | "circle_register_bind" ->
     gen_register_binding_bind env rd (nth 0) (nth 1) (nth 2) (nth 3)
   | "circle_balance_workflow_record" ->
     gen_balance_workflow_record env rd
       (nth 0) (nth 1) (nth 2) (nth 3) (nth 4) (nth 5) (nth 6) (nth 7) (nth 8) (nth 9) (nth 10)
   | "circle_register_workflow_record" ->
     gen_register_workflow_record env rd
       (nth 0) (nth 1) (nth 2) (nth 3) (nth 4) (nth 5) (nth 6) (nth 7) (nth 8)
   | "min" ->
     let ra = nth 0 in
     let rb = nth 1 in
     let cmp = alloc_reg env in
     let use_a = alloc_label env in
     let end_l = alloc_label env in
     emit env (Contract_vm.LT (cmp, ra, rb));
     emit env (Contract_vm.JIF (cmp, use_a));
     emit env (Contract_vm.MOV (rd, rb));
     emit env (Contract_vm.JMP end_l);
     emit env (Contract_vm.JDEST use_a);
     emit env (Contract_vm.MOV (rd, ra));
     emit env (Contract_vm.JDEST end_l)
   | "max" ->
     let ra = nth 0 in
     let rb = nth 1 in
     let cmp = alloc_reg env in
     let use_a = alloc_label env in
     let end_l = alloc_label env in
     emit env (Contract_vm.GT (cmp, ra, rb));
     emit env (Contract_vm.JIF (cmp, use_a));
     emit env (Contract_vm.MOV (rd, rb));
     emit env (Contract_vm.JMP end_l);
     emit env (Contract_vm.JDEST use_a);
     emit env (Contract_vm.MOV (rd, ra));
     emit env (Contract_vm.JDEST end_l)
   | "abs" ->
     let rx = nth 0 in
     let zero = alloc_reg env in
     let cmp = alloc_reg env in
     let neg_l = alloc_label env in
     let end_l = alloc_label env in
     emit env (Contract_vm.LDI (zero, VInt Z.zero));
     emit env (Contract_vm.LT (cmp, rx, zero));
     emit env (Contract_vm.JIF (cmp, neg_l));
     emit env (Contract_vm.MOV (rd, rx));
     emit env (Contract_vm.JMP end_l);
     emit env (Contract_vm.JDEST neg_l);
     emit env (Contract_vm.NEG (rd, rx));
     emit env (Contract_vm.JDEST end_l)
   | "is_address" -> emit env (Contract_vm.ISADDR (rd, nth 0))
   | "is_hex" -> emit env (Contract_vm.ISHEX (rd, nth 0))
   | "assert_address" ->
     emit env (Contract_vm.ASSERT_ADDR (nth 0));
     emit env (Contract_vm.LDI (rd, VBool true))
   | "transfer" -> check_no_transfer env; emit env (Contract_vm.TRANSFER (rd, nth 0, nth 1))
   | "to_int" ->
     let zero = alloc_reg env in
     emit env (Contract_vm.LDI (zero, VInt Z.zero));
     emit env (Contract_vm.ADD (rd, zero, nth 0))
   | "checkpoint" ->
     emit env Contract_vm.CHECKPOINT;
     emit env (Contract_vm.LDI (rd, VBool true))
   | "rollback" ->
     emit env Contract_vm.ROLLBACK;
     emit env (Contract_vm.LDI (rd, VBool true))
   | "commit" ->
     emit env Contract_vm.COMMIT;
     emit env (Contract_vm.LDI (rd, VBool true))
   | "call" ->
     if nargs < 2 then gerr env.line "call: need at least (target, method)";
     let target_r = nth 0 in
     let method_r = nth 1 in
     let call_nargs = nargs - 2 in
     if call_nargs = 0 then
       emit env (Contract_vm.XCALL (rd, target_r, method_r, 0, 0))
     else begin
       let base = alloc_reg env in
       for _ = 1 to call_nargs - 1 do ignore (alloc_reg env) done;
       for i = 0 to call_nargs - 1 do
         let src = nth (i + 2) in
         if src <> base + i then emit env (Contract_vm.MOV (base + i, src))
       done;
       emit env (Contract_vm.XCALL (rd, target_r, method_r, base, call_nargs))
     end
   | "deploy" ->
     if nargs < 1 then gerr env.line "deploy: need at least (bytecode)";
     if nargs = 1 then
       emit env (Contract_vm.SPAWN (rd, nth 0))
     else begin
       let base = nth 1 in
       emit env (Contract_vm.SPAWN2 (rd, nth 0, base, nargs - 1))
     end
   | "circle_spawn" ->
     if nargs <> 2 then
       gerr env.line "circle_spawn: need (payload_json, owner_mode)";
     emit env (Contract_vm.SPAWN2 (rd, nth 0, nth 1, 1))
   | "mget" -> emit env (Contract_vm.MLOADR (rd, nth 0))
   | "mset" ->
     emit env (Contract_vm.MSTORER (nth 0, nth 1));
     emit env (Contract_vm.LDI (rd, VBool true))
   | "parse_ints" -> emit env (Contract_vm.PARSE_INTS (rd, nth 0, nth 1))
   | "substr" -> emit env (Contract_vm.SUBSTR (rd, nth 0, nth 1, nth 2))
   | "index_of" -> emit env (Contract_vm.INDEXOF (rd, nth 0, nth 1))
   | "sha256" | "digest_sha256" -> emit env (Contract_vm.SHA256 (rd, nth 0))
   | "keccak256" | "digest_keccak256" -> emit env (Contract_vm.KECCAK256 (rd, nth 0))
   | "current_tx_hash" -> emit env (Contract_vm.TXHASH rd)
   | "ed25519_ok" | "sig_ok_ed25519" ->
     emit env (Contract_vm.ED25519_OK (rd, nth 0, nth 1, nth 2))
   | "bit_and" -> emit env (Contract_vm.BITAND (rd, nth 0, nth 1))
   | "bit_or" -> emit env (Contract_vm.BITOR (rd, nth 0, nth 1))
   | "bit_xor" -> emit env (Contract_vm.BITXOR (rd, nth 0, nth 1))
   | "bit_shl" -> emit env (Contract_vm.BITSHL (rd, nth 0, nth 1))
   | "bit_shr" -> emit env (Contract_vm.BITSHR (rd, nth 0, nth 1))
   | "matmul" ->
     emit env (Contract_vm.MATMUL (nth 0, nth 1, nth 2, nth 3, nth 4, nth 5));
     emit env (Contract_vm.LDI (rd, VBool true))
   | "vecdot" -> emit env (Contract_vm.VECDOT (rd, nth 0, nth 1, nth 2))
   | "vecdot_q16" ->
     if env.declaration <> ProgramDecl then
       gerr env.line "vecdot_q16 is available only in Program"
     else
       emit env (Contract_vm.VECDOT_Q16 (rd, nth 0, nth 1, nth 2))
   | "exp_lut" -> emit env (Contract_vm.EXP_LUT (rd, nth 0))
   | "exp_q16" ->
     if env.declaration <> ProgramDecl then
       gerr env.line "exp_q16 is available only in Program"
     else
       emit env (Contract_vm.EXP_Q16 (rd, nth 0))
   | "softmax" ->
     emit env (Contract_vm.SOFTMAX_INPLACE (nth 0, nth 1));
     emit env (Contract_vm.LDI (rd, VBool true))
   | "softmax_q16" ->
     if env.declaration <> ProgramDecl then
       gerr env.line "softmax_q16 is available only in Program"
     else begin
       emit env (Contract_vm.SOFTMAX_Q16_INPLACE (nth 0, nth 1));
       emit env (Contract_vm.LDI (rd, VBool true))
     end
   | "layernorm" ->
     emit env (Contract_vm.LAYERNORM_INPLACE (nth 0, nth 1, nth 2, nth 3));
     emit env (Contract_vm.LDI (rd, VBool true))
   | "layernorm_q16" ->
     if env.declaration <> ProgramDecl then
       gerr env.line "layernorm_q16 is available only in Program"
     else begin
       emit env (Contract_vm.LAYERNORM_Q16_INPLACE (nth 0, nth 1, nth 2, nth 3));
       emit env (Contract_vm.LDI (rd, VBool true))
     end
   | "relu" ->
     emit env (Contract_vm.RELU_INPLACE (nth 0, nth 1));
     emit env (Contract_vm.LDI (rd, VBool true))
   | "rmsnorm" ->
     emit env (Contract_vm.RMSNORM_INPLACE (nth 0, nth 1, nth 2));
     emit env (Contract_vm.LDI (rd, VBool true))
   | "rmsnorm_q16" ->
     if env.declaration <> ProgramDecl then
       gerr env.line "rmsnorm_q16 is available only in Program"
     else begin
       emit env (Contract_vm.RMSNORM_Q16_INPLACE (nth 0, nth 1, nth 2));
       emit env (Contract_vm.LDI (rd, VBool true))
     end
   | "silu_q16" ->
     if env.declaration <> ProgramDecl then
       gerr env.line "silu_q16 is available only in Program"
     else begin
       emit env (Contract_vm.SILU_Q16_INPLACE (nth 0, nth 1));
       emit env (Contract_vm.LDI (rd, VBool true))
     end
   | "silu" ->
     emit env (Contract_vm.SILU_INPLACE (nth 0, nth 1));
     emit env (Contract_vm.LDI (rd, VBool true))
   | "elemwise_mul" ->
     emit env (Contract_vm.ELEMWISE_MUL_INPLACE (nth 0, nth 1, nth 2));
     emit env (Contract_vm.LDI (rd, VBool true))
   | "elemwise_mul_q16" ->
     if env.declaration <> ProgramDecl then
       gerr env.line "elemwise_mul_q16 is available only in Program"
     else begin
       emit env (Contract_vm.ELEMWISE_MUL_Q16 (nth 0, nth 1, nth 2));
       emit env (Contract_vm.LDI (rd, VBool true))
     end
   | "load_int8" ->
     emit env (Contract_vm.LOAD_INT8_BYTES_TO_MEM (nth 0, nth 1, nth 2, nth 3, nth 4));
     emit env (Contract_vm.LDI (rd, VBool true))
   | "residual_add" ->
     emit env (Contract_vm.RESIDUAL_ADD (nth 0, nth 1, nth 2));
     emit env (Contract_vm.LDI (rd, VBool true))
   | "residual_add_q16" ->
     if env.declaration <> ProgramDecl then
       gerr env.line "residual_add_q16 is available only in Program"
     else begin
       emit env (Contract_vm.RESIDUAL_ADD_Q16 (nth 0, nth 1, nth 2));
       emit env (Contract_vm.LDI (rd, VBool true))
     end
   | "rope_apply" ->
     emit env (Contract_vm.ROPE_APPLY (nth 0, nth 1, nth 2, nth 3));
     emit env (Contract_vm.LDI (rd, VBool true))
   | "rope_apply_q16" ->
     if env.declaration <> ProgramDecl then
       gerr env.line "rope_apply_q16 is available only in Program"
     else begin
       emit env (Contract_vm.ROPE_APPLY_Q16 (nth 0, nth 1, nth 2, nth 3));
       emit env (Contract_vm.LDI (rd, VBool true))
     end
   | "load_int8_b64" ->
     emit env (Contract_vm.LOAD_INT8_B64_TO_MEM (nth 0, nth 1, nth 2, nth 3, nth 4));
     emit env (Contract_vm.LDI (rd, VBool true))
   | "load_int8_q16" ->
     if env.declaration <> ProgramDecl then
       gerr env.line "load_int8_q16 is available only in Program"
     else begin
       emit env (Contract_vm.LOAD_INT8_Q16 (nth 0, nth 1, nth 2, nth 3, nth 4));
       emit env (Contract_vm.LDI (rd, VBool true))
     end
   | "append_vec_q16" ->
     if env.declaration <> ProgramDecl then
       gerr env.line "append_vec_q16 is available only in Program"
     else begin
       emit env (Contract_vm.APPEND_VEC_Q16 (nth 0, nth 1, nth 2, nth 3));
       emit env (Contract_vm.LDI (rd, VBool true))
     end
   | "argmax_q16" ->
     if env.declaration <> ProgramDecl then
       gerr env.line "argmax_q16 is available only in Program"
     else
       emit env (Contract_vm.ARGMAX_Q16 (rd, nth 0, nth 1))
   | "matmul_q16" ->
     emit env (Contract_vm.MATMUL_Q16 (nth 0, nth 1, nth 2, nth 3, nth 4, nth 5));
     emit env (Contract_vm.LDI (rd, VBool true))
   | "shift_round" ->
     emit env (Contract_vm.SHIFT_ROUND_INPLACE (nth 0, nth 1, nth 2));
     emit env (Contract_vm.LDI (rd, VBool true))
   | "matmul_fp" ->
     emit env (Contract_vm.MATMUL_FP (nth 0, nth 1, nth 2, nth 3, nth 4, nth 5));
     emit env (Contract_vm.LDI (rd, VBool true))
   | "rmsnorm_fp" ->
     emit env (Contract_vm.RMSNORM_FP (nth 0, nth 1, nth 2));
     emit env (Contract_vm.LDI (rd, VBool true))
   | "silu_fp" ->
     emit env (Contract_vm.SILU_FP (nth 0, nth 1));
     emit env (Contract_vm.LDI (rd, VBool true))
   | "elemwise_mul_fp" ->
     emit env (Contract_vm.ELEMWISE_MUL_FP (nth 0, nth 1, nth 2));
     emit env (Contract_vm.LDI (rd, VBool true))
   | "residual_add_fp" ->
     emit env (Contract_vm.RESIDUAL_ADD_FP (nth 0, nth 1, nth 2));
     emit env (Contract_vm.LDI (rd, VBool true))
   | "rope_apply_fp" ->
     emit env (Contract_vm.ROPE_APPLY_FP (nth 0, nth 1, nth 2, nth 3));
     emit env (Contract_vm.LDI (rd, VBool true))
   | "load_int8_fp" ->
     emit env (Contract_vm.LOAD_INT8_FP (nth 0, nth 1, nth 2, nth 3, nth 4));
     emit env (Contract_vm.LDI (rd, VBool true))
   | "vecdot_fp" ->
     emit env (Contract_vm.VECDOT_FP (rd, nth 0, nth 1, nth 2))
   | "argmax_fp" ->
     emit env (Contract_vm.ARGMAX_FP (rd, nth 0, nth 1))
   | "attention_kv_fp" ->
     emit env (Contract_vm.ATTENTION_KV_FP (nth 0, nth 1, nth 2, nth 3, nth 4, nth 5, nth 6, nth 7));
     emit env (Contract_vm.LDI (rd, VBool true))
   | "attention_kv_q16" ->
     if env.declaration <> ProgramDecl then
       gerr env.line "attention_kv_q16 is available only in Program"
     else begin
       emit env (Contract_vm.ATTENTION_KV_Q16 (nth 0, nth 1, nth 2, nth 3, nth 4, nth 5, nth 6, nth 7));
       emit env (Contract_vm.LDI (rd, VBool true))
     end
   | "append_vec_fp" ->
     emit env (Contract_vm.APPEND_VEC_FP (nth 0, nth 1, nth 2, nth 3));
     emit env (Contract_vm.LDI (rd, VBool true))
   | "blob_store" -> emit env (Contract_vm.FSTORE (rd, nth 0))
   | "blob_load" -> emit env (Contract_vm.FLOAD (rd, nth 0))
   | "starts_with" ->
     let idx_r = alloc_reg env in
     emit env (Contract_vm.INDEXOF (idx_r, nth 0, nth 1));
     let zero = alloc_reg env in
     emit env (Contract_vm.LDI (zero, VInt Z.zero));
     emit env (Contract_vm.EQ (rd, idx_r, zero))
   | "split" -> emit_split_builtin env rd (nth 0) (nth 1)
   | "join" -> emit_join_builtin env rd (nth 0) (nth 1)
   | "replace" -> emit_replace_builtin env rd (nth 0) (nth 1) (nth 2)
   | "replace_all" -> emit_replace_all_builtin env rd (nth 0) (nth 1) (nth 2)
   | "pow" ->
     let base_r = nth 0 in
     let exp_r = nth 1 in
     emit env (Contract_vm.LDI (rd, VInt Z.one));
     let i_r = alloc_reg env in
     emit env (Contract_vm.LDI (i_r, VInt Z.zero));
     let test_l = alloc_label env in
     let loop_l = alloc_label env in
     emit env (Contract_vm.JMP test_l);
     emit env (Contract_vm.JDEST loop_l);
     emit env (Contract_vm.MUL (rd, rd, base_r));
     let one = alloc_reg env in
     emit env (Contract_vm.LDI (one, VInt Z.one));
     emit env (Contract_vm.ADD (i_r, i_r, one));
     emit env (Contract_vm.JDEST test_l);
     let cmp = alloc_reg env in
     emit env (Contract_vm.LT (cmp, i_r, exp_r));
     emit env (Contract_vm.JIF (cmp, loop_l))
   | _ ->
     match Hashtbl.find_opt env.func_labels name with
     | Some label ->
       let target_func = List.find (fun f -> f.fn_name = name) env.funcs in
       List.iteri (fun i _ ->
         let src = if i < nargs then nth i else begin
           let z = alloc_reg env in
           emit env (Contract_vm.LDI (z, VInt Z.zero)); z end in
       emit env (Contract_vm.MSTORE (1001 + i, src))
      ) target_func.fn_params;
      emit env (Contract_vm.CALL_INT (rd, label))
     | None -> gerr env.line (Printf.sprintf "unknown function: %s" name));
  rd

and gen_storage_path_key env field keys path =
  let key_regs = List.map (gen_expr env) keys in
  match resolve_storage_length_prefix env field keys path with
  | Some prefix ->
    let kr = gen_storage_path_prefix_key env field key_regs prefix in
    let suffix = alloc_reg env in
    emit env (Contract_vm.LDI (suffix, VString "_len"));
    emit env (Contract_vm.CONCAT (kr, kr, suffix));
    kr
  | None ->
    gen_storage_path_prefix_key env field key_regs path

and gen_storage_path_read env field keys path =
  let typ =
    match resolve_storage_path_type env field keys path with
    | Some t -> t
    | None ->
      gerr env.line (Printf.sprintf "unknown storage path: %s" (storage_path_to_string field path))
  in
  let kr = gen_storage_path_key env field keys path in
  let r = alloc_reg env in
  emit env (Contract_vm.SLOADK (r, kr));
  gen_storage_loaded_value env r typ

and gen_storage_value_for_write env typ expr =
  let r = gen_expr env expr in
  if typ = TBool then begin
    let sr = alloc_reg env in
    let lbl_true = alloc_label env in
    let lbl_end = alloc_label env in
    emit env (Contract_vm.JIF (r, lbl_true));
    emit env (Contract_vm.LDI (sr, VString "false"));
    emit env (Contract_vm.JMP lbl_end);
    emit env (Contract_vm.JDEST lbl_true);
    emit env (Contract_vm.LDI (sr, VString "true"));
    emit env (Contract_vm.JDEST lbl_end);
    sr
  end else begin
    emit_type_check env r typ;
    r
  end

and gen_storage_path_write env field keys path expr =
  let length_prefix = resolve_storage_length_prefix env field keys path in
  if length_prefix <> None then
    gerr env.line (Printf.sprintf "cannot assign to storage path: %s" (storage_path_to_string field path));
  let typ =
    match resolve_storage_path_type env field keys path with
    | Some t -> t
    | None ->
      gerr env.line (Printf.sprintf "unknown storage path: %s" (storage_path_to_string field path))
  in
  let kr = gen_storage_path_key env field keys path in
  let sr = gen_storage_value_for_write env typ expr in
  emit env (Contract_vm.SSTOREK (kr, sr))

and gen_field_prop env field prop =
  gen_storage_path_read env field [] [prop]

and emit_netstring_pack_element env packed_r expr =
  let er = gen_expr env expr in
  let str_r = alloc_reg env in
  emit env (Contract_vm.LDI (str_r, VString ""));
  emit env (Contract_vm.CONCAT (str_r, str_r, er));
  let len_r = alloc_reg env in
  emit env (Contract_vm.STRLEN (len_r, str_r));
  let hash_r = alloc_reg env in
  emit env (Contract_vm.LDI (hash_r, VString "#"));
  emit env (Contract_vm.CONCAT (packed_r, packed_r, len_r));
  emit env (Contract_vm.CONCAT (packed_r, packed_r, hash_r));
  emit env (Contract_vm.CONCAT (packed_r, packed_r, str_r))

and gen_netstring_pack env elems =
  let packed_r = alloc_reg env in
  emit env (Contract_vm.LDI (packed_r, VString ""));
  List.iter (emit_netstring_pack_element env packed_r) elems;
  packed_r

and gen_expr env expr =
  match expr with
  | EInt z ->
    let r = alloc_reg env in
    emit env (Contract_vm.LDI (r, VInt z)); r
  | EBool b ->
    let r = alloc_reg env in
    emit env (Contract_vm.LDI (r, VBool b)); r
  | EString s ->
    let r = alloc_reg env in
    emit env (Contract_vm.LDI (r, VString s)); r
  | ECaller ->
    let r = alloc_reg env in
    emit env (Contract_vm.CALLER r); r
  | EOrigin ->
    let r = alloc_reg env in
    emit env (Contract_vm.ORIGIN r); r
  | ESelfAddr ->
    let r = alloc_reg env in
    emit env (Contract_vm.SELF r); r
  | EEpoch ->
    let r = alloc_reg env in
    emit env (Contract_vm.EPOCH r); r
  | EEpochTime ->
    if env.declaration <> ProgramDecl then
      gerr env.line "epoch_time is available only in Program"
    else
      let r = alloc_reg env in
      emit env (Contract_vm.EPOCH_TIME r); r
  | EValue ->
    let r = alloc_reg env in
    emit env (Contract_vm.VALUE r); r
  | ETreeHash ->
    let r = alloc_reg env in
    emit env (Contract_vm.TREEHASH r); r
  | ENodeId ->
    let r = alloc_reg env in
    emit env (Contract_vm.NODEID r); r
  | ETxHash ->
    let r = alloc_reg env in
    emit env (Contract_vm.TXHASH r); r
  | EBalance e ->
    let ra = gen_expr env e in
    let r = alloc_reg env in
    emit env (Contract_vm.BALANCE (r, ra)); r
  | EVar name ->
    (match find_const env name with
     | Some c -> gen_expr env c.c_value
     | None ->
       (match find_local env name with
        | Some (_, reg, _) -> reg
        | None -> gerr env.line (Printf.sprintf "undefined variable: %s" name)))
  | EField name ->
    (match find_state env name with
     | Some sf ->
       let r = alloc_reg env in
       emit env (Contract_vm.SLOAD (r, storage_key_for_field name));
       if typed_static_storage env sf.sf_typ then r
       else if is_int_storage env sf.sf_typ then gen_int_from_storage env r
       else if sf.sf_typ = TBool then begin
         let tr = alloc_reg env in
         emit env (Contract_vm.LDI (tr, VString "true"));
         emit env (Contract_vm.EQ (r, r, tr));
         r
       end else r
     | None -> gerr env.line (Printf.sprintf "undefined field: %s" name))
  | EIndex (name, keys) ->
    (match find_state env name with
     | Some sf ->
       let key_regs = List.map (gen_expr env) keys in
       let kr = gen_storage_key env name key_regs in
       let r = alloc_reg env in
       emit env (Contract_vm.SLOADK (r, kr));
       let vt = map_value_type sf.sf_typ in
       if is_int_storage env vt then gen_int_from_storage env r
       else if vt = TBool then begin
         let tr = alloc_reg env in
         emit env (Contract_vm.LDI (tr, VString "true"));
         emit env (Contract_vm.EQ (r, r, tr));
         r
       end else r
     | None -> gerr env.line (Printf.sprintf "undefined field: %s" name))
  | EArray elems | ETuple elems -> gen_netstring_pack env elems
  | ECall (name, args) -> gen_builtin env name args
  | EStoragePath (field, keys, path) -> gen_storage_path_read env field keys path
  | EFieldProp (field, prop) -> gen_field_prop env field prop
  | EIndexField (field, keys, sf) -> gen_storage_path_read env field keys [sf]
  | EEnumVariant (enum_name, variant) ->
    let idx = resolve_enum_variant env enum_name variant in
    let r = alloc_reg env in
    emit env (Contract_vm.LDI (r, VInt (Z.of_int idx))); r
  | ETernary (cond, then_e, else_e) ->
    let rc = gen_expr env cond in
    let result = alloc_reg env in
    let then_label = alloc_label env in
    let end_label = alloc_label env in
    emit env (Contract_vm.JIF (rc, then_label));
    let re = gen_expr env else_e in
    emit env (Contract_vm.MOV (result, re));
    emit env (Contract_vm.JMP end_label);
    emit env (Contract_vm.JDEST then_label);
    let rt = gen_expr env then_e in
    emit env (Contract_vm.MOV (result, rt));
    emit env (Contract_vm.JDEST end_label);
    result
  | EBinop (And, l, r_expr) -> gen_short_circuit_and env l r_expr
  | EBinop (Or, l, r_expr) -> gen_short_circuit_or env l r_expr
  | EBinop (op, l, r_expr) as whole ->
    let rl = gen_expr env l in
    let rr = gen_expr env r_expr in
    let rd = alloc_reg env in
    (match op with
     | Add ->
       let ta = typ_of_expr env l in
       let tb = typ_of_expr env r_expr in
       if ta = TString || tb = TString || ta = TAddress || tb = TAddress then
         emit env (Contract_vm.CONCAT (rd, rl, rr))
       else
         emit env (Contract_vm.ADD (rd, rl, rr))
     | Sub -> emit env (Contract_vm.SUB (rd, rl, rr))
     | Mul -> emit env (Contract_vm.MUL (rd, rl, rr))
     | Div -> emit env (Contract_vm.DIV (rd, rl, rr))
     | Mod -> emit env (Contract_vm.MOD (rd, rl, rr))
     | Eq -> emit env (Contract_vm.EQ (rd, rl, rr))
     | Neq -> emit env (Contract_vm.NEQ (rd, rl, rr))
     | Lt -> emit env (Contract_vm.LT (rd, rl, rr))
     | Gt -> emit env (Contract_vm.GT (rd, rl, rr))
     | Le ->
       emit env (Contract_vm.GT (rd, rl, rr));
       let tr = alloc_reg env in
       emit env (Contract_vm.LDI (tr, VBool true));
       emit env (Contract_vm.NEQ (rd, rd, tr))
     | Ge ->
       emit env (Contract_vm.LT (rd, rl, rr));
       let tr = alloc_reg env in
       emit env (Contract_vm.LDI (tr, VBool true));
       emit env (Contract_vm.NEQ (rd, rd, tr))
     | And | Or -> assert false);
    emit_result_type_check env rd (typ_of_expr env whole);
    rd
  | EUnop (Neg, e) ->
    let r = gen_expr env e in
    let rd = alloc_reg env in
    emit env (Contract_vm.NEG (rd, r)); rd
  | EUnop (Not, e) ->
    let r = gen_expr env e in
    let rd = alloc_reg env in
    emit env (Contract_vm.LDI (rd, VBool true));
    emit env (Contract_vm.NEQ (rd, r, rd)); rd

and gen_short_circuit_and env l r_expr =
  let rl = gen_expr env l in
  let result = alloc_reg env in
  let false_label = alloc_label env in
  let end_label = alloc_label env in
  emit env (Contract_vm.LDI (result, VBool true));
  emit env (Contract_vm.NEQ (result, rl, result));
  emit env (Contract_vm.JIF (result, false_label));
  let rr = gen_expr env r_expr in
  emit env (Contract_vm.MOV (result, rr));
  emit env (Contract_vm.JMP end_label);
  emit env (Contract_vm.JDEST false_label);
  emit env (Contract_vm.LDI (result, VBool false));
  emit env (Contract_vm.JDEST end_label);
  result

and gen_short_circuit_or env l r_expr =
  let rl = gen_expr env l in
  let result = alloc_reg env in
  let true_label = alloc_label env in
  let end_label = alloc_label env in
  emit env (Contract_vm.JIF (rl, true_label));
  let rr = gen_expr env r_expr in
  emit env (Contract_vm.MOV (result, rr));
  emit env (Contract_vm.JMP end_label);
  emit env (Contract_vm.JDEST true_label);
  emit env (Contract_vm.LDI (result, VBool true));
  emit env (Contract_vm.JDEST end_label);
  result

and reserve_local_slot env =
  let r = env.base_reg in
  if r > 63 then gerr env.line "too many local variables (max 63 registers)";
  env.base_reg <- r + 1;
  env.next_reg <- env.base_reg;
  r

and tuple_element_types env expr names =
  match typ_of_expr env expr with
  | TTuple ts -> ts
  | _ -> List.map (fun _ -> TString) names

and next_tuple_type types_ref =
  match !types_ref with
  | t :: rest ->
    types_ref := rest;
    t
  | [] -> TString

and emit_netstring_unpack_next env ~hash_r ~remaining_r ~local_r =
  let hash_pos = alloc_reg env in
  emit env (Contract_vm.INDEXOF (hash_pos, remaining_r, hash_r));
  let zero_r = alloc_reg env in
  emit env (Contract_vm.LDI (zero_r, VInt Z.zero));
  let len_str = alloc_reg env in
  emit env (Contract_vm.SUBSTR (len_str, remaining_r, zero_r, hash_pos));
  let elem_len = alloc_reg env in
  emit env (Contract_vm.ADD (elem_len, zero_r, len_str));
  let one_r = alloc_reg env in
  emit env (Contract_vm.LDI (one_r, VInt Z.one));
  let payload_start = alloc_reg env in
  emit env (Contract_vm.ADD (payload_start, hash_pos, one_r));
  emit env (Contract_vm.SUBSTR (local_r, remaining_r, payload_start, elem_len));
  let next_start = alloc_reg env in
  emit env (Contract_vm.ADD (next_start, payload_start, elem_len));
  let total_len = alloc_reg env in
  emit env (Contract_vm.STRLEN (total_len, remaining_r));
  let rem_len = alloc_reg env in
  emit env (Contract_vm.SUB (rem_len, total_len, next_start));
  emit env (Contract_vm.SUBSTR (remaining_r, remaining_r, next_start, rem_len))

and bind_tuple_element env ~hash_r ~remaining_r ~types_ref name =
  let elem_t = next_tuple_type types_ref in
  let local_r = reserve_local_slot env in
  emit_netstring_unpack_next env ~hash_r ~remaining_r ~local_r;
  env.locals <- (name, local_r, elem_t) :: env.locals

and gen_tuple_unpack env names expr =
  let elem_types = tuple_element_types env expr names in
  let hash_r = reserve_local_slot env in
  let remaining_r = reserve_local_slot env in
  let src_r = gen_expr env expr in
  emit env (Contract_vm.LDI (hash_r, VString "#"));
  emit env (Contract_vm.MOV (remaining_r, src_r));
  let types_ref = ref elem_types in
  List.iter (bind_tuple_element env ~hash_r ~remaining_r ~types_ref) names

and emit_list_push_method env field args =
  let len_r = alloc_reg env in
  emit env (Contract_vm.SLOAD (len_r, field ^ "_len"));
  ignore (gen_int_from_storage env len_r);
  let val_r = gen_expr env (List.hd args) in
  let kr = alloc_reg env in
  emit env (Contract_vm.LDI (kr, VString (field ^ ":")));
  emit env (Contract_vm.CONCAT (kr, kr, len_r));
  emit env (Contract_vm.SSTOREK (kr, val_r));
  let one = alloc_reg env in
  emit env (Contract_vm.LDI (one, VInt Z.one));
  emit env (Contract_vm.ADD (len_r, len_r, one));
  emit env (Contract_vm.SSTORE (field ^ "_len", len_r))

and emit_list_delete_method env field args =
  let key_r = gen_expr env (List.hd args) in
  let kr = alloc_reg env in
  emit env (Contract_vm.LDI (kr, VString (field ^ ":")));
  emit env (Contract_vm.CONCAT (kr, kr, key_r));
  emit env (Contract_vm.SDELK kr)

and emit_list_len_method env field =
  let r = alloc_reg env in
  emit env (Contract_vm.SLOAD (r, field ^ "_len"));
  ignore (gen_int_from_storage env r);
  emit env (Contract_vm.MOV (0, r))

and emit_list_pop_method env field =
  let len_r = alloc_reg env in
  emit env (Contract_vm.SLOAD (len_r, field ^ "_len"));
  ignore (gen_int_from_storage env len_r);
  let one = alloc_reg env in
  emit env (Contract_vm.LDI (one, VInt Z.one));
  emit env (Contract_vm.SUB (len_r, len_r, one));
  let kr = alloc_reg env in
  emit env (Contract_vm.LDI (kr, VString (field ^ ":")));
  emit env (Contract_vm.CONCAT (kr, kr, len_r));
  let val_r = alloc_reg env in
  emit env (Contract_vm.SLOADK (val_r, kr));
  emit env (Contract_vm.SDELK kr);
  emit env (Contract_vm.SSTORE (field ^ "_len", len_r));
  emit env (Contract_vm.MOV (0, val_r))

let is_pseudo_event name =
  name = "Log" || name = "Require"

let guard_pure_while env =
  if env.fn_is_pure then
    gerr env.line "pure function cannot use while loops (use for..in with bounded range)"

let reserve_foreach_slot env =
  let r = env.base_reg in
  if r > 63 then gerr env.line "too many local variables";
  env.base_reg <- r + 1;
  env.next_reg <- env.base_reg;
  r

let emit_foreach_item_load env field iter_r item_r =
  let kr = alloc_reg env in
  emit env (Contract_vm.LDI (kr, VString (field ^ ":")));
  emit env (Contract_vm.CONCAT (kr, kr, iter_r));
  emit env (Contract_vm.SLOADK (item_r, kr))

let check_match_exhaustive env arms =
  match arms with
  | (enum_name, _, _) :: _ ->
    (match find_enum env enum_name with
     | Some ed ->
       let covered = List.map (fun (_, v, _) -> v) arms in
       let missing = List.filter (fun v -> not (List.mem v covered)) ed.en_variants in
       if missing <> [] then
         gerr env.line (Printf.sprintf "non-exhaustive match on %s: missing %s"
           enum_name (String.concat ", " missing))
     | None -> ())
  | [] -> gerr env.line "empty match expression"

let rec gen_foreach_loop env var_name field body =
  let len_r = reserve_foreach_slot env in
  let iter_r = reserve_foreach_slot env in
  emit env (Contract_vm.SLOAD (len_r, field ^ "_len"));
  ignore (gen_int_from_storage env len_r);
  emit env (Contract_vm.LDI (iter_r, VInt Z.zero));
  let saved_locals = env.locals in
  let test_label = alloc_label env in
  let loop_label = alloc_label env in
  emit env (Contract_vm.JMP test_label);
  emit env (Contract_vm.JDEST loop_label);
  let item_r = reserve_foreach_slot env in
  emit_foreach_item_load env field iter_r item_r;
  env.locals <- (var_name, item_r, TString) :: env.locals;
  List.iter (gen_stmt env) body;
  env.next_reg <- env.base_reg;
  let one = alloc_reg env in
  emit env (Contract_vm.LDI (one, VInt Z.one));
  emit env (Contract_vm.ADD (iter_r, iter_r, one));
  emit env (Contract_vm.JDEST test_label);
  env.next_reg <- env.base_reg;
  let cmp = alloc_reg env in
  emit env (Contract_vm.LT (cmp, iter_r, len_r));
  emit env (Contract_vm.JIF (cmp, loop_label));
  env.locals <- saved_locals;
  env.base_reg <- len_r;
  env.next_reg <- env.base_reg

and gen_stmt env stmt =
  env.next_reg <- env.base_reg;
  match stmt with
  | SLet (name, typ_ann, expr) ->
    let r = gen_expr env expr in
    let t = match typ_ann with
      | Some t -> t
      | None -> typ_of_expr env expr
    in
    emit_type_check env r t;
    let local_r = env.base_reg in
    if local_r > 63 then gerr env.line "too many local variables (max 63 registers)";
    if r <> local_r then emit env (Contract_vm.MOV (local_r, r));
    env.base_reg <- local_r + 1;
    env.next_reg <- env.base_reg;
    env.locals <- (name, local_r, t) :: env.locals

  | SLetTuple (names, expr) -> gen_tuple_unpack env names expr

  | SAssign (name, expr) ->
    (match find_local env name with
     | Some (_, reg, t) ->
       let r = gen_expr env expr in
       emit_type_check env r t;
       if r <> reg then emit env (Contract_vm.MOV (reg, r))
     | None -> gerr env.line (Printf.sprintf "undefined variable: %s" name))

  | SFieldSet (name, expr) ->
    check_no_storage_write env;
    (match find_state env name with
     | Some sf ->
       (match sf.sf_typ with
        | TOption _ ->
          (match expr with
           | ECall ("none", []) ->
             emit env (Contract_vm.SDEL (storage_key_for_field name));
             emit env (Contract_vm.SDEL (gen_some_key_field name))
           | ECall ("some", [e]) ->
             let r = gen_expr env e in
             emit env (Contract_vm.SSTORE (storage_key_for_field name, r));
             let tr = alloc_reg env in
             emit env (Contract_vm.LDI (tr, VString "true"));
             emit env (Contract_vm.SSTORE (gen_some_key_field name, tr))
           | _ ->
             let r = gen_expr env expr in
             emit env (Contract_vm.SSTORE (storage_key_for_field name, r));
             let tr = alloc_reg env in
             emit env (Contract_vm.LDI (tr, VString "true"));
             emit env (Contract_vm.SSTORE (gen_some_key_field name, tr)))
        | _ ->
          let r = gen_expr env expr in
          if sf.sf_typ = TBool && not (typed_static_storage env sf.sf_typ) then begin
            let sr = alloc_reg env in
            let lbl_true = alloc_label env in
            let lbl_end = alloc_label env in
            emit env (Contract_vm.JIF (r, lbl_true));
            emit env (Contract_vm.LDI (sr, VString "false"));
            emit env (Contract_vm.JMP lbl_end);
            emit env (Contract_vm.JDEST lbl_true);
            emit env (Contract_vm.LDI (sr, VString "true"));
            emit env (Contract_vm.JDEST lbl_end);
            emit env (Contract_vm.SSTORE (storage_key_for_field name, sr))
          end else begin
            emit_type_check env r sf.sf_typ;
            emit env (Contract_vm.SSTORE (storage_key_for_field name, r))
          end)
     | None -> gerr env.line (Printf.sprintf "undefined field: %s" name))

  | SIndexSet (name, keys, expr) ->
    check_no_storage_write env;
    (match find_state env name with
     | Some sf ->
       let vt = map_value_type sf.sf_typ in
       (match vt with
        | TOption _ ->
          let key_regs = List.map (gen_expr env) keys in
          (match expr with
           | ECall ("none", []) ->
             let kr = gen_storage_key env name key_regs in
             let some_kr = gen_some_key_index env name key_regs in
             emit env (Contract_vm.SDELK kr);
             emit env (Contract_vm.SDELK some_kr)
           | ECall ("some", [e]) ->
             let kr = gen_storage_key env name key_regs in
             let r = gen_expr env e in
             emit env (Contract_vm.SSTOREK (kr, r));
             let key_regs2 = List.map (gen_expr env) keys in
             let some_kr = gen_some_key_index env name key_regs2 in
             let tr = alloc_reg env in
             emit env (Contract_vm.LDI (tr, VString "true"));
             emit env (Contract_vm.SSTOREK (some_kr, tr))
           | _ ->
             let kr = gen_storage_key env name key_regs in
             let r = gen_expr env expr in
             emit env (Contract_vm.SSTOREK (kr, r));
             let key_regs2 = List.map (gen_expr env) keys in
             let some_kr = gen_some_key_index env name key_regs2 in
             let tr = alloc_reg env in
             emit env (Contract_vm.LDI (tr, VString "true"));
             emit env (Contract_vm.SSTOREK (some_kr, tr)))
        | _ ->
          let key_regs = List.map (gen_expr env) keys in
          let kr = gen_storage_key env name key_regs in
          let r = gen_expr env expr in
          emit_type_check env r vt;
          emit env (Contract_vm.SSTOREK (kr, r)))
     | None -> gerr env.line (Printf.sprintf "undefined field: %s" name))

  | SReturn (Some expr) ->
    let r = gen_expr env expr in
    if r <> 0 then emit env (Contract_vm.MOV (0, r));
    if env.in_nonreentrant then begin
      let zr = alloc_reg env in
      emit env (Contract_vm.LDI (zr, VString "0"));
      emit env (Contract_vm.SSTORE ("_lock", zr))
    end;
    emit env Contract_vm.STOP

  | SReturn None ->
    if env.in_nonreentrant then begin
      let zr = alloc_reg env in
      emit env (Contract_vm.LDI (zr, VString "0"));
      emit env (Contract_vm.SSTORE ("_lock", zr))
    end;
    emit env Contract_vm.STOP

  | SAssert expr ->
    let r = gen_expr env expr in
    emit env (Contract_vm.ASSERT r)

  | SRequire (cond, msg) ->
    let rc = gen_expr env cond in
    let ok_label = alloc_label env in
    emit env (Contract_vm.JIF (rc, ok_label));
    let rm = gen_expr env msg in
    emit env (Contract_vm.EMIT ("Require", [rm]));
    emit env Contract_vm.REVERT;
    emit env (Contract_vm.JDEST ok_label)

  | SEmit (name, args) ->
    check_no_emit env;
    if is_pseudo_event name then begin
      let regs = List.map (gen_expr env) args in
      emit env (Contract_vm.EMIT (name, regs))
    end else
    (match find_event env name with
     | Some _ev ->
       let regs = List.map (gen_expr env) args in
       emit env (Contract_vm.EMIT (name, regs))
     | None -> gerr env.line (Printf.sprintf "undefined event: %s" name))

  | SIf (cond, then_body, else_body) ->
    let rc = gen_expr env cond in
    let then_label = alloc_label env in
    let end_label = alloc_label env in
    emit env (Contract_vm.JIF (rc, then_label));
    (match else_body with
     | Some stmts -> List.iter (gen_stmt env) stmts
     | None -> ());
    emit env (Contract_vm.JMP end_label);
    emit env (Contract_vm.JDEST then_label);
    List.iter (gen_stmt env) then_body;
    emit env (Contract_vm.JDEST end_label)

  | SWhile (cond, body) ->
    guard_pure_while env;
    let test_label = alloc_label env in
    let loop_label = alloc_label env in
    emit env (Contract_vm.JMP test_label);
    emit env (Contract_vm.JDEST loop_label);
    List.iter (gen_stmt env) body;
    emit env (Contract_vm.JDEST test_label);
    env.next_reg <- env.base_reg;
    let rc = gen_expr env cond in
    emit env (Contract_vm.JIF (rc, loop_label))

  | SFor (name, start_e, end_e, body) ->
    let rs = gen_expr env start_e in
    let re = gen_expr env end_e in
    let iter_r = env.base_reg in
    if iter_r > 63 then gerr env.line "too many local variables (max 63 registers)";
    emit env (Contract_vm.MOV (iter_r, rs));
    env.base_reg <- iter_r + 1;
    env.next_reg <- env.base_reg;
    let saved_locals = env.locals in
    env.locals <- (name, iter_r, TInt) :: env.locals;
    let test_label = alloc_label env in
    let loop_label = alloc_label env in
    emit env (Contract_vm.JMP test_label);
    emit env (Contract_vm.JDEST loop_label);
    List.iter (gen_stmt env) body;
    env.next_reg <- env.base_reg;
    let one = alloc_reg env in
    emit env (Contract_vm.LDI (one, VInt Z.one));
    emit env (Contract_vm.ADD (iter_r, iter_r, one));
    emit env (Contract_vm.JDEST test_label);
    env.next_reg <- env.base_reg;
    let cmp = alloc_reg env in
    emit env (Contract_vm.LT (cmp, iter_r, re));
    emit env (Contract_vm.JIF (cmp, loop_label));
    env.locals <- saved_locals;
    env.base_reg <- iter_r;
    env.next_reg <- env.base_reg

  | SForEach (var_name, field, body) -> gen_foreach_loop env var_name field body

  | SMatch (expr, arms) ->
    check_match_exhaustive env arms;
    let rv = gen_expr env expr in
    let match_r = env.base_reg in
    if rv <> match_r then emit env (Contract_vm.MOV (match_r, rv));
    env.base_reg <- match_r + 1;
    env.next_reg <- env.base_reg;
    let end_label = alloc_label env in
    let arm_labels = List.map (fun _ -> alloc_label env) arms in
    List.iter2 (fun (enum_name, variant, _) label ->
      env.next_reg <- env.base_reg;
      let idx = resolve_enum_variant env enum_name variant in
      let vi = alloc_reg env in
      let cmp = alloc_reg env in
      emit env (Contract_vm.LDI (vi, VInt (Z.of_int idx)));
      emit env (Contract_vm.EQ (cmp, match_r, vi));
      emit env (Contract_vm.JIF (cmp, label))
    ) arms arm_labels;
    emit env Contract_vm.REVERT;
    List.iter2 (fun (_, _, body) label ->
      emit env (Contract_vm.JDEST label);
      List.iter (gen_stmt env) body;
      emit env (Contract_vm.JMP end_label)
    ) arms arm_labels;
    emit env (Contract_vm.JDEST end_label);
    env.base_reg <- match_r;
    env.next_reg <- env.base_reg

  | SStoragePathSet (field, keys, path, expr) ->
    check_no_storage_write env;
    gen_storage_path_write env field keys path expr

  | SIndexFieldSet (field, keys, sf, expr) ->
    check_no_storage_write env;
    gen_storage_path_write env field keys [sf] expr

  | SFieldCall (field, method_name, args) ->
    (match method_name with
     | "push" -> emit_list_push_method env field args
     | "delete" -> emit_list_delete_method env field args
     | "len" -> emit_list_len_method env field
     | "pop" -> emit_list_pop_method env field
     | _ -> gerr env.line (Printf.sprintf "unknown method: %s" method_name))

  | SExpr e ->
    ignore (gen_expr env e)

  | SRevertError (name, args) ->
    (match find_error env name with
     | Some err_def ->
       let code_r = alloc_reg env in
       emit env (Contract_vm.LDI (code_r, VInt (Z.of_int err_def.err_code)));
       let msg_r = alloc_reg env in
       emit env (Contract_vm.LDI (msg_r, VString err_def.err_msg));
       let arg_regs = List.map (gen_expr env) args in
       emit env (Contract_vm.EMIT ("Error:" ^ name, code_r :: msg_r :: arg_regs));
       if env.in_nonreentrant then begin
         let zr = alloc_reg env in
         emit env (Contract_vm.LDI (zr, VString "0"));
         emit env (Contract_vm.SSTORE ("_lock", zr))
       end;
       emit env Contract_vm.REVERT
     | None -> gerr env.line (Printf.sprintf "undefined error: %s" name))

let gen_constructor env (ctor : func_def) =
  let saved_locals = env.locals in
  let saved_base = env.base_reg in
  let saved_next = env.next_reg in
  env.locals <- [];
  env.base_reg <- 1;
  env.next_reg <- 1;
  let check_r = alloc_reg env in
  let ctor_str = alloc_reg env in
  let cmp_r = alloc_reg env in
  emit env (Contract_vm.MLOAD (check_r, 999));
  emit env (Contract_vm.LDI (ctor_str, VString "constructor"));
  emit env (Contract_vm.NEQ (cmp_r, check_r, ctor_str));
  emit env (Contract_vm.JIF (cmp_r, 100));
  env.base_reg <- 1;
  env.next_reg <- 1;
  List.iteri (fun i p ->
    let r = env.base_reg in
    emit env (Contract_vm.MLOAD (r, 1001 + i));
    env.locals <- (p.p_name, r, p.p_typ) :: env.locals;
    env.base_reg <- r + 1
  ) ctor.fn_params;
  env.next_reg <- env.base_reg;
  List.iter (fun p ->
    match find_local env p.p_name with
    | Some (_, reg, _) -> emit_type_check env reg p.p_typ
    | None -> ()
  ) ctor.fn_params;
  List.iter (gen_stmt env) ctor.fn_body;
  (match List.rev env.code with
   | Contract_vm.STOP :: _ -> ()
   | _ -> emit env Contract_vm.STOP);
  env.locals <- saved_locals;
  env.base_reg <- saved_base;
  env.next_reg <- saved_next

let gen_dispatcher env (funcs : func_def list) =
  env.base_reg <- 1;
  env.next_reg <- 1;
  emit env (Contract_vm.JDEST 100);
  let method_r = alloc_reg env in
  emit env (Contract_vm.MLOAD (method_r, 1000));
  let name_r = alloc_reg env in
  let cmp_r = alloc_reg env in
  List.iteri (fun i f ->
    if f.fn_vis <> Private && f.fn_vis <> Internal then begin
      let label = 200 + i * 100 in
      emit env (Contract_vm.LDI (name_r, VString f.fn_name));
      emit env (Contract_vm.EQ (cmp_r, method_r, name_r));
      emit env (Contract_vm.JIF (cmp_r, label))
    end
  ) funcs;
  emit env Contract_vm.REVERT

let should_emit_nonpayable_guard env f =
  env.has_payable && not f.fn_payable && f.fn_vis = Public

let emit_nonpayable_guard env =
  let vr = alloc_reg env in
  emit env (Contract_vm.VALUE vr);
  let zero_r = alloc_reg env in
  emit env (Contract_vm.LDI (zero_r, Contract_vm.VInt Z.zero));
  let cmp_r = alloc_reg env in
  emit env (Contract_vm.EQ (cmp_r, vr, zero_r));
  let ok_label = alloc_label env in
  emit env (Contract_vm.JIF (cmp_r, ok_label));
  emit env Contract_vm.REVERT;
  emit env (Contract_vm.JDEST ok_label);
  env.next_reg <- env.base_reg

let emit_nonreentrant_enter env =
  let lock_r = alloc_reg env in
  emit env (Contract_vm.SLOAD (lock_r, "_lock"));
  let one_r = alloc_reg env in
  emit env (Contract_vm.LDI (one_r, VString "1"));
  let locked = alloc_reg env in
  emit env (Contract_vm.EQ (locked, lock_r, one_r));
  let ok_label = alloc_label env in
  let not_locked = alloc_reg env in
  emit env (Contract_vm.LDI (not_locked, VBool true));
  emit env (Contract_vm.NEQ (not_locked, locked, not_locked));
  emit env (Contract_vm.JIF (not_locked, ok_label));
  emit env Contract_vm.REVERT;
  emit env (Contract_vm.JDEST ok_label);
  emit env (Contract_vm.SSTORE ("_lock", one_r));
  env.next_reg <- env.base_reg

let emit_nonreentrant_exit env =
  let zero_r = alloc_reg env in
  emit env (Contract_vm.LDI (zero_r, VString "0"));
  emit env (Contract_vm.SSTORE ("_lock", zero_r))

let gen_function env (f : func_def) label =
  let saved_locals = env.locals in
  let saved_base = env.base_reg in
  let saved_next = env.next_reg in
  let saved_view = env.fn_is_view in
  let saved_pure = env.fn_is_pure in
  env.fn_is_view <- f.fn_view;
  env.fn_is_pure <- f.fn_pure;
  env.locals <- [];
  env.base_reg <- 1;
  env.next_reg <- 1;
  emit env (Contract_vm.JDEST label);
  List.iteri (fun i p ->
    let r = env.base_reg in
    emit env (Contract_vm.MLOAD (r, 1001 + i));
    env.locals <- (p.p_name, r, p.p_typ) :: env.locals;
    env.base_reg <- r + 1
  ) f.fn_params;
  env.next_reg <- env.base_reg;
  List.iter (fun p ->
    match find_local env p.p_name with
    | Some (_, reg, _) -> emit_type_check env reg p.p_typ
    | None -> ()
  ) f.fn_params;
  List.iter (fun p ->
    match p.p_refine with
    | None -> ()
    | Some refine ->
      (match find_local env p.p_name with
       | Some (_, reg, _) ->
         let val_r = alloc_reg env in
         let cmp_r = alloc_reg env in
         let ok_label = alloc_label env in
         (match refine with
          | RGt (_, v) ->
            emit env (Contract_vm.LDI (val_r, VInt (Z.of_int v)));
            emit env (Contract_vm.GT (cmp_r, reg, val_r));
            emit env (Contract_vm.JIF (cmp_r, ok_label));
            emit env Contract_vm.REVERT;
            emit env (Contract_vm.JDEST ok_label)
          | RGe (_, v) ->
            emit env (Contract_vm.LDI (val_r, VInt (Z.of_int v)));
            emit env (Contract_vm.LT (cmp_r, reg, val_r));
            emit env (Contract_vm.EQ (cmp_r, cmp_r, (let z = alloc_reg env in emit env (Contract_vm.LDI (z, VInt Z.zero)); z)));
            emit env (Contract_vm.JIF (cmp_r, ok_label));
            emit env Contract_vm.REVERT;
            emit env (Contract_vm.JDEST ok_label)
          | RLt (_, v) ->
            emit env (Contract_vm.LDI (val_r, VInt (Z.of_int v)));
            emit env (Contract_vm.LT (cmp_r, reg, val_r));
            emit env (Contract_vm.JIF (cmp_r, ok_label));
            emit env Contract_vm.REVERT;
            emit env (Contract_vm.JDEST ok_label)
          | RLe (_, v) ->
            emit env (Contract_vm.LDI (val_r, VInt (Z.of_int v)));
            emit env (Contract_vm.GT (cmp_r, reg, val_r));
            emit env (Contract_vm.EQ (cmp_r, cmp_r, (let z = alloc_reg env in emit env (Contract_vm.LDI (z, VInt Z.zero)); z)));
            emit env (Contract_vm.JIF (cmp_r, ok_label));
            emit env Contract_vm.REVERT;
            emit env (Contract_vm.JDEST ok_label)
          | RNeq (_, v) ->
            emit env (Contract_vm.LDI (val_r, VInt (Z.of_int v)));
            emit env (Contract_vm.EQ (cmp_r, reg, val_r));
            emit env (Contract_vm.EQ (cmp_r, cmp_r, (let z = alloc_reg env in emit env (Contract_vm.LDI (z, VInt Z.zero)); z)));
            emit env (Contract_vm.JIF (cmp_r, ok_label));
            emit env Contract_vm.REVERT;
            emit env (Contract_vm.JDEST ok_label)
          | RNonZero _ ->
            emit env (Contract_vm.LDI (val_r, VInt Z.zero));
            emit env (Contract_vm.EQ (cmp_r, reg, val_r));
            emit env (Contract_vm.EQ (cmp_r, cmp_r, (let z = alloc_reg env in emit env (Contract_vm.LDI (z, VInt Z.zero)); z)));
            emit env (Contract_vm.JIF (cmp_r, ok_label));
            emit env Contract_vm.REVERT;
            emit env (Contract_vm.JDEST ok_label))
       | None -> ())
  ) f.fn_params;
  if should_emit_nonpayable_guard env f then emit_nonpayable_guard env;
  if f.fn_nonreentrant then emit_nonreentrant_enter env;
  env.in_nonreentrant <- f.fn_nonreentrant;
  List.iter (gen_stmt env) f.fn_body;
  if f.fn_nonreentrant then emit_nonreentrant_exit env;
  (match List.rev env.code with
   | Contract_vm.STOP :: _ -> ()
   | _ -> emit env Contract_vm.STOP);
  env.in_nonreentrant <- false;
  env.fn_is_view <- saved_view;
  env.fn_is_pure <- saved_pure;
  env.locals <- saved_locals;
  env.base_reg <- saved_base;
  env.next_reg <- saved_next

let check_interface_arity iface_name im f =
  let expected_n = List.length im.im_params in
  let actual_n = List.length f.fn_params in
  if expected_n <> actual_n then
    raise (GenError (Printf.sprintf "%s.%s: expected %d params, got %d"
      iface_name im.im_name expected_n actual_n, 0))

let check_interface_params iface_name im f =
  List.iter2 (fun ep ap ->
    if ep.p_typ <> ap.p_typ then
      raise (GenError (Printf.sprintf "%s.%s: param '%s' type mismatch: expected %s, got %s"
        iface_name im.im_name ap.p_name (typ_to_string ep.p_typ) (typ_to_string ap.p_typ), 0))
  ) im.im_params f.fn_params

let check_interface_return iface_name im f =
  if im.im_ret <> f.fn_ret then
    raise (GenError (Printf.sprintf "%s.%s: return type mismatch: expected %s, got %s"
      iface_name im.im_name (typ_to_string im.im_ret) (typ_to_string f.fn_ret), 0))

let check_interface_visibility iface_name im f =
  if f.fn_vis <> Public then
    raise (GenError (Printf.sprintf "%s.%s must be public" iface_name im.im_name, 0))

let check_interface_method iface_name im f =
  check_interface_arity iface_name im f;
  check_interface_params iface_name im f;
  check_interface_return iface_name im f;
  check_interface_visibility iface_name im f

let check_interfaces (ct : contract) =
  List.iter (fun iface_name ->
    let iface = match List.find_opt (fun i -> i.if_name = iface_name) ct.interfaces with
      | Some i -> i
      | None -> raise (GenError (Printf.sprintf "unknown interface: %s" iface_name, 0))
    in
    List.iter (fun im ->
      match List.find_opt (fun f -> f.fn_name = im.im_name) ct.funcs with
      | None ->
        raise (GenError (Printf.sprintf "contract %s implements %s but missing method: %s"
          ct.name iface_name im.im_name, 0))
      | Some f -> check_interface_method iface_name im f
    ) iface.if_methods
  ) ct.implements

let generate (ct : contract) =
  check_interfaces ct;
  let env = make_env ct.declaration ct.structs ct.enums ct.consts ct.state ct.events ct.errors ct.funcs in
  List.iteri (fun i f ->
    Hashtbl.replace env.func_labels f.fn_name (200 + i * 100)
  ) ct.funcs;
  (match ct.ctor with
   | Some ctor -> gen_constructor env ctor
   | None ->
     env.base_reg <- 1;
     env.next_reg <- 1;
     let check_r = alloc_reg env in
     let ctor_str = alloc_reg env in
     let cmp_r = alloc_reg env in
     emit env (Contract_vm.MLOAD (check_r, 999));
     emit env (Contract_vm.LDI (ctor_str, VString "constructor"));
     emit env (Contract_vm.NEQ (cmp_r, check_r, ctor_str));
     emit env (Contract_vm.JIF (cmp_r, 100));
     emit env Contract_vm.STOP);
  gen_dispatcher env ct.funcs;
  List.iteri (fun i f ->
    let label = 200 + i * 100 in
    gen_function env f label
  ) ct.funcs;
  Array.of_list (List.rev env.code)

let to_abi (ct : contract) : Ocs01.abi =
  let typ_to_arg = function
    | TInt -> Ocs01.Credits
    | TBool -> Ocs01.Flag
    | TString -> Ocs01.Text
    | TAddress -> Ocs01.Account
    | TBytes -> Ocs01.Bytes
    | _ -> Ocs01.Bytes
  in
  let public_funcs = List.filter (fun f -> f.fn_vis = Public) ct.funcs in
  let fns = List.map (fun f ->
    f.fn_name,
    { Ocs01.inputs = List.map (fun p -> typ_to_arg p.p_typ) f.fn_params;
      outputs = [typ_to_arg f.fn_ret];
      effort = Z.of_int 100;
      pure = f.fn_view || f.fn_pure;
      payable = f.fn_payable }
  ) public_funcs in
  let events = List.map (fun ev ->
    ev.ev_name,
    List.map (fun (n, t, _indexed) -> (n, typ_to_arg t)) ev.ev_fields
  ) ct.events in
  { Ocs01.fns; events }