(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Oct_lang

type severity =
  | Error
  | Warning

type finding = {
  severity : severity;
  code : string;
  message : string;
  program_name : string;
  function_name : string option;
  state_field : string option;
  parameter : string option;
}

type rule = {
  rule_code : string;
  title : string;
  default_severity : severity;
}

type function_summary = {
  summary_name : string;
  summary_visibility : string;
  direct_calls : string list;
  direct_writes : string list;
  transitive_writes : string list;
  value_writes : string list;
  signed_params : string list;
}

type invariant = {
  invariant_code : string;
  invariant_kind : string option;
  invariant_status : string;
  invariant_expression : string option;
  invariant_message : string;
  invariant_fields : string list;
  invariant_functions : string list;
}

type report = {
  program_name : string;
  findings : finding list;
  summaries : function_summary list;
  invariants : invariant list;
}

type writes_state =
  | Writes_visiting
  | Writes_complete of string list

let severity_to_string = function
  | Error -> "error"
  | Warning -> "warning"

let schema = "aml_safety_report_v1"

let engine = "aml_ast_verifier"

let proof_model = "formal/coq/aml_value_safety_model.v"

let proof_gate = "scripts/check_aml_formal.sh"

let rules = [
  { rule_code = "signed_value_storage"; title = "value-like storage must not use signed int"; default_severity = Error };
  { rule_code = "signed_value_parameter"; title = "value-moving public parameters must not use signed int"; default_severity = Error };
  { rule_code = "signed_value_flow"; title = "signed values must not flow into value-like storage writes"; default_severity = Error };
  { rule_code = "unsigned_arithmetic_range_not_proven"; title = "unsigned arithmetic writes must prove range safety"; default_severity = Error };
  { rule_code = "supply_invariant_unproven"; title = "balance and total supply writes should preserve conservation"; default_severity = Warning };
  { rule_code = "unsigned_parameter_without_positive_guard"; title = "unsigned value parameters should reject zero unless zero is meaningful"; default_severity = Warning };
  { rule_code = "unchecked_transfer_result"; title = "native transfer result should be checked"; default_severity = Warning };
]

let binop_to_string = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Le -> "<="
  | Ge -> ">="
  | And -> "&&"
  | Or -> "||"

let unop_to_string = function
  | Neg -> "-"
  | Not -> "!"

let rec expr_to_string = function
  | EInt value -> Z.to_string value
  | EBool true -> "true"
  | EBool false -> "false"
  | EString value -> Printf.sprintf "%S" value
  | EVar value -> value
  | EField value -> value
  | EIndex (field, indexes) -> field ^ "[" ^ String.concat "," (List.map expr_to_string indexes) ^ "]"
  | EBinop (op, left, right) -> expr_to_string left ^ " " ^ binop_to_string op ^ " " ^ expr_to_string right
  | EUnop (op, value) -> unop_to_string op ^ expr_to_string value
  | ECaller -> "caller"
  | EOrigin -> "origin"
  | ESelfAddr -> "self_addr"
  | EEpoch -> "epoch"
  | EEpochTime -> "epoch_time"
  | EValue -> "value"
  | EBalance value -> "balance(" ^ expr_to_string value ^ ")"
  | ETreeHash -> "tree_hash"
  | ENodeId -> "node_id"
  | ETxHash -> "tx_hash"
  | ECall (name, args) -> name ^ "(" ^ String.concat "," (List.map expr_to_string args) ^ ")"
  | EArray values -> "[" ^ String.concat "," (List.map expr_to_string values) ^ "]"
  | ETuple values -> "(" ^ String.concat "," (List.map expr_to_string values) ^ ")"
  | EStoragePath (field, indexes, path) ->
    field ^ "[" ^ String.concat "," (List.map expr_to_string indexes) ^ "]." ^ String.concat "." path
  | EFieldProp (field, prop) -> field ^ "." ^ prop
  | EIndexField (field, indexes, prop) ->
    field ^ "[" ^ String.concat "," (List.map expr_to_string indexes) ^ "]." ^ prop
  | EEnumVariant (enum_name, variant) -> enum_name ^ "." ^ variant
  | ETernary (cond, yes_value, no_value) ->
    expr_to_string cond ^ " ? " ^ expr_to_string yes_value ^ " : " ^ expr_to_string no_value

let lowercase value =
  String.lowercase_ascii value

let contains haystack needle =
  let haystack = lowercase haystack in
  let needle = lowercase needle in
  let haystack_len = String.length haystack in
  let needle_len = String.length needle in
  let rec loop offset =
    needle_len = 0
    || (offset + needle_len <= haystack_len
        && (String.sub haystack offset needle_len = needle
            || loop (offset + 1)))
  in
  loop 0

let has_any value needles =
  List.exists (contains value) needles

let is_value_name name =
  has_any name [
    "amount";
    "amt";
    "value";
    "supply";
    "balance";
    "reserve";
    "deposit";
    "withdraw";
    "allowance";
    "grant";
    "liquidity";
    "fee";
    "lp";
  ]

let is_value_field name =
  has_any name [
    "balance";
    "balances";
    "reserve";
    "reserves";
    "deposit";
    "deposits";
    "allowance";
    "allowances";
    "grant";
    "grants";
    "total_supply";
    "liquidity";
    "lp";
  ]

let is_value_function name =
  has_any name [
    "transfer";
    "pull";
    "withdraw";
    "deposit";
    "mint";
    "burn";
    "swap";
    "grant";
    "approve";
    "pay";
    "claim";
  ]

let allows_zero_value_function name =
  match lowercase name with
  | "grant" | "approve" -> true
  | _ -> false

let rec map_value_type = function
  | TMap (_, value_type) -> map_value_type value_type
  | typ -> typ

let typ_is_signed = function
  | TInt -> true
  | _ -> false

let is_unsigned = function
  | TU64 | TU128 | TU256 -> true
  | _ -> false

let field_base_type fields name =
  match List.find_opt (fun field -> field.sf_name = name) fields with
  | Some field -> Some (map_value_type field.sf_typ)
  | None -> None

let rec expr_mentions name = function
  | EVar value -> value = name
  | EIndex (value, indexes) -> value = name || List.exists (expr_mentions name) indexes
  | EBinop (_, left, right) -> expr_mentions name left || expr_mentions name right
  | EUnop (_, value) -> expr_mentions name value
  | ECall (_, args) -> List.exists (expr_mentions name) args
  | EArray values | ETuple values -> List.exists (expr_mentions name) values
  | EStoragePath (field, indexes, path) ->
    field = name || List.exists (( = ) name) path || List.exists (expr_mentions name) indexes
  | EFieldProp (field, prop) -> field = name || prop = name
  | EIndexField (field, indexes, prop) ->
    field = name || prop = name || List.exists (expr_mentions name) indexes
  | ETernary (cond, yes_value, no_value) ->
    expr_mentions name cond || expr_mentions name yes_value || expr_mentions name no_value
  | EField value -> value = name
  | EBalance value -> expr_mentions name value
  | EInt _ | EBool _ | EString _ | ECaller | EOrigin | ESelfAddr | EEpoch | EEpochTime | EValue
  | ETreeHash | ENodeId | ETxHash | EEnumVariant _ -> false

let rec expr_is_positive_guard name = function
  | EBinop (Gt, EVar value, EInt zero) when value = name && Z.equal zero Z.zero -> true
  | EBinop (And, left, right) -> expr_is_positive_guard name left || expr_is_positive_guard name right
  | _ -> false

let rec stmt_has_positive_guard name = function
  | SRequire (expr, _) | SAssert expr -> expr_is_positive_guard name expr
  | SIf (_, then_body, else_body) ->
    List.exists (stmt_has_positive_guard name) then_body
    || Option.value ~default:false (Option.map (List.exists (stmt_has_positive_guard name)) else_body)
  | SWhile (_, body) | SFor (_, _, _, body) | SForEach (_, _, body) ->
    List.exists (stmt_has_positive_guard name) body
  | SMatch (_, branches) ->
    List.exists (fun (_, _, body) -> List.exists (stmt_has_positive_guard name) body) branches
  | SLet _ | SAssign _ | SFieldSet _ | SIndexSet _ | SReturn _ | SEmit _
  | SFieldCall _ | SStoragePathSet _ | SIndexFieldSet _ | SLetTuple _
  | SExpr _ | SRevertError _ -> false

let function_has_positive_guard name body =
  List.exists (stmt_has_positive_guard name) body

let rec stmt_requires_var name = function
  | SRequire (expr, _) | SAssert expr -> expr_mentions name expr
  | SIf (_, then_body, else_body) ->
    List.exists (stmt_requires_var name) then_body
    || Option.value ~default:false (Option.map (List.exists (stmt_requires_var name)) else_body)
  | SWhile (_, body) | SFor (_, _, _, body) | SForEach (_, _, body) ->
    List.exists (stmt_requires_var name) body
  | SMatch (_, branches) ->
    List.exists (fun (_, _, body) -> List.exists (stmt_requires_var name) body) branches
  | SLet _ | SAssign _ | SFieldSet _ | SIndexSet _ | SReturn _ | SEmit _
  | SFieldCall _ | SStoragePathSet _ | SIndexFieldSet _ | SLetTuple _
  | SExpr _ | SRevertError _ -> false

let function_checks_var name body =
  List.exists (stmt_requires_var name) body

let env_find name env =
  List.assoc_opt name env

let rec expr_typ fields env = function
  | EVar name -> env_find name env
  | EIndex (field, _) | EStoragePath (field, _, _) | EField field
  | EFieldProp (field, _) | EIndexField (field, _, _) -> field_base_type fields field
  | EInt _ | EUnop (Neg, _) -> Some TInt
  | EBinop (_, left, right) ->
    begin
      match expr_typ fields env left, expr_typ fields env right with
      | Some TInt, _ | _, Some TInt -> Some TInt
      | Some typ, _ -> Some typ
      | _, Some typ -> Some typ
      | _ -> None
    end
  | ETernary (_, yes_value, no_value) ->
    begin
      match expr_typ fields env yes_value, expr_typ fields env no_value with
      | Some TInt, _ | _, Some TInt -> Some TInt
      | Some typ, _ -> Some typ
      | _, Some typ -> Some typ
      | _ -> None
    end
  | EBool _ -> Some TBool
  | EString _ -> Some TString
  | ECaller | EOrigin | ESelfAddr -> Some TAddress
  | EValue -> Some TU128
  | EArray _ | ETuple _ | ECall _ | EUnop (Not, _) | EBalance _ | EEpoch | EEpochTime
  | ETreeHash | ENodeId | ETxHash | EEnumVariant _ -> None

let rec expr_signed_source fields env = function
  | EVar name ->
    begin
      match env_find name env with
      | Some TInt -> Some name
      | _ -> None
    end
  | EIndex (field, indexes) ->
    begin
      match field_base_type fields field with
      | Some TInt -> Some field
      | _ -> first_signed_source fields env indexes
    end
  | EStoragePath (field, indexes, _) | EIndexField (field, indexes, _) ->
    begin
      match field_base_type fields field with
      | Some TInt -> Some field
      | _ -> first_signed_source fields env indexes
    end
  | EField field | EFieldProp (field, _) ->
    begin
      match field_base_type fields field with
      | Some TInt -> Some field
      | _ -> None
    end
  | EUnop (Neg, _) -> Some "literal"
  | EBinop (_, left, right) ->
    begin
      match expr_signed_source fields env left with
      | Some source -> Some source
      | None -> expr_signed_source fields env right
    end
  | EUnop (_, value) | EBalance value -> expr_signed_source fields env value
  | ECall (_, args) | EArray args | ETuple args -> first_signed_source fields env args
  | ETernary (cond, yes_value, no_value) ->
    first_signed_source fields env [cond; yes_value; no_value]
  | EInt _ | EBool _ | EString _ | ECaller | EOrigin | ESelfAddr | EEpoch | EEpochTime | EValue
  | ETreeHash | ENodeId | ETxHash | EEnumVariant _ -> None

and first_signed_source fields env values =
  match values with
  | [] -> None
  | value :: rest ->
    match expr_signed_source fields env value with
    | Some source -> Some source
    | None -> first_signed_source fields env rest

let expr_has_signed_flow fields env expr =
  expr_signed_source fields env expr <> None

let arithmetic_op_needs_range_check = function
  | Add | Sub | Mul | Div | Mod -> true
  | Eq | Neq | Lt | Gt | Le | Ge | And | Or -> false

let expr_same left right =
  String.equal (expr_to_string left) (expr_to_string right)

let rec expr_proves_sub_guard left right = function
  | EBinop ((Ge | Gt), guard_left, guard_right) ->
    expr_same left guard_left && expr_same right guard_right
  | EBinop (And, left_guard, right_guard) ->
    expr_proves_sub_guard left right left_guard || expr_proves_sub_guard left right right_guard
  | _ -> false

let rec stmt_proves_sub_guard left right = function
  | SRequire (expr, _) | SAssert expr -> expr_proves_sub_guard left right expr
  | SIf (_, then_body, else_body) ->
    List.exists (stmt_proves_sub_guard left right) then_body
    || Option.value ~default:false (Option.map (List.exists (stmt_proves_sub_guard left right)) else_body)
  | SWhile (_, body) | SFor (_, _, _, body) | SForEach (_, _, body) ->
    List.exists (stmt_proves_sub_guard left right) body
  | SMatch (_, branches) ->
    List.exists (fun (_, _, body) -> List.exists (stmt_proves_sub_guard left right) body) branches
  | SLet _ | SAssign _ | SFieldSet _ | SIndexSet _ | SReturn _ | SEmit _
  | SFieldCall _ | SStoragePathSet _ | SIndexFieldSet _ | SLetTuple _
  | SExpr _ | SRevertError _ -> false

let function_proves_sub_guard left right body =
  List.exists (stmt_proves_sub_guard left right) body

let rec expr_has_unproven_unsigned_arithmetic fields env body = function
  | EBinop (op, left, right) ->
    let unsigned_op =
      arithmetic_op_needs_range_check op
      && begin
       match expr_typ fields env left, expr_typ fields env right with
       | Some typ, _ when is_unsigned typ -> true
       | _, Some typ when is_unsigned typ -> true
       | _ -> false
      end
    in
    let proven =
      match op with
      | Sub -> function_proves_sub_guard left right body
      | _ -> false
    in
    (unsigned_op && not proven)
    || expr_has_unproven_unsigned_arithmetic fields env body left
    || expr_has_unproven_unsigned_arithmetic fields env body right
  | EUnop (_, value) | EBalance value ->
    expr_has_unproven_unsigned_arithmetic fields env body value
  | ECall (_, args) | EArray args | ETuple args ->
    List.exists (expr_has_unproven_unsigned_arithmetic fields env body) args
  | EIndex (_, indexes) | EStoragePath (_, indexes, _) | EIndexField (_, indexes, _) ->
    List.exists (expr_has_unproven_unsigned_arithmetic fields env body) indexes
  | ETernary (cond, yes_value, no_value) ->
    List.exists (expr_has_unproven_unsigned_arithmetic fields env body) [cond; yes_value; no_value]
  | EInt _ | EBool _ | EString _ | EVar _ | EField _ | ECaller | EOrigin | ESelfAddr
  | EEpoch | EEpochTime | EValue | ETreeHash | ENodeId | ETxHash | EFieldProp _ | EEnumVariant _ -> false

let visibility_to_string = function
  | Public -> "public"
  | Private -> "private"
  | Internal -> "internal"

let uniq values =
  values
  |> List.sort_uniq String.compare

let rec expr_calls = function
  | ECall (name, args) -> name :: List.concat_map expr_calls args
  | EIndex (_, args) | EArray args | ETuple args -> List.concat_map expr_calls args
  | EBinop (_, left, right) -> expr_calls left @ expr_calls right
  | EUnop (_, value) | EBalance value -> expr_calls value
  | EStoragePath (_, indexes, _) | EIndexField (_, indexes, _) -> List.concat_map expr_calls indexes
  | ETernary (cond, yes_value, no_value) -> expr_calls cond @ expr_calls yes_value @ expr_calls no_value
  | EInt _ | EBool _ | EString _ | EVar _ | EField _ | ECaller | EOrigin | ESelfAddr
  | EEpoch | EEpochTime | EValue | ETreeHash | ENodeId | ETxHash | EFieldProp _ | EEnumVariant _ -> []

let rec stmt_direct_calls = function
  | SLet (_, _, expr) | SAssign (_, expr) | SFieldSet (_, expr)
  | SIndexSet (_, _, expr) | SReturn (Some expr) | SAssert expr
  | SRequire (expr, _) | SStoragePathSet (_, _, _, expr)
  | SIndexFieldSet (_, _, _, expr) | SLetTuple (_, expr) | SExpr expr ->
    expr_calls expr
  | SReturn None -> []
  | SEmit (_, args) | SFieldCall (_, _, args) | SRevertError (_, args) ->
    List.concat_map expr_calls args
  | SIf (cond, then_body, else_body) ->
    expr_calls cond @ block_direct_calls then_body
    @ Option.value ~default:[] (Option.map block_direct_calls else_body)
  | SWhile (cond, body) ->
    expr_calls cond @ block_direct_calls body
  | SFor (_, start_expr, end_expr, body) ->
    expr_calls start_expr @ expr_calls end_expr @ block_direct_calls body
  | SForEach (_, _, body) ->
    block_direct_calls body
  | SMatch (expr, branches) ->
    expr_calls expr @ List.concat_map (fun (_, _, body) -> block_direct_calls body) branches

and block_direct_calls body =
  List.concat_map stmt_direct_calls body

let rec stmt_direct_writes = function
  | SFieldSet (field, _) | SIndexSet (field, _, _)
  | SStoragePathSet (field, _, _, _) | SIndexFieldSet (field, _, _, _) -> [field]
  | SIf (_, then_body, else_body) ->
    block_direct_writes then_body
    @ Option.value ~default:[] (Option.map block_direct_writes else_body)
  | SWhile (_, body) | SFor (_, _, _, body) | SForEach (_, _, body) ->
    block_direct_writes body
  | SMatch (_, branches) ->
    List.concat_map (fun (_, _, body) -> block_direct_writes body) branches
  | SLet _ | SAssign _ | SReturn _ | SAssert _ | SRequire _ | SEmit _
  | SFieldCall _ | SLetTuple _ | SExpr _ | SRevertError _ -> []

and block_direct_writes body =
  List.concat_map stmt_direct_writes body

let finding severity code message program_name function_name state_field parameter = {
  severity;
  code;
  message;
  program_name;
  function_name;
  state_field;
  parameter;
}

let state_findings program_name field =
  let base_type = map_value_type field.sf_typ in
  if is_value_field field.sf_name && base_type = TInt then
    [finding Error "signed_value_storage"
      "value-like storage uses signed int"
      program_name None (Some field.sf_name) None]
  else
    []

let param_findings program_name fn param =
  let value_like = is_value_name param.p_name || is_value_function fn.fn_name in
  if fn.fn_vis = Public && value_like && param.p_typ = TInt then
    [finding Error "signed_value_parameter"
      "value-moving public parameter uses signed int"
      program_name (Some fn.fn_name) None (Some param.p_name)]
  else if fn.fn_vis = Public && value_like && is_unsigned param.p_typ
          && not (allows_zero_value_function fn.fn_name)
          && not (function_has_positive_guard param.p_name fn.fn_body) then
    [finding Warning "unsigned_parameter_without_positive_guard"
      "unsigned value parameter allows zero unless guarded"
      program_name (Some fn.fn_name) None (Some param.p_name)]
  else
    []

let stmt_findings program_name fn stmt =
  match stmt with
  | SExpr (ECall ("transfer", _)) ->
    [finding Warning "unchecked_transfer_result"
      "transfer result is not checked"
      program_name (Some fn.fn_name) None None]
  | SLet (name, _, ECall ("transfer", _)) when not (function_checks_var name fn.fn_body) ->
    [finding Warning "unchecked_transfer_result"
      "transfer result is assigned but not required"
      program_name (Some fn.fn_name) None None]
  | _ -> []

let value_write_finding program_name fn field source =
  finding Error "signed_value_flow"
    "signed value can flow into value-like storage"
    program_name (Some fn.fn_name) (Some field) source

let unsigned_arithmetic_finding program_name fn field =
  finding Error "unsigned_arithmetic_range_not_proven"
    "unsigned arithmetic result can exceed the declared range"
    program_name (Some fn.fn_name) field None

let is_value_storage_write fields field =
  is_value_field field && field_base_type fields field <> None

let rec flow_stmt_findings program_name fields fn env stmt =
  let write_findings field expr =
    let signed =
      if is_value_storage_write fields field && expr_has_signed_flow fields env expr then
        [value_write_finding program_name fn field (expr_signed_source fields env expr)]
      else
        []
    in
    let unsigned_arith =
      match field_base_type fields field with
      | Some typ when is_value_storage_write fields field && is_unsigned typ && expr_has_unproven_unsigned_arithmetic fields env fn.fn_body expr ->
        [unsigned_arithmetic_finding program_name fn (Some field)]
      | _ -> []
    in
    signed @ unsigned_arith
  in
  match stmt with
  | SLet (name, typ_opt, expr) ->
    let env =
      match typ_opt with
      | Some typ -> (name, typ) :: env
      | None ->
        match expr_typ fields env expr with
        | Some typ -> (name, typ) :: env
        | None -> env
    in
    ([], env)
  | SAssign (name, expr) ->
    let env =
      match expr_typ fields env expr with
      | Some typ -> (name, typ) :: List.remove_assoc name env
      | None -> env
    in
    ([], env)
  | SFieldSet (field, expr) | SIndexSet (field, _, expr)
  | SStoragePathSet (field, _, _, expr) | SIndexFieldSet (field, _, _, expr) ->
    (write_findings field expr, env)
  | SIf (_, then_body, else_body) ->
    let then_findings = flow_block_findings program_name fields fn env then_body in
    let else_findings =
      match else_body with
      | Some body -> flow_block_findings program_name fields fn env body
      | None -> []
    in
    (then_findings @ else_findings, env)
  | SWhile (_, body) | SFor (_, _, _, body) | SForEach (_, _, body) ->
    (flow_block_findings program_name fields fn env body, env)
  | SMatch (_, branches) ->
    let findings =
      List.concat_map (fun (_, _, body) -> flow_block_findings program_name fields fn env body) branches
    in
    (findings, env)
  | SLetTuple (names, expr) ->
    let typ_opt = expr_typ fields env expr in
    let env =
      match typ_opt with
      | Some typ -> List.fold_left (fun acc name -> (name, typ) :: List.remove_assoc name acc) env names
      | None -> env
    in
    ([], env)
  | SReturn _ | SAssert _ | SRequire _ | SEmit _ | SFieldCall _
  | SExpr _ | SRevertError _ -> ([], env)

and flow_block_findings program_name fields fn env body =
  let findings, _ =
    List.fold_left (fun (items, env) stmt ->
      let more, env = flow_stmt_findings program_name fields fn env stmt in
      (items @ more, env)
    ) ([], env) body
  in
  findings

let flow_findings program_name fields fn =
  let env = List.map (fun param -> (param.p_name, param.p_typ)) fn.fn_params in
  flow_block_findings program_name fields fn env fn.fn_body

let function_findings program_name fields fn =
  let param_issues = List.concat_map (param_findings program_name fn) fn.fn_params in
  let stmt_issues = List.concat_map (stmt_findings program_name fn) fn.fn_body in
  let flow_issues = flow_findings program_name fields fn in
  param_issues @ stmt_issues @ flow_issues

let write_count field body =
  block_direct_writes body
  |> List.filter (( = ) field)
  |> List.length

let fn_by_name functions name =
  List.find_opt (fun fn -> fn.fn_name = name) functions

let function_summary fields transitive fn =
  let direct_writes = block_direct_writes fn.fn_body in
  let transitive_writes = transitive fn.fn_name in
  {
    summary_name = fn.fn_name;
    summary_visibility = visibility_to_string fn.fn_vis;
    direct_calls = uniq (block_direct_calls fn.fn_body);
    direct_writes = uniq direct_writes;
    transitive_writes;
    value_writes =
      uniq
        (List.filter
           (is_value_storage_write fields)
           transitive_writes);
    signed_params = fn.fn_params |> List.filter (fun param -> typ_is_signed param.p_typ) |> List.map (fun param -> param.p_name);
  }

let summarize_functions fields functions =
  let by_name = Hashtbl.create (List.length functions) in
  List.iter (fun fn -> Hashtbl.replace by_name fn.fn_name fn) functions;
  let states = Hashtbl.create (List.length functions) in
  let rec transitive name =
    match Hashtbl.find_opt states name with
    | Some Writes_visiting -> []
    | Some (Writes_complete writes) -> writes
    | None ->
      match Hashtbl.find_opt by_name name with
      | None -> []
      | Some fn ->
        Hashtbl.replace states name Writes_visiting;
        let direct = block_direct_writes fn.fn_body in
        let nested =
          block_direct_calls fn.fn_body
          |> List.filter (Hashtbl.mem by_name)
          |> List.concat_map transitive
        in
        let writes = uniq (direct @ nested) in
        Hashtbl.replace states name (Writes_complete writes);
        writes
  in
  List.map (function_summary fields transitive) functions

let summary_by_name summaries name =
  List.find_opt (fun summary -> summary.summary_name = name) summaries

let function_of_summary functions summary =
  fn_by_name functions summary.summary_name

let field_names fields pred =
  fields
  |> List.filter (fun field -> pred field.sf_name)
  |> List.map (fun field -> field.sf_name)

let list_intersects left right =
  List.exists (fun value -> List.mem value right) left

let supply_balance_invariant program_name fields functions summaries =
  let balance_fields = field_names fields (fun name -> contains name "balance") in
  let supply_fields = field_names fields (fun name -> contains name "total_supply" || name = "supply") in
  if balance_fields = [] || supply_fields = [] then
    ([], [{
      invariant_code = "supply_balance_conservation";
      invariant_kind = Some "derived";
      invariant_status = "not_applicable";
      invariant_expression = None;
      invariant_message = "program does not expose both balance and supply storage";
      invariant_fields = balance_fields @ supply_fields;
      invariant_functions = [];
    }])
  else
    let risky =
      summaries
      |> List.filter_map (fun summary ->
        let writes_balance = list_intersects summary.transitive_writes balance_fields in
        let writes_supply = list_intersects summary.transitive_writes supply_fields in
        if not (writes_balance || writes_supply) then None
        else
          match function_of_summary functions summary with
          | None -> None
          | Some fn ->
            let direct_balance_count =
              List.fold_left (fun total field -> total + write_count field fn.fn_body) 0 balance_fields
            in
            if writes_balance && not writes_supply && direct_balance_count = 2 then None
            else if writes_balance && writes_supply then None
            else Some summary.summary_name)
    in
    let invariant = {
      invariant_code = "supply_balance_conservation";
      invariant_kind = Some "derived";
      invariant_status = if risky = [] then "pass" else "warning";
      invariant_expression = None;
      invariant_message =
        if risky = [] then "balance and supply write pattern looks conserved"
        else "some functions update balances or supply without a proven matching conservation update";
      invariant_fields = balance_fields @ supply_fields;
      invariant_functions = risky;
    } in
    let findings =
      List.map (fun fn_name ->
        finding Warning "supply_invariant_unproven"
          "balance and supply conservation is not proven for this function"
          program_name (Some fn_name) None None
      ) risky
    in
    (findings, [invariant])

let invariant_analysis program_name fields functions summaries =
  supply_balance_invariant program_name fields functions summaries

let field_name_of_expr = function
  | EVar name | EField name -> Some name
  | _ -> None

let invariant_kind = function
  | EBinop (Eq, ECall ("sum", [left]), right)
  | EBinop (Eq, right, ECall ("sum", [left])) ->
    begin match field_name_of_expr left, field_name_of_expr right with
    | Some balance_field, Some supply_field
      when contains balance_field "balance"
        && (contains supply_field "total_supply" || supply_field = "supply") ->
      Some (`SupplyBalance (balance_field, supply_field))
    | _ -> None
    end
  | EString value when contains value "sum(" && contains value "total_supply" ->
    Some (`SupplyBalance ("balances", "total_supply"))
  | _ -> None

let declared_invariants ast derived =
  List.map (fun item ->
    let status, fields, kind =
      match invariant_kind item.inv_expr with
      | Some (`SupplyBalance (balance_field, supply_field)) ->
        let status =
          match List.find_opt (fun inv -> inv.invariant_code = "supply_balance_conservation") derived with
          | Some inv when inv.invariant_status = "pass" -> "pass"
          | Some inv when inv.invariant_status = "warning" -> "warning"
          | _ -> "declared"
        in
        status, [balance_field; supply_field], Some "declared_supply_balance"
      | None ->
        "declared", [], Some "declared"
    in
    let message =
      match item.inv_expr with
      | EString value -> value
      | expr -> expr_to_string expr
    in
    {
      invariant_code = "declared:" ^ item.inv_name;
      invariant_kind = kind;
      invariant_status = status;
      invariant_expression = Some message;
      invariant_message = message;
      invariant_fields = fields;
      invariant_functions = [];
    }
  ) ast.invariants_decl

let verify_ast ast =
  let program_name =
    if ast.name = "" then "interface" else ast.name
  in
  let state_issues = List.concat_map (state_findings program_name) ast.state in
  let functions = ast.funcs @ Option.to_list ast.ctor in
  let summaries = summarize_functions ast.state functions in
  let invariant_issues, invariants = invariant_analysis program_name ast.state functions summaries in
  let invariants = invariants @ declared_invariants ast invariants in
  let function_issues = List.concat_map (function_findings program_name ast.state) functions in
  { program_name; findings = state_issues @ function_issues @ invariant_issues; summaries; invariants }

let verify_source source =
  Oct_parse.parse source |> verify_ast

let has_errors report =
  List.exists (fun item -> item.severity = Error) report.findings

let count severity report =
  List.fold_left (fun total item -> if item.severity = severity then total + 1 else total) 0 report.findings

let code_count code report =
  List.fold_left (fun total item -> if item.code = code then total + 1 else total) 0 report.findings

let safety_level report =
  if has_errors report then "error"
  else if count Warning report > 0 then "warning"
  else "safe"

let json_of_rule report rule =
  let finding_count = code_count rule.rule_code report in
  let status =
    if finding_count = 0 then "pass"
    else severity_to_string rule.default_severity
  in
  `Assoc [
    "code", `String rule.rule_code;
    "title", `String rule.title;
    "status", `String status;
    "severity", `String (severity_to_string rule.default_severity);
    "findings", `Int finding_count;
  ]

let json_of_finding item =
  `Assoc [
    "severity", `String (severity_to_string item.severity);
    "code", `String item.code;
    "message", `String item.message;
    "program_name", `String item.program_name;
    "function_name", (match item.function_name with Some value -> `String value | None -> `Null);
    "state_field", (match item.state_field with Some value -> `String value | None -> `Null);
    "parameter", (match item.parameter with Some value -> `String value | None -> `Null);
  ]

let json_string_list values =
  `List (List.map (fun value -> `String value) values)

let json_of_summary item =
  `Assoc [
    "name", `String item.summary_name;
    "visibility", `String item.summary_visibility;
    "direct_calls", json_string_list item.direct_calls;
    "direct_writes", json_string_list item.direct_writes;
    "transitive_writes", json_string_list item.transitive_writes;
    "value_writes", json_string_list item.value_writes;
    "signed_params", json_string_list item.signed_params;
  ]

let json_of_invariant item =
  `Assoc [
    "code", `String item.invariant_code;
    "kind", (match item.invariant_kind with Some value -> `String value | None -> `Null);
    "status", `String item.invariant_status;
    "expression", (match item.invariant_expression with Some value -> `String value | None -> `Null);
    "message", `String item.invariant_message;
    "fields", json_string_list item.invariant_fields;
    "functions", json_string_list item.invariant_functions;
  ]

let json_of_report report =
  `Assoc [
    "schema", `String schema;
    "engine", `String engine;
    "proof_model", `String proof_model;
    "proof_gate", `String proof_gate;
    "program_name", `String report.program_name;
    "verified", `Bool (not (has_errors report));
    "safety", `String (safety_level report);
    "errors", `Int (count Error report);
    "warnings", `Int (count Warning report);
    "trace", `List (List.map (json_of_rule report) rules);
    "function_summaries", `List (List.map json_of_summary report.summaries);
    "invariants", `List (List.map json_of_invariant report.invariants);
    "findings", `List (List.map json_of_finding report.findings);
  ]