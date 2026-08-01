(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type call = {
  target : string;
  method_name : string;
  params : Yojson.Safe.t list;
  amount : Z.t;
}

type direct_call = {
  method_name : string;
  params : Yojson.Safe.t list;
}

type readonly_call = {
  readonly_method_name : string;
  readonly_params : Yojson.Safe.t list;
  readonly_caller_addr : string;
  readonly_include_storage : bool;
}

type value_transfer =
  | No_value
  | Transfer of {
      from_addr : string;
      target : string;
      amount : Z.t;
    }
  | Insufficient

type balance_delta = {
  addr : string;
  delta : Z.t;
}

type journal_entry = {
  from_addr : string;
  target : string;
  amount : Z.t;
}

type value_effect =
  | Value_noop
  | Value_reject
  | Value_apply of {
      deltas : balance_delta list;
      journal : journal_entry;
    }

type direct_exec = {
  method_name : string;
  params : Yojson.Safe.t list;
  amount : Z.t;
  value_effect : value_effect;
  effort_limit : int;
}

type direct_exec_plan =
  | Direct_exec_ready of direct_exec
  | Direct_exec_invalid_format
  | Direct_exec_insufficient_value

type direct_exec_domain =
  | Circle_exec
  | Program_exec

type direct_exec_reject = {
  error_type : string;
  log_reason : string;
  notify_reason : string;
  consume_nonce : bool;
  discard_effects : bool;
}

type deploy_payload =
  | Deploy_ready of {
      bytecode_raw : string;
      bytecode : Contract_vm.instr array;
    }
  | Deploy_invalid_bytecode of string
  | Deploy_address_mismatch

type deploy_reject = {
  deploy_error_type : string;
  deploy_log_reason : string;
  deploy_notify_reason : string;
  deploy_consume_nonce : bool;
}

type deploy_fee_plan =
  | Deploy_fee_ready
  | Deploy_fee_rejected of deploy_reject

type deploy_input_plan =
  | Deploy_input_ready of {
      bytecode_raw : string;
      bytecode : Contract_vm.instr array;
    }
  | Deploy_input_rejected of deploy_reject
  | Deploy_input_exception of string

type multi_exec_step =
  | Multi_exec_ready of {
      remaining_effort : int;
      value_effect : value_effect;
    }
  | Multi_exec_effort_exhausted
  | Multi_exec_insufficient_value

let z_of_json = function
  | `String s -> Z.of_string s
  | `Int n -> Z.of_int n
  | `Intlit s -> Z.of_string s
  | `Null -> Z.zero
  | _ -> Z.zero

let value_transfer ~from_addr ~target ~amount ~balance =
  if Z.leq amount Z.zero then
    No_value
  else if Z.geq balance amount then
    Transfer { from_addr; target; amount }
  else
    Insufficient

let value_effect ~from_addr ~target ~amount ~balance =
  match value_transfer ~from_addr ~target ~amount ~balance with
  | No_value ->
    Value_noop
  | Insufficient ->
    Value_reject
  | Transfer transfer ->
    Value_apply {
      deltas = [
        { addr = transfer.from_addr; delta = Z.neg transfer.amount };
        { addr = transfer.target; delta = transfer.amount };
      ];
      journal = {
        from_addr = transfer.from_addr;
        target = transfer.target;
        amount = transfer.amount;
      };
    }

let effort_limit ou =
  max 1_000_000
    (try Z.to_int ou with _ -> 1_000_000)

let parse_params_json params_json =
  match Yojson.Safe.from_string params_json with
  | `List values -> values
  | _ -> []

let parse_direct_call ~method_name ~params_json =
  match method_name, params_json with
  | Some method_name, Some params_json ->
    Some { method_name; params = parse_params_json params_json }
  | _ -> None

let plan_direct_exec ~method_name ~params_json ~from_addr ~target ~amount ~balance ~ou =
  match parse_direct_call ~method_name ~params_json with
  | None ->
    Direct_exec_invalid_format
  | Some call ->
    match value_effect ~from_addr ~target ~amount ~balance with
    | Value_reject ->
      Direct_exec_insufficient_value
    | effect ->
      Direct_exec_ready {
        method_name = call.method_name;
        params = call.params;
        amount;
        value_effect = effect;
        effort_limit = effort_limit ou;
      }

let direct_exec_subject = function
  | Circle_exec -> "circle"
  | Program_exec -> "contract"

let direct_exec_invalid_notify = function
  | Circle_exec -> "Invalid circle call format"
  | Program_exec -> "Invalid contract call format"

let direct_exec_invalid_consumes_nonce = function
  | Circle_exec -> false
  | Program_exec -> true

let direct_exec_reject domain = function
  | Direct_exec_ready _ ->
    invalid_arg "direct exec ready is not a reject"
  | Direct_exec_insufficient_value ->
    {
      error_type = "insufficient_balance";
      log_reason = "insufficient balance for " ^ direct_exec_subject domain ^ " value";
      notify_reason = "Insufficient balance for value";
      consume_nonce = true;
      discard_effects = true;
    }
  | Direct_exec_invalid_format ->
    {
      error_type = "malformed_transaction";
      log_reason = "invalid " ^ direct_exec_subject domain ^ " call format";
      notify_reason = direct_exec_invalid_notify domain;
      consume_nonce = direct_exec_invalid_consumes_nonce domain;
      discard_effects = false;
    }

let plan_multi_exec_step ~from_addr ~(call : call) ~effort_used ~effort_limit ~balance =
  if effort_used >= effort_limit then
    Multi_exec_effort_exhausted
  else
    match value_effect ~from_addr ~target:call.target ~amount:call.amount ~balance with
    | Value_reject ->
      Multi_exec_insufficient_value
    | effect ->
      Multi_exec_ready {
        remaining_effort = max 1 (effort_limit - effort_used);
        value_effect = effect;
      }

let parse_deploy_params = function
  | Some params_json ->
    begin
      try parse_params_json params_json
      with _ -> []
    end
  | None -> []

let params_json = function
  | Some (`List values) -> values
  | _ -> []

let bool_json ~default = function
  | Some (`Bool value) -> value
  | _ -> default

let default_readonly_caller =
  "oct00000000000000000000000000000000000000000000"

let readonly_storage_default method_name =
  not (String.equal method_name "balance_of")

let plan_readonly_call ~method_name ~params ~caller_addr ~include_storage =
  {
    readonly_method_name = method_name;
    readonly_params = params_json params;
    readonly_caller_addr = Option.value caller_addr ~default:default_readonly_caller;
    readonly_include_storage =
      bool_json ~default:(readonly_storage_default method_name) include_storage;
  }

let parse_deploy_payload_with_keys ~trusted ~bytecode_b64 ~deployer ~nonce ~target =
  let bytecode_raw = Base64.decode_exn bytecode_b64 in
  match Admission.decode_deploy ~trusted bytecode_raw with
  | Error err -> Deploy_invalid_bytecode (Admission.error_message err)
  | Ok admitted ->
    let bytecode = Admission.code admitted in
    let expected_addr = Contract.addr_from_code bytecode_raw deployer nonce in
    if not (String.equal expected_addr target) then
      Deploy_address_mismatch
    else
      Deploy_ready { bytecode_raw; bytecode }

let parse_deploy_payload ~bytecode_b64 ~deployer ~nonce ~target =
  parse_deploy_payload_with_keys
    ~trusted:[]
    ~bytecode_b64
    ~deployer
    ~nonce
    ~target

let deploy_missing_bytecode_reject = {
  deploy_error_type = "missing_bytecode";
  deploy_log_reason = "no bytecode in transaction";
  deploy_notify_reason = "Missing bytecode";
  deploy_consume_nonce = true;
}

let deploy_sender_missing_reject = {
  deploy_error_type = "sender_not_found";
  deploy_log_reason = "sender account does not exist";
  deploy_notify_reason = "Sender not found";
  deploy_consume_nonce = false;
}

let deploy_fee_insufficient_reject = {
  deploy_error_type = "insufficient_balance";
  deploy_log_reason = "insufficient balance for deploy fee";
  deploy_notify_reason = "Insufficient balance";
  deploy_consume_nonce = false;
}

let plan_deploy_fee ~balance ~fee =
  match balance with
  | None ->
    Deploy_fee_rejected deploy_sender_missing_reject
  | Some balance when Z.lt balance fee ->
    Deploy_fee_rejected deploy_fee_insufficient_reject
  | Some _ ->
    Deploy_fee_ready

let deploy_payload_reject = function
  | Deploy_ready _ ->
    invalid_arg "deploy ready is not a reject"
  | Deploy_invalid_bytecode err ->
    {
      deploy_error_type = "invalid_bytecode";
      deploy_log_reason = err;
      deploy_notify_reason = "Invalid bytecode: " ^ err;
      deploy_consume_nonce = false;
    }
  | Deploy_address_mismatch ->
    {
      deploy_error_type = "contract_address_mismatch";
      deploy_log_reason = "expected address does not match";
      deploy_notify_reason = "Contract address mismatch";
      deploy_consume_nonce = false;
    }

let plan_deploy_input_with_keys ~trusted ~bytecode_b64_opt ~deployer ~nonce ~target =
  match bytecode_b64_opt with
  | None ->
    Deploy_input_rejected deploy_missing_bytecode_reject
  | Some bytecode_b64 ->
    try
      match parse_deploy_payload_with_keys
        ~trusted ~bytecode_b64 ~deployer ~nonce ~target with
      | Deploy_ready deploy ->
        Deploy_input_ready {
          bytecode_raw = deploy.bytecode_raw;
          bytecode = deploy.bytecode;
        }
      | (Deploy_invalid_bytecode _ | Deploy_address_mismatch) as rejected ->
        Deploy_input_rejected (deploy_payload_reject rejected)
    with _ ->
      Deploy_input_exception "Program deploy input failed"

let plan_deploy_input ~bytecode_b64_opt ~deployer ~nonce ~target =
  plan_deploy_input_with_keys
    ~trusted:[]
    ~bytecode_b64_opt
    ~deployer
    ~nonce
    ~target

let parse_call index = function
  | `Assoc fields ->
    let str name =
      match List.assoc_opt name fields with
      | Some (`String value) when value <> "" -> Ok value
      | _ -> Error (Printf.sprintf "multi_exec call %d missing %s" index name)
    in
    let params =
      match List.assoc_opt "params" fields with
      | Some (`List values) -> Ok values
      | Some `Null | None -> Ok []
      | _ -> Error (Printf.sprintf "multi_exec call %d params must be list" index)
    in
    let amount =
      match List.assoc_opt "amount" fields with
      | Some value -> z_of_json value
      | None -> Z.zero
    in
    (match str "to", str "method", params with
    | Ok target, Ok method_name, Ok params ->
      if not (Octra_core.Crypto.Address.is_valid_address target) then
        Error (Printf.sprintf "multi_exec call %d invalid program address" index)
      else if Z.sign amount < 0 then
        Error (Printf.sprintf "multi_exec call %d amount must not be negative" index)
      else
        Ok { target; method_name; params; amount }
    | Error e, _, _
    | _, Error e, _
    | _, _, Error e -> Error e)
  | _ -> Error (Printf.sprintf "multi_exec call %d must be object" index)

let parse_multi_exec_calls ~max_calls message =
  try
    let json = Yojson.Safe.from_string message in
    let call_items =
      match json with
      | `List calls -> Ok calls
      | `Assoc fields ->
        (match List.assoc_opt "calls" fields with
        | Some (`List calls) -> Ok calls
        | _ -> Error "multi_exec requires calls list")
      | _ -> Error "multi_exec requires calls list"
    in
    match call_items with
    | Error e -> Error e
    | Ok [] -> Error "multi_exec requires at least one call"
    | Ok call_items when List.length call_items > max_calls ->
      Error (Printf.sprintf "multi_exec call count exceeds %d" max_calls)
    | Ok call_items ->
      let rec loop index acc = function
        | [] -> Ok (List.rev acc)
        | item :: tail ->
          match parse_call index item with
          | Error e -> Error e
          | Ok call -> loop (index + 1) (call :: acc) tail
      in
      loop 0 [] call_items
  with _ -> Error "multi_exec payload is invalid"