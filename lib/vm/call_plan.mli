(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


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

val z_of_json :
  Yojson.Safe.t ->
  Z.t

val value_transfer :
  from_addr:string ->
  target:string ->
  amount:Z.t ->
  balance:Z.t ->
  value_transfer

val value_effect :
  from_addr:string ->
  target:string ->
  amount:Z.t ->
  balance:Z.t ->
  value_effect

val effort_limit :
  Z.t ->
  int

val parse_direct_call :
  method_name:string option ->
  params_json:string option ->
  direct_call option

val plan_direct_exec :
  method_name:string option ->
  params_json:string option ->
  from_addr:string ->
  target:string ->
  amount:Z.t ->
  balance:Z.t ->
  ou:Z.t ->
  direct_exec_plan

val direct_exec_reject :
  direct_exec_domain ->
  direct_exec_plan ->
  direct_exec_reject

val plan_multi_exec_step :
  from_addr:string ->
  call:call ->
  effort_used:int ->
  effort_limit:int ->
  balance:Z.t ->
  multi_exec_step

val parse_deploy_params :
  string option ->
  Yojson.Safe.t list

val params_json :
  Yojson.Safe.t option ->
  Yojson.Safe.t list

val bool_json :
  default:bool ->
  Yojson.Safe.t option ->
  bool

val plan_readonly_call :
  method_name:string ->
  params:Yojson.Safe.t option ->
  caller_addr:string option ->
  include_storage:Yojson.Safe.t option ->
  readonly_call

val parse_deploy_payload :
  bytecode_b64:string ->
  deployer:string ->
  nonce:int ->
  target:string ->
  deploy_payload

val deploy_missing_bytecode_reject :
  deploy_reject

val plan_deploy_fee :
  balance:Z.t option ->
  fee:Z.t ->
  deploy_fee_plan

val deploy_payload_reject :
  deploy_payload ->
  deploy_reject

val plan_deploy_input :
  bytecode_b64_opt:string option ->
  deployer:string ->
  nonce:int ->
  target:string ->
  deploy_input_plan

val parse_multi_exec_calls :
  max_calls:int ->
  string ->
  (call list, string) result