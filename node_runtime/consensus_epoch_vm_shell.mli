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


module Contract = Octra_vm.Contract
module ContractVM = Octra_vm.Contract_vm
module Call_plan = Octra_vm.Call_plan
module Direct_exec = Octra_vm.Direct_exec
module Multi_exec = Octra_vm.Multi_exec
module Receipt_view = Octra_vm.Receipt_view
module Transaction = Octra_core.Transaction
module Circle_exec = Octra_circle_runtime.Circle_exec

type ('journal_snapshot, 'pending_snapshot) deps = {
  get_balance : string -> Z.t;
  transfer : from_addr:string -> to_addr:string -> amount:Z.t -> bool;
  snapshot_journal : unit -> 'journal_snapshot;
  restore_journal : 'journal_snapshot -> unit;
  snapshot_pending : unit -> 'pending_snapshot;
  restore_pending : 'pending_snapshot -> unit;
  execute_call :
    ctx:ContractVM.exec_ctx ->
    depth:int ->
    target:string ->
    method_name:string ->
    params:Yojson.Safe.t list ->
    caller:string ->
    amount:Z.t ->
    Contract.exec_result;
  deploy_internal :
    ctx:ContractVM.exec_ctx ->
    depth:int ->
    params:ContractVM.v list ->
    deployer:string ->
    bytecode_raw:string ->
    nonce:int ->
    (ContractVM.spawn_result, string) result;
  get_fhe_pubkey : string -> Pvac_ffi.pubkey option;
  current_epoch : int;
  tree_hash : string;
  node_id : string;
  tx_hash : string;
}

type deploy_result = {
  contract_addr : string;
  receipt : Contract.exec_result;
}

type receipt_deps = {
  save :
    tx_hash:string ->
    contract_addr:string ->
    method_name:string ->
    success:bool ->
    effort_used:int ->
    events_json:Yojson.Safe.t ->
    error:string option ->
    epoch_id:int ->
    unit;
  epoch : unit -> int;
}

type tx_reject =
  ?consume_nonce:bool ->
  ?notify_reason:string ->
  string ->
  string ->
  unit Lwt.t

type multi_exec_deps = {
  with_debited_fee : Z.t -> (unit -> unit Lwt.t) -> unit Lwt.t;
  make_ctx : string -> ContractVM.exec_ctx;
  balance : string -> Z.t;
  apply_value_effect : Call_plan.value_effect -> unit;
  execute_call :
    ctx:ContractVM.exec_ctx ->
    limit:int ->
    target:string ->
    method_name:string ->
    params:Yojson.Safe.t list ->
    caller:string ->
    amount:Z.t ->
    Contract.exec_result;
  save_receipt_raw : tx_hash:string -> json:string -> unit;
  commit_effects : unit -> unit;
  log_success : calls:int -> effort:int -> unit;
  log_failed : string -> unit;
  reject_malformed : string -> unit Lwt.t;
  reject_after_fee : Z.t -> string -> string -> unit Lwt.t;
  confirm : unit -> unit Lwt.t;
  now : unit -> float;
}

type 'result direct_call_deps = {
  with_debited_fee : Z.t -> (unit -> unit Lwt.t) -> unit Lwt.t;
  make_ctx : string -> ContractVM.exec_ctx;
  balance : string -> Z.t;
  apply_value_effect : Call_plan.value_effect -> unit;
  log_failed : Receipt_view.direct_call_meta -> string -> string -> string -> unit;
  reject_after_fee : Z.t -> string -> string -> unit Lwt.t;
  reject : Call_plan.direct_exec_reject -> unit Lwt.t;
  exec : ctx:ContractVM.exec_ctx -> Call_plan.direct_exec -> 'result Lwt.t;
  receipt_of_result : 'result -> Contract.exec_result;
  save : tx_hash:string -> Call_plan.direct_exec -> 'result -> unit;
  ok : Receipt_view.direct_call_meta -> Call_plan.direct_exec -> 'result -> unit Lwt.t;
}

type circle_call_deps = {
  with_debited_fee : Z.t -> (unit -> unit Lwt.t) -> unit Lwt.t;
  make_ctx : string -> ContractVM.exec_ctx;
  balance : string -> Z.t;
  apply_value_effect : Call_plan.value_effect -> unit;
  log_failed : Receipt_view.direct_call_meta -> string -> string -> string -> unit;
  reject_after_fee : Z.t -> string -> string -> unit Lwt.t;
  reject : Call_plan.direct_exec_reject -> unit Lwt.t;
  exec : ctx:ContractVM.exec_ctx -> Call_plan.direct_exec -> Circle_exec.call_result Lwt.t;
  save : tx_hash:string -> Call_plan.direct_exec -> Circle_exec.call_result -> unit;
  commit : Circle_exec.call_result -> (unit, string) result Lwt.t;
  commit_effects : unit -> unit;
  log_ok : Receipt_view.direct_call_meta -> Call_plan.direct_exec -> Circle_exec.call_result -> unit;
  confirm : unit -> unit Lwt.t;
}

type program_call_deps = {
  with_debited_fee : Z.t -> (unit -> unit Lwt.t) -> unit Lwt.t;
  make_ctx : string -> ContractVM.exec_ctx;
  balance : string -> Z.t;
  apply_value_effect : Call_plan.value_effect -> unit;
  log_failed : Receipt_view.direct_call_meta -> string -> string -> string -> unit;
  reject_after_fee : Z.t -> string -> string -> unit Lwt.t;
  reject : Call_plan.direct_exec_reject -> unit Lwt.t;
  exec : ctx:ContractVM.exec_ctx -> Call_plan.direct_exec -> Contract.exec_result Lwt.t;
  save : tx_hash:string -> Call_plan.direct_exec -> Contract.exec_result -> unit;
  commit_effects : unit -> unit;
  log_ok : Receipt_view.direct_call_meta -> Call_plan.direct_exec -> Contract.exec_result -> unit;
  confirm : unit -> unit Lwt.t;
}

type call_runtime = {
  handle_deploy_reject : Call_plan.deploy_reject -> unit Lwt.t;
  with_debited_fee : Z.t -> (unit -> unit Lwt.t) -> unit Lwt.t;
  make_ctx : string -> ContractVM.exec_ctx;
  balance : string -> Z.t;
  apply_value_effect : Call_plan.value_effect -> unit;
  log_failed : Receipt_view.direct_call_meta -> string -> string -> string -> unit;
  reject_after_fee : Z.t -> string -> string -> unit Lwt.t;
  reject : Call_plan.direct_exec_reject -> unit Lwt.t;
  commit_effects : unit -> unit;
  confirm : unit -> unit Lwt.t;
  log_deployed : string -> int -> unit;
  log_constructor_failed : string -> string -> unit;
}

type vm_tx_deps = {
  runtime : call_runtime;
  deploy_balance : Transaction.t -> Z.t option;
  deploy_and_save :
    Transaction.t ->
    params:Yojson.Safe.t list ->
    bytecode:ContractVM.instr array ->
    bytecode_raw:string ->
    deploy_result;
  ensure_account : string -> unit;
  circle_exec :
    Transaction.t ->
    ctx:ContractVM.exec_ctx ->
    Call_plan.direct_exec ->
    Circle_exec.call_result Lwt.t;
  circle_save :
    Transaction.t ->
    tx_hash:string ->
    Call_plan.direct_exec ->
    Circle_exec.call_result ->
    unit;
  circle_commit :
    Transaction.t ->
    Circle_exec.call_result ->
    (unit, string) result Lwt.t;
  circle_log_ok :
    Transaction.t ->
    Receipt_view.direct_call_meta ->
    Call_plan.direct_exec ->
    Circle_exec.call_result ->
    unit;
  program_exec :
    Transaction.t ->
    ctx:ContractVM.exec_ctx ->
    Call_plan.direct_exec ->
    Contract.exec_result Lwt.t;
  program_save :
    Transaction.t ->
    tx_hash:string ->
    Call_plan.direct_exec ->
    Contract.exec_result ->
    unit;
  program_log_ok :
    Transaction.t ->
    Receipt_view.direct_call_meta ->
    Call_plan.direct_exec ->
    Contract.exec_result ->
    unit;
  multi_execute_call :
    ctx:ContractVM.exec_ctx ->
    limit:int ->
    target:string ->
    method_name:string ->
    params:Yojson.Safe.t list ->
    caller:string ->
    amount:Z.t ->
    Contract.exec_result;
  save_receipt_raw : tx_hash:string -> json:string -> unit;
  reject_malformed : string -> unit Lwt.t;
  max_multi_exec_calls : int;
  epoch : int;
  now : unit -> float;
}

type live_vm_tx_args = {
  runtime : call_runtime;
  ledger : Octra_core.Ledger.t;
  store : Octra_core.Store_irmin.t;
  chaindata : Octra_core.Store_chaindata.t;
  receipt_epoch : unit -> int;
  ctx_for_hash : string -> ContractVM.exec_ctx;
  reject_malformed : string -> unit Lwt.t;
  max_multi_exec_calls : int;
  epoch : int;
  now : unit -> float;
}

type live_contract_ctx_args = {
  journal : Octra_vm.Value_journal.t;
  store : Octra_core.Store_irmin.t;
  get_fhe_pubkey : string -> Pvac_ffi.pubkey option;
  current_epoch : int;
  tree_hash : string;
  node_id : string;
  tx_hash : string;
}

val make_live_vm_tx_deps :
  live_vm_tx_args ->
  vm_tx_deps

val make_live_contract_ctx :
  live_contract_ctx_args ->
  ContractVM.exec_ctx

val make_contract_ctx :
  ('journal_snapshot, 'pending_snapshot) deps ->
  ContractVM.exec_ctx

val direct_exec_spec_of_tx :
  domain:Receipt_view.direct_call_domain ->
  reject_domain:Call_plan.direct_exec_domain ->
  balance:Z.t ->
  Transaction.t ->
  Direct_exec.spec

val run_direct_exec :
  Direct_exec.spec ->
  fee:Z.t ->
  target:string ->
  apply_value_effect:(Call_plan.value_effect -> unit) ->
  log_failed:(Receipt_view.direct_call_meta -> string -> string -> string -> unit) ->
  reject_after_fee:(Z.t -> string -> string -> unit Lwt.t) ->
  reject:(Call_plan.direct_exec_reject -> unit Lwt.t) ->
  exec:(Call_plan.direct_exec -> 'a Lwt.t) ->
  receipt:('a -> Contract.exec_result) ->
  save:(Call_plan.direct_exec -> 'a -> unit) ->
  ok:(Receipt_view.direct_call_meta -> Call_plan.direct_exec -> 'a -> unit Lwt.t) ->
  unit Lwt.t

val run_direct_call :
  'result direct_call_deps ->
  domain:Receipt_view.direct_call_domain ->
  reject_domain:Call_plan.direct_exec_domain ->
  Transaction.t ->
  unit Lwt.t

val run_circle_call_tx :
  circle_call_deps ->
  Transaction.t ->
  unit Lwt.t

val run_program_call_tx :
  program_call_deps ->
  Transaction.t ->
  unit Lwt.t

val make_circle_call_deps :
  call_runtime ->
  exec:(ctx:ContractVM.exec_ctx -> Call_plan.direct_exec -> Circle_exec.call_result Lwt.t) ->
  save:(tx_hash:string -> Call_plan.direct_exec -> Circle_exec.call_result -> unit) ->
  commit:(Circle_exec.call_result -> (unit, string) result Lwt.t) ->
  log_ok:(Receipt_view.direct_call_meta -> Call_plan.direct_exec -> Circle_exec.call_result -> unit) ->
  circle_call_deps

val make_program_call_deps :
  call_runtime ->
  exec:(ctx:ContractVM.exec_ctx -> Call_plan.direct_exec -> Contract.exec_result Lwt.t) ->
  save:(tx_hash:string -> Call_plan.direct_exec -> Contract.exec_result -> unit) ->
  log_ok:(Receipt_view.direct_call_meta -> Call_plan.direct_exec -> Contract.exec_result -> unit) ->
  program_call_deps

val make_multi_exec_deps :
  call_runtime ->
  execute_call:(
    ctx:ContractVM.exec_ctx ->
    limit:int ->
    target:string ->
    method_name:string ->
    params:Yojson.Safe.t list ->
    caller:string ->
    amount:Z.t ->
    Contract.exec_result) ->
  save_receipt_raw:(tx_hash:string -> json:string -> unit) ->
  log_success:(calls:int -> effort:int -> unit) ->
  log_failed:(string -> unit) ->
  reject_malformed:(string -> unit Lwt.t) ->
  now:(unit -> float) ->
  multi_exec_deps

val run_contract_deploy :
  fee:Z.t ->
  balance:Z.t option ->
  bytecode_b64_opt:string option ->
  deployer:string ->
  nonce:int ->
  target:string ->
  message:string option ->
  handle_reject:(Call_plan.deploy_reject -> unit Lwt.t) ->
  with_debited_fee:(Z.t -> (unit -> unit Lwt.t) -> unit Lwt.t) ->
  reject_after_fee:(Z.t -> string -> string -> unit Lwt.t) ->
  deploy_and_save:(
    params:Yojson.Safe.t list ->
    bytecode:ContractVM.instr array ->
    bytecode_raw:string ->
    deploy_result) ->
  ensure_account:(string -> unit) ->
  commit_effects:(unit -> unit) ->
  log_deployed:(string -> int -> unit) ->
  log_constructor_failed:(string -> string -> unit) ->
  confirm:(unit -> unit Lwt.t) ->
  unit Lwt.t

val run_deploy_tx :
  balance:Z.t option ->
  Transaction.t ->
  handle_reject:(Call_plan.deploy_reject -> unit Lwt.t) ->
  with_debited_fee:(Z.t -> (unit -> unit Lwt.t) -> unit Lwt.t) ->
  reject_after_fee:(Z.t -> string -> string -> unit Lwt.t) ->
  deploy_and_save:(
    params:Yojson.Safe.t list ->
    bytecode:ContractVM.instr array ->
    bytecode_raw:string ->
    deploy_result) ->
  ensure_account:(string -> unit) ->
  commit_effects:(unit -> unit) ->
  log_deployed:(string -> int -> unit) ->
  log_constructor_failed:(string -> string -> unit) ->
  confirm:(unit -> unit Lwt.t) ->
  unit Lwt.t

val run_deploy_tx_runtime :
  call_runtime ->
  balance:Z.t option ->
  Transaction.t ->
  deploy_and_save:(
    params:Yojson.Safe.t list ->
    bytecode:ContractVM.instr array ->
    bytecode_raw:string ->
    deploy_result) ->
  ensure_account:(string -> unit) ->
  unit Lwt.t

val run_vm_tx :
  vm_tx_deps ->
  Transaction.t ->
  unit Lwt.t

val run_multi_exec :
  multi_exec_deps ->
  max_calls:int ->
  epoch:int ->
  tx_hash:string ->
  from_addr:string ->
  message:string option ->
  fee:Z.t ->
  unit Lwt.t

val run_multi_exec_tx :
  multi_exec_deps ->
  max_calls:int ->
  epoch:int ->
  Transaction.t ->
  unit Lwt.t

val handle_deploy_reject :
  reject:tx_reject ->
  Call_plan.deploy_reject ->
  unit Lwt.t

val handle_direct_exec_reject :
  discard:(unit -> unit) ->
  reject:tx_reject ->
  Call_plan.direct_exec_reject ->
  unit Lwt.t

val with_debited_fee :
  debit:(string -> Z.t -> int -> (unit, string) result) ->
  reject:(string -> string -> unit Lwt.t) ->
  Transaction.t ->
  Z.t ->
  (unit -> unit Lwt.t) ->
  unit Lwt.t

val reject_after_fee :
  discard_fee:(Z.t -> unit) ->
  reject:tx_reject ->
  Z.t ->
  string ->
  string ->
  unit Lwt.t

val save_receipt :
  receipt_deps ->
  ?program:bool ->
  tx_hash:string ->
  contract_addr:string ->
  method_name:string ->
  Contract.exec_result ->
  unit

val log_deployed : string -> int -> unit
val log_constructor_failed : string -> string -> unit
val log_multi_exec_success : calls:int -> effort:int -> unit
val log_multi_exec_failed : string -> unit

val log_direct_call_ok :
  Receipt_view.direct_call_meta ->
  string ->
  string ->
  int ->
  unit

val log_direct_call_failed :
  Receipt_view.direct_call_meta ->
  string ->
  string ->
  string ->
  unit

val log_program_call_ok :
  string ->
  Receipt_view.direct_call_meta ->
  Call_plan.direct_exec ->
  Contract.exec_result ->
  unit

val log_circle_call_ok :
  string ->
  Receipt_view.direct_call_meta ->
  Call_plan.direct_exec ->
  Circle_exec.call_result ->
  unit

val max_multi_exec_calls : env:(string -> string option) -> int