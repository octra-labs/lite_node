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


val planned_txid_hi :
  next_txid:int64 ->
  confirmed_count:int ->
  Octra_core.Head_manifest.t option ->
  int64

val prev_epoch_index_root :
  Octra_core.Head_manifest.t option ->
  string

type index_plan = {
  start_txid : int64;
  planned_txid_hi : int64;
  epoch_index_hash : string;
  epoch_index_root : string;
}

val index_plan :
  next_txid:int64 ->
  confirmed_count:int ->
  confirmed_txs:Octra_core.Transaction.t list ->
  epoch_id:int ->
  Octra_core.Head_manifest.t option ->
  index_plan

val prepare_record :
  commit_id:string ->
  prev_generation:int ->
  epoch_id:int ->
  planned_txid_hi:int64 ->
  planned_state_root:string ->
  ts:float ->
  Octra_core.Commit_journal.record

val commit_record :
  commit_id:string ->
  generation:int ->
  ts:float ->
  Octra_core.Commit_journal.record

val wal_entry :
  epoch_id:int ->
  pre_state_root:string ->
  post_state_root:string ->
  parent_commit:string ->
  start_txid:int64 ->
  tx_count:int ->
  finalized_by:string ->
  finalized_at:float ->
  irmin_last_epoch_before:int ->
  Octra_core.Wal.entry

type boundary_log = {
  level : [ `Info | `Warn ];
  line : string;
}

type commit_boundary = {
  boundary : Octra_core.Epoch_boundary.plan;
  irmin_last_before : int;
  log : boundary_log option;
}

type rollback_offsets = {
  head_epoch : int;
  txlog_seg : int;
  txlog_off : int;
  epochlog_off : int;
}

type rollback_refusal = {
  head_epoch : int;
  commit_epoch : int;
}

type rollback_ok = {
  tx_loc : int;
  epoch_meta : int;
  addr_tx : int;
  txid_loc : int;
}

type rollback_plan =
  | Rollback_to_head of rollback_offsets
  | Rollback_missing_offsets
  | Rollback_refused of rollback_refusal
  | Rollback_missing_head

type rollback_effects = {
  rollback_to_head : rollback_offsets -> int * int * int * int;
  delete_wal : unit -> unit;
  clear_marker : unit -> unit;
  log : string -> unit;
}

type commit_progress

type failure_effects = {
  rollback : unit -> bool;
  abort_chaindata : unit -> unit;
  abort_irmin : unit -> unit;
  log : string -> unit;
  exit_later : unit -> unit Lwt.t;
}

val boundary_log :
  pre_state_hash:string ->
  irmin_last_before_meta:int ->
  head_before_commit:Octra_core.Head_manifest.t option ->
  Octra_core.Epoch_boundary.plan ->
  boundary_log option

val commit_boundary :
  commit_epoch:int ->
  pre_state_hash:string ->
  irmin_last_before_meta:int ->
  head_before_commit:Octra_core.Head_manifest.t option ->
  commit_boundary

val rollback_plan :
  commit_epoch:int ->
  boundary:Octra_core.Epoch_boundary.plan ->
  Octra_core.Head_manifest.t option ->
  rollback_plan

val rollback_start_log : rollback_offsets -> string

val rollback_ok_log : rollback_ok -> string

val rollback_failed_log : string -> string

val rollback_refused_log : rollback_refusal -> string

val rollback_unavailable_log : string

val commit_failed_logs : epoch_id:int -> reason:string -> string list

val commit_progress : unit -> commit_progress

val mark_chaindata_committed : commit_progress -> unit

val mark_irmin_commit_started : commit_progress -> unit

val mark_head_committed : commit_progress -> unit

val rollback_needed : commit_progress -> bool

val handle_failure :
  progress:commit_progress ->
  epoch_id:int ->
  effects:failure_effects ->
  exn ->
  'a Lwt.t

val history_incomplete_lines :
  epoch_id:int ->
  start_txid:int64 ->
  tx_count:int ->
  Octra_core.Store_chaindata.epoch_index_status ->
  string list

val run_rollback : effects:rollback_effects -> rollback_plan -> bool

val epoch_header :
  epoch_id:int ->
  state_root:string ->
  prev_state_root:string ->
  parent_commit:string ->
  start_txid:int64 ->
  tx_count:int ->
  finalized_by:string ->
  finalized_at:float ->
  proposer:Octra_core.Epochlog.proposer_info ->
  confirmed_fees:Z.t ->
  plan:Octra_core.Epoch_exec.reward_plan ->
  reward_recipients:Octra_core.Epochlog.reward_recipient list ->
  Octra_core.Epochlog.epoch_header

val head_manifest :
  generation:int ->
  state_root:string ->
  ledger_root:string ->
  irmin_commit:string option ->
  txid_hi:int64 ->
  txlog_seg:int ->
  txlog_off:int ->
  epochlog_off:int ->
  commit_id:string ->
  ts:float ->
  epoch_index_hash:string ->
  epoch_index_root:string ->
  Octra_core.Head_manifest.t