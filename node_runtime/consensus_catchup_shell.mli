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


type queued = {
  target_epoch : int64;
  reason : string;
}

type queue_event = {
  queued_target_epoch : int64;
  queued_reason : string;
  queued_active : bool;
  queued_label : string;
}

type deps = {
  catchup_active : unit -> bool;
  set_catchup_active : bool -> unit;
  queue_target : target_epoch:int64 -> reason:string -> string;
  committed_head_epoch : unit -> int;
  start_height : int64 -> unit Lwt.t;
  take_queued_after : head:int64 -> queued option;
  clear_queue : unit -> unit;
  read_local_root : unit -> string Lwt.t;
  set_state_attested : head:int -> root:string -> unit;
  clear_quarantine : string -> unit;
  mark_quarantine : string -> unit;
  observer : bool;
  drain_pending_finalized : unit -> unit Lwt.t;
  wake_ready : unit -> unit Lwt.t;
}

type run_one =
  target_epoch:int64 ->
  reason:string ->
  finish_success:(string -> unit Lwt.t) ->
  fail_catchup:(string -> unit Lwt.t) ->
  unit Lwt.t

type range_plan = {
  remain : int;
  max_epochs : int;
  timeout_seconds : float;
  log_progress : bool;
}

type query_deps = {
  sleep : float -> unit Lwt.t;
  query_range :
    from_epoch:int64 ->
    max_epochs:int ->
    timeout_seconds:float ->
    validate:(Octra_consensus.C_driver.catchup_range_response_record -> bool) ->
    Octra_consensus.C_driver.catchup_range_response_record option Lwt.t;
}

type chunk_query_deps = {
  env_timeout : unit -> string option;
  read_query_root : unit -> string Lwt.t;
  range_query : query_deps;
}

type gate_action =
  | Gate_continue
  | Gate_finish of string
  | Gate_retry
  | Gate_fail of string

type apply_gate =
  | Apply_chunk of Octra_consensus.C_driver.catchup_range_response_record
  | Apply_finish of string
  | Apply_retry
  | Apply_fail of string

type chunk_query_step =
  | Query_chunk of Octra_consensus.C_driver.catchup_range_response_record
  | Query_failed of string

type chunk_apply_step =
  | Chunk_continue of Octra_consensus.C_driver.catchup_range_response_record
  | Chunk_finish of string
  | Chunk_retry
  | Chunk_fail of string

type record_apply_action =
  | Record_apply
  | Record_skip_applied
  | Record_retry_moved_head of string

type validated_record = {
  record : Octra_consensus.C_codec.catchup_epoch_record;
  parsed_txs : Octra_core.Transaction.t list;
  parsed_tx_hashes : string list;
  expected_eic : string;
  expected_txid : int64;
  proposer : Octra_core.Epochlog.proposer_info option;
  expected_root : string option;
  apply_action : record_apply_action;
}

type cached_apply_head = {
  cached_root : string;
  cached_eic : string option;
}

type apply_point_source = {
  head_epoch : unit -> int;
  read_root : unit -> string Lwt.t;
  cached_head : unit -> cached_apply_head;
  next_txid : unit -> int64;
}

type record_apply_deps = {
  head_before_record : unit -> int;
  put_proposer : int -> Octra_core.Epochlog.proposer_info -> unit;
  put_expected_root : int -> string -> unit;
  activate_gap : unit -> unit;
  point_source : apply_point_source;
  write_finality :
    Octra_consensus.C_codec.catchup_epoch_record ->
    unit;
  apply_record :
    validated_record ->
    unit Lwt.t;
}

type chunk_apply_deps = {
  read_local_root : unit -> string Lwt.t;
  base_eic : unit -> string;
  next_txid : unit -> int64;
  current_head : unit -> int64;
  gap_active : unit -> bool;
  record_apply : record_apply_deps;
}

type target_deps = {
  normalize : source:string -> unit;
  head_epoch : unit -> int;
  query : chunk_query_deps;
  apply : chunk_apply_deps;
}

type target_wiring = {
  normalize : source:string -> unit;
  head_epoch : unit -> int;
  env_timeout : unit -> string option;
  read_query_root : unit -> string Lwt.t;
  range_query : query_deps;
  read_apply_root : unit -> string Lwt.t;
  cached_head : unit -> cached_apply_head;
  next_txid : unit -> int64;
  put_proposer : int -> Octra_core.Epochlog.proposer_info -> unit;
  put_expected_root : int -> string -> unit;
  activate_gap : unit -> unit;
  write_finality :
    Octra_consensus.C_codec.catchup_epoch_record ->
    unit;
  apply_record :
    validated_record ->
    unit Lwt.t;
  read_local_root : unit -> string Lwt.t;
  base_eic : unit -> string;
  current_head : unit -> int64;
  gap_active : unit -> bool;
}

type node_deps_wiring = {
  catchup_active : bool ref;
  queue : Consensus_catchup_queue.t;
  committed_head_epoch : unit -> int;
  start_height : int64 -> unit Lwt.t;
  read_local_root : unit -> string Lwt.t;
  set_state_attested : head:int -> root:string -> unit;
  clear_quarantine : string -> unit;
  mark_quarantine : string -> unit;
  observer : bool;
  drain_pending_finalized : unit -> unit Lwt.t;
  wake_ready : unit -> unit Lwt.t;
}

type node_target_wiring = {
  normalize : source:string -> unit;
  head_epoch : unit -> int;
  env_timeout : unit -> string option;
  read_local_root : unit -> string Lwt.t;
  range_query : query_deps;
  cached_head : unit -> cached_apply_head;
  next_txid : unit -> int64;
  finality : Consensus_finality_state.callbacks;
  queue : Consensus_catchup_queue.t;
  write_finality :
    Octra_consensus.C_codec.catchup_epoch_record ->
    unit;
  apply_record :
    validated_record ->
    unit Lwt.t;
  base_eic : unit -> string;
  current_head : unit -> int64;
}

type driver_io = {
  start_height : int64 -> unit Lwt.t;
  wake_ready : unit -> unit Lwt.t;
  range_query : query_deps;
}

type driver_runner_wiring = {
  catchup_active : bool ref;
  queue : Consensus_catchup_queue.t;
  committed_head_epoch : unit -> int;
  normalize : source:string -> unit;
  env_timeout : unit -> string option;
  read_local_root : unit -> string Lwt.t;
  cached_head : unit -> cached_apply_head;
  next_txid : unit -> int64;
  finality : Consensus_finality_state.callbacks;
  write_finality :
    Octra_consensus.C_codec.catchup_epoch_record ->
    unit;
  apply_record :
    validated_record ->
    unit Lwt.t;
  base_eic : unit -> string;
  current_head : unit -> int64;
  set_state_attested : head:int -> root:string -> unit;
  clear_quarantine : string -> unit;
  mark_quarantine : string -> unit;
  observer : bool;
  drain_pending_finalized : unit -> unit Lwt.t;
}

val range_plan :
  env_timeout:string option ->
  from_epoch:int64 ->
  target_epoch:int64 ->
  range_plan

val query_range :
  query_deps ->
  attempts:int ->
  retry_delay:float ->
  from_epoch:int64 ->
  max_epochs:int ->
  timeout_seconds:float ->
  reason:string ->
  validate:(Octra_consensus.C_driver.catchup_range_response_record -> bool) ->
  Octra_consensus.C_driver.catchup_range_response_record option Lwt.t

val query_chunk :
  chunk_query_deps ->
  target_epoch:int64 ->
  from_epoch:int64 ->
  reason:string ->
  chunk_query_step Lwt.t

val base_gate :
  target_epoch:int64 ->
  start_head:int64 ->
  current_head:int64 ->
  from_epoch:int64 ->
  reason:string ->
  head:Octra_consensus.C_catchup.apply_point ->
  gate_action

val continuity_gate :
  records:Octra_consensus.C_codec.catchup_epoch_record list ->
  from_epoch:int64 ->
  prev_root:string ->
  reason:string ->
  gate_action

val final_apply_gate :
  last_epoch:int64 ->
  expected_root:string ->
  expected_eic:string ->
  expected_txid:int64 ->
  head:Octra_consensus.C_catchup.apply_point ->
  Octra_consensus.C_driver.catchup_range_response_record ->
  (Octra_consensus.C_driver.catchup_range_response_record, string) result

val validate_record :
  prev_eic:string ->
  start_txid:int64 ->
  head_before_record:int ->
  Octra_consensus.C_codec.catchup_epoch_record ->
  (validated_record, string) result

val read_apply_point :
  apply_point_source ->
  Octra_consensus.C_catchup.apply_point Lwt.t

val cached_apply_point :
  apply_point_source ->
  Octra_consensus.C_catchup.apply_point

val apply_chunk_records :
  record_apply_deps ->
  prev_eic:string ->
  start_txid:int64 ->
  Octra_consensus.C_driver.catchup_range_response_record ->
  (Octra_consensus.C_driver.catchup_range_response_record, string) result Lwt.t

val apply_result_gate :
  gap_active:bool ->
  target_epoch:int64 ->
  start_head:int64 ->
  current_head:int64 ->
  from_epoch:int64 ->
  reason:string ->
  (Octra_consensus.C_driver.catchup_range_response_record, string) result ->
  apply_gate

val apply_chunk_gate :
  chunk_apply_deps ->
  target_epoch:int64 ->
  start_head:int64 ->
  from_epoch:int64 ->
  reason:string ->
  Octra_consensus.C_driver.catchup_range_response_record ->
  chunk_apply_step Lwt.t

val run_target :
  target_deps ->
  target_epoch:int64 ->
  reason:string ->
  finish_success:(string -> unit Lwt.t) ->
  fail_catchup:(string -> unit Lwt.t) ->
  unit Lwt.t

val run :
  deps ->
  run_one:run_one ->
  target_epoch:int64 ->
  reason:string ->
  unit Lwt.t

val run_with_target :
  deps ->
  target:target_deps ->
  target_epoch:int64 ->
  reason:string ->
  unit Lwt.t

val target_of_wiring :
  target_wiring ->
  target_deps

val node_deps :
  node_deps_wiring ->
  deps

val target_wiring_of_node :
  node_target_wiring ->
  target_wiring

val driver_io_of_driver :
  Octra_consensus.C_driver.t ->
  driver_io

val run_driver_wired :
  driver_runner_wiring ->
  driver_io ->
  target_epoch:int64 ->
  reason:string ->
  unit Lwt.t

val queue_target_event :
  Consensus_catchup_queue.t ->
  active:bool ->
  target_epoch:int64 ->
  reason:string ->
  queue_event

val queue_gap_event :
  Consensus_catchup_queue.t ->
  active:bool ->
  target_epoch:int64 ->
  reason:string ->
  queue_event

val log_queue_event :
  queue_event ->
  unit

val run_wired :
  deps ->
  target:target_wiring ->
  target_epoch:int64 ->
  reason:string ->
  unit Lwt.t