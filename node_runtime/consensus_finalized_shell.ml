(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C_types = Octra_consensus.C_types
module Flow = Consensus_finalized_flow
module Log = Octra_log

type 'driver deps = {
  prune_frozen : finalized_epoch:int64 -> unit;
  store_proposer : Flow.proposer_info -> unit;
  store_expected_root : epoch:int -> root:string -> unit;
  store_finalized : epoch:int -> C_types.finalize -> unit;
  remove_finalized : epoch:int -> unit;
  remove_proposer : epoch:int -> unit;
  committed_head_epoch : unit -> int;
  driver : unit -> 'driver option;
  observer_mode : bool;
  catchup_active : unit -> bool;
  quarantine_active : unit -> bool;
  state_attested : unit -> bool;
  current_epoch : unit -> int;
  read_local_root_raw : unit -> string Lwt.t;
  queue_catchup_target : target_epoch:int64 -> reason:string -> unit;
  run_catchup_to_target :
    'driver ->
    target_epoch:int64 ->
    reason:string ->
    unit Lwt.t;
  mark_quarantine : string -> unit;
  clear_quarantine : string -> unit;
  apply_finalized : C_types.finalize -> unit Lwt.t;
  replay_stashed_while_safe : source:string -> unit Lwt.t;
}

let log_notice ~header_epoch ~head = function
  | Flow.No_notice -> ()
  | Flow.Finality_gap_required ->
    Log.warn "finality"
      "finalized height = %d head = %d requires catchup"
      header_epoch head
  | Flow.Observer_queued ->
    Log.warn "finality"
      "observer queued finalized height = %d head = %d for batched catchup"
      header_epoch head

let remove_stashed deps epoch =
  deps.remove_finalized ~epoch;
  deps.remove_proposer ~epoch

let record_finalized deps finalize (header : C_types.epoch_header) header_epoch =
  deps.prune_frozen ~finalized_epoch:header.C_types.epoch_id;
  (match Flow.proposer_info
           ~epoch:header_epoch
           ~creator_addr:header.C_types.creator_addr
           ~commit_round:finalize.C_types.commit_round with
   | Some info -> deps.store_proposer info
   | None -> ());
  (match Flow.expected_root header.C_types.proposed_state_root with
   | Some root -> deps.store_expected_root ~epoch:header_epoch ~root
   | None -> ());
  deps.store_finalized ~epoch:header_epoch finalize

let run_catchup_plan deps header_epoch (header : C_types.epoch_header)
    (catchup : Flow.catchup_plan) driver_opt =
  log_notice ~header_epoch ~head:(deps.committed_head_epoch ()) catchup.Flow.notice;
  match driver_opt with
  | Some driver ->
    if catchup.Flow.drop_stashed then remove_stashed deps header_epoch;
    let target_epoch = header.C_types.epoch_id in
    if catchup.Flow.queue_before_catchup then
      deps.queue_catchup_target ~target_epoch ~reason:catchup.reason;
    deps.run_catchup_to_target driver ~target_epoch ~reason:catchup.reason
  | None -> Lwt.return_unit

let run_stashed_plan deps finalize (header : C_types.epoch_header)
    header_epoch phase =
  let open Lwt.Syntax in
  let phase_label = Flow.phase_label phase in
  Log.info "consensus"
    "stashed on_finalized during %s epoch = %Ld (will replay after recovery)"
    phase_label header.C_types.epoch_id;
  let* local_root = deps.read_local_root_raw () in
  let prev_root_matches = header.C_types.prev_state_root = local_root in
  match Flow.stashed_plan
          ~phase
          ~header_epoch
          ~current_epoch:(deps.current_epoch ())
          ~prev_root_matches
          ~state_attested:(deps.state_attested ())
          ~catchup_active:(deps.catchup_active ()) with
  | Flow.Direct_apply_stashed { clear_reason } ->
    Log.warn "consensus"
      "directly applying stashed finalized epoch = %d during %s (prev_root matches local HEAD)"
      header_epoch phase_label;
    let* () = deps.apply_finalized finalize in
    deps.clear_quarantine clear_reason;
    Lwt.return_unit
  | Flow.Wait_stashed ->
    Lwt.return_unit
  | Flow.Replay_stashed ->
    deps.replay_stashed_while_safe ~source:"on_finalized_quarantine"

let handle deps finalize =
  let header : C_types.epoch_header = finalize.C_types.header in
  let header_epoch = Int64.to_int header.C_types.epoch_id in
  record_finalized deps finalize header header_epoch;
  let head = deps.committed_head_epoch () in
  let driver_opt = deps.driver () in
  let plan =
    Flow.plan
      ~head
      ~header_epoch
      ~observer:deps.observer_mode
      ~driver_available:(driver_opt <> None)
      ~catchup_active:(deps.catchup_active ())
      ~quarantine_active:(deps.quarantine_active ())
      ~state_attested:(deps.state_attested ())
  in
  match plan with
  | Flow.Drop_duplicate ->
    Log.warn "finality"
      "drop duplicate finalized height = %d head = %d"
      header_epoch head;
    deps.remove_finalized ~epoch:header_epoch;
    Lwt.return_unit
  | Flow.Run_catchup catchup ->
    run_catchup_plan deps header_epoch header catchup driver_opt
  | Flow.Quarantine_no_driver no_driver ->
    log_notice ~header_epoch ~head no_driver.Flow.notice;
    deps.mark_quarantine no_driver.reason;
    Lwt.return_unit
  | Flow.Apply_now ->
    deps.apply_finalized finalize
  | Flow.Stash phase ->
    run_stashed_plan deps finalize header header_epoch phase