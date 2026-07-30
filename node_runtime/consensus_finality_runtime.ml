(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Log = Octra_log
module C_codec = Octra_consensus.C_codec

type replay_deps = {
  current_epoch : unit -> int;
  catchup_active : unit -> bool;
  quarantine_active : unit -> bool;
  finality : Consensus_finality_state.callbacks;
  read_local_root_raw : unit -> string Lwt.t;
}

type deps = {
  apply : Consensus_finalized_apply.node_deps;
  replay : replay_deps;
}

type node_deps = {
  check_finality : Octra_consensus.C_types.finalize -> unit;
  write_finality : Octra_consensus.C_types.finalize -> unit;
  persist_finality_certificate :
    Octra_consensus.C_types.finalize ->
    unit;
  persist_finality_bundle :
    Octra_consensus.C_types.finalize ->
    Consensus_finality_journal.bundle ->
    unit;
  chaos_after_finality_log : unit -> unit;
  cached_bundle_for_pid :
    string ->
    (string list * Octra_core.Transaction.t list * string list) option;
  header_has_empty_bundle : Octra_consensus.C_types.epoch_header -> bool;
  store_empty_bundle : Octra_consensus.C_types.epoch_header -> unit;
  driver : unit -> Octra_consensus.C_driver.t option;
  set_proposal : Octra_core.Transaction.t list -> string list -> unit;
  store_proposal_bundle :
    proposal_id:string ->
    tx_hashes:string list ->
    txs:Octra_core.Transaction.t list ->
    receipts_json:string list ->
    unit;
  deactivate_gap : unit -> unit;
  set_consensus_finalized : bool -> unit;
  current_epoch : unit -> int;
  committed_head_epoch : unit -> int;
  sleep : float -> unit Lwt.t;
  read_pre_finalize_root : unit -> string option;
  read_commit_root : unit -> string option Lwt.t;
  read_local_root_raw : unit -> string Lwt.t;
  commit_finality_journal : unit -> unit;
  remove_pending_finalized : epoch:int -> unit;
  apply_timeout_seconds : float;
  bundle_wait_expired : epoch_id:int64 -> unit;
  bundle_wait_recovered : epoch_id:int64 -> unit;
  fatal_exit : unit -> unit;
  catchup_active : unit -> bool;
  quarantine_active : unit -> bool;
  finality : Consensus_finality_state.callbacks;
}

type node_runtime = {
  data_dir : string;
  validator_set : unit -> Octra_consensus.C_types.validator_set;
  bundles : Consensus_bundle_cache.node_runtime;
  driver_ref : Octra_consensus.C_driver.t option ref;
  proposal_state : Consensus_proposal_state.t;
  catchup_queue : Consensus_catchup_queue.t;
  consensus_finalized : bool ref;
  current_epoch : int ref;
  committed_head_epoch : unit -> int;
  sleep : float -> unit Lwt.t;
  read_pre_finalize_root : unit -> string option;
  read_commit_root : unit -> string option Lwt.t;
  read_local_root_raw : unit -> string Lwt.t;
  apply_timeout_seconds : float;
  fatal_exit : unit -> unit;
  catchup_active : bool ref;
  runtime_state : Consensus_runtime_state.t;
  finality : Consensus_finality_state.callbacks;
}

type t = {
  apply_finalized : Octra_consensus.C_types.finalize -> unit Lwt.t;
  drain_pending : unit -> unit Lwt.t;
  replay_stashed_while_safe : source:string -> unit Lwt.t;
}

let create_with_failure deps on_failure =
  let apply_deps = Consensus_finalized_apply.node_deps deps.apply in
  let active_apply = ref None in
  let rec apply_finalized finalize =
    let epoch = finalize.Octra_consensus.C_types.epoch_id in
    let encoded = C_codec.encode_finalize finalize in
    match !active_apply with
    | Some (active_epoch, active_encoded, pending) ->
      if encoded = active_encoded then begin
        Log.info "consensus"
          "event = finalized_apply_join epoch = %Ld"
          epoch;
        pending
      end else if Int64.equal epoch active_epoch then
        Lwt.fail_with "conflicting concurrent finalized apply"
      else
        let open Lwt.Syntax in
        let* () = pending in
        apply_finalized finalize
    | None ->
      let pending, resolver = Lwt.wait () in
      active_apply := Some (epoch, encoded, pending);
      let running =
        Lwt.catch
          (fun () ->
            Consensus_finalized_apply.run
              apply_deps
              finalize)
          (on_failure finalize)
      in
      Lwt.on_any
        running
        (fun () ->
          active_apply := None;
          Lwt.wakeup_later resolver ())
        (fun exn ->
          active_apply := None;
          Lwt.wakeup_later_exn resolver exn);
      pending
  in
  let replay =
    Consensus_finalized_replay.node_runner
      {
        current_epoch = deps.replay.current_epoch;
        catchup_active = deps.replay.catchup_active;
        quarantine_active = deps.replay.quarantine_active;
        finality = deps.replay.finality;
        read_local_root_raw = deps.replay.read_local_root_raw;
        apply_finalized;
      }
  in
  {
    apply_finalized;
    drain_pending = replay.drain_pending;
    replay_stashed_while_safe = replay.replay_stashed_while_safe;
  }

let create deps =
  create_with_failure deps (fun _ exn -> Lwt.fail exn)

let stop_after_fatal () =
  let pending, _ = Lwt.wait () in
  pending

let create_node deps =
  create_with_failure
    {
      apply =
        {
          check_finality = deps.check_finality;
          write_finality = deps.write_finality;
          persist_finality_certificate = deps.persist_finality_certificate;
          persist_finality_bundle = deps.persist_finality_bundle;
          chaos_after_finality_log = deps.chaos_after_finality_log;
          cached_bundle = (fun ~proposal_id ->
            deps.cached_bundle_for_pid proposal_id <> None);
          cached_bundle_data = (fun ~proposal_id ->
            deps.cached_bundle_for_pid proposal_id);
          cached_bundle_len = (fun ~proposal_id ->
            match deps.cached_bundle_for_pid proposal_id with
            | Some (_tx_hashes, txs, _receipts_json) -> List.length txs
            | None -> 0);
          header_has_empty_bundle = deps.header_has_empty_bundle;
          store_empty_bundle = deps.store_empty_bundle;
          driver = deps.driver;
          set_proposal = deps.set_proposal;
          store_proposal_bundle = deps.store_proposal_bundle;
          sleep = deps.sleep;
          bundle_wait_timeout_seconds = deps.apply_timeout_seconds;
          bundle_wait_expired = deps.bundle_wait_expired;
          bundle_wait_recovered = deps.bundle_wait_recovered;
          post_finalize = (fun ~epoch_id ~proposed_root ->
            Consensus_post_finalize.run
              {
                deactivate_gap = deps.deactivate_gap;
                set_consensus_finalized = deps.set_consensus_finalized;
                committed_head_epoch = deps.committed_head_epoch;
                sleep = deps.sleep;
                read_pre_finalize_root = deps.read_pre_finalize_root;
                read_commit_root = deps.read_commit_root;
                read_local_root_raw = deps.read_local_root_raw;
                commit_finality_journal = deps.commit_finality_journal;
                remove_pending_finalized = deps.remove_pending_finalized;
                apply_timeout_seconds = deps.apply_timeout_seconds;
                fatal_exit = deps.fatal_exit;
              }
              ~epoch_id
              ~proposed_root);
        };
      replay =
        {
          current_epoch = deps.current_epoch;
          catchup_active = deps.catchup_active;
          quarantine_active = deps.quarantine_active;
          finality = deps.finality;
          read_local_root_raw = deps.read_local_root_raw;
      };
    }
    (fun finalize exn ->
      Log.fatal "consensus"
        "event = finalized_apply_failure epoch = %Ld reason = %s action = exit"
        finalize.Octra_consensus.C_types.epoch_id
        (Printexc.to_string exn);
      deps.fatal_exit ();
      stop_after_fatal ())

let node_deps_of_runtime runtime =
  let bundle_wait_reason epoch_id =
    "finalized_bundle_unavailable:" ^ Int64.to_string epoch_id
  in
  {
    check_finality = (fun finalize ->
      Octra_consensus.Finality_log.check_write
        runtime.data_dir
        (Octra_consensus.Finality_log.of_finalize finalize));
    write_finality = (fun finalize ->
      Octra_consensus.Finality_log.write runtime.data_dir
        (Octra_consensus.Finality_log.of_finalize finalize));
    persist_finality_certificate = (fun finalize ->
      Consensus_finality_journal.persist_certificate
        runtime.data_dir
        ~validator_set:(runtime.validator_set ())
        finalize);
    persist_finality_bundle = (fun finalize bundle ->
      Consensus_finality_journal.persist_bundle
        runtime.data_dir
        finalize
        bundle);
    chaos_after_finality_log = (fun () ->
      Octra_core.Chaos.inject "after_finality_log");
    cached_bundle_for_pid = runtime.bundles.cached_bundle;
    header_has_empty_bundle = runtime.bundles.header_has_empty_bundle;
    store_empty_bundle = runtime.bundles.store_empty_bundle;
    driver = (fun () -> !(runtime.driver_ref));
    set_proposal = Consensus_proposal_state.set runtime.proposal_state;
    store_proposal_bundle = runtime.bundles.store_bundle;
    deactivate_gap = (fun () ->
      Consensus_catchup_queue.deactivate_gap runtime.catchup_queue);
    set_consensus_finalized = (fun finalized ->
      runtime.consensus_finalized := finalized);
    current_epoch = (fun () -> !(runtime.current_epoch));
    committed_head_epoch = runtime.committed_head_epoch;
    sleep = runtime.sleep;
    read_pre_finalize_root = runtime.read_pre_finalize_root;
    read_commit_root = runtime.read_commit_root;
    read_local_root_raw = runtime.read_local_root_raw;
    commit_finality_journal = (fun () ->
      Consensus_finality_journal.promote runtime.data_dir);
    apply_timeout_seconds = runtime.apply_timeout_seconds;
    bundle_wait_expired = (fun ~epoch_id ->
      ignore
        (Consensus_runtime_state.enter_quarantine
           runtime.runtime_state
           ~epoch:(Int64.to_int epoch_id)
           ~reason:(bundle_wait_reason epoch_id)));
    bundle_wait_recovered = (fun ~epoch_id ->
      if
        Consensus_runtime_state.quarantine_active runtime.runtime_state
        && String.equal
             (Consensus_runtime_state.quarantine_reason runtime.runtime_state)
             (bundle_wait_reason epoch_id)
      then
        Consensus_runtime_state.clear_quarantine runtime.runtime_state);
    remove_pending_finalized = runtime.finality.remove_finalized;
    fatal_exit = runtime.fatal_exit;
    catchup_active = (fun () -> !(runtime.catchup_active));
    quarantine_active = (fun () ->
      Consensus_runtime_state.quarantine_active runtime.runtime_state);
    finality = runtime.finality;
  }

let create_node_runtime runtime =
  create_node (node_deps_of_runtime runtime)