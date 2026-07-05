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
  write_finality : Octra_consensus.C_types.finalize -> unit;
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
  queue_missing_bundle : target_epoch:int64 -> reason:string -> unit;
  deactivate_gap : unit -> unit;
  set_consensus_finalized : bool -> unit;
  current_epoch : unit -> int;
  sleep : float -> unit Lwt.t;
  read_pre_finalize_root : unit -> string option;
  read_commit_root : unit -> string option Lwt.t;
  read_local_root_raw : unit -> string Lwt.t;
  remove_pending_finalized : epoch:int -> unit;
  fatal_exit : unit -> unit;
  catchup_active : unit -> bool;
  quarantine_active : unit -> bool;
  finality : Consensus_finality_state.callbacks;
}

type t = {
  apply_finalized : Octra_consensus.C_types.finalize -> unit Lwt.t;
  drain_pending : unit -> unit Lwt.t;
  replay_stashed_while_safe : source:string -> unit Lwt.t;
}

let create deps =
  let apply_deps = Consensus_finalized_apply.node_deps deps.apply in
  let apply_finalized finalize =
    Consensus_finalized_apply.run apply_deps finalize
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

let create_node deps =
  create
    {
      apply =
        {
          write_finality = deps.write_finality;
          chaos_after_finality_log = deps.chaos_after_finality_log;
          cached_bundle = (fun ~proposal_id ->
            deps.cached_bundle_for_pid proposal_id <> None);
          cached_bundle_len = (fun ~proposal_id ->
            match deps.cached_bundle_for_pid proposal_id with
            | Some (_tx_hashes, txs, _receipts_json) -> List.length txs
            | None -> 0);
          header_has_empty_bundle = deps.header_has_empty_bundle;
          store_empty_bundle = deps.store_empty_bundle;
          driver = deps.driver;
          set_proposal = deps.set_proposal;
          store_proposal_bundle = deps.store_proposal_bundle;
          queue_missing_bundle = deps.queue_missing_bundle;
          post_finalize = (fun ~epoch_id ~proposed_root ->
            Consensus_post_finalize.run
              {
                deactivate_gap = deps.deactivate_gap;
                set_consensus_finalized = deps.set_consensus_finalized;
                current_epoch = deps.current_epoch;
                sleep = deps.sleep;
                read_pre_finalize_root = deps.read_pre_finalize_root;
                read_commit_root = deps.read_commit_root;
                read_local_root_raw = deps.read_local_root_raw;
                remove_pending_finalized = deps.remove_pending_finalized;
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