(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Finality_log = Octra_consensus.Finality_log
module Finality_recovery = Octra_consensus.Finality_recovery
module Log = Octra_log

type deps = {
  head : unit -> int;
  last_finality : unit -> Finality_log.entry option;
  mark_quarantine : string -> unit;
}

type node_runtime = {
  committed_head_epoch : unit -> int;
  current_epoch : int ref;
  last_finality : unit -> Finality_log.entry option;
  drop_uncommitted_after : int -> (int, string) result;
  mark_quarantine : string -> unit;
}

let handle_pending (deps : deps) head action entry =
  Log.warn "finality"
    "event = startup_finality_log_pending height = %d head = %d action = quarantine reason = certificate_required"
    entry.Finality_log.height
    head;
  deps.mark_quarantine (Finality_recovery.reason ~head action)

let handle_startup (deps : deps) =
  let head = deps.head () in
  let action = Finality_recovery.decide ~head (deps.last_finality ()) in
  match action with
  | Finality_recovery.Clean -> ()
  | Finality_recovery.Pending entry ->
    handle_pending deps head action entry
  | Finality_recovery.Gap entry ->
    Log.warn "finality"
      "event = startup_finality_log_gap height = %d head = %d action = quarantine"
      entry.Finality_log.height
      head;
    deps.mark_quarantine (Finality_recovery.reason ~head action)

let node_normalizer runtime =
  Consensus_driver_wiring.normalize_next_epoch_for_head
    {
      current_epoch = runtime.current_epoch;
      committed_head_epoch = runtime.committed_head_epoch;
      warn = (fun ~source ~current ~committed_head ~expected_next ->
        Log.warn "catchup"
          "event = normalize_epoch source = %s current = %d committed_head = %d expected_next = %d"
          source current committed_head expected_next);
    }

let node_deps runtime =
  {
    head = runtime.committed_head_epoch;
    last_finality = runtime.last_finality;
    mark_quarantine = runtime.mark_quarantine;
  }

let run_node_startup runtime =
  let head = runtime.committed_head_epoch () in
  match runtime.drop_uncommitted_after head with
  | Error reason ->
    Log.warn "finality"
      "event = startup_finality_trim action = quarantine reason = %s"
      reason;
    runtime.mark_quarantine ("finality_trim_failed:" ^ reason)
  | Ok dropped ->
    if dropped > 0 then
      Log.warn "finality"
        "event = startup_finality_trim action = drop_uncommitted head = %d dropped = %d"
        head
        dropped;
    handle_startup (node_deps runtime)