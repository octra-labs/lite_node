(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C_types = Octra_consensus.C_types
module Log = Octra_log

type deps = {
  committed_head_epoch : unit -> int;
  catchup_active : unit -> bool;
  quarantine_active : unit -> bool;
  find_finalized :
    int ->
    (C_types.finalize * C_types.validator_set) option;
  read_local_root_raw : unit -> string Lwt.t;
  apply_finalized :
    validator_set:C_types.validator_set ->
    C_types.finalize ->
    unit Lwt.t;
}

type node_deps = {
  committed_head_epoch : unit -> int;
  catchup_active : unit -> bool;
  quarantine_active : unit -> bool;
  finality : Consensus_finality_state.callbacks;
  read_local_root_raw : unit -> string Lwt.t;
  apply_finalized :
    validator_set:C_types.validator_set ->
    C_types.finalize ->
    unit Lwt.t;
}

type runner = {
  drain_pending : unit -> unit Lwt.t;
  replay_stashed_while_safe : source:string -> unit Lwt.t;
}

let short_hex8 s =
  String.concat ""
    (List.init
       (min 8 (String.length s))
       (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let rec drain (deps : deps) =
  let open Lwt.Syntax in
  let head = deps.committed_head_epoch () in
  let epoch = head + 1 in
  match deps.find_finalized epoch with
  | Some (finalize, validator_set) ->
    Log.info "consensus" "replaying stashed finalized epoch = %d reason = drain" epoch;
    let* () = deps.apply_finalized ~validator_set finalize in
    if deps.committed_head_epoch () <= head then
      Lwt.fail_with "finalized replay did not advance committed head"
    else
      drain deps
  | None -> Lwt.return_unit

let rec replay_while_safe (deps : deps) ~source =
  let open Lwt.Syntax in
  if deps.catchup_active () then
    Lwt.return_unit
  else
    let head = deps.committed_head_epoch () in
    let epoch = head + 1 in
    match deps.find_finalized epoch with
    | None -> Lwt.return_unit
    | Some (finalize, validator_set) ->
      let header = finalize.C_types.header in
      let* local_root = deps.read_local_root_raw () in
      if header.prev_state_root <> local_root then begin
        if deps.quarantine_active () then
          Log.info "consensus"
            "quarantine replay paused source = %s epoch = %d local_root = %s prev_root = %s"
            source epoch (short_hex8 local_root) (short_hex8 header.prev_state_root);
        Lwt.return_unit
      end else begin
        Log.warn "consensus"
          "quarantine replay applying stashed finalized epoch = %d source = %s"
          epoch source;
        let* () = deps.apply_finalized ~validator_set finalize in
        if deps.committed_head_epoch () <= head then
          Lwt.fail_with "finalized replay did not advance committed head"
        else
          replay_while_safe deps ~source
      end

let node_deps (deps : node_deps) =
  {
    committed_head_epoch = deps.committed_head_epoch;
    catchup_active = deps.catchup_active;
    quarantine_active = deps.quarantine_active;
    find_finalized = deps.finality.find_finalized_with_set;
    read_local_root_raw = deps.read_local_root_raw;
    apply_finalized = deps.apply_finalized;
  }

let node_runner (deps : node_deps) =
  let deps = node_deps deps in
  {
    drain_pending = (fun () ->
      drain deps);
    replay_stashed_while_safe = (fun ~source ->
      replay_while_safe deps ~source);
  }