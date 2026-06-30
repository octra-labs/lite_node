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


module C_types = Octra_consensus.C_types
module Log = Octra_log

type deps = {
  current_epoch : unit -> int;
  catchup_active : unit -> bool;
  quarantine_active : unit -> bool;
  find_finalized : int -> C_types.finalize option;
  read_local_root_raw : unit -> string Lwt.t;
  apply_finalized : C_types.finalize -> unit Lwt.t;
}

type node_deps = {
  current_epoch : unit -> int;
  catchup_active : unit -> bool;
  quarantine_active : unit -> bool;
  finality : Consensus_finality_state.callbacks;
  read_local_root_raw : unit -> string Lwt.t;
  apply_finalized : C_types.finalize -> unit Lwt.t;
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
  let epoch = deps.current_epoch () in
  match deps.find_finalized epoch with
  | Some finalize ->
    Log.info "consensus" "replaying stashed finalized epoch = %d reason = drain" epoch;
    let* () = deps.apply_finalized finalize in
    drain deps
  | None -> Lwt.return_unit

let rec replay_while_safe (deps : deps) ~source =
  let open Lwt.Syntax in
  if deps.catchup_active () then
    Lwt.return_unit
  else
    let epoch = deps.current_epoch () in
    match deps.find_finalized epoch with
    | None -> Lwt.return_unit
    | Some finalize ->
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
        let* () = deps.apply_finalized finalize in
        replay_while_safe deps ~source
      end

let node_deps (deps : node_deps) =
  {
    current_epoch = deps.current_epoch;
    catchup_active = deps.catchup_active;
    quarantine_active = deps.quarantine_active;
    find_finalized = deps.finality.find_finalized;
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