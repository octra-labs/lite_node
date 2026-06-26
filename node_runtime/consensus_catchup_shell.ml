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


module Log = Octra_log

type queued = {
  target_epoch : int64;
  reason : string;
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

let queue_active deps ~target_epoch ~reason =
  let queued = deps.queue_target ~target_epoch ~reason in
  Log.warn "catchup"
    "queued catchup target = %Ld reason = %s active = %b queued = %s"
    target_epoch reason true queued;
  Lwt.return_unit

let run deps ~run_one ~target_epoch ~reason =
  let open Lwt.Syntax in
  let rec finish_success active_reason =
    let next_height = Int64.succ (Int64.of_int (deps.committed_head_epoch ())) in
    let* () = deps.start_height next_height in
    deps.set_catchup_active false;
    let head = Int64.of_int (deps.committed_head_epoch ()) in
    match deps.take_queued_after ~head with
    | Some queued ->
      deps.set_catchup_active true;
      Log.warn "catchup"
        "CATCHUP_CONTINUE active_reason = %s next_target = %Ld next_reason = %s head = %Ld"
        active_reason queued.target_epoch queued.reason head;
      run_queued queued
    | None ->
      deps.clear_queue ();
      let head = deps.committed_head_epoch () in
      let* root = deps.read_local_root () in
      deps.set_state_attested ~head ~root;
      deps.clear_quarantine ("catchup_complete:" ^ active_reason);
      if deps.observer then Lwt.return_unit
      else
        let* () = deps.drain_pending_finalized () in
        deps.wake_ready ()
  and fail_catchup quarantine_tag =
    deps.clear_queue ();
    deps.set_catchup_active false;
    deps.mark_quarantine quarantine_tag;
    Lwt.return_unit
  and run_queued queued =
    run_one
      ~target_epoch:queued.target_epoch
      ~reason:queued.reason
      ~finish_success
      ~fail_catchup
  in
  if deps.catchup_active () then
    queue_active deps ~target_epoch ~reason
  else begin
    deps.set_catchup_active true;
    run_queued { target_epoch; reason }
  end