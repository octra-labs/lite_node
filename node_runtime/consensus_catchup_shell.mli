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

val run :
  deps ->
  run_one:run_one ->
  target_epoch:int64 ->
  reason:string ->
  unit Lwt.t