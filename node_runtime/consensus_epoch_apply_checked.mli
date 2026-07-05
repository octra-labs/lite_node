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


type deps = {
  head : unit -> int;
  set_current_epoch : int -> unit;
  catchup_active : unit -> bool;
  queue_gap :
    active:bool ->
    target_epoch:int64 ->
    reason:string ->
    Consensus_catchup_shell.queue_event;
  clear_state_attested : unit -> unit;
  log_already : current_epoch:int -> head:int -> unit;
  log_defer :
    Consensus_epoch_apply_admission.catchup ->
    Consensus_catchup_shell.queue_event ->
    unit;
  apply : unit -> unit Lwt.t;
}

val run :
  deps ->
  consensus_mode:bool ->
  current_epoch:int ->
  unit Lwt.t

val run_node :
  consensus_mode:bool ->
  current_epoch:int ref ->
  head:(unit -> int) ->
  catchup_queue:Consensus_catchup_queue.t ->
  catchup_in_progress:bool ref ->
  clear_state_attested:(unit -> unit) ->
  apply:(unit -> unit Lwt.t) ->
  unit Lwt.t