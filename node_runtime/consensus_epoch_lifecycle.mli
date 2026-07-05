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
  trace : string -> unit;
  log_head : string -> unit;
  log_gc : int -> unit;
  save_state_root : unit -> unit Lwt.t;
  get_head_hash : unit -> string option Lwt.t;
  cleanup_old_tags : int -> unit Lwt.t;
}

type ctx = {
  current_epoch : int;
}

val should_cleanup_old_tags : int -> bool

val run : deps -> ctx -> unit Lwt.t

val run_node :
  store:Octra_core.Store_irmin.t ->
  current_epoch:int ->
  unit Lwt.t