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

type deps = {
  current_epoch : unit -> int;
  catchup_active : unit -> bool;
  quarantine_active : unit -> bool;
  find_finalized : int -> C_types.finalize option;
  read_local_root_raw : unit -> string Lwt.t;
  apply_finalized : C_types.finalize -> unit Lwt.t;
}

val drain :
  deps ->
  unit Lwt.t

val replay_while_safe :
  deps ->
  source:string ->
  unit Lwt.t