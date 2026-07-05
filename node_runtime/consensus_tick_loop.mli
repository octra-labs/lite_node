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
  now : unit -> float;
  last_epoch_time : unit -> float;
  current_epoch : unit -> int;
  consensus_finalized : unit -> bool;
  find_finalized : int -> Octra_consensus.C_types.finalize option;
  cached_bundle_for_pid : string -> bool;
  header_has_empty_bundle : Octra_consensus.C_types.epoch_header -> bool;
  store_empty_bundle_for_header : Octra_consensus.C_types.epoch_header -> unit;
  queue_missing_bundle : target_epoch:int64 -> reason:string -> unit;
  warn : string -> unit;
  info : string -> unit;
  clear_finalized : unit -> unit;
  apply : now:float -> elapsed:float -> unit Lwt.t;
  sleep : float -> unit Lwt.t;
}

val step :
  deps ->
  consensus_mode:bool ->
  epoch_duration:float ->
  float Lwt.t

val run :
  deps ->
  consensus_mode:bool ->
  epoch_duration:float ->
  unit Lwt.t