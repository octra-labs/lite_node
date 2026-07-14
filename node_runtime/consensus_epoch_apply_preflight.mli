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


type effects = {
  begin_store_batch : unit -> unit Lwt.t;
  begin_chaindata_batch : unit -> unit;
  ledger_hash : unit -> string Lwt.t;
  cached_head : unit -> Octra_core.Head_manifest.t option;
  expected_prev_root : int -> string option;
  fatal : string -> unit;
  exit : unit -> unit;
}

type request = {
  epoch_id : int;
}

type result = {
  pre_state : Consensus_epoch_apply_env.pre_state;
}

val run :
  effects ->
  request ->
  result Lwt.t