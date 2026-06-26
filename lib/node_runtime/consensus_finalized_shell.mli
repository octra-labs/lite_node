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

type 'driver deps = {
  prune_frozen : finalized_epoch:int64 -> unit;
  store_proposer : Consensus_finalized_flow.proposer_info -> unit;
  store_expected_root : epoch:int -> root:string -> unit;
  store_finalized : epoch:int -> C_types.finalize -> unit;
  remove_finalized : epoch:int -> unit;
  remove_proposer : epoch:int -> unit;
  committed_head_epoch : unit -> int;
  driver : unit -> 'driver option;
  observer_mode : bool;
  catchup_active : unit -> bool;
  quarantine_active : unit -> bool;
  state_attested : unit -> bool;
  current_epoch : unit -> int;
  read_local_root_raw : unit -> string Lwt.t;
  queue_catchup_target : target_epoch:int64 -> reason:string -> unit;
  run_catchup_to_target :
    'driver ->
    target_epoch:int64 ->
    reason:string ->
    unit Lwt.t;
  mark_quarantine : string -> unit;
  clear_quarantine : string -> unit;
  set_catchup_active : bool -> unit;
  apply_finalized : C_types.finalize -> unit Lwt.t;
  replay_stashed_while_safe : source:string -> unit Lwt.t;
}

val handle : 'driver deps -> C_types.finalize -> unit Lwt.t