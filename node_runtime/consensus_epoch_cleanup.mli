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
  log_deferred : int -> unit;
  log_expired : int -> unit;
  log_broadcast : Consensus_epoch_apply_broadcast.log_view -> peers:int -> unit;
  set_last_epoch_time : float -> unit;
  reset_tree : epoch_id:int -> parent_commit:string -> unit;
  remove_processed : string list -> unit;
  clear_deferred : unit -> int;
  reset_private_counters : unit -> unit;
  expire_old_count : unit -> int;
  cleanup_dropped : unit -> unit;
  sweep_low_fee : unit -> unit;
  retain_live_preverify : unit -> unit;
  prune_preverify : unit -> unit;
  notify_staging_update : unit -> unit;
  peer_count : unit -> int;
  broadcast : Consensus_epoch_apply_broadcast.message -> unit Lwt.t;
}

type ctx = {
  now : float;
  epoch_id : int;
  parent_commit : string;
  post_consensus_root : string;
  confirmed_count : int;
  producer : string;
  processed_hashes : string list;
  txs_serialized : string list;
}

type node_refs

val refs :
  last_epoch_time:float ref ->
  tree:Octra_core.Tree.t ref ->
  deferred_stealth_txs:Octra_core.Transaction.t list ref ->
  stealth_in_epoch_counter:int ref ->
  fhe_in_epoch_counter:int ref ->
  swarm_opt:Octra_net.P2p_swarm.t option ref ->
  node_refs

val run : deps -> ctx -> unit Lwt.t

val run_node : node_refs -> ctx -> unit Lwt.t