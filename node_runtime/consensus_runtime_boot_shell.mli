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
  current_epoch : int ref;
  chaindata : Octra_core.Store_chaindata.t;
  runtime_env : Startup_runtime_limits.env;
  default_max_ou : Z.t;
  now : unit -> float;
}

type t = {
  last_epoch_time : float ref;
  stealth_defer_count : (string, int) Hashtbl.t;
  private_limits : Startup_runtime_limits.private_limits;
  stealth_in_epoch_counter : int ref;
  fhe_in_epoch_counter : int ref;
  tree : Octra_core.Tree.t ref;
  swarm_opt : Octra_net.P2p_swarm.t option ref;
  consensus_finalized : bool ref;
  catchup_in_progress : bool ref;
  catchup_queue : Consensus_catchup_queue.t;
  consensus_state : Consensus_runtime_state.t;
  consensus_limits : Startup_runtime_limits.consensus_limits;
  driver_ref : Octra_consensus.C_driver.t option ref;
  consensus_liveness : Consensus_liveness.state ref;
  proposal_state : Consensus_proposal_state.t;
  finality_state : Consensus_finality_state.t;
  finality_callbacks : Consensus_finality_state.callbacks;
  proposal_bundles : Consensus_bundle_cache.t;
  proposal_bundle_runtime : Consensus_bundle_cache.node_runtime;
  bft_proposal_limits : Consensus_proposal.limits;
  current_consensus_round : unit -> int;
  clear_state_attested : unit -> unit;
  set_state_attested : head:int -> root:string -> unit;
  mark_quarantine : string -> unit;
  clear_quarantine : string -> unit;
  catchup_node_queue : Consensus_catchup_shell.node_queue;
}

val parent_commit_or_genesis :
  Octra_core.Epochlog.epoch_header option ->
  string

val last_commit :
  Octra_core.Store_chaindata.t ->
  string

val driver_round :
  Octra_consensus.C_driver.t option ref ->
  int

val create :
  deps ->
  t