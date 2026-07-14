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


module Env = Consensus_epoch_apply_env
module Finalize = Consensus_epoch_apply_finalize
module Footer = Consensus_epoch_apply_footer
module Post = Consensus_epoch_apply_post
module Proposer = Consensus_epoch_apply_proposer
module Transaction = Octra_core.Transaction

type deps = {
  current_epoch : unit -> int;
  validator_pubkeys : Env.node_env -> (string * string) list;
  validator_context : (string * string) list -> Footer.validator_context;
  proposer : Proposer.runtime_request -> Proposer.runtime_result;
  trace : unit -> Footer.trace;
  emit_replay_proposer :
    Footer.trace ->
    epoch_id:int ->
    proposer_source:string ->
    proposer:string ->
    validators_sha:string ->
    unit;
  finalize : Finalize.input -> Finalize.result Lwt.t;
  post : Post.node_input -> Post.result Lwt.t;
}

type request = {
  now : float;
  consensus_mode : bool;
  override_proposer_info : Octra_core.Epochlog.proposer_info option;
  epoch_env : Env.node_env;
  tree_ref : Octra_core.Tree.t ref;
  epoch_start : float;
  validator_addr : string;
  ready_state_root_at : int -> string option Lwt.t;
  ready_max_lag : int;
  pending_tx_saves : (Transaction.t * int) list ref;
  confirmed_fees : Z.t ref;
  processed_hashes : string list ref;
  pre_state_hash : string;
  pre_consensus_root : string;
  epoch_receipts_json : string list;
  ordered_txs_count : int;
  deferred_stealth_txs : Transaction.t list ref;
  producer : string;
  short : string -> string;
}

type result = {
  proposer_source : string;
  proposer_addr : string;
  post : Post.result;
}

type node_deps = {
  data_dir : string;
  store : Octra_core.Store_irmin.t;
  ledger : Octra_core.Ledger.t;
  chaindata : Octra_core.Store_chaindata.t;
  finality_state : Consensus_finality_state.t;
  current_epoch : int ref;
  last_epoch_time : float ref;
  tree : Octra_core.Tree.t ref;
  stealth_in_epoch_counter : int ref;
  fhe_in_epoch_counter : int ref;
  swarm_opt : Octra_net.P2p_swarm.t option ref;
  get_meta : string -> string option;
  env : string -> string option;
  hash : string -> string -> string;
  raw_to_hex : string -> string;
  stdout : string -> unit;
  log_epoch : string -> unit;
  fatal_epoch : string -> unit;
  short : string -> string;
  exit : unit -> unit;
}

val run : deps -> request -> result Lwt.t
val run_node : node_deps -> request -> result Lwt.t