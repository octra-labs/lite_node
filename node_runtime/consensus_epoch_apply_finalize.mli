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


module Footer = Consensus_epoch_apply_footer
module Tree = Consensus_epoch_apply_tree

type effects = {
  footer : Footer.node_deps;
  tree : Tree.finalize_effects;
  hash : unit -> string Lwt.t;
}

type input = {
  tree_ref : Octra_core.Tree.t ref;
  epoch_id : int;
  epoch_start : float;
  proposer_addr : string;
  validator_addr : string;
  validator_pubkeys : (string * string) list;
  active_validators : string list;
  ready_state_root_at : int -> string option Lwt.t;
  ready_max_lag : int;
  confirmed_fees : Z.t;
  confirmed_txs : Octra_core.Transaction.t list;
  deferred_count : int;
  short : string -> string;
}

type result = {
  reward_meta : Footer.meta;
  plan : Octra_core.Epoch_exec.reward_plan;
  reward_recipients : Octra_core.Epochlog.reward_recipient list;
  state_hash : string;
  tree : Tree.finalize_result;
}

type node_effects = {
  store : Octra_core.Store_irmin.t;
  ledger : Octra_core.Ledger.t;
  get_meta : string -> string option;
}

val node_effects :
  node_effects ->
  effects

val run :
  effects ->
  input ->
  result Lwt.t