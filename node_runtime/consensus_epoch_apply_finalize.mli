(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Footer = Consensus_epoch_apply_footer
module Tree = Consensus_epoch_apply_tree

type effects = {
  footer : Footer.node_deps;
  tree : Tree.finalize_effects;
  hash : unit -> string Lwt.t;
}

type input = {
  tree_ref : Octra_core.Tree.t ref;
  chain_id : string;
  epoch_id : int;
  epoch_ts : float;
  epoch_start : float;
  proposer_addr : string;
  validator_addr : string;
  validator_pubkeys : (string * string) list;
  active_validators : string list;
  reward : Consensus_reward_attribution.t;
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