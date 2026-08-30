(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type commit_result = {
  post_consensus_root : string;
}

type result = {
  applied_epoch_id : int;
  current_epoch : int;
  post_consensus_root : string;
}

type effects = {
  current_epoch : unit -> int;
  set_current_epoch : int -> unit;
  expected_root : int -> string option;
  remove_proposer : int -> unit;
  prune_proposers_before : int -> unit;
  clear_spent_nonces : unit -> unit;
  commit :
    epoch_id:int ->
    current_epoch:int ->
    expected_root:string option ->
    commit_result Lwt.t;
  cleanup :
    epoch_id:int ->
    post_consensus_root:string ->
    unit Lwt.t;
  lifecycle : current_epoch:int -> unit Lwt.t;
}

type node_refs

val refs :
  current_epoch:int ref ->
  finality_state:Consensus_finality_state.t ->
  ledger:Octra_core.Ledger.t ->
  last_epoch_time:float ref ->
  tree:Octra_core.Tree.t ref ->
  deferred_stealth_txs:Octra_core.Transaction.t list ref ->
  stealth_in_epoch_counter:int ref ->
  fhe_in_epoch_counter:int ref ->
  swarm_opt:Octra_net.P2p_swarm.t option ref ->
  node_refs

type node_effects = {
  data_dir : string;
  store : Octra_core.Store_irmin.t;
  chaindata : Octra_core.Store_chaindata.t;
  save_drops : Octra_core.Tx_staging.drop_record list -> unit;
  irmin_last_epoch : unit -> int;
  require_sync : Sync_need.t -> unit;
  exit : unit -> unit;
}

type node_input = {
  now : float;
  epoch_ts : float;
  consensus_mode : bool;
  layera_diag : bool;
  replay_trace : bool;
  pre_state_hash : string;
  pre_consensus_root : string;
  parent_commit : string;
  proposer_addr : string;
  proposer_info : Octra_core.Epochlog.proposer_info;
  proposer_source : string;
  round : int;
  validators : int;
  validators_sha : string;
  ordered_txs_count : int;
  confirmed_txs : Octra_core.Transaction.t list;
  confirmed_count : int;
  confirmed_fees : Z.t;
  plan : Octra_core.Epoch_exec.reward_plan;
  prev_supply : Z.t;
  emission_remaining : Z.t;
  reward_recipients : Octra_core.Epochlog.reward_recipient list;
  reward_source : Octra_consensus.C_types.reward_source;
  epoch_receipts_json : string list;
  active_validators : string list;
  processed_hashes : string list;
  txs_serialized : string list;
  producer : string;
  short : string -> string;
}

val input_of_finalized :
  now:float ->
  epoch_ts:float ->
  consensus_mode:bool ->
  trace:Consensus_epoch_apply_footer.trace ->
  pre_state_hash:string ->
  pre_consensus_root:string ->
  proposer_addr:string ->
  proposer_info:Octra_core.Epochlog.proposer_info ->
  proposer_source:string ->
  round:int ->
  validators:int ->
  validators_sha:string ->
  ordered_txs_count:int ->
  confirmed_txs:Octra_core.Transaction.t list ->
  confirmed_fees:Z.t ->
  epoch_receipts_json:string list ->
  active_validators:string list ->
  processed_hashes:string list ->
  reward_source:Octra_consensus.C_types.reward_source ->
  producer:string ->
  short:(string -> string) ->
  Consensus_epoch_apply_finalize.result ->
  node_input

val run :
  effects ->
  result Lwt.t

val run_node :
  node_refs ->
  node_effects ->
  node_input ->
  result Lwt.t