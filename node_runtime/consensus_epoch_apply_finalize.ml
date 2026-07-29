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

let node_effects deps =
  let live_backend =
    Octra_core.Epoch_exec.make_live_backend deps.store deps.ledger
  in
  {
    footer = {
      get_meta = deps.get_meta;
      set_meta = (fun key value ->
        Octra_core.Store_irmin.set_meta deps.store key value);
      policy = Octra_core.Emission_policy.of_env Sys.getenv_opt;
      schedule = Octra_core.Emission_schedule.of_env_exn Sys.getenv_opt;
      legacy_total = Octra_core.Emission_policy.legacy_total Sys.getenv_opt;
      public_supply = (fun () -> Octra_core.Ledger.get_total_supply deps.ledger);
      apply_footer = (fun env reward plan ->
        Octra_core.Epoch_exec.apply_epoch_footer_with_reward
          ~reward
          ~backend:live_backend
          ~env
          ~plan);
      log = Log.info "epoch" "%s";
    };
    tree = {
      now = Unix.gettimeofday;
      log = Log.info "epoch" "%s";
      record_epoch_complete = Octra_core.Metrics.record_epoch_complete;
      notify_new_epoch = Octra_core.Ws_server.notify_new_epoch;
      notify_epoch_finalized = (fun epoch count ->
        Octra_core.Webhooks.notify
          (Octra_core.Webhooks.EpochFinalized (epoch, count)));
    };
    hash = (fun () -> Octra_core.Ledger.hash deps.ledger);
  }

let run effects input =
  let open Lwt.Syntax in
  let* footer =
    Footer.run_node
      effects.footer
      {
        epoch_id = input.epoch_id;
        epoch_ts = input.epoch_ts;
        proposer_addr = input.proposer_addr;
        validator_pubkeys = input.validator_pubkeys;
        reward = input.reward;
        ready_state_root_at = input.ready_state_root_at;
        ready_max_lag = input.ready_max_lag;
        confirmed_fees = input.confirmed_fees;
        short = input.short;
      }
  in
  let* state_hash = effects.hash () in
  let tree =
    Tree.finalize_epoch
      effects.tree
      {
        tree = input.tree_ref;
        epoch_id = input.epoch_id;
        epoch_ts = input.epoch_ts;
        epoch_start = input.epoch_start;
        proposer_addr = input.proposer_addr;
        validator_addr = input.validator_addr;
        confirmed_txs = input.confirmed_txs;
        deferred_count = input.deferred_count;
        state_hash;
        short = input.short;
      }
  in
  Lwt.return {
    reward_meta = footer.meta;
    plan = footer.plan;
    reward_recipients = footer.reward_recipients;
    state_hash;
    tree;
  }