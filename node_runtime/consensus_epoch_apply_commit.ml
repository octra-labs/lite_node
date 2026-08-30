(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type effects = {
  prepare : Consensus_epoch_commit.prepare_effects;
  stage : Consensus_epoch_apply_guard.stage_batch_effects;
  post_root : Consensus_epoch_apply_guard.post_root_effects;
  commit : Consensus_epoch_commit.commit_effects;
  rollback :
    epoch_id:int ->
    start_txid:int64 ->
    tx_count:int ->
    Consensus_epoch_commit.rollback_effects;
  failure :
    rollback:(unit -> bool) ->
    Consensus_epoch_commit.failure_effects;
  log_boundary : Consensus_epoch_commit.boundary_log -> unit;
}

type input = {
  epoch_id : int;
  epoch_ts : float;
  current_epoch : int;
  consensus_mode : bool;
  layera_diag : bool;
  replay_trace : bool;
  pre_state_hash : string;
  pre_consensus_root : string;
  expected_root : string option;
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
  account_addrs : string list;
  short : string -> string;
  find_account : string -> Consensus_epoch_apply_guard.account_view option;
  progress : Consensus_epoch_commit.commit_progress;
}

type result = {
  post_state_hash : string;
  post_consensus_root : string;
}

type node_effects = {
  data_dir : string;
  store : Octra_core.Store_irmin.t;
  ledger : Octra_core.Ledger.t;
  chaindata : Octra_core.Store_chaindata.t;
  finality_state : Consensus_finality_state.t;
  irmin_last_epoch : unit -> int;
  exit : unit -> unit;
}

let node_effects (deps : node_effects) =
  let commit_live =
    Consensus_epoch_commit.{
      data_dir = deps.data_dir;
      store = deps.store;
      ledger = deps.ledger;
      chaindata = deps.chaindata;
      trace = Log.trace "epoch" "%s";
      fatal = Log.fatal "epoch" "%s";
      log = (fun line -> Log.stderr "%s\n%!" line);
      exit = deps.exit;
    }
  in
  {
    prepare = {
      head = Octra_core.Head_manifest.get_cached;
      irmin_last_epoch = deps.irmin_last_epoch;
      next_txid = (fun () -> Octra_core.Store_chaindata.next_txid deps.chaindata);
      commit_id = Octra_core.Commit_journal.mk_commit_id;
    };
    stage =
      Consensus_epoch_apply_guard.live_stage_batch_effects {
        data_dir = deps.data_dir;
        store = deps.store;
        ledger = deps.ledger;
        trace = Log.trace "epoch" "%s";
        warn = Log.warn "epoch" "%s";
        level_output = (fun line -> Log.stderr "%s\n%!" line);
        replay_output = (fun line -> Log.stdout "%s\n%!" line);
      };
    post_root = {
      warn = Log.warn "epoch" "%s";
      error = Log.error "epoch" "%s";
      fatal = Log.fatal "epoch" "%s";
      remove_expected_root =
        Consensus_finality_state.remove_expected_root deps.finality_state;
      prune_expected_roots_before =
        Consensus_finality_state.prune_expected_roots_before deps.finality_state;
      exit = deps.exit;
    };
    commit = Consensus_epoch_commit.live_commit_effects commit_live;
    rollback = (fun ~epoch_id ~start_txid ~tx_count ->
      Consensus_epoch_commit.live_rollback_effects
        commit_live
        ~commit_epoch:epoch_id
        ~start_txid
        ~tx_count);
    failure = Consensus_epoch_commit.live_failure_effects commit_live;
    log_boundary = (fun (entry : Consensus_epoch_commit.boundary_log) ->
      match entry.level with
      | `Info -> Log.info "epoch" "%s" entry.line
      | `Warn -> Log.warn "epoch" "%s" entry.line);
  }

let run effects input =
  let open Lwt.Syntax in
  let prepared =
    Consensus_epoch_commit.prepare_commit
      effects.prepare
      {
        epoch_id = input.epoch_id;
        finalized_at = input.epoch_ts;
        pre_state_hash = input.pre_state_hash;
        confirmed_count = input.confirmed_count;
        confirmed_txs = input.confirmed_txs;
      }
  in
  Option.iter effects.log_boundary prepared.log;
  let index = prepared.index in
  let* stage =
    Consensus_epoch_apply_guard.run_stage_batch
      effects.stage
      {
        epoch_id = input.epoch_id;
        current_epoch = input.current_epoch;
        replay_trace = input.replay_trace;
        layera_diag = input.layera_diag;
        txs_in = input.ordered_txs_count;
        confirmed = input.confirmed_count;
        fees = Z.to_string input.confirmed_fees;
        base = Z.to_string input.plan.Octra_core.Epoch_exec.base_reward;
      }
  in
  let post_consensus_root =
    Octra_core.Epoch_index_commitment.folded_state_root
      ~ledger_state_root:stage.post_state_hash
      ~epoch_index_root:index.epoch_index_root
  in
  ignore (Consensus_epoch_apply_guard.run_post_root_input
    effects.post_root
    {
      consensus_mode = input.consensus_mode;
      layera_diag = input.layera_diag;
      epoch_id = input.epoch_id;
      current_epoch = input.current_epoch;
      actual_root = post_consensus_root;
      expected_root = input.expected_root;
      proposer_source = input.proposer_source;
      proposer = input.proposer_addr;
      round = input.round;
      validators = input.validators;
      validators_sha = input.validators_sha;
      tx_count = input.confirmed_count;
      plan = input.plan;
      prev_supply = input.prev_supply;
      emission_remaining = input.emission_remaining;
      confirmed_fees = input.confirmed_fees;
      replay_subtrees = stage.replay_subtrees;
      layera_meta_pairs = stage.layera_meta_pairs;
      root = stage.post_state_hash;
      account_addrs = input.account_addrs;
      short = input.short;
      find_account = input.find_account;
    });
  let rollback () =
    prepared.rollback
    |> Consensus_epoch_commit.run_rollback
      ~effects:(effects.rollback
                  ~epoch_id:input.epoch_id
                  ~start_txid:index.start_txid
                  ~tx_count:input.confirmed_count)
  in
  let* () =
    Consensus_epoch_commit.run_commit
      ~effects:effects.commit
      ~failure_effects:(effects.failure ~rollback)
      {
        epoch_id = input.epoch_id;
        pre_state_root = input.pre_state_hash;
        post_state_root = stage.post_state_hash;
        post_consensus_root;
        prev_state_root = input.pre_consensus_root;
        parent_commit = input.parent_commit;
        start_txid = index.start_txid;
        tx_count = input.confirmed_count;
        finalized_by = input.proposer_addr;
        finalized_at = prepared.finalized_at;
        proposer = input.proposer_info;
        confirmed_fees = input.confirmed_fees;
        plan = input.plan;
        reward_recipients = input.reward_recipients;
        reward_source = input.reward_source;
        epoch_receipts_json = input.epoch_receipts_json;
        commit_id = prepared.commit_id;
        prev_generation = prepared.irmin_last_before;
        planned_txid_hi = index.planned_txid_hi;
        epoch_index_hash = index.epoch_index_hash;
        epoch_index_root = index.epoch_index_root;
        progress = input.progress;
      }
  in
  Lwt.return {
    post_state_hash = stage.post_state_hash;
    post_consensus_root;
  }