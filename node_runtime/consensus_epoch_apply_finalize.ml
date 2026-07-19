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
  epoch_ts : float;
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

let node_effects deps =
  let live_backend =
    Octra_core.Epoch_exec.make_live_backend deps.store deps.ledger
  in
  {
    footer = {
      get_meta = deps.get_meta;
      apply_footer = (fun env plan ->
        Octra_core.Epoch_exec.apply_epoch_footer
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
  let validator_count = List.length input.active_validators in
  let* footer =
    Footer.run_node
      effects.footer
      {
        epoch_id = input.epoch_id;
        epoch_ts = input.epoch_ts;
        proposer_addr = input.proposer_addr;
        validator_pubkeys = input.validator_pubkeys;
        active_validators = input.active_validators;
        ready_state_root_at = input.ready_state_root_at;
        ready_max_lag = input.ready_max_lag;
        validator_count;
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