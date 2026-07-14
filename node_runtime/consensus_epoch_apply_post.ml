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


module Commit = Consensus_epoch_apply_commit
module Cleanup = Consensus_epoch_cleanup
module Finalize = Consensus_epoch_apply_finalize
module Footer = Consensus_epoch_apply_footer
module Guard = Consensus_epoch_apply_guard
module Lifecycle = Consensus_epoch_lifecycle
module Tree = Consensus_epoch_apply_tree

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

type node_refs = {
  current_epoch : int ref;
  finality_state : Consensus_finality_state.t;
  ledger : Octra_core.Ledger.t;
  last_epoch_time : float ref;
  tree : Octra_core.Tree.t ref;
  deferred_stealth_txs : Octra_core.Transaction.t list ref;
  stealth_in_epoch_counter : int ref;
  fhe_in_epoch_counter : int ref;
  swarm_opt : Octra_net.P2p_swarm.t option ref;
}

let refs ~current_epoch ~finality_state ~ledger ~last_epoch_time ~tree
    ~deferred_stealth_txs ~stealth_in_epoch_counter ~fhe_in_epoch_counter
    ~swarm_opt =
  {
    current_epoch;
    finality_state;
    ledger;
    last_epoch_time;
    tree;
    deferred_stealth_txs;
    stealth_in_epoch_counter;
    fhe_in_epoch_counter;
    swarm_opt;
  }

type node_effects = {
  data_dir : string;
  store : Octra_core.Store_irmin.t;
  chaindata : Octra_core.Store_chaindata.t;
  irmin_last_epoch : unit -> int;
  exit : unit -> unit;
}

type node_input = {
  now : float;
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
  epoch_receipts_json : string list;
  active_validators : string list;
  processed_hashes : string list;
  txs_serialized : string list;
  producer : string;
  short : string -> string;
}

let input_of_finalized ~now ~consensus_mode ~trace ~pre_state_hash
    ~pre_consensus_root ~proposer_addr ~proposer_info ~proposer_source ~round
    ~validators ~validators_sha ~ordered_txs_count ~confirmed_txs
    ~confirmed_fees ~epoch_receipts_json ~active_validators ~processed_hashes
    ~producer ~short (finalized : Finalize.result) =
  let reward_meta = finalized.reward_meta in
  let tree = finalized.tree in
  {
    now;
    consensus_mode;
    layera_diag = trace.Footer.layera;
    replay_trace = trace.Footer.replay;
    pre_state_hash;
    pre_consensus_root;
    parent_commit = tree.Tree.new_commit;
    proposer_addr;
    proposer_info;
    proposer_source;
    round;
    validators;
    validators_sha;
    ordered_txs_count;
    confirmed_txs;
    confirmed_count = tree.Tree.confirmed_count;
    confirmed_fees;
    plan = finalized.plan;
    prev_supply = reward_meta.Footer.prev_supply;
    emission_remaining = reward_meta.Footer.emission_remaining;
    reward_recipients = finalized.reward_recipients;
    epoch_receipts_json;
    active_validators;
    processed_hashes;
    txs_serialized = tree.Tree.tx_material.Tree.txs_serialized;
    producer;
    short;
  }

let run (effects : effects) =
  let open Lwt.Syntax in
  let applied_epoch_id = effects.current_epoch () in
  let current_epoch = applied_epoch_id + 1 in
  effects.set_current_epoch current_epoch;
  effects.remove_proposer applied_epoch_id;
  effects.prune_proposers_before (current_epoch - 32);
  effects.clear_spent_nonces ();
  let expected_root = effects.expected_root applied_epoch_id in
  let* committed =
    effects.commit
      ~epoch_id:applied_epoch_id
      ~current_epoch
      ~expected_root
  in
  let* () =
    effects.cleanup
      ~epoch_id:applied_epoch_id
      ~post_consensus_root:committed.post_consensus_root
  in
  let* () = effects.lifecycle ~current_epoch in
  Lwt.return {
    applied_epoch_id;
    current_epoch;
    post_consensus_root = committed.post_consensus_root;
  }

let node_effects refs effects input =
  {
    current_epoch = (fun () -> !(refs.current_epoch));
    set_current_epoch = (fun epoch -> refs.current_epoch := epoch);
    expected_root = Consensus_finality_state.find_expected_root refs.finality_state;
    remove_proposer = Consensus_finality_state.remove_proposer refs.finality_state;
    prune_proposers_before =
      Consensus_finality_state.prune_proposers_before refs.finality_state;
    clear_spent_nonces = (fun () -> Octra_core.Ledger.clear_spent_nonces refs.ledger);
    commit = (fun ~epoch_id ~current_epoch ~expected_root ->
      let open Lwt.Syntax in
      let* committed =
        Commit.run
          (Commit.node_effects {
            data_dir = effects.data_dir;
            store = effects.store;
            ledger = refs.ledger;
            chaindata = effects.chaindata;
            finality_state = refs.finality_state;
            irmin_last_epoch = effects.irmin_last_epoch;
            exit = effects.exit;
          })
          {
            epoch_id;
            current_epoch;
            consensus_mode = input.consensus_mode;
            layera_diag = input.layera_diag;
            replay_trace = input.replay_trace;
            pre_state_hash = input.pre_state_hash;
            pre_consensus_root = input.pre_consensus_root;
            expected_root;
            parent_commit = input.parent_commit;
            proposer_addr = input.proposer_addr;
            proposer_info = input.proposer_info;
            proposer_source = input.proposer_source;
            round = input.round;
            validators = input.validators;
            validators_sha = input.validators_sha;
            ordered_txs_count = input.ordered_txs_count;
            confirmed_txs = input.confirmed_txs;
            confirmed_count = input.confirmed_count;
            confirmed_fees = input.confirmed_fees;
            plan = input.plan;
            prev_supply = input.prev_supply;
            emission_remaining = input.emission_remaining;
            reward_recipients = input.reward_recipients;
            epoch_receipts_json = input.epoch_receipts_json;
            account_addrs = input.proposer_addr :: input.active_validators;
            short = input.short;
            find_account = (fun addr ->
              Octra_core.Ledger.find_opt refs.ledger addr
              |> Option.map Guard.account_view_of_ledger_account);
            progress = Consensus_epoch_commit.commit_progress ();
          }
      in
      Lwt.return { post_consensus_root = committed.post_consensus_root });
    cleanup = (fun ~epoch_id ~post_consensus_root ->
      Cleanup.run_node
        (Cleanup.refs
          ~last_epoch_time:refs.last_epoch_time
          ~tree:refs.tree
          ~deferred_stealth_txs:refs.deferred_stealth_txs
          ~stealth_in_epoch_counter:refs.stealth_in_epoch_counter
          ~fhe_in_epoch_counter:refs.fhe_in_epoch_counter
          ~swarm_opt:refs.swarm_opt)
        {
          now = input.now;
          epoch_id;
          parent_commit = input.parent_commit;
          post_consensus_root;
          confirmed_count = input.confirmed_count;
          producer = input.producer;
          processed_hashes = input.processed_hashes;
          txs_serialized = input.txs_serialized;
        });
    lifecycle = (fun ~current_epoch ->
      Lifecycle.run_node ~store:effects.store ~current_epoch);
  }

let run_node refs effects input =
  run (node_effects refs effects input)