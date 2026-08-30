(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Env = Consensus_epoch_apply_env
module Finalize = Consensus_epoch_apply_finalize
module Footer = Consensus_epoch_apply_footer
module Post = Consensus_epoch_apply_post
module Proposer = Consensus_epoch_apply_proposer
module Reward = Consensus_reward_attribution
module Transaction = Octra_core.Transaction

type deps = {
  current_epoch : unit -> int;
  validator_pubkeys : Env.node_env -> (string * string) list;
  validator_context : (string * string) list -> Footer.validator_context;
  proposer : Proposer.runtime_request -> Proposer.runtime_result;
  reward :
    consensus_mode:bool ->
    epoch_id:int ->
    proposer_addr:string ->
    validator_pubkeys:(string * string) list ->
    (Reward.t, string) result;
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
  override_reward : Reward.t option;
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
  save_drops : Octra_core.Tx_staging.drop_record list -> unit;
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

let run (deps : deps) (request : request) =
  let open Lwt.Syntax in
  let epoch_id = deps.current_epoch () in
  let validator_pubkeys = deps.validator_pubkeys request.epoch_env in
  let validator_ctx = deps.validator_context validator_pubkeys in
  let active_validators = validator_ctx.Footer.active in
  let n_validators = validator_ctx.Footer.count in
  let proposer_runtime =
    deps.proposer
      Proposer.{
        runtime_epoch_id = epoch_id;
        runtime_consensus_mode = request.consensus_mode;
        runtime_active_validators = active_validators;
        runtime_override_proposer = request.override_proposer_info;
      }
  in
  let proposer_source = proposer_runtime.Proposer.source_label in
  let proposer_info = proposer_runtime.Proposer.proposer in
  let proposer_addr = proposer_info.Octra_core.Epochlog.creator_addr in
  let reward =
    match request.override_reward with
    | Some reward -> reward
    | None ->
      begin
        match
          deps.reward
            ~consensus_mode:request.consensus_mode
            ~epoch_id
            ~proposer_addr
            ~validator_pubkeys
        with
        | Ok reward -> reward
        | Error error -> failwith ("reward attribution rejected: " ^ error)
      end
  in
  let reward_source =
    match Reward.to_source reward with
    | Ok source -> source
    | Error error -> failwith ("reward source rejected: " ^ error)
  in
  let applied_commit_round = proposer_info.Octra_core.Epochlog.commit_round in
  let trace = deps.trace () in
  let validators_sha = validator_ctx.Footer.sha in
  deps.emit_replay_proposer
    trace
    ~epoch_id
    ~proposer_source
    ~proposer:proposer_addr
    ~validators_sha;
  let confirmed_txs = List.rev_map fst !(request.pending_tx_saves) in
  let epoch_ts =
    match
      Env.resolve_epoch_ts
        ~consensus_mode:request.consensus_mode
        request.epoch_env
        epoch_id
    with
    | Ok value -> value
    | Error error -> invalid_arg error
  in
  let* finalized =
    deps.finalize
      Finalize.{
        tree_ref = request.tree_ref;
        chain_id = request.epoch_env.chain_id;
        epoch_id;
        epoch_ts;
        epoch_start = request.epoch_start;
        proposer_addr;
        validator_addr = request.validator_addr;
        validator_pubkeys;
        active_validators;
        reward;
        ready_state_root_at = request.ready_state_root_at;
        ready_max_lag = request.ready_max_lag;
        confirmed_fees = !(request.confirmed_fees);
        confirmed_txs;
        deferred_count = List.length !(request.deferred_stealth_txs);
        short = request.short;
      }
  in
  let post_input =
    Post.input_of_finalized
      ~now:request.now
      ~epoch_ts
      ~consensus_mode:request.consensus_mode
      ~trace
      ~pre_state_hash:request.pre_state_hash
      ~pre_consensus_root:request.pre_consensus_root
      ~proposer_addr
      ~proposer_info
      ~proposer_source
      ~round:applied_commit_round
      ~validators:n_validators
      ~validators_sha
      ~ordered_txs_count:request.ordered_txs_count
      ~confirmed_txs
      ~confirmed_fees:!(request.confirmed_fees)
      ~epoch_receipts_json:request.epoch_receipts_json
      ~active_validators
      ~processed_hashes:!(request.processed_hashes)
      ~reward_source
      ~producer:request.producer
      ~short:request.short
      finalized
  in
  let* post = deps.post post_input in
  Lwt.return { proposer_source; proposer_addr; post }

let run_node (deps : node_deps) request =
  run
    {
      current_epoch = (fun () -> !(deps.current_epoch));
      validator_pubkeys = Env.current_validator_pubkeys;
      validator_context =
        Footer.validator_context ~hash:deps.hash ~raw_to_hex:deps.raw_to_hex;
      proposer = (fun runtime_request ->
        Proposer.run_node
          Proposer.{
            env = deps.env;
            finality = deps.finality_state;
            epoch_json = (fun epoch_id ->
              Octra_core.Chaindata_index.get_epoch
                (Octra_core.Store_chaindata.index deps.chaindata)
                epoch_id);
            log = deps.log_epoch;
            fatal = deps.fatal_epoch;
            short = deps.short;
          }
          runtime_request
          ~exit:(fun () ->
            deps.exit ();
            {
              Proposer.source_label = "exit";
              proposer = {
                Octra_core.Epochlog.creator_addr = "";
                commit_round = 0;
              };
            }));
      reward = (fun ~consensus_mode ~epoch_id ~proposer_addr ~validator_pubkeys ->
        match
          Consensus_finality_state.find_finalized
            deps.finality_state
            epoch_id
        with
        | Some finalize ->
          Reward.resolve_for_epoch
            ~epoch_id:(Int64.of_int epoch_id)
            ~proposer_addr
            ~validator_pubkeys
            finalize.Octra_consensus.C_types.parent_commit
        | None when consensus_mode ->
          Error "finalized reward source is missing"
        | None ->
          Ok (Reward.full_set ~proposer_addr ~validator_pubkeys));
      trace = (fun () -> Footer.trace ~env:deps.env);
      emit_replay_proposer = (fun trace ~epoch_id ~proposer_source ~proposer ~validators_sha ->
        Footer.emit_replay_proposer
          trace
          ~emit:deps.stdout
          ~epoch_id
          ~proposer_source
          ~proposer
          ~validators_sha);
      finalize = (fun input ->
        Finalize.run
          (Finalize.node_effects {
            store = deps.store;
            ledger = deps.ledger;
            get_meta = deps.get_meta;
          })
          input);
      post = (fun input ->
        Post.run_node
          (Post.refs
            ~current_epoch:deps.current_epoch
            ~finality_state:deps.finality_state
            ~ledger:deps.ledger
            ~last_epoch_time:deps.last_epoch_time
            ~tree:deps.tree
            ~deferred_stealth_txs:request.deferred_stealth_txs
            ~stealth_in_epoch_counter:deps.stealth_in_epoch_counter
            ~fhe_in_epoch_counter:deps.fhe_in_epoch_counter
            ~swarm_opt:deps.swarm_opt)
          Post.{
            data_dir = deps.data_dir;
            store = deps.store;
            chaindata = deps.chaindata;
            save_drops = deps.save_drops;
            irmin_last_epoch = (fun () ->
              match deps.get_meta "last_epoch" with
              | Some s -> Startup_process_shell.parse_int ~default:(-1) s
              | None -> -1);
            exit = deps.exit;
          }
          input);
    }
    request