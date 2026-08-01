(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Transaction = Octra_core.Transaction
module Address = Octra_core.Crypto.Address
module Staging = Octra_core.Tx_staging

type gates = {
  consensus_mode : unit -> bool;
  voting : unit -> bool;
  state_attested : unit -> bool;
  p2p_upgrade_ready : unit -> bool;
  catchup_active : unit -> bool;
  catchup_gap_active : unit -> bool;
  pending_finalized : unit -> bool;
  quarantine_active : unit -> bool;
  quarantine_reason : unit -> string;
  mark_quarantine : string -> unit;
  clear_quarantine : string -> unit;
  prev_root_streak : unit -> int;
  set_prev_root_streak : int -> unit;
  state_root_streak : unit -> int;
  set_state_root_streak : int -> unit;
}

type node_gates_input = {
  consensus_mode : bool;
  voting : bool;
  p2p_upgrade_ready : unit -> bool;
  catchup_active : bool ref;
  catchup_queue : Consensus_catchup_queue.t;
  pending_finalized : unit -> bool;
  runtime_state : Consensus_runtime_state.t;
  mark_quarantine : string -> unit;
  clear_quarantine : string -> unit;
}

type node_gates_runtime = {
  consensus_mode : bool;
  voting : bool;
  consensus_config_hash : string;
  p2p_config : P2p_config.t;
  current_epoch : unit -> int;
  log_blocked : string -> int -> unit;
  catchup_active : bool ref;
  catchup_queue : Consensus_catchup_queue.t;
  pending_finalized : unit -> bool;
  runtime_state : Consensus_runtime_state.t;
  mark_quarantine : string -> unit;
  clear_quarantine : string -> unit;
}

type epoch_normalizer_runtime = {
  current_epoch : int ref;
  committed_head_epoch : unit -> int;
  warn :
    source:string ->
    current:int ->
    committed_head:int ->
    expected_next:int ->
    unit;
}

type node_standard_adapter_runtime = {
  getenv : string -> string option;
  get_meta : string -> string option;
  wallet_addr : string;
  wallet_pub : string;
  find_account : string -> Octra_core.Ledger.account option;
  cached_head : unit -> Octra_core.Head_manifest.t option;
  read_prev_ledger_root : unit -> string option Lwt.t;
  next_txid : unit -> int64;
  proposal_state : Consensus_proposal_state.t;
  catchup_active : bool ref;
  staging_epoch_capacity : Z.t;
  write_pending : Octra_core.Wal.pending_commit -> unit;
  validator_pubkeys_for_epoch :
    wallet_addr:string ->
    wallet_pub:string ->
    epoch:int ->
    (string * string) list;
}

type standard_adapters = {
  layera_diag_live : unit -> bool;
  layera_env : unit -> string option;
  layera_fallback_addr : string;
  layera_meta : string -> string;
  public_key_for_tx : Transaction.t -> string option;
  verify_address_pubkey : addr:string -> pubkey:string -> bool;
  verify_tx_signature : Transaction.t -> pubkey:string -> bool;
  validator_pubkeys_fallback : int64 -> (string * string) list;
  prev_eic_root : unit -> string;
  next_txid : unit -> int64;
  read_prev_ledger_root : unit -> string option Lwt.t;
  staging_txs : unit -> Transaction.t list;
  staging_epoch_txs : unit -> Transaction.t list;
  staging_total : unit -> int;
  remove_rejected : string list -> unit;
  proposer : unit -> string;
  head_txid_hi : unit -> int64 option;
  set_proposal : Transaction.t list -> string list -> unit;
  current_tx_hashes : unit -> string list;
  mark_unsynced : unit -> unit;
  write_pending : Octra_core.Wal.pending_commit -> unit;
  set_catchup_active : bool -> unit;
}

type proposal_preview_runtime = {
  chain_id : string;
  ready_state_root_at : int -> string option Lwt.t;
  ready_max_lag : int;
  warn : string -> unit;
  run_preview :
    Consensus_proposal.build_preview_request ->
    reward:Consensus_reward_attribution.t ->
    env:Octra_core.Epoch_exec.env ->
    (Octra_core.Epoch_exec.exec_result, string) result Lwt.t;
}

type deps = {
  chain_id : string;
  my_addr : string;
  sign_fn : string -> string;
  validator_set : Octra_consensus.C_types.validator_set;
  gates : gates;
  proposal_limits : Consensus_proposal.limits;
  layera_diag_live : unit -> bool;
  layera_env : unit -> string option;
  layera_fallback_addr : string;
  layera_meta : string -> string;
  read_local_root_raw : unit -> string Lwt.t;
  read_local_ledger_root_raw : unit -> string Lwt.t;
  sleep : float -> unit Lwt.t;
  quarantine_mismatch_threshold : int;
  staging_txs : unit -> Transaction.t list;
  staging_epoch_txs : unit -> Transaction.t list;
  staging_total : unit -> int;
  remove_rejected : string list -> unit;
  notify_staging_update : unit -> unit;
  build_preverify : Consensus_preverify_role.build;
  validate_preverify : Consensus_preverify_role.validate;
  proposal_bundles : Consensus_bundle_cache.t;
  store_bundle :
    proposal_id:string ->
    tx_hashes:string list ->
    txs:Transaction.t list ->
    receipts_json:string list ->
    unit;
  driver_ref : Octra_consensus.C_driver.t option ref;
  public_key_for_tx : Transaction.t -> string option;
  verify_address_pubkey : addr:string -> pubkey:string -> bool;
  verify_tx_signature : Transaction.t -> pubkey:string -> bool;
  validator_pubkeys_fallback : int64 -> (string * string) list;
  proposal_preview :
    ?catch_exn:bool ->
    Consensus_proposal.build_preview_request ->
    (Octra_core.Epoch_exec.exec_result, string) result Lwt.t;
  prev_eic_root : unit -> string;
  next_txid : unit -> int64;
  root_to_raw32 : string -> string;
  current_epoch : unit -> int;
  current_round : unit -> int;
  committed_head_epoch : unit -> int;
  load_parent_commit :
    epoch_id:int64 ->
    (Octra_consensus.C_types.parent_commit option, string) result;
  verify_parent_commit :
    epoch_id:int64 ->
    Octra_consensus.C_types.parent_commit option ->
    (unit, string) result;
  finality : Consensus_finality_state.callbacks;
  read_prev_ledger_root : unit -> string option Lwt.t;
  cached_head : unit -> Octra_core.Head_manifest.t option;
  proposer : unit -> string;
  head_txid_hi : unit -> int64 option;
  set_proposal : Transaction.t list -> string list -> unit;
  current_tx_hashes : unit -> string list;
  mark_unsynced : unit -> unit;
  write_pending : Octra_core.Wal.pending_commit -> unit;
  now : unit -> float;
  observer_mode : bool;
  queue_catchup_target : target_epoch:int64 -> reason:string -> unit;
  run_catchup_to_target :
    Octra_consensus.C_driver.t ->
    target_epoch:int64 ->
    reason:string ->
    unit Lwt.t;
  set_catchup_active : bool -> unit;
  apply_finalized : Octra_consensus.C_types.finalize -> unit Lwt.t;
  replay_stashed_while_safe : source:string -> unit Lwt.t;
  driver_read_deps : Consensus_driver_read.deps;
  scheduled_validator_set_config :
    Octra_consensus.C_driver.scheduled_validator_set_config option;
  load_scheduled_validator_set_config :
    unit ->
    Octra_consensus.C_driver.scheduled_validator_set_config option Lwt.t;
}

type config_with_standard_input = {
  standard : standard_adapters;
  chain_id : string;
  my_addr : string;
  sign_fn : string -> string;
  validator_set : Octra_consensus.C_types.validator_set;
  gates : gates;
  proposal_limits : Consensus_proposal.limits;
  read_local_root_raw : unit -> string Lwt.t;
  read_local_ledger_root_raw : unit -> string Lwt.t;
  sleep : float -> unit Lwt.t;
  quarantine_mismatch_threshold : int;
  notify_staging_update : unit -> unit;
  build_preverify : Consensus_preverify_role.build;
  validate_preverify : Consensus_preverify_role.validate;
  proposal_bundles : Consensus_bundle_cache.t;
  store_bundle :
    proposal_id:string ->
    tx_hashes:string list ->
    txs:Transaction.t list ->
    receipts_json:string list ->
    unit;
  driver_ref : Octra_consensus.C_driver.t option ref;
  proposal_preview :
    ?catch_exn:bool ->
    Consensus_proposal.build_preview_request ->
    (Octra_core.Epoch_exec.exec_result, string) result Lwt.t;
  root_to_raw32 : string -> string;
  current_epoch : unit -> int;
  current_round : unit -> int;
  committed_head_epoch : unit -> int;
  load_parent_commit :
    epoch_id:int64 ->
    (Octra_consensus.C_types.parent_commit option, string) result;
  verify_parent_commit :
    epoch_id:int64 ->
    Octra_consensus.C_types.parent_commit option ->
    (unit, string) result;
  finality : Consensus_finality_state.callbacks;
  cached_head : unit -> Octra_core.Head_manifest.t option;
  now : unit -> float;
  observer_mode : bool;
  queue_catchup_target : target_epoch:int64 -> reason:string -> unit;
  run_catchup_to_target :
    Octra_consensus.C_driver.t ->
    target_epoch:int64 ->
    reason:string ->
    unit Lwt.t;
  apply_finalized : Octra_consensus.C_types.finalize -> unit Lwt.t;
  replay_stashed_while_safe : source:string -> unit Lwt.t;
  driver_read_deps : Consensus_driver_read.deps;
  scheduled_validator_set_config :
    Octra_consensus.C_driver.scheduled_validator_set_config option;
  load_scheduled_validator_set_config :
    unit ->
    Octra_consensus.C_driver.scheduled_validator_set_config option Lwt.t;
}

type node_driver_config_runtime = {
  standard : node_standard_adapter_runtime;
  chain_id : string;
  my_addr : string;
  sign_fn : string -> string;
  validator_set : Octra_consensus.C_types.validator_set;
  gates : gates;
  proposal_limits : Consensus_proposal.limits;
  read_local_root_raw : unit -> string Lwt.t;
  read_local_ledger_root_raw : unit -> string Lwt.t;
  sleep : float -> unit Lwt.t;
  quarantine_mismatch_threshold : int;
  notify_staging_update : unit -> unit;
  build_preverify : Consensus_preverify_role.build;
  validate_preverify : Consensus_preverify_role.validate;
  proposal_bundles : Consensus_bundle_cache.t;
  store_bundle :
    proposal_id:string ->
    tx_hashes:string list ->
    txs:Transaction.t list ->
    receipts_json:string list ->
    unit;
  driver_ref : Octra_consensus.C_driver.t option ref;
  proposal_preview :
    ?catch_exn:bool ->
    Consensus_proposal.build_preview_request ->
    (Octra_core.Epoch_exec.exec_result, string) result Lwt.t;
  root_to_raw32 : string -> string;
  current_epoch : unit -> int;
  current_round : unit -> int;
  committed_head_epoch : unit -> int;
  load_parent_commit :
    epoch_id:int64 ->
    (Octra_consensus.C_types.parent_commit option, string) result;
  verify_parent_commit :
    epoch_id:int64 ->
    Octra_consensus.C_types.parent_commit option ->
    (unit, string) result;
  finality : Consensus_finality_state.callbacks;
  cached_head : unit -> Octra_core.Head_manifest.t option;
  now : unit -> float;
  observer_mode : bool;
  queue_catchup_target : target_epoch:int64 -> reason:string -> unit;
  run_catchup_to_target :
    Octra_consensus.C_driver.t ->
    target_epoch:int64 ->
    reason:string ->
    unit Lwt.t;
  apply_finalized : Octra_consensus.C_types.finalize -> unit Lwt.t;
  replay_stashed_while_safe : source:string -> unit Lwt.t;
  driver_read_deps : Consensus_driver_read.deps;
  scheduled_validator_set_config :
    Octra_consensus.C_driver.scheduled_validator_set_config option;
  load_scheduled_validator_set_config :
    unit ->
    Octra_consensus.C_driver.scheduled_validator_set_config option Lwt.t;
}

let can_vote (deps : deps) =
  deps.gates.voting ()
  && deps.gates.state_attested ()
  && deps.gates.p2p_upgrade_ready ()
  && not (deps.gates.catchup_active ())
  && not (deps.gates.catchup_gap_active ())
  && not (deps.gates.pending_finalized ())
  && not (deps.gates.quarantine_active ())

let node_gates (input : node_gates_input) =
  {
    consensus_mode = (fun () -> input.consensus_mode);
    voting = (fun () -> input.voting);
    state_attested = (fun () ->
      Consensus_runtime_state.state_attested input.runtime_state);
    p2p_upgrade_ready = input.p2p_upgrade_ready;
    catchup_active = (fun () -> !(input.catchup_active));
    catchup_gap_active = (fun () ->
      Consensus_catchup_queue.gap_active input.catchup_queue);
    pending_finalized = input.pending_finalized;
    quarantine_active = (fun () ->
      Consensus_runtime_state.quarantine_active input.runtime_state);
    quarantine_reason = (fun () ->
      Consensus_runtime_state.quarantine_reason input.runtime_state);
    mark_quarantine = input.mark_quarantine;
    clear_quarantine = input.clear_quarantine;
    prev_root_streak = (fun () ->
      Consensus_runtime_state.prev_root_streak input.runtime_state);
    set_prev_root_streak = (fun n ->
      Consensus_runtime_state.set_prev_root_streak input.runtime_state n);
    state_root_streak = (fun () ->
      Consensus_runtime_state.state_root_streak input.runtime_state);
    set_state_root_streak = (fun n ->
      Consensus_runtime_state.set_state_root_streak input.runtime_state n);
  }

let p2p_upgrade_ready_of_runtime runtime =
  P2p_config.upgrade_ready_checker
    ~log_blocked:(fun reason ->
      runtime.log_blocked reason (runtime.current_epoch ()))
    ~epoch:(fun () -> Int64.of_int (runtime.current_epoch ()))
    ~consensus_config_hash:runtime.consensus_config_hash
    runtime.p2p_config

let node_gates_of_runtime runtime =
  node_gates
    {
      consensus_mode = runtime.consensus_mode;
      voting = runtime.voting;
      p2p_upgrade_ready = p2p_upgrade_ready_of_runtime runtime;
      catchup_active = runtime.catchup_active;
      catchup_queue = runtime.catchup_queue;
      pending_finalized = runtime.pending_finalized;
      runtime_state = runtime.runtime_state;
      mark_quarantine = runtime.mark_quarantine;
      clear_quarantine = runtime.clear_quarantine;
    }

let normalize_next_epoch_for_head (runtime : epoch_normalizer_runtime) ~source =
  let committed_head = runtime.committed_head_epoch () in
  let expected_next = committed_head + 1 in
  let current = !(runtime.current_epoch) in
  if current <> expected_next then begin
    runtime.warn ~source ~current ~committed_head ~expected_next;
    if current < expected_next then
      runtime.current_epoch := expected_next
  end

let node_standard_adapters runtime =
  {
    layera_diag_live = (fun () ->
      runtime.getenv "OCTRA_LAYERA_DIAG" = Some "1");
    layera_env = (fun () ->
      runtime.getenv "OCTRA_VALIDATORS");
    layera_fallback_addr = runtime.wallet_addr;
    layera_meta = (fun key ->
      match runtime.get_meta key with
      | Some s when s <> "" -> s
      | _ -> "-");
    public_key_for_tx = (fun tx ->
      Tx_sender_key.resolve ~find_account:runtime.find_account tx);
    verify_address_pubkey = (fun ~addr ~pubkey ->
      Address.verify_address_pubkey addr pubkey);
    verify_tx_signature = (fun tx ~pubkey ->
      Transaction.verify tx pubkey);
    validator_pubkeys_fallback = (fun epoch_id ->
      runtime.validator_pubkeys_for_epoch
        ~wallet_addr:runtime.wallet_addr
        ~wallet_pub:runtime.wallet_pub
        ~epoch:(Int64.to_int epoch_id));
    prev_eic_root = (fun () ->
      Consensus_proposal.prev_eic_root_from_head (runtime.cached_head ()));
    next_txid = runtime.next_txid;
    read_prev_ledger_root = runtime.read_prev_ledger_root;
    staging_txs = Staging.all;
    staging_epoch_txs = (fun () ->
      Staging.get_epoch_txs ~capacity:runtime.staging_epoch_capacity);
    staging_total = (fun () -> List.length (Staging.all ()));
    remove_rejected = Staging.remove_processed;
    proposer = (fun () -> runtime.wallet_addr);
    head_txid_hi = (fun () ->
      match runtime.cached_head () with
      | Some h -> Some h.Octra_core.Head_manifest.txid_hi
      | None -> None);
    set_proposal = Consensus_proposal_state.set runtime.proposal_state;
    current_tx_hashes = (fun () ->
      Consensus_proposal_state.tx_hashes runtime.proposal_state);
    mark_unsynced = (fun () ->
      Consensus_proposal_state.mark_unsynced runtime.proposal_state);
    write_pending = runtime.write_pending;
    set_catchup_active = (fun active ->
      runtime.catchup_active := active);
  }

let preview_with_optional_catch ~catch_exn ~warn run =
  if catch_exn then
    Lwt.catch
      run
      (fun exn ->
        let reason = Printexc.to_string exn in
        warn reason;
        Lwt.return (Stdlib.Error reason))
  else
    run ()

let node_proposal_preview
    (runtime : proposal_preview_runtime)
    ?(catch_exn = false)
    (request : Consensus_proposal.build_preview_request) =
  let run () =
    let reward =
      match
        Consensus_reward_attribution.resolve
          ~proposer_addr:request.proposer
          ~validator_pubkeys:request.validator_pubkeys
          request.parent_commit
      with
      | Ok reward -> reward
      | Error error -> failwith ("reward attribution rejected: " ^ error)
    in
    let env =
      Consensus_proposal.epoch_exec_env
        ~chain_id:runtime.chain_id
        ~epoch_id:request.epoch_id
        ~epoch_ts:request.epoch_ts
        ~proposer:request.proposer
        ~validator_pubkeys:request.validator_pubkeys
        ~prev_state_root:request.prev_state_root
        ~ready_state_root_at:runtime.ready_state_root_at
        ~ready_max_lag:runtime.ready_max_lag
    in
    runtime.run_preview request ~reward ~env
  in
  preview_with_optional_catch ~catch_exn ~warn:runtime.warn run

let layera_hash_validators =
  Octra_net.Hash_domain.hash "octra:validators:v1"

let make_layera_diag_context (deps : deps) ~validator_addrs =
  Consensus_proposal.layera_diag_context
    ~validator_addrs
    ~hash_validators:layera_hash_validators
    ~meta:deps.layera_meta

let make_layera_env_diag_context (deps : deps) () =
  Consensus_proposal.layera_env_diag_context
    ~env:(deps.layera_env ())
    ~fallback:deps.layera_fallback_addr
    ~hash_validators:layera_hash_validators
    ~meta:deps.layera_meta

let proposal_validator_pubkeys (deps : deps) epoch_id =
  Consensus_proposal.validator_pubkeys
    ~driver:!(deps.driver_ref)
    ~fallback:(fun () -> deps.validator_pubkeys_fallback epoch_id)

let driver_available (deps : deps) () =
  match !(deps.driver_ref) with
  | Some _ -> true
  | None -> false

let query_bundle (deps : deps) ~epoch_id ~proposal_id ~validate =
  match !(deps.driver_ref) with
  | None ->
    Lwt.return_none
  | Some driver ->
    Octra_consensus.C_driver.query_bundle
      driver
      ~epoch_id
      ~proposal_id
      ~timeout_seconds:3.0
      ~validate

let validate_bundle ~header ~expected_hashes response =
  match
    Consensus_bundle_validation.proposal
      ~header
      ~expected_hashes
      response
  with
  | Error _ -> None
  | Ok bundle -> Some bundle

let cached_proposal_bundle (deps : deps) ~proposal_id =
  match Consensus_bundle_cache.cached_with_log deps.proposal_bundles proposal_id with
  | Some bundle ->
    Some Consensus_bundle_fetch.{ txs = bundle.txs; receipts_json = bundle.receipts_json }
  | None ->
    None

let current_validator_set (deps : deps) =
  match !(deps.driver_ref) with
  | Some driver -> Octra_consensus.C_driver.active_validator_set driver
  | None -> deps.validator_set

let previous_epoch_ts (deps : deps) epoch =
  match deps.finality.find_finalized (Int64.to_int epoch) with
  | Some finalize ->
    Some finalize.Octra_consensus.C_types.header.ts
  | None ->
    Consensus_driver_read.epoch_time deps.driver_read_deps epoch

let verify_proposal_deps (deps : deps) =
  let validate_runner =
    Consensus_preverify_role.run_validate deps.validate_preverify
  in
  Consensus_proposal.{
    now = deps.now;
    previous_epoch_ts = previous_epoch_ts deps;
    quarantine_active = deps.gates.quarantine_active;
    quarantine_reason = deps.gates.quarantine_reason;
    mark_quarantine = deps.gates.mark_quarantine;
    prev_root_streak = deps.gates.prev_root_streak;
    set_prev_root_streak = deps.gates.set_prev_root_streak;
    state_root_streak = deps.gates.state_root_streak;
    set_state_root_streak = deps.gates.set_state_root_streak;
    limits = deps.proposal_limits;
    layera_diag_live = deps.layera_diag_live;
    layera_env_diag_context = make_layera_env_diag_context deps;
    layera_diag_context = make_layera_diag_context deps;
    wait_prev_root = {
      read_root = deps.read_local_root_raw;
      replay_stashed = (fun () ->
        deps.replay_stashed_while_safe ~source:"proposal_prev_root");
      sleep = deps.sleep;
    };
    max_prev_root_wait_tries = 60;
    prev_root_wait_delay_seconds = 0.05;
    quarantine_mismatch_threshold = deps.quarantine_mismatch_threshold;
    staging_txs = deps.staging_txs;
    cached_bundle = cached_proposal_bundle deps;
    validate_preverify_once = (fun ~state_root ~tx_hashes txs ->
      Consensus_bundle_cache.run_preverify_once
        deps.proposal_bundles
        ~purpose:Consensus_bundle_cache.Validate_proposal
        ~state_root
        ~tx_hashes
        ~txs
        validate_runner);
    driver_available = driver_available deps;
    validate_bundle;
    query_bundle = query_bundle deps;
    store_bundle = deps.store_bundle;
    public_key_for_tx = deps.public_key_for_tx;
    verify_address_pubkey = deps.verify_address_pubkey;
    verify_tx_signature = deps.verify_tx_signature;
    validator_pubkeys = proposal_validator_pubkeys deps;
    read_local_ledger_root = deps.read_local_ledger_root_raw;
    preview = deps.proposal_preview;
    prev_eic_root = deps.prev_eic_root;
    next_txid = deps.next_txid;
    root_to_raw32 = deps.root_to_raw32;
    set_proposal = deps.set_proposal;
    verify_parent_commit = deps.verify_parent_commit;
  }

let make_proposal_deps (deps : deps) =
  let build_runner =
    Consensus_preverify_role.run_build deps.build_preverify
  in
  Consensus_proposal.{
    start_height = (fun target_epoch ->
      match !(deps.driver_ref) with
      | Some driver ->
        Octra_consensus.C_driver.start_height driver target_epoch
      | None ->
        Lwt.return_unit);
    current_epoch = deps.current_epoch;
    state_attested = deps.gates.state_attested;
    quarantine_active = deps.gates.quarantine_active;
    quarantine_reason = deps.gates.quarantine_reason;
    read_prev_ledger_root = deps.read_prev_ledger_root;
    cached_head = deps.cached_head;
    current_round = deps.current_round;
    parent_commit = deps.load_parent_commit;
    frozen_bundle = Consensus_bundle_cache.find_frozen deps.proposal_bundles;
    store_bundle = deps.store_bundle;
    staging_txs = deps.staging_epoch_txs;
    admits_tx = (fun tx ->
      (not (deps.gates.consensus_mode ()))
      || Transaction.bft_consensus_admits_op
           tx.Transaction.op_type);
    build_preverify_once = (fun ~state_root ~tx_hashes txs ->
      Consensus_bundle_cache.run_preverify_once
        deps.proposal_bundles
        ~purpose:Consensus_bundle_cache.Build_proposal
        ~state_root
        ~tx_hashes
        ~txs
        build_runner);
    staging_total = deps.staging_total;
    proposer = deps.proposer;
    validator_pubkeys = proposal_validator_pubkeys deps;
    preview = deps.proposal_preview ~catch_exn:true;
    prev_eic_root = deps.prev_eic_root;
    next_txid = deps.next_txid;
    remove_rejected = deps.remove_rejected;
    notify_staging_update = deps.notify_staging_update;
    set_proposal = deps.set_proposal;
    head_txid_hi = deps.head_txid_hi;
    freeze = Consensus_bundle_cache.freeze deps.proposal_bundles;
    now = deps.now;
    previous_epoch_ts = previous_epoch_ts deps;
  }

let finalized_deps (deps : deps) =
  Consensus_finalized_shell.{
    prune_frozen = (fun ~finalized_epoch ->
      Consensus_bundle_cache.prune_frozen
        deps.proposal_bundles
        ~finalized_epoch);
    store_proposer = deps.finality.store_flow_proposer;
    store_expected_root = deps.finality.store_expected_root;
    store_finalized = deps.finality.store_finalized;
    remove_finalized = deps.finality.remove_finalized;
    remove_proposer = deps.finality.remove_proposer;
    committed_head_epoch = deps.committed_head_epoch;
    driver = (fun () -> !(deps.driver_ref));
    observer_mode = deps.observer_mode;
    catchup_active = deps.gates.catchup_active;
    quarantine_active = deps.gates.quarantine_active;
    state_attested = deps.gates.state_attested;
    current_epoch = deps.current_epoch;
    read_local_root_raw = deps.read_local_root_raw;
    queue_catchup_target = deps.queue_catchup_target;
    run_catchup_to_target = deps.run_catchup_to_target;
    mark_quarantine = deps.gates.mark_quarantine;
    clear_quarantine = deps.gates.clear_quarantine;
    set_catchup_active = deps.set_catchup_active;
    apply_finalized = deps.apply_finalized;
    replay_stashed_while_safe = deps.replay_stashed_while_safe;
  }

let before_precommit (deps : deps) ~epoch_id ~round ~proposal_id ~proposed_state_root
    ~txid_hi ~proposal_wire ~vote_wire =
  Lwt.return
    (Consensus_proposal.handle_before_precommit
      {
        chain_id = deps.chain_id;
        validator_set = (fun () -> current_validator_set deps);
        current_tx_hashes = deps.current_tx_hashes;
        cached_bundle = Consensus_bundle_cache.peek_raw deps.proposal_bundles;
        sync_bundle = (fun ~tx_hashes ~txs ~receipts_json:_ ->
          deps.set_proposal txs tx_hashes);
        mark_unsynced = deps.mark_unsynced;
        write_pending = deps.write_pending;
        now = deps.now;
      }
      ~epoch_id
      ~round
      ~proposal_id
      ~proposed_state_root
      ~txid_hi
      ~validator_addr:deps.my_addr
      ~proposal_wire
      ~vote_wire)

let config (deps : deps) =
  Octra_consensus.C_driver.{
    chain_id = deps.chain_id;
    my_addr = deps.my_addr;
    sign_fn = deps.sign_fn;
    verify_fn = (fun addr msg sig_bytes ->
      match
        Octra_consensus.C_types.pubkey_of_addr
          (current_validator_set deps)
          addr
      with
      | Some pk ->
        Octra_consensus.C_hash.verify_ed25519
          ~pubkey_raw:pk
          ~msg
          ~signature:sig_bytes
      | None ->
        false);
    role_can_vote = deps.gates.voting;
    can_vote = (fun () -> can_vote deps);
    execute_fn = (fun _propose -> true);
    verify_proposal = (fun propose ->
      Consensus_proposal.verify_proposal
        (verify_proposal_deps deps)
        ~chain_id:deps.chain_id
        propose);
    verify_parent_commit = deps.verify_parent_commit;
    on_finalized = (fun finalize ->
      Consensus_finalized_shell.handle (finalized_deps deps) finalize);
    make_proposal = (fun epoch_id ->
      Consensus_proposal.make_proposal
        (make_proposal_deps deps)
        ~chain_id:deps.chain_id
        ~root_to_raw32:deps.root_to_raw32
        ~limits:deps.proposal_limits
        ~epoch_id);
    before_precommit_broadcast = before_precommit deps;
    lookup_epoch_root = Consensus_driver_read.epoch_root deps.driver_read_deps;
    local_head_epoch = (fun () ->
      Consensus_driver_read.local_head_epoch deps.driver_read_deps);
    lookup_bundle = Consensus_driver_read.bundle deps.driver_read_deps;
    lookup_catchup_range =
      Consensus_driver_read.catchup_range deps.driver_read_deps;
    on_resource_attestation = Consensus_resource_attestation.log_seen;
    scheduled_validator_set_config = deps.scheduled_validator_set_config;
    load_scheduled_validator_set_config =
      deps.load_scheduled_validator_set_config;
    resource_committee_config = None;
  }

let config_with_standard (input : config_with_standard_input) =
  let standard = input.standard in
  config
    {
      chain_id = input.chain_id;
      my_addr = input.my_addr;
      sign_fn = input.sign_fn;
      validator_set = input.validator_set;
      gates = input.gates;
      proposal_limits = input.proposal_limits;
      layera_diag_live = standard.layera_diag_live;
      layera_env = standard.layera_env;
      layera_fallback_addr = standard.layera_fallback_addr;
      layera_meta = standard.layera_meta;
      read_local_root_raw = input.read_local_root_raw;
      read_local_ledger_root_raw = input.read_local_ledger_root_raw;
      sleep = input.sleep;
      quarantine_mismatch_threshold = input.quarantine_mismatch_threshold;
      staging_txs = standard.staging_txs;
      staging_epoch_txs = standard.staging_epoch_txs;
      staging_total = standard.staging_total;
      remove_rejected = standard.remove_rejected;
      notify_staging_update = input.notify_staging_update;
      build_preverify = input.build_preverify;
      validate_preverify = input.validate_preverify;
      proposal_bundles = input.proposal_bundles;
      store_bundle = input.store_bundle;
      driver_ref = input.driver_ref;
      public_key_for_tx = standard.public_key_for_tx;
      verify_address_pubkey = standard.verify_address_pubkey;
      verify_tx_signature = standard.verify_tx_signature;
      validator_pubkeys_fallback = standard.validator_pubkeys_fallback;
      proposal_preview = input.proposal_preview;
      prev_eic_root = standard.prev_eic_root;
      next_txid = standard.next_txid;
      root_to_raw32 = input.root_to_raw32;
      current_epoch = input.current_epoch;
      current_round = input.current_round;
      committed_head_epoch = input.committed_head_epoch;
      load_parent_commit = input.load_parent_commit;
      verify_parent_commit = input.verify_parent_commit;
      finality = input.finality;
      read_prev_ledger_root = standard.read_prev_ledger_root;
      cached_head = input.cached_head;
      proposer = standard.proposer;
      head_txid_hi = standard.head_txid_hi;
      set_proposal = standard.set_proposal;
      current_tx_hashes = standard.current_tx_hashes;
      mark_unsynced = standard.mark_unsynced;
      write_pending = standard.write_pending;
      now = input.now;
      observer_mode = input.observer_mode;
      queue_catchup_target = input.queue_catchup_target;
      run_catchup_to_target = input.run_catchup_to_target;
      set_catchup_active = standard.set_catchup_active;
      apply_finalized = input.apply_finalized;
      replay_stashed_while_safe = input.replay_stashed_while_safe;
      driver_read_deps = input.driver_read_deps;
      scheduled_validator_set_config = input.scheduled_validator_set_config;
      load_scheduled_validator_set_config =
        input.load_scheduled_validator_set_config;
    }

let node_driver_config (runtime : node_driver_config_runtime) =
  config_with_standard
    {
      standard = node_standard_adapters runtime.standard;
      chain_id = runtime.chain_id;
      my_addr = runtime.my_addr;
      sign_fn = runtime.sign_fn;
      validator_set = runtime.validator_set;
      gates = runtime.gates;
      proposal_limits = runtime.proposal_limits;
      read_local_root_raw = runtime.read_local_root_raw;
      read_local_ledger_root_raw = runtime.read_local_ledger_root_raw;
      sleep = runtime.sleep;
      quarantine_mismatch_threshold =
        runtime.quarantine_mismatch_threshold;
      notify_staging_update = runtime.notify_staging_update;
      build_preverify = runtime.build_preverify;
      validate_preverify = runtime.validate_preverify;
      proposal_bundles = runtime.proposal_bundles;
      store_bundle = runtime.store_bundle;
      driver_ref = runtime.driver_ref;
      proposal_preview = runtime.proposal_preview;
      root_to_raw32 = runtime.root_to_raw32;
      current_epoch = runtime.current_epoch;
      current_round = runtime.current_round;
      committed_head_epoch = runtime.committed_head_epoch;
      load_parent_commit = runtime.load_parent_commit;
      verify_parent_commit = runtime.verify_parent_commit;
      finality = runtime.finality;
      cached_head = runtime.cached_head;
      now = runtime.now;
      observer_mode = runtime.observer_mode;
      queue_catchup_target = runtime.queue_catchup_target;
      run_catchup_to_target = runtime.run_catchup_to_target;
      apply_finalized = runtime.apply_finalized;
      replay_stashed_while_safe = runtime.replay_stashed_while_safe;
      driver_read_deps = runtime.driver_read_deps;
      scheduled_validator_set_config =
        runtime.scheduled_validator_set_config;
      load_scheduled_validator_set_config =
        runtime.load_scheduled_validator_set_config;
    }