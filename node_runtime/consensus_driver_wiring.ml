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


module Transaction = Octra_core.Transaction

type gates = {
  consensus_mode : unit -> bool;
  voting : unit -> bool;
  state_attested : unit -> bool;
  p2p_upgrade_ready : unit -> bool;
  catchup_active : unit -> bool;
  catchup_gap_active : unit -> bool;
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
  runtime_state : Consensus_runtime_state.t;
  mark_quarantine : string -> unit;
  clear_quarantine : string -> unit;
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
  run_preverify : Transaction.t list -> Octra_core.Preverify_worker.batch Lwt.t;
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

let can_vote (deps : deps) =
  deps.gates.voting ()
  && deps.gates.state_attested ()
  && deps.gates.p2p_upgrade_ready ()
  && not (deps.gates.catchup_active ())
  && not (deps.gates.catchup_gap_active ())
  && not (deps.gates.quarantine_active ())

let node_gates input =
  {
    consensus_mode = (fun () -> input.consensus_mode);
    voting = (fun () -> input.voting);
    state_attested = (fun () ->
      Consensus_runtime_state.state_attested input.runtime_state);
    p2p_upgrade_ready = input.p2p_upgrade_ready;
    catchup_active = (fun () -> !(input.catchup_active));
    catchup_gap_active = (fun () ->
      Consensus_catchup_queue.gap_active input.catchup_queue);
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

let verify_proposal_deps (deps : deps) =
  Consensus_proposal.{
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
      sleep = deps.sleep;
    };
    max_prev_root_wait_tries = 60;
    prev_root_wait_delay_seconds = 0.05;
    quarantine_mismatch_threshold = deps.quarantine_mismatch_threshold;
    staging_txs = deps.staging_txs;
    cached_bundle = cached_proposal_bundle deps;
    run_preverify = deps.run_preverify;
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
  }

let make_proposal_deps (deps : deps) =
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
    frozen_bundle = Consensus_bundle_cache.find_frozen deps.proposal_bundles;
    store_bundle = deps.store_bundle;
    staging_txs = deps.staging_epoch_txs;
    admits_tx = (fun tx ->
      (not (deps.gates.consensus_mode ()))
      || Transaction.bft_admits_op tx.Transaction.op_type);
    run_preverify = deps.run_preverify;
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
    ~txid_hi =
  Consensus_proposal.handle_before_precommit
    {
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
    ~validator_addr:deps.my_addr;
  Lwt.return_unit

let config (deps : deps) =
  Octra_consensus.C_driver.{
    chain_id = deps.chain_id;
    my_addr = deps.my_addr;
    sign_fn = deps.sign_fn;
    verify_fn = (fun addr msg sig_bytes ->
      match Octra_consensus.C_types.pubkey_of_addr deps.validator_set addr with
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