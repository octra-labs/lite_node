(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type wallet = {
  address : string;
  pub : string;
  priv : string;
}

type p2p_refs = {
  consensus_config_hash : string ref;
  consensus_validator_set : Octra_consensus.C_types.validator_set ref;
  scheduled_validator_set : Octra_consensus.C_config.scheduled option ref;
  set_swarm : Octra_net.P2p_swarm.t -> unit;
}

type deps = {
  env : string -> string option;
  env_int : string -> int -> int;
  data_dir : string;
  chain_id : string;
  store : Octra_core.Store_irmin.t;
  chaindata : Octra_core.Store_chaindata.t;
  consensus_mode : bool;
  voting : bool;
  observer : bool;
  consensus_port : int;
  consensus_peers : string list;
  role_label : string;
  wallet : wallet;
  p2p_refs : p2p_refs;
  current_epoch : int ref;
  consensus_finalized : bool ref;
  catchup_active : bool ref;
  catchup_queue : Consensus_catchup_queue.t;
  runtime_state : Consensus_runtime_state.t;
  liveness_state : Consensus_liveness.state ref;
  driver_ref : Octra_consensus.C_driver.t option ref;
  proposal_state : Consensus_proposal_state.t;
  proposal_bundles : Consensus_bundle_cache.t;
  bundle_runtime : Consensus_bundle_cache.node_runtime;
  proposal_limits : Consensus_proposal.limits;
  current_round : unit -> int;
  finality : Consensus_finality_state.callbacks;
  catchup_queue_node : Consensus_catchup_shell.node_queue;
  read_active_validator_meta : unit -> string option;
  read_pending_validator_meta : unit -> string option;
  read_head_hash : unit -> string option;
  get_meta : string -> string option;
  read_persistent_pending : unit -> string option Lwt.t;
  read_persistent_marker : string -> string option Lwt.t;
  root_of_head_hash : string -> string;
  root_to_raw32 : string -> string;
  raw_to_hex : string -> string;
  read_prev_ledger_root : unit -> string option Lwt.t;
  find_account : string -> Octra_core.Ledger.account option;
  run_preverify :
    Octra_core.Transaction.t list ->
    Octra_core.Preverify_worker.batch Lwt.t;
  proposal_preview :
    ?catch_exn:bool ->
    Consensus_proposal.build_preview_request ->
    (Octra_core.Epoch_exec.exec_result, string) result Lwt.t;
  apply_catchup_record :
    Consensus_catchup_shell.validated_record ->
    unit Lwt.t;
  catchup_base_eic : unit -> string;
  next_txid : unit -> int64;
  cached_head : unit -> Octra_core.Head_manifest.t option;
  committed_epoch_root_raw : int -> string option;
  committed_head_epoch : unit -> int;
  read_local_root_raw : unit -> string Lwt.t;
  read_local_ledger_root_raw : unit -> string Lwt.t;
  cached_root : unit -> Consensus_driver_read.cached_root;
  clear_state_attested : unit -> unit;
  set_state_attested : head:int -> root:string -> unit;
  clear_quarantine : string -> unit;
  mark_quarantine : string -> unit;
  validator_pubkeys_for_epoch :
    wallet_addr:string ->
    wallet_pub:string ->
    epoch:int ->
    (string * string) list;
  proposal_capacity : Z.t;
  notify_staging_update : unit -> unit;
  quarantine_mismatch_threshold : int;
  soft_catchup_max_lag : int;
  quarantine_ahead_streak_threshold : int;
  quarantine_ahead_grace_epochs : int;
  quarantine_ahead_drift_tolerance : int;
  quarantine_poll_sec : float;
  liveness_stall_sec : float;
  state_readable : unit -> bool;
  sleep : float -> unit Lwt.t;
  now : unit -> float;
  exit_error : unit -> unit;
}

let make_install (deps : deps) =
  Startup_p2p_shell.install_refs
    ~consensus_config_hash:deps.p2p_refs.consensus_config_hash
    ~consensus_validator_set:deps.p2p_refs.consensus_validator_set
    ~scheduled_validator_set:deps.p2p_refs.scheduled_validator_set
    ~set_swarm:deps.p2p_refs.set_swarm

let start_p2p (deps : deps) =
  Startup_p2p_shell.start_node
    Startup_p2p_shell.{
      getenv = deps.env;
      info = Log.info "init" "%s";
      warn = Log.warn "init" "%s";
      fatal = (fun reason ->
        Log.fatal "init" "event = p2p_start_refused reason = %s" reason;
        deps.exit_error ());
      current_epoch = (fun () -> !(deps.current_epoch));
      read_active_validator_meta = deps.read_active_validator_meta;
      read_pending_validator_meta = deps.read_pending_validator_meta;
      read_head_hash = deps.read_head_hash;
      root_of_head_hash = deps.root_of_head_hash;
      install = make_install deps;
      activation_epoch = Validators.activation_epoch_int64;
      chain_id = deps.chain_id;
      consensus_mode = deps.consensus_mode;
      consensus_port = deps.consensus_port;
      consensus_peers = deps.consensus_peers;
      address = deps.wallet.address;
      priv_b64 = deps.wallet.priv;
      pub_b64 = deps.wallet.pub;
      voting = deps.voting;
      role_label = deps.role_label;
      read_persistent_pending = deps.read_persistent_pending;
      read_persistent_marker = deps.read_persistent_marker;
      current_height = (fun () -> Int64.of_int !(deps.current_epoch));
    }

let startup_finality (deps : deps) =
  Consensus_startup_finality.{
    committed_head_epoch = deps.committed_head_epoch;
    current_epoch = deps.current_epoch;
    last_finality = (fun () -> Octra_consensus.Finality_log.last deps.data_dir);
    drop_uncommitted_after =
      Octra_consensus.Finality_log.drop_uncommitted_after deps.data_dir;
    mark_quarantine = deps.mark_quarantine;
  }

let current_committed_root (deps : deps) =
  let head = deps.committed_head_epoch () in
  match deps.cached_head () with
  | Some manifest when manifest.Octra_core.Head_manifest.epoch_id = head ->
    Some (deps.root_to_raw32 manifest.state_root)
  | _ ->
    deps.committed_epoch_root_raw head

let committed_root_at_epoch (deps : deps) epoch =
  if epoch = deps.committed_head_epoch () then
    current_committed_root deps
  else
    deps.committed_epoch_root_raw epoch

let startup_pending (deps : deps) validator_set =
  match
    Consensus_finality_journal.read_validated
      ~chain_id:deps.chain_id
      ~validator_set
      deps.data_dir
  with
  | Consensus_finality_journal.Missing ->
    Consensus_finality_journal.Missing
  | Consensus_finality_journal.Invalid _ as invalid ->
    invalid
  | Consensus_finality_journal.Valid record ->
    begin
      match record.bundle with
      | Some _ ->
        Consensus_finality_journal.Valid record
      | None ->
        begin
          match Consensus_finality_journal.replayable record with
          | Error _ ->
            Consensus_finality_journal.Valid record
          | Ok replay ->
            begin
              match replay.bundle with
              | Some bundle ->
                Consensus_finality_journal.persist_bundle
                  deps.data_dir
                  record.finalize
                  bundle
              | None ->
                ()
            end;
            Consensus_finality_journal.Valid replay
        end
    end

let startup_journal (deps : deps) pending =
  Consensus_finality_journal_recovery.run
    Consensus_finality_journal_recovery.{
      read_journal = (fun () -> pending);
      head_epoch = deps.committed_head_epoch;
      root_at_epoch = committed_root_at_epoch deps;
      current_root = (fun () -> current_committed_root deps);
      write_finality = (fun finalize ->
        Octra_consensus.Finality_log.write
          deps.data_dir
          (Octra_consensus.Finality_log.of_finalize finalize));
      store_finalized = deps.finality.store_finalized;
      store_proposer = deps.finality.store_flow_proposer;
      store_expected_root = deps.finality.store_expected_root;
      store_bundle = deps.bundle_runtime.store_bundle;
      set_proposal = Consensus_proposal_state.set deps.proposal_state;
      reset_proposal_state = (fun () ->
        Consensus_proposal_state.reset_unsynced deps.proposal_state);
      set_consensus_finalized = (fun finalized ->
        deps.consensus_finalized := finalized);
      clear_state_attested = deps.clear_state_attested;
      commit_journal = (fun ~epoch ~state_root ->
        Consensus_finality_journal.promote_applied
          deps.data_dir
          ~epoch
          ~state_root);
      mark_quarantine = deps.mark_quarantine;
    }

let startup_backlog_anchor (deps : deps) pending =
  let head = deps.committed_head_epoch () in
  match pending with
  | Consensus_finality_journal.Valid record
    when Int64.equal
           record.finalize.Octra_consensus.C_types.epoch_id
           (Int64.of_int (head + 1)) ->
    Some
      (record.finalize.Octra_consensus.C_types.epoch_id,
       record.finalize.header.proposed_state_root)
  | Consensus_finality_journal.Valid _
  | Consensus_finality_journal.Missing
  | Consensus_finality_journal.Invalid _ ->
    Option.map
      (fun root -> Int64.of_int head, root)
      (current_committed_root deps)

let startup_backlog (deps : deps) validator_set pending =
  match startup_backlog_anchor deps pending with
  | None ->
    deps.mark_quarantine "finality_backlog_head_root_missing";
    Consensus_finality_backlog.Blocked
  | Some (head_epoch, head_root) ->
    Consensus_finality_backlog.run
      Consensus_finality_backlog.{
        read_backlog = (fun () ->
          Consensus_finality_journal.read_replay_backlog
            ~chain_id:deps.chain_id
            ~validator_set
            ~head_epoch
            ~head_root
            deps.data_dir);
        write_finality = (fun finalize ->
          Octra_consensus.Finality_log.write
            deps.data_dir
            (Octra_consensus.Finality_log.of_finalize finalize));
        store_finalized = deps.finality.store_finalized;
        store_proposer = deps.finality.store_flow_proposer;
        store_expected_root = deps.finality.store_expected_root;
        store_bundle = deps.bundle_runtime.store_bundle;
        set_consensus_finalized = (fun finalized ->
          deps.consensus_finalized := finalized);
        clear_state_attested = deps.clear_state_attested;
        mark_quarantine = deps.mark_quarantine;
      }

let startup_recovery (deps : deps) validator_set =
  let pending = startup_pending deps validator_set in
  match startup_journal deps pending with
  | Consensus_finality_journal_recovery.Blocked ->
    Consensus_finality_journal_recovery.Blocked
  | pending_outcome ->
    begin
      match startup_backlog deps validator_set pending with
      | Consensus_finality_backlog.Blocked ->
        Consensus_finality_journal_recovery.Blocked
      | Consensus_finality_backlog.Armed ->
        Consensus_finality_journal_recovery.Armed
      | Consensus_finality_backlog.Clean ->
        pending_outcome
    end

let finality_runtime (deps : deps) =
  Consensus_finality_runtime.create_node_runtime
    Consensus_finality_runtime.{
      data_dir = deps.data_dir;
      validator_set = (fun () -> !(deps.p2p_refs.consensus_validator_set));
      bundles = deps.bundle_runtime;
      driver_ref = deps.driver_ref;
      proposal_state = deps.proposal_state;
      catchup_queue = deps.catchup_queue;
      consensus_finalized = deps.consensus_finalized;
      current_epoch = deps.current_epoch;
      committed_head_epoch = deps.committed_head_epoch;
      sleep = deps.sleep;
      read_pre_finalize_root = deps.read_head_hash;
      read_commit_root = deps.read_prev_ledger_root;
      read_local_root_raw = deps.read_local_root_raw;
      apply_timeout_seconds =
        float_of_int
          (max 1 (deps.env_int "OCTRA_BFT_APPLY_TIMEOUT_SEC" 300));
      fatal_exit = deps.exit_error;
      catchup_active = deps.catchup_active;
      runtime_state = deps.runtime_state;
      finality = deps.finality;
    }

let fork_repair_runtime (deps : deps) =
  Consensus_health_wiring.node_fork_repair_runtime
    Consensus_health_wiring.{
      chain_id = deps.chain_id;
      committed_head_epoch = deps.committed_head_epoch;
      data_dir = deps.data_dir;
      store = deps.store;
      chaindata = deps.chaindata;
      finality = deps.finality;
      current_epoch = deps.current_epoch;
      catchup_active = deps.catchup_active;
      set_state_attested = deps.set_state_attested;
      clear_quarantine = deps.clear_quarantine;
      mark_quarantine = deps.mark_quarantine;
    }

let run_catchup_to_target (deps : deps) normalize finality_runtime =
  let drain_pending =
    finality_runtime.Consensus_finality_runtime.drain_pending
  in
  let validator_anchor = Consensus_validator_anchor.{
    getenv = deps.env;
    chain_id = deps.chain_id;
    current_height = (fun () -> Int64.of_int (deps.committed_head_epoch ()));
    active_raw = deps.read_active_validator_meta;
    pending_raw = deps.read_pending_validator_meta;
  } in
  Consensus_catchup_shell.node_driver_runner
    Consensus_catchup_shell.{
      chain_id = deps.chain_id;
      expected_validator_set_hash = (fun epoch ->
        Result.map
          Octra_consensus.C_config.validator_set_hash
          (Consensus_validator_anchor.expected_set
             validator_anchor
             ~epoch));
      catchup_active = deps.catchup_active;
      queue = deps.catchup_queue;
      committed_head_epoch = deps.committed_head_epoch;
      normalize;
      env_timeout = (fun () -> deps.env "OCTRA_CATCHUP_RANGE_TIMEOUT_SEC");
      read_local_root = deps.read_local_root_raw;
      cached_root = deps.cached_root;
      next_txid = deps.next_txid;
      finality = deps.finality;
      write_finality = (fun validated ->
        match validated.record.Octra_consensus.C_codec.finality with
        | None ->
          failwith "catchup finality is missing"
        | Some finality ->
          let entry =
            Octra_consensus.Finality_log.of_finalize finality.finalize
          in
          Octra_consensus.Finality_log.check_write deps.data_dir entry;
          Consensus_finality_journal.persist_certificate
            deps.data_dir
            ~validator_set:finality.validator_set
            finality.finalize;
          Consensus_finality_journal.persist_bundle
            deps.data_dir
            finality.finalize
            Consensus_finality_journal.{
              tx_hashes = validated.record.tx_hashes;
              txs = validated.parsed_txs;
              receipts_json = validated.record.receipts_json;
            };
          Octra_consensus.Finality_log.write deps.data_dir entry);
      promote_finality = (fun validated ->
        Consensus_finality_journal.promote_applied
          deps.data_dir
          ~epoch:validated.record.Octra_consensus.C_codec.epoch_id
          ~state_root:validated.record.state_root);
      apply_record = deps.apply_catchup_record;
      base_eic = deps.catchup_base_eic;
      set_state_attested = deps.set_state_attested;
      clear_quarantine = deps.clear_quarantine;
      mark_quarantine = deps.mark_quarantine;
      observer = deps.observer;
      drain_pending_finalized = drain_pending;
    }

let driver_gates (deps : deps) p2p =
  Consensus_driver_wiring.node_gates_of_runtime
    {
      consensus_mode = deps.consensus_mode;
      voting = deps.voting;
      consensus_config_hash = p2p.Startup_p2p_shell.consensus_config_hash;
      p2p_config = p2p.p2p_config;
      current_epoch = (fun () -> !(deps.current_epoch));
      log_blocked = (fun reason epoch ->
        Log.warn "consensus"
          "event = voting_paused reason = p2p_upgrade_not_ready detail = %s epoch = %d"
          reason
          epoch);
      catchup_active = deps.catchup_active;
      catchup_queue = deps.catchup_queue;
      pending_finalized = (fun () ->
        deps.finality.has_finalized !(deps.current_epoch));
      runtime_state = deps.runtime_state;
      mark_quarantine = deps.mark_quarantine;
      clear_quarantine = deps.clear_quarantine;
    }

let committed_reads ~readable (reads : Consensus_driver_read.deps) =
  Consensus_driver_read.{
    reads with
    get_epoch_json = (fun epoch ->
      if readable () then reads.get_epoch_json epoch else None);
    epoch_time = (fun epoch ->
      if readable () then reads.epoch_time epoch else None);
    get_tx_by_txid = (fun txid ->
      if readable () then reads.get_tx_by_txid txid else None);
    read_receipts = (fun epoch ->
      if readable () then reads.read_receipts epoch else []);
  }

let driver_config (deps : deps) p2p_start p2p gates run_catchup_to_target
    finality_runtime =
  let apply_finalized =
    finality_runtime.Consensus_finality_runtime.apply_finalized
  in
  let replay_stashed_while_safe =
    finality_runtime.Consensus_finality_runtime.replay_stashed_while_safe
  in
  let validator_policy =
    Octra_core.Validator_policy.of_env_exn deps.env
  in
  let parent_commit_source =
    match
      Consensus_parent_commit.create
        ~chain_id:deps.chain_id
        ~data_dir:deps.data_dir
        deps.env
    with
    | Ok source -> source
    | Error reason -> failwith reason
  in
  Consensus_driver_wiring.{
    standard = {
      getenv = deps.env;
      get_meta = deps.get_meta;
      wallet_addr = deps.wallet.address;
      wallet_pub = deps.wallet.pub;
      find_account = deps.find_account;
      cached_head = deps.cached_head;
      read_prev_ledger_root = deps.read_prev_ledger_root;
      next_txid = deps.next_txid;
      proposal_state = deps.proposal_state;
      catchup_active = deps.catchup_active;
      staging_epoch_capacity = deps.proposal_capacity;
      write_pending = Octra_core.Wal.write_pending_commit deps.data_dir;
      validator_pubkeys_for_epoch = deps.validator_pubkeys_for_epoch;
    };
    chain_id = deps.chain_id;
    my_addr = deps.wallet.address;
    sign_fn = p2p.Startup_p2p_shell.swarm_params.sign_fn;
    validator_set = p2p.active_vs;
    gates;
    proposal_limits = deps.proposal_limits;
    read_local_root_raw = deps.read_local_root_raw;
    read_local_ledger_root_raw = deps.read_local_ledger_root_raw;
    sleep = deps.sleep;
    quarantine_mismatch_threshold = deps.quarantine_mismatch_threshold;
    notify_staging_update = deps.notify_staging_update;
    run_preverify = deps.run_preverify;
    proposal_bundles = deps.proposal_bundles;
    store_bundle = deps.bundle_runtime.store_bundle;
    driver_ref = deps.driver_ref;
    proposal_preview = deps.proposal_preview;
    root_to_raw32 = deps.root_to_raw32;
    current_epoch = (fun () -> !(deps.current_epoch));
    current_round = deps.current_round;
    committed_head_epoch = deps.committed_head_epoch;
    load_parent_commit =
      Consensus_parent_commit.load parent_commit_source;
    verify_parent_commit =
      Consensus_parent_commit.verify parent_commit_source;
    finality = deps.finality;
    cached_head = deps.cached_head;
    now = deps.now;
    observer_mode = deps.observer;
    queue_catchup_target = deps.catchup_queue_node.queue_catchup_target;
    run_catchup_to_target;
    apply_finalized;
    replay_stashed_while_safe;
    driver_read_deps =
      Consensus_driver_read.node_store_deps
        ~chain_id:deps.chain_id
        ~chaindata:deps.chaindata
        ~data_dir:deps.data_dir
        ~root_to_raw32:deps.root_to_raw32
        ~reward_source:(fun epoch header ->
          Consensus_reward_attribution.epoch_source
            ~validator_activation_epoch:
              (Octra_core.Validator_policy.activation_epoch validator_policy)
            ~validator_pubkeys:
              (deps.validator_pubkeys_for_epoch
                 ~wallet_addr:deps.wallet.address
                 ~wallet_pub:deps.wallet.pub
                 ~epoch)
            header)
        ~cached_head:deps.cached_head
        ~lookup_bundle:deps.bundle_runtime.lookup_raw
      |> committed_reads ~readable:deps.state_readable;
    scheduled_validator_set_config = p2p.scheduled_validator_set_config;
    load_scheduled_validator_set_config =
      p2p_start.Startup_p2p_shell.load_scheduled_validator_set_config;
  }

let health (deps : deps) normalize finality_runtime fork_repair_runtime
    run_catchup_to_target =
  let drain_pending =
    finality_runtime.Consensus_finality_runtime.drain_pending
  in
  Consensus_health_wiring.{
    env_int = deps.env_int;
    getenv = deps.env;
    soft_catchup_max_lag = deps.soft_catchup_max_lag;
    quarantine_ahead_streak_threshold =
      deps.quarantine_ahead_streak_threshold;
    quarantine_ahead_grace_epochs = deps.quarantine_ahead_grace_epochs;
    quarantine_ahead_drift_tolerance =
      deps.quarantine_ahead_drift_tolerance;
    normalize_next_epoch_for_head = normalize;
    committed_head_epoch = deps.committed_head_epoch;
    current_epoch = (fun () -> !(deps.current_epoch));
    catchup_queue = deps.catchup_queue;
    catchup_active = deps.catchup_active;
    runtime_state = deps.runtime_state;
    set_state_attested = deps.set_state_attested;
    clear_quarantine = deps.clear_quarantine;
    mark_quarantine = deps.mark_quarantine;
    read_local_root_raw = deps.read_local_root_raw;
    committed_epoch_root_raw = deps.committed_epoch_root_raw;
    drain_pending_finalized = drain_pending;
    fork_repair = fork_repair_runtime;
    run_catchup_to_target;
    liveness_state = deps.liveness_state;
    now = deps.now;
    stall_sec = deps.liveness_stall_sec;
    observer = (fun () -> deps.observer);
    voting = (fun () -> deps.voting);
    finality = deps.finality;
  }

let pending (deps : deps) run_catchup_to_target validator_set =
  Consensus_pending_commit_recovery.{
    data_dir = deps.data_dir;
    query_timeout = 3.0;
    run_catchup_to_target;
    chain_id = deps.chain_id;
    validator_set;
    store_bundle = deps.bundle_runtime.store_bundle;
    validator_count = validator_set.Octra_consensus.C_types.n;
    quorum = validator_set.quorum;
  }

let durable_start_height (deps : deps) =
  let current = Int64.of_int !(deps.current_epoch) in
  let logged =
    match Octra_consensus.Finality_log.last deps.data_dir with
    | Some entry -> Int64.of_int (entry.height + 1)
    | None -> current
  in
  let recovering =
    if !(deps.consensus_finalized)
       || Consensus_finality_journal.pending deps.data_dir then
      Int64.succ current
    else
      current
  in
  max current (max logged recovering)

let run_driver (deps : deps) p2p_start p2p normalize finality_runtime
    run_catchup_to_target =
  let fork_repair = fork_repair_runtime deps in
  let gates = driver_gates deps p2p in
  let start_height = durable_start_height deps in
  ignore (Consensus_driver_launch_shell.run
    Consensus_driver_launch_shell.{
      driver_config =
        driver_config deps p2p_start p2p gates run_catchup_to_target
          finality_runtime;
      validator_set = p2p.Startup_p2p_shell.active_vs;
      swarm = p2p.swarm;
      activate_validator_set = p2p_start.activate_validator_set;
      driver_ref = deps.driver_ref;
      start_height;
      sleep = deps.sleep;
      health =
        health deps normalize finality_runtime fork_repair
          run_catchup_to_target;
      pending = pending deps run_catchup_to_target p2p.active_vs;
      recovery_pending = (fun () ->
        Consensus_finality_journal.pending deps.data_dir);
      poll_interval = deps.quarantine_poll_sec;
      pending_delay = 12.0;
      role_label = deps.role_label;
    })

let enabled consensus_port =
  consensus_port > 0

let run (deps : deps) =
  if not (enabled deps.consensus_port) then
    Log.info "init" "event = consensus_p2p_disabled reason = no_port"
  else
    let p2p_start = start_p2p deps in
    let p2p = p2p_start.view in
    if deps.consensus_mode then
      let startup = startup_finality deps in
      let normalize =
        Consensus_startup_finality.node_normalizer startup
      in
      (match startup_recovery deps p2p.active_vs with
       | Consensus_finality_journal_recovery.Continue ->
         Consensus_startup_finality.run_node_startup startup
       | Consensus_finality_journal_recovery.Armed
       | Consensus_finality_journal_recovery.Blocked ->
         ());
      let finality_runtime = finality_runtime deps in
      let run_catchup_to_target =
        run_catchup_to_target deps normalize finality_runtime
      in
      run_driver deps p2p_start p2p normalize finality_runtime
        run_catchup_to_target