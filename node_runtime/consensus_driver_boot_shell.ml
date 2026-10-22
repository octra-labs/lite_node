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
  sync_validators :
    unit ->
    (Octra_consensus.C_types.validator_set, string) result;
  sync_exporters :
    unit ->
    (Octra_consensus.C_types.validator_set, string) result;
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
  on_driver : Octra_consensus.C_driver.t -> unit;
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
  duty_state : Octra_core.Head_manifest.t -> (Octra_core.Set_fold.t, string) result;
  read_persistent_pending : unit -> string option Lwt.t;
  root_of_head_hash : string -> string;
  root_to_raw32 : string -> string;
  raw_to_hex : string -> string;
  read_prev_ledger_root : unit -> string option Lwt.t;
  find_account : string -> Octra_core.Ledger.account option;
  build_preverify : Consensus_preverify_role.build;
  validate_preverify : Consensus_preverify_role.validate;
  proposal_preview :
    ?catch_exn:bool ->
    Consensus_proposal.build_preview_request ->
    (Octra_core.Epoch_exec.exec_result, string) result Lwt.t;
  prepare_at : Consensus_driver_wiring.prepare_at;
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
  save_drops : Octra_core.Tx_staging.drop_record list -> unit;
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
  require_sync : Sync_need.t -> unit;
  exit_error : unit -> unit;
  exit_refused : unit -> unit;
}

let make_install (deps : deps) =
  Startup_p2p_shell.install_refs
    ~consensus_config_hash:deps.p2p_refs.consensus_config_hash
    ~consensus_validator_set:deps.p2p_refs.consensus_validator_set
    ~scheduled_validator_set:deps.p2p_refs.scheduled_validator_set
    ~set_swarm:deps.p2p_refs.set_swarm

let validator_state_height ~committed_head_epoch =
  Int64.of_int (committed_head_epoch ())

let start_p2p (deps : deps) =
  Startup_p2p_shell.start_node
    Startup_p2p_shell.{
      getenv = deps.env;
      info = Log.info "init" "%s";
      warn = Log.warn "init" "%s";
      fatal = (fun reason ->
        Log.fatal "init" "event = p2p_start_refused reason = %s" reason;
        deps.exit_error ());
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
      current_height = (fun () ->
        validator_state_height
          ~committed_head_epoch:deps.committed_head_epoch);
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

let check_conflict (deps : deps) =
  match Sync_mark.read ~data_dir:deps.data_dir ~chain:deps.chain_id with
  | Sync_mark.Ready need when need.Sync_need.cause = Sync_need.Conflict ->
    deps.clear_state_attested ();
    deps.mark_quarantine "finality_conflict";
    deps.require_sync need;
    failwith "finality conflict recovery did not stop"
  | Sync_mark.Invalid reason ->
    deps.clear_state_attested ();
    deps.mark_quarantine reason;
    deps.exit_error ();
    failwith reason
  | Sync_mark.Ready _ | Sync_mark.Missing -> ()

let journal_write (deps : deps) finalize action =
  let epoch = finalize.Octra_consensus.C_types.epoch_id in
  check_conflict deps;
  try Consensus_finality_journal.guard deps.data_dir finalize action with exn ->
    check_conflict deps;
    match Consensus_finality_journal.classify_conflict exn with
    | Some conflict
      when Int64.compare epoch 0L >= 0
           && Int64.compare epoch (Int64.of_int max_int) <= 0 ->
        Log.fatal "consensus"
          "event = finality_conflict kind = %s epoch = %Ld action = signed_snapshot"
          (Consensus_finality_journal.conflict_label conflict)
          epoch;
        let epoch = Int64.to_int epoch in
        deps.require_sync
          (Sync_need.conflict ~epoch ~head:(max 0 (epoch - 1)));
        failwith "finality conflict recovery did not stop"
    | _ -> raise exn

let validator_anchor (deps : deps) =
  Consensus_validator_anchor.{
    getenv = deps.env;
    chain_id = deps.chain_id;
    current_height = (fun () -> Int64.of_int (deps.committed_head_epoch ()));
    active_raw = deps.read_active_validator_meta;
    pending_raw = deps.read_pending_validator_meta;
    relief = (fun through_height ->
      Octra_consensus.C_relief_log.latest
        (Octra_consensus.C_relief_log.disk ~data_dir:deps.data_dir) ~through_height);
  }

let selected_pending ?repair (deps : deps) =
  Consensus_finality_journal.read_selected ?repair ~chain_id:deps.chain_id
    ~expected_set:(fun epoch ->
      Consensus_validator_anchor.expected_set (validator_anchor deps) ~epoch)
    deps.data_dir

let startup_pending (deps : deps) =
  let repair = Option.map (fun root -> deps.committed_head_epoch (), root)
    (current_committed_root deps) in
  match selected_pending ?repair deps with
  | Error _ as error -> error
  | Ok Consensus_finality_journal.Missing as missing -> missing
  | Ok (Consensus_finality_journal.Invalid _) as invalid -> invalid
  | Ok (Consensus_finality_journal.Valid record) as valid ->
    begin
      match record.bundle with
      | Some _ -> valid
      | None ->
        begin
          match Consensus_finality_journal.replayable record with
          | Error _ -> valid
          | Ok replay ->
            begin
              match replay.bundle with
              | Some bundle ->
                journal_write
                  deps
                  record.finalize
                  (fun () ->
                    Consensus_finality_journal.persist_bundle
                      deps.data_dir
                      record.finalize
                      bundle)
              | None ->
                ()
            end;
            Ok (Consensus_finality_journal.Valid replay)
        end
    end

let startup_journal (deps : deps) pending =
  Consensus_finality_journal_recovery.run
    Consensus_finality_journal_recovery.{
      read_journal = (fun () -> pending);
      read_pending_epoch = (fun () ->
        Consensus_finality_journal.read_pending_epoch deps.data_dir);
      drop_invalid_unapplied = (fun ~head_epoch ->
        match selected_pending deps with
        | Ok (Consensus_finality_journal.Invalid _) ->
          Consensus_finality_journal.drop_invalid_unapplied
            deps.data_dir
            ~head:head_epoch
        | Ok Consensus_finality_journal.Missing ->
          Error "pending finality journal disappeared"
        | Ok (Consensus_finality_journal.Valid _) ->
          Error "pending finality journal became valid"
        | Error (Consensus_finality_journal.Read_set reason
            | Consensus_finality_journal.Read_record reason
            | Consensus_finality_journal.Read_bundle reason) -> Error reason);
      head_epoch = deps.committed_head_epoch;
      root_at_epoch = committed_root_at_epoch deps;
      current_root = (fun () -> current_committed_root deps);
      write_finality = (fun finalize ->
        Consensus_finality_journal.guard deps.data_dir finalize (fun () ->
          Octra_consensus.Finality_log.write
            deps.data_dir
            (Octra_consensus.Finality_log.of_finalize finalize)));
      store_finalized = deps.finality.store_finalized_with_set;
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
      require_sync = deps.require_sync;
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

let startup_backlog ?genesis (deps : deps) pending =
  match startup_backlog_anchor deps pending with
  | None when pending = Consensus_finality_journal.Missing
      && deps.cached_head () = None
      && Option.fold ~none:false ~some:(fun value ->
        Genesis_start.consume value ~data_dir:deps.data_dir ~chain:deps.chain_id
          ~epoch:!(deps.current_epoch) ~head:(deps.committed_head_epoch ())) genesis ->
    Consensus_finality_backlog.Clean
  | None ->
    deps.mark_quarantine "finality_backlog_head_root_missing";
    Consensus_finality_backlog.Blocked
  | Some (head_epoch, head_root) ->
    Consensus_finality_backlog.run
      Consensus_finality_backlog.{
        read_backlog = (fun () ->
          Consensus_finality_journal.read_replay_backlog
            ~chain_id:deps.chain_id
            ~expected_set:(fun epoch ->
              Consensus_validator_anchor.expected_set (validator_anchor deps) ~epoch)
            ~head_epoch
            ~head_root
            deps.data_dir);
        write_finality = (fun finalize ->
          Consensus_finality_journal.guard deps.data_dir finalize (fun () ->
            Octra_consensus.Finality_log.write
              deps.data_dir
              (Octra_consensus.Finality_log.of_finalize finalize)));
        store_finalized = deps.finality.store_finalized_with_set;
        store_proposer = deps.finality.store_flow_proposer;
        store_expected_root = deps.finality.store_expected_root;
        store_bundle = deps.bundle_runtime.store_bundle;
        set_consensus_finalized = (fun finalized ->
          deps.consensus_finalized := finalized);
        clear_state_attested = deps.clear_state_attested;
        mark_quarantine = deps.mark_quarantine;
      }

let startup_recovery ?genesis (deps : deps) =
  let pending = startup_pending deps in
  match startup_journal deps pending, pending with
  | Consensus_finality_journal_recovery.Blocked, _
  | _, Error _ ->
    Consensus_finality_journal_recovery.Blocked
  | pending_outcome, Ok pending ->
    begin
      match startup_backlog ?genesis deps pending with
      | Consensus_finality_backlog.Blocked ->
        Consensus_finality_journal_recovery.Blocked
      | Consensus_finality_backlog.Armed ->
        Consensus_finality_journal_recovery.Armed
      | Consensus_finality_backlog.Clean ->
        pending_outcome
    end

let replay_applied_pending (deps : deps) () =
  if Consensus_finality_journal.pending deps.data_dir then
    match startup_pending deps with
    | Ok (Consensus_finality_journal.Valid record)
      when record.finalize.Octra_consensus.C_types.epoch_id
        <= Int64.of_int (deps.committed_head_epoch ()) ->
      let outcome = startup_journal deps
        (Ok (Consensus_finality_journal.Valid record)) in
      let result = match outcome with
        | Consensus_finality_journal_recovery.Continue -> "continue"
        | Consensus_finality_journal_recovery.Armed -> "armed"
        | Consensus_finality_journal_recovery.Blocked -> "blocked" in
      Octra_log.info "consensus"
        "event = finality_journal_recovery action = replay_pending epoch = %Ld result = %s"
        record.finalize.epoch_id result
    | Error (Consensus_finality_journal.Read_record reason
      | Consensus_finality_journal.Read_bundle reason) ->
      raise (Consensus_pending_commit_recovery.Store_fault reason)
    | _ -> ()

let finality_set (deps : deps) epoch =
  if epoch < 0L || epoch = Int64.max_int then Error "historical finality epoch is invalid"
  else
    Result.bind (Consensus_parent_commit.create ~chain_id:deps.chain_id
      ~data_dir:deps.data_dir ~chaindata:deps.chaindata deps.env) (fun source ->
      Result.bind (Consensus_parent_commit.load source ~epoch_id:(Int64.succ epoch)) (function
        | Some parent -> Ok parent.Octra_consensus.C_types.validator_set
        | None -> Error "historical validator set is missing"))

let repair_set ?policy (deps : deps) epoch =
  let policy = Option.value policy
    ~default:(Octra_consensus.C_quorum_policy.for_chain deps.chain_id) in
  let current = match !(deps.driver_ref) with
    | Some driver -> Octra_consensus.C_driver.current_height driver
    | None -> Int64.max epoch (Int64.of_int !(deps.current_epoch)) in
  if Octra_consensus.C_quorum_policy.select policy current = Octra_consensus.C_quorum_policy.Stake
  then finality_set deps epoch
  else Consensus_validator_anchor.expected_set (validator_anchor deps) ~epoch

let attest_committed (deps : deps) validator_set =
  check_conflict deps;
  let head = deps.committed_head_epoch () in
  match Octra_consensus.Finality_log.last deps.data_dir,
        current_committed_root deps with
  | Some entry, Some root ->
    begin
      let selected =
        if Octra_consensus.C_quorum_policy.linear_weight
          ~chain_id:deps.chain_id ~epoch_id:(Int64.of_int head)
        then finality_set deps (Int64.of_int head) else Ok validator_set in
      match Result.bind selected (fun validator_set ->
        Consensus_finality_journal.attested_root
          ~chain_id:deps.chain_id
          ~validator_set
          ~head
          ~root
          ~entry
          deps.data_dir)
      with
      | Ok root ->
        deps.set_state_attested ~head ~root;
        Log.info "consensus"
          "event = startup_state_attested head = %d root = %s"
          head
          (deps.raw_to_hex root)
      | Error reason ->
        Log.warn "consensus"
          "event = startup_state_attestation status = held head = %d reason = %s"
          head
          reason
    end
  | None, _ ->
    Log.warn "consensus"
      "event = startup_state_attestation status = held head = %d reason = finality_missing"
      head
  | _, None ->
    Log.warn "consensus"
      "event = startup_state_attestation status = held head = %d reason = root_missing"
      head

let rebind_committed (deps : deps) validator_set =
  match deps.read_active_validator_meta () with
  | None
  | Some "" ->
    Log.warn "finality"
      "event = finality_journal_rebind status = skipped reason = active_validator_set_missing";
    false
  | Some _ ->
    match Octra_consensus.Finality_log.last deps.data_dir with
    | None ->
      false
    | Some entry ->
      let selected =
        if Octra_consensus.C_quorum_policy.linear_weight
          ~chain_id:deps.chain_id ~epoch_id:(Int64.of_int entry.height)
        then finality_set deps (Int64.of_int entry.height) else Ok validator_set in
      match selected with
      | Error reason ->
        Log.warn "finality" "event = finality_journal_rebind status = held reason = %s" reason;
        true
      | Ok validator_set ->
      let proof_needed =
        Consensus_finality_journal.proof_needed
          ~chain_id:deps.chain_id
          ~validator_set
          ~entry
          deps.data_dir
      in
      begin
        match
          Consensus_finality_journal.rebind_committed
            ~chain_id:deps.chain_id
            ~validator_set
            ~entry
            deps.data_dir
        with
        | Ok Consensus_finality_journal.Rewritten ->
          Log.warn "finality"
            "event = finality_journal_rebind status = repaired height = %d"
            entry.Octra_consensus.Finality_log.height;
          false
        | Ok Consensus_finality_journal.Unchanged ->
          false
        | Error reason ->
          Log.warn "finality"
            "event = finality_journal_rebind status = skipped height = %d reason = %s"
            entry.Octra_consensus.Finality_log.height
            reason;
          proof_needed
      end

let repair_finality_proof (deps : deps) validator_set finalize =
  check_conflict deps;
  match Octra_consensus.Finality_log.last deps.data_dir with
  | None ->
    Lwt.return_false
  | Some entry ->
    begin
      let result =
        Consensus_finality_journal.repair_committed
          ~chain_id:deps.chain_id
          ~validator_set
          ~entry
          ~finalize
          deps.data_dir
      in
      check_conflict deps;
      match result with
      | Ok Consensus_finality_journal.Proof_repaired ->
        Log.warn "finality"
          "event = finality_journal_proof status = repaired height = %d"
          entry.Octra_consensus.Finality_log.height;
        Lwt.return_true
      | Ok Consensus_finality_journal.Proof_current ->
        Lwt.return_true
      | Error reason ->
        Log.warn "finality"
          "event = finality_journal_proof status = refused height = %d reason = %s"
          entry.Octra_consensus.Finality_log.height
          reason;
        Lwt.return_false
    end

let check_finality_proof (deps : deps) validator_set finalize =
  match Octra_consensus.Finality_log.last deps.data_dir with
  | None ->
    Error "finality proof finality entry is missing"
  | Some entry
    when not
           (Int64.equal
              finalize.Octra_consensus.C_types.epoch_id
              (Int64.of_int entry.Octra_consensus.Finality_log.height)) ->
    Error "finality proof epoch does not match local finality"
  | Some entry ->
    Consensus_finality_journal.check_proof
      ~chain_id:deps.chain_id
      ~validator_set
      ~entry
      ~finalize
      deps.data_dir

let finality_runtime (deps : deps) =
  let keep validator_set finalize =
    if validator_set.Octra_consensus.C_types.authors = None then Lwt.return_unit
    else
      let chain = Sync_chain.{data_dir = deps.data_dir; chain_id = deps.chain_id;
        store = deps.store; chaindata = deps.chaindata;
        read_finality = (fun epoch -> Consensus_finality_journal.read_committed_epoch
          ~chain_id:deps.chain_id ~epoch deps.data_dir);
        certificate_path = (fun () -> State_sync_http.certificate_path ~data_dir:deps.data_dir)} in
      let open Lwt.Syntax in
      let* saved = Sync_chain.keep_relief chain validator_set finalize in
      match saved with Ok () -> Lwt.return_unit | Error reason -> Lwt.fail_with reason in
  Consensus_finality_runtime.create_node_runtime ~before_apply:keep
    Consensus_finality_runtime.{
      data_dir = deps.data_dir;
      bundles = deps.bundle_runtime;
      committed_epoch_root_raw = committed_root_at_epoch deps;
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
      stall_timeout_seconds =
        Consensus_post_finalize.stall_seconds (deps.env_int "OCTRA_BFT_APPLY_STALL_SEC" 600);
      require_sync = deps.require_sync;
      fatal_exit = deps.exit_error;
      catchup_active = deps.catchup_active;
      runtime_state = deps.runtime_state;
      set_state_attested = deps.set_state_attested;
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
      mark_quarantine = deps.mark_quarantine;
      exit_error = deps.exit_error;
    }

let run_catchup_to_target (deps : deps) normalize finality_runtime =
  let drain_pending =
    finality_runtime.Consensus_finality_runtime.drain_pending
  in
  let validator_anchor = validator_anchor deps in
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
        | None -> failwith "catchup finality is missing"
        | Some finality ->
          journal_write deps finality.finalize (fun () ->
              let saved = Consensus_finality_journal.stage deps.data_dir
                ~chain_id:deps.chain_id ~validator_set:finality.validator_set
                ~bundle:Consensus_finality_journal.{
                  tx_hashes = validated.record.tx_hashes;
                  txs = validated.parsed_txs;
                  receipts_json = validated.record.receipts_json;
                } finality.finalize in
              let entry =
                Octra_consensus.Finality_log.of_finalize saved
              in
              Octra_consensus.Finality_log.check_write deps.data_dir entry;
              Octra_consensus.Finality_log.write deps.data_dir entry;
              match Consensus_catchup_shell.bind_finality validated saved with
              | Ok selected -> selected
              | Error reason -> failwith reason));
      promote_finality = (fun validated ->
        match validated.record.Octra_consensus.C_codec.finality with
        | None -> failwith "catchup finality is missing"
        | Some finality ->
          journal_write deps finality.finalize (fun () ->
            Consensus_finality_journal.promote_applied
              deps.data_dir
              ~epoch:validated.record.epoch_id
              ~state_root:validated.record.state_root));
      apply_record = deps.apply_catchup_record;
      base_eic = deps.catchup_base_eic;
      set_state_attested = deps.set_state_attested;
      clear_quarantine = deps.clear_quarantine;
      mark_quarantine = deps.mark_quarantine;
      observer = deps.observer;
      drain_pending_finalized = drain_pending;
      http_range = (fun ~budget ->
        Consensus_join_rpc.http_range ~budget
          ~fetch_json:Consensus_join_rpc.http_get_json
          deps.env);
    }

let driver_gates (deps : deps) (p2p : Startup_p2p_shell.node_view) =
  Consensus_driver_wiring.node_gates_of_runtime
    {
      consensus_mode = deps.consensus_mode;
      voting = deps.voting;
      consensus_config_hash = deps.p2p_refs.consensus_config_hash;
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
        ~chaindata:deps.chaindata
        deps.env
    with
    | Ok source -> source
    | Error reason -> failwith reason
  in
  Consensus_driver_wiring.{
    standard = {
      getenv = deps.env;
      get_meta = deps.get_meta;
      duty_state = deps.duty_state;
      wallet_addr = deps.wallet.address;
      wallet_pub = deps.wallet.pub;
      find_account = deps.find_account;
      cached_head = deps.cached_head;
      read_prev_ledger_root = deps.read_prev_ledger_root;
      next_txid = deps.next_txid;
      proposal_state = deps.proposal_state;
      catchup_active = deps.catchup_active;
      staging_epoch_capacity = deps.proposal_capacity;
      save_drops = deps.save_drops;
      chain_id = deps.chain_id;
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
    build_preverify = deps.build_preverify;
    validate_preverify = deps.validate_preverify;
    proposal_bundles = deps.proposal_bundles;
    store_bundle = deps.bundle_runtime.store_bundle;
    driver_ref = deps.driver_ref;
    proposal_preview = deps.proposal_preview;
    prepare_at = deps.prepare_at;
    root_to_raw32 = deps.root_to_raw32;
    current_epoch = (fun () -> !(deps.current_epoch));
    current_round = deps.current_round;
    committed_head_epoch = deps.committed_head_epoch;
    load_parent_commit =
      Consensus_parent_commit.proposal parent_commit_source;
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
    scheduled_validator_set_config = None;
    load_scheduled_validator_set_config =
      p2p_start.Startup_p2p_shell.load_scheduled_validator_set_config;
  }

let health (deps : deps) normalize finality_runtime fork_repair_runtime
    run_catchup_to_target =
  let drain_pending =
    (fun () -> finality_runtime.Consensus_finality_runtime.replay_stashed_while_safe
      ~source:"probe")
  in
  Consensus_health_wiring.{
    probe_range = (fun driver ->
      let module S = Consensus_catchup_shell in
      let head = deps.committed_head_epoch () in
      let allowed () =
        not !(deps.catchup_active) && deps.state_readable ()
        && (not (Consensus_runtime_state.quarantine_active deps.runtime_state)
          || Consensus_runtime_state.root_attestation_recovers
            (Consensus_runtime_state.quarantine_reason deps.runtime_state)) in
      let epoch_time = Option.map (fun header -> header.Octra_core.Epochlog.finalized_at)
        (Octra_core.Store_chaindata.get_epoch_header deps.chaindata head) in
      if not (allowed ()) || Option.fold ~none:false
          ~some:(fun ts -> ts >= deps.now () -. 60.) epoch_time then Lwt.return_unit
      else
        let io = S.driver_io_of_driver driver in
        let anchor = validator_anchor deps in
        let query = S.{
          env_timeout = (fun () -> Some "8");
          read_query_root = deps.read_local_root_raw;
          range_query = { io.range_query with http_range =
            (fun ~budget ~from_epoch ~max_epochs ~validate ->
              Consensus_join_rpc.http_range ~budget:(min 8. budget)
                deps.env ~from_epoch ~max_epochs ~validate) };
          verify_http = S.check_qcs ~chain_id:deps.chain_id
            ~expected_validator_set_hash:(fun epoch ->
              Result.map Octra_consensus.C_config.validator_set_hash
                (Consensus_validator_anchor.expected_set anchor ~epoch))
            ~start_txid:(deps.next_txid ());
        } in
        S.probe_range query ~now:deps.now ~head:(Int64.of_int head)
          ~current_head:(fun () -> Int64.of_int (deps.committed_head_epoch ()))
          ~allowed ~run_catchup:(run_catchup_to_target driver));
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
    require_sync = deps.require_sync;
    fork_repair = fork_repair_runtime;
    run_catchup_to_target;
    liveness_state = deps.liveness_state;
    now = deps.now;
    stall_sec = deps.liveness_stall_sec;
    observer = (fun () -> deps.observer);
    voting = (fun () -> deps.voting);
    finality = deps.finality;
  }

let pending (deps : deps) run_catchup_to_target ~epoch =
  let anchor = validator_anchor deps in
  Result.map (fun validator_set -> Consensus_pending_commit_recovery.{
    data_dir = deps.data_dir;
    query_timeout = 3.0;
    run_catchup_to_target;
    chain_id = deps.chain_id;
    select_set = (fun ~epoch ~round ->
      Consensus_validator_anchor.expected_set ~round anchor ~epoch);
    store_bundle = deps.bundle_runtime.store_bundle;
    validator_count = validator_set.Octra_consensus.C_types.n;
    quorum = validator_set.quorum;
  }) (Consensus_validator_anchor.expected_set anchor ~epoch)

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

let seed_finality (deps : deps) (p2p : Startup_p2p_shell.node_view) =
  let head = deps.committed_head_epoch () in
  let root, txid =
    Sync_finality.local_head
      ~raw_to_hex:deps.raw_to_hex
      ~head
      ~cached:(deps.cached_head ())
      ~epoch_root:deps.committed_epoch_root_raw
  in
  match
    Sync_finality.seed
      ~data_dir:deps.data_dir
      ~chain_id:deps.chain_id
      ~floor:(Octra_core.Store_chaindata.history_floor deps.chaindata)
      ~head
      ~root
      ~txid
      ~trusted:deps.sync_validators
      ~exporters:deps.sync_exporters
      ~active:p2p.active_vs
  with
  | Ok Sync_finality.Missing
  | Ok Sync_finality.Pending -> ()
  | Ok Sync_finality.Advanced ->
    Log.info "consensus"
      "event = sync_finality status = advanced head = %d"
      head
  | Ok Sync_finality.Current ->
    Log.info "consensus"
      "event = sync_finality status = current head = %d"
      head
  | Ok Sync_finality.Seeded ->
    Log.warn "consensus"
      "event = sync_finality status = seeded head = %d"
      head
  | Error fault ->
    let reason = Sync_finality.reason fault in
    Log.fatal "consensus"
      "event = sync_finality status = refused head = %d reason = %s"
      head
      reason;
    deps.mark_quarantine "sync_finality_invalid";
    check_conflict deps;
    match Sync_finality.recovery ~head fault with
    | None -> deps.exit_error ()
    | Some need -> deps.require_sync need

let refuse_fork_resume (deps : deps) reason =
  Log.fatal "consensus"
    "event = fork_repair phase = finality status = refused reason = %s"
    reason;
  deps.exit_error ();
  failwith "fork repair finality resume failed"

let resume_fork (deps : deps) =
  let result =
    try
      Fork_repair_boot.run Fork_repair_boot.{
        read_plan = (fun () ->
          Octra_core.Fork_head_repair.read_plan deps.data_dir);
        head = deps.cached_head;
        finality_at = (fun target ->
          Octra_consensus.Finality_log.find
            (Octra_consensus.Finality_log.index deps.data_dir)
            target);
        journal_committed = (fun () ->
          Consensus_finality_journal.committed deps.data_dir);
        committed_for = (fun entry ->
          Consensus_finality_journal.committed_for
            ~chain_id:deps.chain_id
            ~entry
            deps.data_dir);
        rewind_journal = (fun entry ->
          Consensus_finality_journal.rewind_committed
            ~chain_id:deps.chain_id
            ~entry
            deps.data_dir);
        drop_after = Octra_consensus.Finality_log.drop_after deps.data_dir;
        clear = (fun () ->
          Octra_core.Fork_head_repair.clear deps.data_dir);
      }
    with exn -> Error (Printexc.to_string exn)
  in
  match result with
  | Error reason -> refuse_fork_resume deps reason
  | Ok Fork_repair_boot.Idle -> ()
  | Ok (Fork_repair_boot.Resumed { target; head; dropped }) ->
    Log.warn "consensus"
      "event = fork_repair phase = finality status = resumed target = %d head = %d dropped = %d"
      target
      head
      dropped

let run_driver (deps : deps) p2p_start p2p normalize finality_runtime
    run_catchup_to_target finality_proof_needed =
  check_conflict deps;
  let fork_repair = fork_repair_runtime deps in
  let gates = driver_gates deps p2p in
  let start_height = durable_start_height deps in
  let refuse reason =
    deps.clear_state_attested ();
    deps.mark_quarantine reason;
    deps.exit_error ();
    failwith reason
  in
  let stake_set =
    match Consensus_validator_anchor.stake_set
      (validator_anchor deps) ~epoch:start_height with
    | Ok selected -> selected
    | Error reason -> refuse reason
  in
  let start_set =
    Octra_consensus.C_types.validator_set_for_epoch
      ~chain_id:deps.chain_id
      ~epoch_id:start_height
      stake_set
  in
  (match p2p_start.Startup_p2p_shell.prepare_set ~epoch:start_height start_set with
   | Ok () -> ()
   | Error reason -> refuse reason);
  let p2p = {p2p with Startup_p2p_shell.stake_vs = stake_set; active_vs = start_set} in
  ignore (Consensus_driver_launch_shell.run ~resolve:(repair_set deps)
    Consensus_driver_launch_shell.{
      driver_config =
        driver_config deps p2p_start p2p gates run_catchup_to_target
          finality_runtime;
      stake_set;
      validator_set = start_set;
      swarm = p2p.swarm;
      activate_validator_set = p2p_start.activate_validator_set;
      activate_validator_set_relief =
        p2p_start.activate_validator_set_relief;
      finality_proof_needed;
      check_finality_proof = check_finality_proof deps;
      on_finality_proof = repair_finality_proof deps;
      driver_ref = deps.driver_ref;
      on_driver = deps.on_driver;
      data_dir = deps.data_dir;
      start_height;
      sleep = deps.sleep;
      health =
        health deps normalize finality_runtime fork_repair
          run_catchup_to_target;
      pending = (match pending deps run_catchup_to_target ~epoch:start_height with
        | Ok saved -> saved
        | Error reason -> refuse reason);
      recovery_pending = (fun () ->
        Consensus_finality_journal.pending deps.data_dir);
      replay_pending = replay_applied_pending deps;
      poll_interval = deps.quarantine_poll_sec;
      pending_delay = 12.0;
      role_label = deps.role_label;
    })

let enabled consensus_port =
  consensus_port > 0

let sync_plan ~head ~limit state =
  match Sync_mark.need state with
  | Error _ as error -> error
  | Ok (Some need)
    when need.Sync_need.cause = Sync_need.Conflict
         || (need.cause = Sync_need.Root && not (Sync_need.snapshot_ready ~head ~limit need))
         || (need.cause = Sync_need.Range
             && not (Sync_need.range_ready ~head ~limit need))
         || (need.cause = Sync_need.Journal && head < need.head) ->
      Ok (Some need)
  | Ok _ -> Ok None

let root_replay (deps : deps) need =
  let head = deps.committed_head_epoch () in
  if not (Sync_need.valid need) || need.Sync_need.cause <> Sync_need.Root
     || need.target <> None || need.head <> head
     || not deps.consensus_mode || not (enabled deps.consensus_port)
     || deps.voting || not deps.observer then Ok false
  else
    let ( let* ) = Result.bind in
    let* manifest = match deps.cached_head () with
      | Some manifest when manifest.Octra_core.Head_manifest.epoch_id = head -> Ok manifest
      | _ -> Error "recovery head is missing" in
    if not (deps.state_readable ()) then Error "recovery state is being applied" else
    let commit, root, epoch = Lwt_main.run (
      let open Lwt.Syntax in
      let* commit = Octra_core.Store_irmin.get_commit_hash deps.store in
      let* root = Octra_core.Store_irmin.get_head_hash deps.store in
      let* epoch = Octra_core.Store_irmin.get_meta deps.store "last_epoch" in
      Lwt.return (commit, root, epoch)) in
    let* root = match root with
      | Some root when commit <> None && commit = manifest.irmin_commit
          && Some root = manifest.ledger_state_root
          && epoch = Some (string_of_int head) -> Ok root
      | _ -> Error "recovery store differs from head" in
    let* () = Octra_core.Store_irmin.snapshot_matches
      ?index_root:manifest.epoch_index_root ~state_root:manifest.state_root root in
    let* record = match selected_pending deps with
      | Ok (Consensus_finality_journal.Valid record) -> Ok record
      | Ok Consensus_finality_journal.Missing -> Error "recovery certificate is missing"
      | Ok (Consensus_finality_journal.Invalid reason)
      | Error (Consensus_finality_journal.Read_set reason
          | Consensus_finality_journal.Read_record reason
          | Consensus_finality_journal.Read_bundle reason) -> Error reason in
    let* record = Consensus_finality_journal.replayable record in
    let finalize = record.Consensus_finality_journal.finalize in
    if finalize.epoch_id <> Int64.of_int need.epoch
       || finalize.header.prev_state_root <> deps.root_to_raw32 manifest.state_root
    then Error "recovery certificate does not extend head"
    else Ok true

let start ?genesis (deps : deps) =
  let () = resume_fork deps in
  if not (enabled deps.consensus_port) then
    Log.info "init" "event = consensus_p2p_disabled reason = no_port"
  else
    let p2p_start = start_p2p deps in
    let p2p = p2p_start.view in
    if deps.consensus_mode then
      let () = seed_finality deps p2p in
      let finality_proof_needed = rebind_committed deps p2p.active_vs in
      check_conflict deps;
      let startup = startup_finality deps in
      let normalize =
        Consensus_startup_finality.node_normalizer startup
      in
      let recovery = startup_recovery ?genesis deps in
      check_conflict deps;
      (match recovery with
       | Consensus_finality_journal_recovery.Continue ->
         Consensus_startup_finality.run_node_startup startup
       | Consensus_finality_journal_recovery.Armed
       | Consensus_finality_journal_recovery.Blocked ->
         ());
      (match recovery with
       | Consensus_finality_journal_recovery.Continue
       | Consensus_finality_journal_recovery.Armed ->
         attest_committed deps p2p.active_vs
       | Consensus_finality_journal_recovery.Blocked ->
         ());
      let finality_runtime = finality_runtime deps in
      let run_catchup_to_target =
        run_catchup_to_target deps normalize finality_runtime
      in
      run_driver deps p2p_start p2p normalize finality_runtime
        run_catchup_to_target
        finality_proof_needed

let run ?genesis (deps : deps) =
  try
  let head = deps.committed_head_epoch () in
  match
    sync_plan ~head
      ~limit:(Consensus_health_wiring.snapshot_policy_threshold ~getenv:deps.env)
      (Sync_mark.read ~data_dir:deps.data_dir ~chain:deps.chain_id)
  with
  | Ok None -> start ?genesis deps
  | Ok (Some need) ->
      deps.clear_state_attested ();
      deps.mark_quarantine
        (Printf.sprintf "signed_snapshot_required epoch = %d head = %d"
          need.Sync_need.epoch head);
      let replay = try root_replay deps need with error -> Error (Printexc.to_string error) in
      (match replay with
       | Ok true ->
         Log.warn "init"
           "event = sync_recovery status = replay cause = root epoch = %d head = %d"
           need.epoch head;
         start ?genesis deps
       | Ok false | Error _ ->
         let reason = match replay with Error reason -> reason | _ -> "snapshot required" in
         Log.warn "init"
           "event = sync_recovery status = waiting cause = %s epoch = %d head = %d action = recover.sh reason = %s"
           (Sync_need.label need.cause) need.epoch head reason)
  | Error reason ->
      Log.fatal "init" "event = sync_recovery status = rejected reason = %s" reason;
      deps.exit_error ()
  with Consensus_pending_commit_recovery.Store_fault reason as error ->
    Fun.protect ~finally:deps.exit_refused (fun () ->
      Log.fatal "init" "event = startup_store_fault reason = %s action = exit code = 78" reason);
    raise error