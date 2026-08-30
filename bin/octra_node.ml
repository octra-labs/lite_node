(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_core.Crypto
module Ledger = Octra_core.Ledger
module Transaction = Octra_core.Transaction
module Staging = Octra_core.Tx_staging
module Webhooks = Octra_core.Webhooks
module Epochlog = Octra_core.Epochlog
module Store_chaindata = Octra_core.Store_chaindata
module Store_irmin = Octra_core.Store_irmin
module Tx_drop = Octra_core.Tx_drop
module Pvac_migration_entitlement = Octra_core.Pvac_migration_entitlement
module Pvac_verify_worker = Octra_core.Pvac_verify_worker
module Private_ledger = Octra_core.Private_ledger
module Consensus_bundle_cache = Octra_node_runtime.Consensus_bundle_cache
module Consensus_catchup_shell = Octra_node_runtime.Consensus_catchup_shell
module Consensus_circle_preverify = Octra_node_runtime.Consensus_circle_preverify
module Consensus_circle_cell_preverify = Octra_node_runtime.Consensus_circle_cell_preverify
module Consensus_driver_boot_shell = Octra_node_runtime.Consensus_driver_boot_shell
module Consensus_driver_read = Octra_node_runtime.Consensus_driver_read
module Consensus_epoch_apply_checked = Octra_node_runtime.Consensus_epoch_apply_checked
module Consensus_epoch_apply_env = Octra_node_runtime.Consensus_epoch_apply_env
module Consensus_epoch_apply_finish_shell = Octra_node_runtime.Consensus_epoch_apply_finish_shell
module Consensus_epoch_apply_shared = Octra_node_runtime.Consensus_epoch_apply_shared
module Consensus_epoch_apply_sender = Octra_node_runtime.Consensus_epoch_apply_sender
module Consensus_epoch_apply_source = Octra_node_runtime.Consensus_epoch_apply_source
module Consensus_epoch_apply_start_shell = Octra_node_runtime.Consensus_epoch_apply_start_shell
module Consensus_finality_state = Octra_node_runtime.Consensus_finality_state
module Consensus_finality_journal = Octra_node_runtime.Consensus_finality_journal
module Consensus_join_rpc = Octra_node_runtime.Consensus_join_rpc
module Consensus_key_switch_preverify = Octra_node_runtime.Consensus_key_switch_preverify
module Consensus_private_preverify = Octra_node_runtime.Consensus_private_preverify
module Consensus_preverify_role = Octra_node_runtime.Consensus_preverify_role
module Consensus_proposal_preview_shell = Octra_node_runtime.Consensus_proposal_preview_shell
module Consensus_proposal_state = Octra_node_runtime.Consensus_proposal_state
module Consensus_runtime_boot_shell = Octra_node_runtime.Consensus_runtime_boot_shell
module Consensus_tick_loop = Octra_node_runtime.Consensus_tick_loop
module Set_actor = Octra_node_runtime.Set_actor
module Set_rule = Octra_node_runtime.Set_rule
module Epoch_atomic = Octra_node_runtime.Epoch_atomic
module Epoch_visibility = Octra_node_runtime.Epoch_visibility
module Log = Octra_node_runtime.Log
module Rest = Octra_node_runtime.Node_rest_facade
module Rule_graph = Octra_core.Rule_graph
module History_floor = Octra_core.History_floor
module Preverify_cache = Octra_node_runtime.Preverify_cache
module Preverify_submit = Octra_node_runtime.Preverify_submit
module Resource_compute_config = Octra_node_runtime.Resource_compute_provider_config
module Resource_compute_engine = Octra_node_runtime.Resource_compute_provider_engine
module Resource_compute_service = Octra_node_runtime.Resource_compute_service
module Startup_process_shell = Octra_node_runtime.Startup_process_shell
module Startup_network_boot_shell = Octra_node_runtime.Startup_network_boot_shell
module Startup_node_boot_shell = Octra_node_runtime.Startup_node_boot_shell
module Startup_node_launch_shell = Octra_node_runtime.Startup_node_launch_shell
module Startup_private_profile = Octra_node_runtime.Startup_private_profile
module Startup_runtime_limits = Octra_node_runtime.Startup_runtime_limits
module Startup_store_shell = Octra_node_runtime.Startup_store_shell
module State_sync_http = Octra_node_runtime.State_sync_http
module Sync_publish = Octra_node_runtime.Sync_publish
module Program_trust = Octra_vm.Program_trust

let addr_short = Octra_node_runtime.Text.addr_short
let raw_to_hex = Octra_node_runtime.Text.raw_to_hex
let hex_to_raw32_lossy = Octra_node_runtime.Text.hex_to_raw32_lossy
let env_int = Octra_node_runtime.Env.int_value
let env_opt = Sys.getenv_opt

let exit_error () = exit 1
let exit_success () = exit 0

let cli_has_flag name =
  Array.exists (fun a -> a = name) Sys.argv

let validator_pubkeys_of_env = Octra_node_runtime.Validators.pubkeys_of_env
let validator_pubkeys_for_epoch = Octra_node_runtime.Validators.pubkeys_for_epoch

let swarm_ref : Octra_net.P2p_swarm.t option ref = ref None
let tx_gossip_guard = Octra_net.P2p_tx_gossip_guard.create ()

let irmin_get_meta store key = Rest.run_s (Store_irmin.get_meta store key)
let irmin_get_head_hash store = Rest.run_s (Store_irmin.get_head_hash store)

  let () =
    Startup_process_shell.configure_process ~exit_fatal:exit_error;

    begin
      match Pvac_verify_worker.ready_sync () with
      | Ok () ->
        Log.info "init" "event = pvac_worker status = ready"
      | Error reason ->
        Log.fatal "init"
          "event = pvac_worker status = rejected reason = %s"
          reason;
        exit_error ()
    end;

    let init_mode = cli_has_flag "--init" in
    let role =
      Octra_node_runtime.Consensus_mode.of_inputs
        ~cli_observer:(cli_has_flag "--observer")
        ~env_mode:(env_opt "OCTRA_CONSENSUS_MODE")
    in
    let consensus_role = role.role in
    let consensus_mode = role.consensus_enabled in
    let voting_consensus_mode = role.voting_enabled in
    let observer_mode = role.observer_enabled in
    let permissionless_validator_lifecycle =
      match Octra_core.Validator_policy.of_env env_opt with
      | Ok policy ->
        Octra_core.Validator_policy.lifecycle_enabled policy
      | Error error ->
        Log.fatal "init"
          "event = validator_policy status = rejected reason = %s"
          error;
        exit_error ()
    in

    let data_dir = Startup_process_shell.data_dir ~env:env_opt in
    Startup_process_shell.ensure_data_dir
      ~init_mode
      ~data_dir
      ~exit_fatal:exit_error;
    let drop_db = Tx_drop.open_db data_dir in
    let save_drops drops =
      match Tx_drop.save_many drop_db (List.map Staging.drop_row drops) with
      | Ok () -> ()
      | Error reason ->
        Log.warn "staging"
          "event = drop_persist_failed reason = %s"
          reason
    in

    let startup_network =
      Startup_process_shell.network_config ~env:env_opt
    in
    let private_result_activation_epoch =
      match Octra_core.Private_result_policy.activation_epoch_of env_opt with
      | Ok epoch -> epoch
      | Error reason ->
        Log.fatal "init"
          "event = private_result_policy status = rejected reason = %s"
          reason;
        exit_error ()
    in
    let private_result_policy epoch =
      Octra_core.Private_result_policy.for_epoch
        ~activation_epoch:private_result_activation_epoch
        epoch
    in
    let migration_entitlements =
      match
        Pvac_migration_entitlement.load_env
          ~chain_id:startup_network.chain_id
          ~data_dir
          ~getenv:env_opt
      with
      | Ok value -> value
      | Error reason ->
        Log.fatal "init"
          "event = migration_entitlements status = rejected reason = %s"
          reason;
        exit_error ()
    in
    Log.info "init"
      "event = migration_entitlements status = loaded entries = %d root = %s"
      (Pvac_migration_entitlement.entry_count migration_entitlements)
      (Option.value
         ~default:"none"
         (Pvac_migration_entitlement.root migration_entitlements));
    let legacy_replay ~epoch ~address ~cipher =
      Pvac_migration_entitlement.decision
        migration_entitlements
        ~epoch
        ~address
        ~cipher
    in

    let wallet =
      Startup_process_shell.load_wallet
        ~path:(data_dir ^ "/wallet.json")
        ~ensure:Wallet.ensure
        ~address:(fun wallet -> wallet.Wallet.address)
        ~exit_fatal:exit_error
    in
    let program_trust =
      match Program_trust.of_env env_opt with
      | Ok value -> value
      | Error error ->
        Log.fatal "init" "program_trust = invalid reason = %s"
          (Program_trust.error_message error);
        exit_error ()
    in
    Log.info "init" "event = consensus_mode role = %s consensus = %b voting = %b"
      role.label
      consensus_mode
      voting_consensus_mode;
    ignore (Startup_process_shell.admit_wallet_validators
      ~permissionless:permissionless_validator_lifecycle
      ~validator_env_present:(env_opt "OCTRA_VALIDATORS" <> None)
      ~current:(Octra_node_runtime.Validators.pubkeys_of_env_name_in_order
        "OCTRA_VALIDATORS")
      ~next:(Octra_node_runtime.Validators.pubkeys_of_env_name_in_order
        "OCTRA_VALIDATORS_NEXT")
      ~fallback:(validator_pubkeys_of_env
        ~wallet_addr:wallet.address
        ~wallet_pub:wallet.pub)
      ~wallet_addr:wallet.address
      ~wallet_pub:wallet.pub
      ~voting_consensus_mode
      ~consensus_mode
      ~role_label:role.label
      ~exit_fatal:exit_error);

    let validator_view_sk, validator_view_pub =
      StealthAddress.derive_view_keypair wallet.Wallet.priv
    in
    Log.info "init" "event = validator_view_key pubkey = %s"
      (Base64.encode_exn validator_view_pub);

    Startup_process_shell.apply_gc_config
      (Startup_process_shell.gc_config ~env:env_opt);
    Startup_process_shell.start_gc_reporter ~prune:Preverify_cache.prune;

    let store_path = Startup_store_shell.irmin_path data_dir in
    let store = Lwt_main.run (Store_irmin.open_store store_path) in
    Log.info "init" "event = storage_ready path = %s cwd = %s"
      store_path (Sys.getcwd ());
    Startup_node_boot_shell.run_store
      Startup_node_boot_shell.{
        data_dir;
        store;
        exit_fatal = exit_error;
      };
    let ledger = Ledger.create store in
    Log.info "init" "event = ledger_loaded accounts = %d" (Ledger.length ledger);
    Startup_private_profile.run
      ~enabled:(Transaction.bft_crypto_active ())
      ~store
      ~exit_fatal:exit_error;

    let chaindata = Store_chaindata.open_chaindata (data_dir ^ "/chaindata") in
    let history_floor =
      match Store_chaindata.history_floor chaindata with
      | Ok value -> value
      | Error reason ->
          Log.fatal "init" "event = history_floor status = rejected reason = %s" reason;
          exit_error ()
    in
    let floor_config_hash =
      match history_floor with
      | None -> None
      | Some floor when History_floor.chain_id floor <> startup_network.chain_id ->
          Log.fatal "init"
            "event = history_floor status = rejected reason = chain_id_mismatch";
          exit_error ()
      | Some floor ->
          begin
            match State_sync_http.configured_config_hash () with
            | Ok expected when expected = History_floor.config_hash floor ->
                Some expected
            | Ok _ ->
                Log.fatal "init"
                  "event = history_floor status = rejected reason = config_hash_mismatch";
                exit_error ()
            | Error reason ->
                Log.fatal "init"
                  "event = history_floor status = rejected reason = %s"
                  reason;
                exit_error ()
          end
    in
    let rules =
      Rule_graph.create
        ~chain_id:startup_network.chain_id
        ~root_at:(fun epoch ->
          match Store_chaindata.get_bound_epoch_header chaindata epoch with
          | Ok header -> Rule_graph.Root header.Epochlog.state_root
          | Error "epoch not found" ->
              begin
                match history_floor with
                | Some floor ->
                    begin
                      match
                        Rule_graph.root_after_floor
                          ~chain_id:startup_network.chain_id
                          ~floor_epoch:(History_floor.epoch floor)
                          ~epoch
                      with
                      | Some root -> Rule_graph.Root root
                      | None -> Rule_graph.Missing
                    end
                | None -> Rule_graph.Missing
              end
          | Error reason -> Rule_graph.Unreadable reason)
    in
    let (startup_txlog_seg, startup_txlog_off) =
      Store_chaindata.txlog_position chaindata in
    Log.info "init" "event = txlog_open seg = %d off = %d"
      startup_txlog_seg startup_txlog_off;
    let total_tx_count = ref (Store_chaindata.tx_count chaindata) in
    Log.info "init" "event = chaindata_open tx_count = %d mode = lmdb_txlog_epochlog"
      !total_tx_count;
    begin
      match Pvac_migration_entitlement.root migration_entitlements with
      | None ->
        Log.info "init"
          "event = migration_snapshot status = disabled"
      | Some root ->
        let binding =
          match Pvac_migration_entitlement.snapshot_epoch migration_entitlements with
          | None -> Error "migration snapshot epoch is missing"
          | Some epoch ->
              begin
                match Store_chaindata.get_bound_epoch_header chaindata epoch with
                | Ok header ->
                    Pvac_migration_entitlement.bind_snapshot
                      migration_entitlements
                      (fun _ -> Ok header.Epochlog.state_root)
                | Error "epoch not found" ->
                    begin
                      match history_floor, floor_config_hash with
                      | Some floor, Some config_hash ->
                          Pvac_migration_entitlement.bind_floor
                            migration_entitlements
                            ~config_hash
                            ~floor_config_hash:(History_floor.config_hash floor)
                            ~floor_epoch:(History_floor.epoch floor)
                      | _ -> Error "migration snapshot unavailable: epoch not found"
                    end
                | Error reason -> Error ("migration snapshot unavailable: " ^ reason)
              end
        in
        match binding
        with
        | Ok () ->
          Log.info "init"
            "event = migration_snapshot status = bound epoch = %s root = %s"
            (Pvac_migration_entitlement.snapshot_epoch migration_entitlements
             |> Option.map string_of_int
             |> Option.value ~default:"none")
            root
        | Error reason ->
          Log.fatal "init"
            "event = migration_snapshot status = rejected reason = %s"
            reason;
          exit_error ()
    end;

    let current_epoch =
      ref (Startup_node_boot_shell.run_node
        Startup_node_boot_shell.{
          data_dir;
          store;
          ledger;
          chaindata;
          total_tx_count;
          observer_mode;
          wallet = { address = wallet.address; pub = wallet.pub };
          consensus_mode;
          voting_consensus_mode;
          consensus_port_configured = (fun () ->
            env_opt "OCTRA_CONSENSUS_PORT" <> None);
          validators = (fun () ->
            validator_pubkeys_of_env
              ~wallet_addr:wallet.address
              ~wallet_pub:wallet.pub);
          int_value = env_int;
          env = env_opt;
          exit_fatal = exit_error;
        })
    in
    let private_field_policy epoch =
      match Rule_graph.private_payload rules ~epoch with
      | Ok mode -> Private_ledger.field_policy_of_mode mode
      | Error fault -> failwith (Rule_graph.fault_message fault)
    in
    let bind_rule
        name
        (activation : Rule_graph.activation option)
        evaluate =
      match activation with
      | None ->
        (match evaluate 0 with
         | Ok Rule_graph.Active ->
           Log.info "init"
             "event = rule_graph rule = %s status = bound activation_epoch = 0"
             name
         | Ok Rule_graph.Prior ->
           Log.info "init"
             "event = rule_graph rule = %s status = prior"
             name
         | Error fault ->
           Log.fatal "init"
             "event = rule_graph rule = %s status = rejected reason = %s"
             name
             (Rule_graph.fault_message fault);
           exit_error ())
      | Some activation ->
        if !current_epoch <= activation.anchor_epoch then
          Log.info "init"
            "event = rule_graph rule = %s status = awaiting_anchor anchor_epoch = %d activation_epoch = %d"
            name
            activation.anchor_epoch
            activation.activation_epoch
        else
          match evaluate activation.activation_epoch with
          | Ok Rule_graph.Active ->
            Log.info "init"
              "event = rule_graph rule = %s status = bound anchor_epoch = %d activation_epoch = %d"
              name
              activation.anchor_epoch
              activation.activation_epoch
          | Ok Rule_graph.Prior ->
            Log.fatal "init"
              "event = rule_graph rule = %s status = rejected reason = activation_unreachable"
              name;
            exit_error ()
          | Error fault ->
            Log.fatal "init"
              "event = rule_graph rule = %s status = rejected reason = %s"
              name
              (Rule_graph.fault_message fault);
            exit_error ()
    in
    bind_rule
      "circle"
      (Rule_graph.circle_activation rules)
      (fun epoch -> Rule_graph.circle rules ~epoch);
    bind_rule
      "validator_quorum"
      (Rule_graph.validator_quorum_activation rules)
      (fun epoch -> Rule_graph.validator_quorum rules ~epoch);
    bind_rule
      "epoch_time"
      (Rule_graph.epoch_time_activation rules)
      (fun epoch -> Rule_graph.epoch_time rules ~epoch);
    bind_rule
      "owner_migration"
      (Rule_graph.owner_migration_activation rules)
      (fun epoch -> Rule_graph.owner_migration rules ~epoch);
    bind_rule
      "wasm_compute"
      (Rule_graph.wasm_compute_activation rules)
      (fun epoch -> Rule_graph.wasm_compute rules ~epoch);
    bind_rule
      "private_payload"
      (Rule_graph.private_payload_activation rules)
      (fun epoch -> Rule_graph.private_payload rules ~epoch);
    bind_rule
      "set_fold"
      (Rule_graph.set_fold_activation rules)
      (fun epoch -> Rule_graph.set_fold rules ~epoch);

    let validator_ready_max_lag =
      max 0 (env_int "OCTRA_VALIDATOR_READY_MAX_LAG_EPOCHS" 64)
    in
    let ready_state_root_at epoch_id =
      if epoch_id < 0 then Lwt.return_none
      else
        match Store_chaindata.get_epoch_header chaindata epoch_id with
        | Some h when String.length h.Epochlog.state_root > 0 ->
          Lwt.return_some h.Epochlog.state_root
        | _ -> Lwt.return_none
    in

    let runtime_env = Startup_runtime_limits.{
      int_value = env_int;
      opt = env_opt;
    } in
    let Consensus_runtime_boot_shell.{
      last_epoch_time;
      stealth_defer_count;
      private_limits;
      stealth_in_epoch_counter;
      fhe_in_epoch_counter;
      tree;
      swarm_opt;
      consensus_finalized;
      catchup_in_progress;
      catchup_queue;
      consensus_state;
      consensus_limits;
      driver_ref;
      consensus_liveness;
      proposal_state;
      finality_state;
      finality_callbacks;
      proposal_bundles;
      proposal_bundle_runtime;
      bft_proposal_limits;
      current_consensus_round;
      clear_state_attested;
      set_state_attested;
      mark_quarantine;
      clear_quarantine;
      catchup_node_queue;
    } =
      Consensus_runtime_boot_shell.create
        Consensus_runtime_boot_shell.{
          current_epoch;
          chaindata;
          runtime_env;
          default_max_ou = Staging.max_ou_per_epoch;
          now = Unix.gettimeofday;
        }
    in
    let epoch_visibility = Epoch_visibility.create () in
    let Startup_runtime_limits.{
      max_stealth_defer;
      max_stealth_per_epoch;
      max_fhe_per_epoch;
      stealth_inline_verify_allowed;
    } = private_limits in
    let _peers = Octra_core.Config.read_nodes "nodes.config" in
    let Startup_runtime_limits.{
      quarantine_mismatch_threshold;
      quarantine_poll_sec;
      quarantine_ahead_streak_threshold;
      quarantine_ahead_grace_epochs;
      quarantine_ahead_drift_tolerance;
      soft_catchup_max_lag;
      consensus_liveness_stall_sec;
      _;
    } = consensus_limits in

  let apply_finalized_epoch ?override_ordered_txs ?override_receipts_json
      ?override_proposer_info ?override_reward ?override_epoch_ts
      ~parent_commit
      ~now ~elapsed () =
    let open Lwt.Syntax in
    let* start =
      Consensus_epoch_apply_start_shell.run
        Consensus_epoch_apply_start_shell.{
          source = Consensus_epoch_apply_source.{
            check_override_receipts = (fun ~epoch_id ~receipts txs ->
              Octra_core.Preverify_receipt_policy.check ~epoch_id ~receipts txs);
            find_finalized = (fun epoch ->
              Consensus_finality_state.find_finalized finality_state epoch);
            cached_bundle = proposal_bundle_runtime.cached_bundle;
            receipt_root_matches = proposal_bundle_runtime.receipt_root_matches;
            header_has_empty_bundle = proposal_bundle_runtime.header_has_empty_bundle;
            staging_txs = (fun () ->
              Staging.get_epoch_txs ~capacity:Staging.max_ou_per_epoch);
            store_empty_bundle = proposal_bundle_runtime.store_empty_bundle;
            fatal = Log.fatal "epoch" "%s";
            exit = exit_error;
          };
          preflight = {
            begin_store_batch = (fun () ->
              match Ledger.begin_journal ledger with
              | Error error -> Lwt.fail_with error
              | Ok () ->
                Lwt.catch
                  (fun () ->
                    match Rule_graph.account_pack rules ~epoch:!current_epoch with
                    | Error fault ->
                      Lwt.fail_with (Rule_graph.fault_message fault)
                    | Ok mode -> Store_irmin.begin_epoch_batch ~mode store)
                  (fun exn ->
                    ignore (Ledger.abort_journal ledger);
                    Lwt.fail exn));
            begin_chaindata_batch = (fun () -> Store_chaindata.begin_batch chaindata);
            ledger_hash = (fun () -> Ledger.hash ledger);
            cached_head = Octra_core.Head_manifest.get_cached;
            expected_prev_root = (fun epoch ->
              Consensus_finality_state.find_finalized finality_state epoch
              |> Option.map (fun finalize ->
                let header = finalize.Octra_consensus.C_types.header in
                header.Octra_consensus.C_types.prev_state_root));
            fatal = Log.fatal "epoch" "%s";
            exit = exit_error;
          };
          epoch_env = Consensus_epoch_apply_env.{
            chain_id = startup_network.chain_id;
            current_epoch = (fun () -> !current_epoch);
            epoch_ts = (fun epoch ->
              match override_epoch_ts with
              | Some epoch_ts when epoch = !current_epoch -> Some epoch_ts
              | _ ->
                Consensus_finality_state.find_finalized finality_state epoch
                |> Option.map (fun finalize ->
                  finalize.Octra_consensus.C_types.header.ts));
            driver = (fun () -> !driver_ref);
            validator_fallback = (fun epoch ->
              validator_pubkeys_for_epoch
                ~wallet_addr:wallet.address
                ~wallet_pub:wallet.pub
                ~epoch);
            ready_state_root_at;
            ready_max_lag = validator_ready_max_lag;
          };
          now = Unix.gettimeofday;
          staging_size = Staging.staging_size;
          clear_spent_nonces = (fun () -> Ledger.clear_spent_nonces ledger);
          log_processing = (fun ~epoch_id ~tx_count ~deferred ~elapsed ->
            Log.info "epoch"
              "event = epoch_processing epoch = %d txs = %d deferred = %d elapsed = %.2fs"
              epoch_id
              tx_count
              deferred
              elapsed);
        }
        Consensus_epoch_apply_start_shell.{
          epoch_id = !current_epoch;
          override_ordered_txs;
          override_receipts_json;
          consensus_mode;
          elapsed;
        }
    in
    let Consensus_epoch_apply_start_shell.{
      ordered_txs;
      epoch_receipts_json;
      preverify_receipts_json;
      epoch_rejections;
      epoch_start;
      pending_tx_saves;
      confirmed_fees;
      processed_hashes;
      pre_state_hash;
      pre_consensus_root;
      epoch_env;
    } = start in

    let preverify =
      match
        Octra_core.Preverify_commit.gate_of_strings
          ~required:consensus_mode
          preverify_receipts_json
      with
      | Ok gate -> gate
      | Error reason ->
        failwith ("finalized preverify receipt parse failed: " ^ reason)
    in
    let* shared_result =
      Consensus_epoch_apply_shared.run_node
        ?preverify
        ?parent_commit
        {
          consensus_mode;
          ledger;
          store;
          chaindata;
          program_trust;
          rules;
          wallet_addr = wallet.address;
          pre_state_hash;
          standard_env = (fun () ->
            Consensus_epoch_apply_env.standard_for_proposer epoch_env
              ~proposer_addr:wallet.address
              ~prev_state_root:pre_consensus_root);
          current_epoch = (fun () -> !current_epoch);
          max_fhe_per_epoch;
          max_stealth_per_epoch;
          max_stealth_defer;
          stealth_inline_verify_allowed;
          fhe_in_epoch_counter;
          stealth_in_epoch_counter;
          stealth_defer_count;
          pending_tx_saves;
          total_tx_count;
          confirmed_fees;
          processed_hashes;
          log_shared = (fun ~tx_count ->
            Log.info "epoch"
              "event = shared_bft_executor epoch = %d txs = %d"
              !current_epoch tx_count);
          short = addr_short;
          fatal = Log.fatal "epoch" "%s";
          exit = exit_error;
          notify_new_account = (fun addr ->
            Webhooks.notify (Webhooks.NewAccount addr));
          notify_confirmed = (fun tx epoch ->
            Webhooks.notify (Webhooks.TxConfirmed (tx, epoch)));
          notify_rejected = (fun tx reason ->
            Webhooks.notify (Webhooks.TxRejected (tx, reason)));
          legacy_replay;
          private_result_policy;
        }
        ordered_txs
    in
    let deferred_stealth_txs = shared_result.deferred_stealth_txs in
    let epoch_ts =
      match
        Consensus_epoch_apply_env.resolve_epoch_ts
          ~consensus_mode
          epoch_env
          !current_epoch
      with
      | Ok value -> value
      | Error error -> failwith error
    in
    begin
      match
        Consensus_epoch_apply_sender.save_live_rejections
          ~chaindata
          ~epoch_id:!current_epoch
          ~ts:epoch_ts
          ~processed_hashes
          ~notify_rejected:(fun tx reason ->
            Webhooks.notify (Webhooks.TxRejected (tx, reason)))
          epoch_rejections
      with
      | Ok () -> ()
      | Error error -> failwith ("finalized rejection persist failed: " ^ error)
    end;
    let* _finish =
      Consensus_epoch_apply_finish_shell.run_node
        Consensus_epoch_apply_finish_shell.{
          data_dir;
          store;
          ledger;
          chaindata;
          save_drops;
          finality_state;
          current_epoch;
          last_epoch_time;
          tree;
          stealth_in_epoch_counter;
          fhe_in_epoch_counter;
          swarm_opt;
          get_meta = irmin_get_meta store;
          env = env_opt;
          hash = Octra_net.Hash_domain.hash;
          raw_to_hex;
          stdout = (fun line -> Log.stdout "%s\n%!" line);
          log_epoch = Log.info "epoch" "%s";
          fatal_epoch = Log.fatal "epoch" "%s";
          short = addr_short;
          exit = exit_error;
        }
        Consensus_epoch_apply_finish_shell.{
          now;
          consensus_mode;
          override_proposer_info;
          override_reward;
          epoch_env;
          tree_ref = tree;
          epoch_start;
          validator_addr = wallet.address;
          ready_state_root_at;
          ready_max_lag = validator_ready_max_lag;
          pending_tx_saves;
          confirmed_fees;
          processed_hashes;
          pre_state_hash;
          pre_consensus_root;
          epoch_receipts_json;
          ordered_txs_count = List.length ordered_txs;
          deferred_stealth_txs;
          producer = wallet.address;
          short = addr_short;
        }
    in
    Lwt.return_unit
  in

  let finality_head_epoch () =
    match Octra_core.Head_manifest.get_cached () with
    | Some h -> h.Octra_core.Head_manifest.epoch_id
    | None -> !current_epoch - 1
  in

  let apply_finalized_epoch_checked ?override_ordered_txs
      ?override_receipts_json ?override_proposer_info ?override_reward
      ?override_epoch_ts ~parent_commit ~now ~elapsed () =
    Consensus_epoch_apply_checked.run_node
      ~consensus_mode
      ~current_epoch
      ~head:finality_head_epoch
      ~catchup_queue
      ~catchup_in_progress
      ~clear_state_attested
      ~preflight:(fun () ->
        Set_rule.bind
          rules
          ~chain_id:startup_network.chain_id
          ~parent:parent_commit
          ~epoch:!current_epoch
        |> Result.map (fun _ -> ()))
      ~defer:(fun reason ->
        Log.error "finality" "event = refuse_apply reason = %s" reason;
        mark_quarantine reason;
        catchup_node_queue.queue_finalized_gap
          ~target_epoch:(Int64.of_int !current_epoch)
          ~reason)
      ~apply:(fun () ->
        Epoch_visibility.with_apply epoch_visibility (fun () ->
          Epoch_atomic.run
            {
              abort_ledger = (fun () ->
                match Ledger.abort_journal ledger with
                | Ok () -> ()
                | Error error -> failwith error);
              abort_store = (fun () -> Store_irmin.abort_epoch_batch store);
              abort_history = (fun () -> Store_chaindata.abort_batch chaindata);
              fatal = Log.fatal "epoch" "%s";
              exit = exit_error;
            }
            (fun () ->
              apply_finalized_epoch
                ?override_ordered_txs
                ?override_receipts_json
                ?override_proposer_info
                ?override_reward
                ?override_epoch_ts
                ~parent_commit
                ~now
                ~elapsed
                ())))
  in

  let tick_loop () =
    Consensus_tick_loop.run_node
      Consensus_tick_loop.{
        now = Unix.gettimeofday;
        last_epoch_time;
        current_epoch;
        consensus_finalized;
        finality = finality_callbacks;
        bundle = proposal_bundle_runtime;
        queue_missing_bundle = catchup_node_queue.queue_finalized_gap;
        warn = Log.warn "epoch" "%s";
        info = Log.info "epoch" "%s";
        apply = (fun ~finalize ~now ~elapsed ->
          let parent_commit =
            Option.bind
              finalize
              (fun value -> value.Octra_consensus.C_types.parent_commit)
          in
          apply_finalized_epoch_checked
            ~parent_commit
            ~now
            ~elapsed
            ());
        sleep = Lwt_unix.sleep;
      }
      ~consensus_mode
      in

    let Startup_network_boot_shell.{
      api_port;
      p2p_port;
      consensus_port;
      consensus_peers;
      chain_id;
      consensus_config_hash_ref;
      consensus_validator_set_ref;
      scheduled_validator_set_ref;
    } =
      Startup_network_boot_shell.run
        Startup_network_boot_shell.{
          env = env_opt;
          read_active_validator_meta = (fun () ->
            irmin_get_meta
              store
              Octra_core.Validator_set_update.active_meta_key);
          read_pending_validator_meta = (fun () ->
            irmin_get_meta
              store
              Octra_core.Validator_set_update.pending_meta_key);
          data_dir;
          store_path = Startup_store_shell.irmin_path data_dir;
          current_epoch;
          chaindata;
          finality = finality_callbacks;
          set_proposal = Consensus_proposal_state.set proposal_state;
          apply_finalized = (fun ?override_ordered_txs
              ?override_receipts_json ?override_proposer_info
              ?override_reward ?override_epoch_ts ?override_parent_commit
              ~now ~elapsed () ->
            apply_finalized_epoch_checked
              ?override_ordered_txs
              ?override_receipts_json
              ?override_proposer_info
              ?override_reward
              ?override_epoch_ts
              ~parent_commit:override_parent_commit
              ~now
              ~elapsed
              ());
          consensus_role = Octra_consensus.C_role.label consensus_role;
          wallet = {
            address = wallet.address;
            pub = wallet.pub;
            priv = wallet.Wallet.priv;
          };
          exit_error;
          exit_success;
        }
    in
    let catchup_base_eic_root () =
      Consensus_join_rpc.base_eic_root_from_head
        (Octra_core.Head_manifest.get_cached ()) in
    let root_to_raw32 =
      Consensus_driver_read.eic_root_to_raw32_or_zero
        ~hex_to_raw32:hex_to_raw32_lossy
    in
    let driver_roots =
      Consensus_driver_read.node_store_root_deps
        ~chaindata
        ~store
        ~current_epoch:(fun () -> !current_epoch)
        ~root_to_raw32
        ~cached_head:Octra_core.Head_manifest.get_cached
    in
    let proposal_preview =
      Consensus_proposal_preview_shell.run
        Consensus_proposal_preview_shell.{
          chain_id;
          program_trust;
          backend =
            Consensus_proposal_preview_shell.node_backend
              ~program_trust
              ~rules
              ~legacy_replay
              ~private_result_policy
              ~max_fhe:max_fhe_per_epoch
              ~max_stealth:max_stealth_per_epoch
              store
              ledger;
          ready_state_root_at;
          ready_max_lag = validator_ready_max_lag;
          warn = (fun reason ->
            Log.warn "consensus"
              "event = preview_exception reason = %s"
              reason);
        }
    in
    let apply_catchup_record (validated : Consensus_catchup_shell.validated_record) =
      let record = validated.record in
      let now = Unix.gettimeofday () in
      let override_proposer_info = validated.proposer in
      let parent_commit =
        Option.bind
          validated.record.Octra_consensus.C_codec.finality
          (fun finality ->
            finality.Octra_consensus.C_codec.finalize.parent_commit)
      in
      apply_finalized_epoch_checked
        ~override_ordered_txs:validated.parsed_txs
        ~override_receipts_json:record.receipts_json
        ?override_proposer_info
        ~override_reward:validated.reward
        ~override_epoch_ts:record.epoch_ts
        ~parent_commit
        ~now
        ~elapsed:0.0
        ()
    in
    let circle_preverify =
      Consensus_circle_preverify.{
        store;
        ledger;
        program_trust;
        rules;
        env = (fun ~pre_state_root ->
          let epoch_id = !current_epoch in
          let validator_pubkeys =
            validator_pubkeys_for_epoch
              ~wallet_addr:wallet.address
              ~wallet_pub:wallet.pub
              ~epoch:epoch_id
          in
          Octra_core.Epoch_exec.{
            chain_id;
            epoch_id;
            proposer_addr = wallet.address;
            validator_addrs = List.map fst validator_pubkeys;
            validator_pubkeys;
            prev_state_root = pre_state_root;
            epoch_ts = 0.0;
            ready_state_root_at = Some ready_state_root_at;
            ready_max_lag = validator_ready_max_lag;
          });
      }
    in
    let circle_cell_preverify =
      Consensus_circle_cell_preverify.{
        store;
        ledger;
        rules;
        env = circle_preverify.env;
      }
    in
    let key_switch_preverify =
      Consensus_key_switch_preverify.create
        ~field_policy:(fun () -> private_field_policy !current_epoch)
        ledger
    in
    let private_preverify =
      Consensus_private_preverify.create
        ~field_policy:(fun () -> private_field_policy !current_epoch)
        ~result_policy:(fun () -> private_result_policy !current_epoch)
        ledger
    in
    if consensus_mode then
      List.iter
        (fun tx ->
           ignore (Consensus_key_switch_preverify.admit key_switch_preverify tx);
           ignore (Consensus_private_preverify.admit private_preverify tx))
        (Staging.all ());
    let stealth_preverify =
      Preverify_submit.{
        get_pvac_pubkey =
          (fun addr -> Ledger.get_pvac_pubkey ledger addr);
        sender_enc =
          (fun addr ->
            Option.value
              ~default:"0"
              (Ledger.find ledger addr).encrypted_balance);
        start_task = Preverify_cache.start_task;
        verify_ranges =
          (fun ~pubkey_blob ~sender_enc ptd ->
            Octra_node_runtime.Tx_view.preverify_stealth_ranges
              ~pubkey_blob
              ~sender_enc
              ptd);
        now = Unix.gettimeofday;
      }
    in
    let preverify_admit tx =
      if consensus_mode then begin
        Consensus_key_switch_preverify.retain
          key_switch_preverify
          (fun hash -> Option.is_some (Staging.find_by_hash hash));
        Consensus_private_preverify.retain
          private_preverify
          (fun hash -> Option.is_some (Staging.find_by_hash hash));
        ignore (Consensus_key_switch_preverify.admit key_switch_preverify tx);
        ignore (Consensus_private_preverify.admit private_preverify tx);
        Ok ()
      end
      else
        match
          Preverify_submit.launch_for_tx
            stealth_preverify
            ~tx_hash:(Transaction.hash tx)
            tx
        with
        | Preverify_submit.Busy ->
          Error
            (Printf.sprintf
               "pre_verify_busy pending = %d limit = %d"
               (Preverify_cache.pending_count ())
               (Preverify_cache.pending_max ()))
        | Preverify_submit.Unmanaged
        | Preverify_submit.Started
        | Preverify_submit.Existing -> Ok ()
    in
    let rest_runtime =
      Rest.{
        swarm_ref;
        preverify_admit;
        save_drops;
        find_drop = Tx_drop.find drop_db;
        drops_by_addr = Tx_drop.by_addr drop_db;
      }
    in
    let fold_enabled epoch =
      match Rule_graph.set_fold rules ~epoch with
      | Ok Rule_graph.Active -> true
      | Ok Rule_graph.Prior
      | Error _ -> false
    in
    let fold_bonded () =
      match
        irmin_get_meta store Octra_core.Validator_registry.meta_key
      with
      | None -> false
      | Some raw ->
        begin
          match Octra_core.Validator_registry.of_string raw with
          | Error _ -> false
          | Ok registry ->
            begin
              match
                Octra_core.Validator_registry.find wallet.address registry
              with
              | Some candidate ->
                candidate.Octra_core.Validator_admission.exit_epoch = None
              | None -> false
            end
        end
    in
    let fold_message action ~head_epoch ~state_root =
      let fields = [
        "consensus_pubkey", `String wallet.pub;
        "head_epoch", `String (Int64.to_string head_epoch);
        "state_root", `String state_root;
      ] in
      let fields =
        match action with
        | Set_actor.Pulse -> fields
        | Set_actor.Appeal proof ->
          fields @ [
            "duty_vote",
              `String
                (Base64.encode_exn
                   (Octra_consensus.C_codec.encode_vote proof.vote));
            "duty_commit",
              `String
                (Base64.encode_exn
                   (Octra_consensus.C_parent_commit.encode proof.commit));
          ]
      in
      Yojson.Safe.to_string (`Assoc fields)
    in
    let send_fold action =
      let open Lwt.Syntax in
      let epoch = !current_epoch in
      if not (fold_enabled epoch) then
        Lwt.return_error "validator set fold rule is not active"
      else
        let head_epoch = Int64.of_int (epoch - 1) in
        let* state_root = ready_state_root_at (epoch - 1) in
        match state_root, Ledger.find_opt ledger wallet.address with
        | None, _ -> Lwt.return_error "validator set fold head root is unavailable"
        | _, None -> Lwt.return_error "validator set fold account is unavailable"
        | Some state_root, Some account ->
          let nonce =
            Staging.pending_nonce wallet.address account.Ledger.nonce + 1
          in
          let draft = Transaction.{
            from = wallet.address;
            to_ = wallet.address;
            amount = Z.zero;
            nonce;
            ou = Z.zero;
            timestamp = Unix.gettimeofday ();
            signature = "";
            public_key = Some wallet.pub;
            message = Some (fold_message action ~head_epoch ~state_root);
            op_type = ValidatorReady;
            encrypted_data = None;
          } in
          let tx = { draft with ou = Transaction.ou_cost draft } in
          let tx = Transaction.sign_with_privkey tx wallet.priv in
          begin
            match
              Rest.add_tx_to_staging
                ~bft_mode:consensus_mode
                rest_runtime
                ledger
                tx
            with
            | Ok _ -> Lwt.return_ok ()
            | Error error -> Lwt.return_error error
          end
    in
    let fold_actor =
      Set_actor.create Set_actor.{
        sample = (fun () ->
          let epoch = !current_epoch in
          {
            epoch = Int64.of_int epoch;
            active =
              Octra_consensus.C_types.is_validator
                !consensus_validator_set_ref
                wallet.address;
            bonded = fold_enabled epoch && fold_bonded ();
          });
        send = send_fold;
        warn = (fun reason ->
          Log.warn "validator"
            "event = set_actor_send_failed reason = %s"
            reason);
      }
    in
    let bind_fold driver =
      Octra_consensus.C_driver.set_fold_handler
        driver
        (fun ~next_epoch proof ->
          match Set_actor.notify fold_actor ~epoch:next_epoch proof with
          | Set_actor.Accepted -> ()
          | Set_actor.Busy ->
            Log.warn "validator"
              "event = set_actor_overload epoch = %Ld"
              next_epoch
          | Set_actor.Stopped ->
            Log.warn "validator"
              "event = set_actor_stopped epoch = %Ld"
              next_epoch)
    in
    let run_preverify prepared txs =
      Octra_core.Preverify_worker.run_many
        ~field_policy:(private_field_policy !current_epoch)
        ~ledger
        ~result_policy:(private_result_policy !current_epoch)
        ~circle_preverify:
          (Consensus_circle_preverify.run circle_preverify)
        ~circle_cell_preverify:
          (Consensus_circle_cell_preverify.run circle_cell_preverify)
        ~legacy_replay:(fun ~address ~cipher ->
          legacy_replay
            ~epoch:!current_epoch
            ~address
            ~cipher)
        ~prepared
        txs
    in
    let prepared_lookup private_lookup key_switch_lookup tx =
      let open Lwt.Syntax in
      let* available = private_lookup tx in
      match available with
      | Octra_core.Preverify_availability.Unmanaged -> key_switch_lookup tx
      | Octra_core.Preverify_availability.Pending
      | Octra_core.Preverify_availability.Ready _
      | Octra_core.Preverify_availability.Invalid _ -> Lwt.return available
    in
    let build_preverify =
      run_preverify
        (prepared_lookup
           (Consensus_private_preverify.observe private_preverify)
           (Consensus_key_switch_preverify.observe key_switch_preverify))
      |> Consensus_preverify_role.build
    in
    let validate_preverify =
      run_preverify
        (prepared_lookup
           (Consensus_private_preverify.await private_preverify)
           (Consensus_key_switch_preverify.await key_switch_preverify))
      |> Consensus_preverify_role.validate
    in
    Consensus_driver_boot_shell.run
      Consensus_driver_boot_shell.{
        env = env_opt;
        env_int;
        data_dir;
        chain_id;
        store;
        chaindata;
        consensus_mode;
        voting = voting_consensus_mode;
        observer = observer_mode;
        consensus_port;
        consensus_peers;
        role_label = Octra_consensus.C_role.label consensus_role;
        wallet = {
          address = wallet.address;
          pub = wallet.pub;
          priv = wallet.Wallet.priv;
        };
        p2p_refs = {
          consensus_config_hash = consensus_config_hash_ref;
          consensus_validator_set = consensus_validator_set_ref;
          scheduled_validator_set = scheduled_validator_set_ref;
          set_swarm = (fun swarm ->
            swarm_opt := Some swarm;
            swarm_ref := Some swarm);
        };
        current_epoch;
        consensus_finalized;
        catchup_active = catchup_in_progress;
        catchup_queue;
        runtime_state = consensus_state;
        liveness_state = consensus_liveness;
        driver_ref;
        on_driver = bind_fold;
        proposal_state;
        proposal_bundles;
        bundle_runtime = proposal_bundle_runtime;
        proposal_limits = bft_proposal_limits;
        current_round = current_consensus_round;
        finality = finality_callbacks;
        catchup_queue_node = catchup_node_queue;
        read_active_validator_meta = (fun () ->
          irmin_get_meta store Octra_core.Validator_set_update.active_meta_key);
        read_pending_validator_meta = (fun () ->
          irmin_get_meta store Octra_core.Validator_set_update.pending_meta_key);
        read_head_hash = (fun () -> irmin_get_head_hash store);
        get_meta = irmin_get_meta store;
        read_persistent_pending = (fun () ->
          Store_irmin.get_meta store
            Octra_core.Validator_set_update.pending_meta_key);
        root_of_head_hash = hex_to_raw32_lossy;
        root_to_raw32;
        raw_to_hex;
        read_prev_ledger_root = (fun () -> Store_irmin.get_head_hash store);
        find_account = Ledger.find_opt ledger;
        build_preverify;
        validate_preverify;
        proposal_preview;
        apply_catchup_record;
        catchup_base_eic = catchup_base_eic_root;
        next_txid = (fun () -> Store_chaindata.next_txid chaindata);
        cached_head = Octra_core.Head_manifest.get_cached;
        committed_epoch_root_raw = driver_roots.committed_epoch_root_raw;
        committed_head_epoch = driver_roots.committed_head_epoch;
        read_local_root_raw = driver_roots.read_local_root_raw;
        read_local_ledger_root_raw = driver_roots.read_local_ledger_root_raw;
        cached_root = driver_roots.cached_root;
        clear_state_attested;
        set_state_attested;
        clear_quarantine;
        mark_quarantine;
        validator_pubkeys_for_epoch;
        proposal_capacity = Staging.max_ou_per_epoch;
        quarantine_mismatch_threshold;
        soft_catchup_max_lag;
        quarantine_ahead_streak_threshold;
        quarantine_ahead_grace_epochs;
        quarantine_ahead_drift_tolerance;
        quarantine_poll_sec;
        liveness_stall_sec = consensus_liveness_stall_sec;
        state_readable = (fun () ->
          not (Epoch_visibility.is_applying epoch_visibility));
        sleep = Lwt_unix.sleep;
        now = Unix.gettimeofday;
        exit_error;
      };

    let sync_task () =
      Sync_publish.run
        Sync_publish.{
          data_dir;
          chain_id;
          store;
          chaindata;
          wallet;
          config_hash = State_sync_http.configured_config_hash;
          trusted_validator_set = State_sync_http.configured_validator_set;
          head = Octra_core.Head_manifest.get_cached;
          read_finality = (fun epoch ->
            Consensus_finality_journal.read_committed_epoch
              ~chain_id
              ~epoch
              data_dir);
          exporter_set = State_sync_http.exporter_set;
          certificate_path = (fun () ->
            State_sync_http.certificate_path ~data_dir);
          now = Unix.gettimeofday;
          sleep = Lwt_unix.sleep;
          info = Log.info "state_sync" "%s";
          warn = Log.warn "state_sync" "%s";
        }
    in
    let compute_identity =
      Octra_node_runtime.P2p_config.identity
        ~priv_b64:wallet.Wallet.priv
        ~pub_b64:wallet.pub in
    let compute_raw_root value =
      if Octra_node_runtime.Text.is_hex_len 64 value then
        Some (Octra_node_runtime.Text.hex_to_string value)
      else
        None in
    let compute_state_root_at epoch_id =
      if epoch_id < 0L || epoch_id > Int64.of_int max_int then None
      else
        match Store_chaindata.get_epoch_header chaindata (Int64.to_int epoch_id) with
        | None -> None
        | Some header -> compute_raw_root header.Octra_core.Epochlog.state_root in
    let compute_provider =
      match
        Resource_compute_config.of_inputs
          ~cli_enabled:(cli_has_flag "--compute-provider")
          ~getenv:env_opt
      with
      | Error reason ->
        Log.fatal "compute"
          "event = provider_config status = rejected reason = %s"
          reason;
        exit_error ()
      | Ok Resource_compute_config.Disabled ->
        Log.info "compute" "event = provider status = disabled";
        None
      | Ok (Resource_compute_config.Enabled limits) ->
        let deps =
          Resource_compute_engine.native_deps
            ~limits
            ~store
            ~state_root_at:compute_state_root_at in
        let engine = Resource_compute_engine.create ~limits ~deps in
        Log.info "compute"
          "event = provider status = enabled accelerator = %s lanes = %d memory_bytes = %Ld"
          (Resource_compute_config.accelerator_name limits.accelerator)
          limits.lanes
          limits.memory_bytes;
        Some Resource_compute_service.{ limits; engine }
    in
    let compute_head () =
      match Octra_core.Head_manifest.get_cached () with
      | None -> None
      | Some head ->
        Option.map
          (fun state_root -> Resource_compute_service.{
             epoch_id = Int64.of_int head.Octra_core.Head_manifest.epoch_id;
             state_root;
           })
          (compute_raw_root head.state_root) in
    let compute_frame payload =
      Octra_net.P2p_frame.{ msg_type = msg_resource_compute; payload } in
    let resource_compute =
      Resource_compute_service.create
        ~deps:Resource_compute_service.{
          chain_id;
          node_id = wallet.address;
          sign = compute_identity.sign_fn;
          validator_set = (fun () -> !consensus_validator_set_ref);
          head = compute_head;
          state_root_at = compute_state_root_at;
          self_test = Resource_compute_engine.native_self_test;
          nonce = (fun () -> Mirage_crypto_rng.generate 32);
          broadcast = (fun payload ->
            match !swarm_opt with
            | Some swarm -> Octra_net.P2p_swarm.broadcast swarm (compute_frame payload)
            | None -> Lwt.return_unit);
          relay = (fun ~source payload ->
            match !swarm_opt with
            | Some swarm ->
              Octra_net.P2p_swarm.broadcast_except
                swarm
                ~except:source
                (compute_frame payload)
            | None -> Lwt.return_unit);
          sleep = Lwt_unix.sleep;
        }
        ~provider:compute_provider in

    let rpc_task =
      Rest.start_task rest_runtime
        ~port:api_port ~data_dir ~store ~ledger ~tree_ref:tree ~wallet
        ~chain_id ~consensus_config_hash_ref ~consensus_validator_set_ref
        ~scheduled_validator_set_ref ~current_epoch ~total_tx_count
        ~validator_view_sk ~validator_view_pub ~program_trust ~chaindata
        ~migration_entitlements
        ~rules
        ~consensus_driver_ref:driver_ref
        ~epoch_visibility
        ~resource_compute
    in
    Startup_node_launch_shell.run
      Startup_node_launch_shell.{
        p2p_port;
        rpc = rpc_task;
        services = [sync_task];
        observer = observer_mode;
        tick_loop;
        swarm = !swarm_opt;
        guard = tx_gossip_guard;
        find_tx = (fun hash ->
          match Staging.find_by_hash hash with
          | Some _ as tx -> tx
          | None -> Consensus_bundle_cache.find_shared proposal_bundles hash);
        find_account = Ledger.find_opt ledger;
        add_tx = (fun tx ->
          Rest.add_tx_to_staging
            ~relay:false
            ~bft_mode:consensus_mode
            rest_runtime
            ledger
            tx);
        now = Unix.gettimeofday;
        max_drift = Rest.max_timestamp_drift;
        driver_ref;
        resource_compute = Some resource_compute;
        close_chaindata = (fun () -> Store_chaindata.close chaindata);
        exit_fatal = exit_error;
      }