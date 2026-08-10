(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Ledger = Octra_core.Ledger
module Store_chaindata = Octra_core.Store_chaindata
module Store_irmin = Octra_core.Store_irmin
module Epochlog = Octra_core.Epochlog
module Head_manifest = Octra_core.Head_manifest
module Emission_policy = Octra_core.Emission_policy
module Emission_schedule = Octra_core.Emission_schedule
module Denomination = Octra_core.Denomination

type wallet = {
  address : string;
  pub : string;
}

type store_deps = {
  data_dir : string;
  store : Store_irmin.t;
  exit_fatal : unit -> unit;
}

type deps = {
  data_dir : string;
  store : Store_irmin.t;
  ledger : Ledger.t;
  chaindata : Store_chaindata.t;
  total_tx_count : int ref;
  observer_mode : bool;
  wallet : wallet;
  consensus_mode : bool;
  voting_consensus_mode : bool;
  consensus_port_configured : unit -> bool;
  validators : unit -> (string * string) list;
  int_value : string -> int -> int;
  env : string -> string option;
  exit_fatal : unit -> unit;
}

let run_s = Ledger.run_s

let meta_int ~default = function
  | Some raw -> Startup_process_shell.parse_int ~default raw
  | None -> default

let last_epoch_id = function
  | Some header -> Some header.Epochlog.id
  | None -> None

let last_epoch_or ~default header =
  match last_epoch_id header with
  | Some epoch -> epoch
  | None -> default

let recovery_override_error ~consensus_mode ~skip_recovery ~skip_reconcile =
  if not consensus_mode then None
  else if skip_recovery then
    Some "consensus mode rejects OCTRA_SKIP_RECOVERY"
  else if skip_reconcile then
    Some "consensus mode rejects OCTRA_SKIP_RECONCILE"
  else None

let get_meta (deps : deps) key =
  run_s (Store_irmin.get_meta deps.store key)

let set_meta deps ~key ~value =
  run_s (Store_irmin.set_meta deps.store key value)

let get_store_meta (deps : store_deps) key =
  run_s (Store_irmin.get_meta deps.store key)

let check_emission deps =
  match
    Emission_policy.check_remaining
      (Emission_policy.of_env deps.env)
      (get_meta deps "emission_remaining")
  with
  | Ok () -> ()
  | Error error ->
    Octra_log.fatal "init" "event = emission_guard reason = %s" error;
    deps.exit_fatal ()

let supply_envelope deps state =
  match Emission_schedule.of_env deps.env with
  | Error error -> Error error
  | Ok schedule ->
    let epoch_id =
      meta_int
        ~default:0
        (get_meta deps "current_epoch")
    in
    Emission_schedule.bind
      ~schedule
      ~epoch_id
      ~remaining:state.Emission_policy.emission_remaining
      ~headroom:(Z.sub Denomination.max_supply state.total_supply)
      ~stored_standard:(get_meta deps Emission_schedule.standard_key)
      ~stored_activation:(get_meta deps Emission_schedule.activation_key)
      ~stored_initial:(get_meta deps Emission_schedule.initial_key)
      ~stored_retired:(get_meta deps Emission_schedule.retired_key)

let check_supply deps =
  if Ledger.length deps.ledger = 0 then ()
  else
    let total = get_meta deps "total_supply" in
    let legacy = Emission_policy.legacy_total deps.env in
    if deps.consensus_mode && Option.is_none total then begin
      Octra_log.fatal "init"
        "event = supply_guard reason = consensus mode rejects legacy supply bootstrap";
      deps.exit_fatal ()
    end else
      match
        Emission_policy.runtime_state
          ~policy:(Emission_policy.of_env deps.env)
          ~public_supply:(Ledger.get_total_supply deps.ledger)
          ~emission:(get_meta deps "emission_remaining")
          ~total
          ~legacy
      with
      | Error error ->
        Octra_log.fatal "init" "event = supply_guard reason = %s" error;
        deps.exit_fatal ()
      | Ok state ->
        begin
          match supply_envelope deps state with
          | Ok _ -> ()
          | Error error ->
            Octra_log.fatal "init"
              "event = supply_guard reason = %s"
              error;
            deps.exit_fatal ()
        end

let chain_last_epoch deps () =
  match Store_chaindata.last_epoch_id deps.chaindata with
  | Ok value -> value
  | Error reason -> failwith reason

let chain_last_epoch_or deps ~default () =
  match Store_chaindata.last_epoch_id deps.chaindata with
  | Ok (Some value) -> value
  | Ok None -> default
  | Error reason -> failwith reason

let irmin_stealth_counter deps () =
  get_meta deps "stealth_counter"
  |> Option.map (Startup_process_shell.parse_int64 ~default:0L)
  |> Option.value ~default:0L

let run_store_integrity (deps : store_deps) =
  Startup_store_shell.run_integrity {
    head_state = (fun () ->
      match Head_manifest.load_result deps.data_dir with
      | Head_manifest.Missing -> Startup_store_shell.Head_missing
      | Head_manifest.Corrupt reason ->
        Startup_store_shell.Head_corrupt reason
      | Head_manifest.Present head ->
        Startup_store_shell.Head_ready {
          epoch = head.Head_manifest.epoch_id;
          root = Head_manifest.ledger_state_root head;
        });
    store_root = (fun () -> run_s (Store_irmin.get_head_hash deps.store));
    epoch_root = (fun epoch ->
      match run_s (Store_irmin.epoch_binding deps.store epoch) with
      | Ok binding -> Some binding.Store_irmin.root
      | Error _ -> None);
    rollback_epoch = (fun epoch ->
      run_s (Store_irmin.rollback_to_epoch deps.store epoch));
    verify_integrity = (fun () ->
      run_s (Store_irmin.verify_integrity deps.store));
    save_state_root = (fun () ->
      run_s (Store_irmin.save_state_root deps.store));
    exit_fatal = deps.exit_fatal;
  }

let run_epoch_tags (deps : store_deps) =
  Startup_store_shell.run_epoch_tags {
    list_epoch_tags = (fun () ->
      run_s (Store_irmin.list_epoch_tags deps.store));
    last_epoch = (fun () -> get_store_meta deps "last_epoch");
    tag_epoch = (fun epoch ->
      run_s (Store_irmin.tag_epoch deps.store epoch));
  }

let run_recovery deps =
  Startup_recovery_shell.run_atomic_recovery {
    skip_recovery = (fun () -> deps.env "OCTRA_SKIP_RECOVERY" = Some "1");
    run_recovery = (fun () ->
      run_s (Octra_core.Startup_recovery.recover
        ~data_dir:deps.data_dir
        ~chaindata:deps.chaindata
        ~store:deps.store));
    txlog_position = (fun () -> Store_chaindata.txlog_position deps.chaindata);
    tx_count = (fun () -> Store_chaindata.tx_count deps.chaindata);
    set_total_tx_count = (fun n -> deps.total_tx_count := n);
  }

let run_reconciliation deps =
  Startup_recovery_shell.run_reconciliation {
    irmin_last_epoch = (fun () -> meta_int ~default:(-1) (get_meta deps "last_epoch"));
    chain_last_epoch = chain_last_epoch_or deps ~default:(-1);
    skip_reconcile = (fun () -> deps.env "OCTRA_SKIP_RECONCILE" = Some "1");
    exit_fatal = deps.exit_fatal;
  }

let run_history deps =
  Startup_history_shell.run_startup_checks {
    int_value = deps.int_value;
    last_epoch = chain_last_epoch deps;
    repair_tx_loc = (fun ~from_epoch ~to_epoch ->
      Store_chaindata.verify_and_repair_tx_loc_recent_epochs
        deps.chaindata
        ~from_epoch
        ~to_epoch);
    repair_txid_loc = (fun ~from_epoch ~to_epoch ->
      Store_chaindata.verify_and_repair_txid_loc_recent_epochs
        deps.chaindata
        ~from_epoch
        ~to_epoch);
    status_at = (fun epoch ->
      Store_chaindata.get_visible_epoch_index_status deps.chaindata epoch);
    marker_path = Filename.concat deps.data_dir "chaindata/.reindex_in_progress";
    marker_exists = Sys.file_exists;
    irmin_stealth_counter = irmin_stealth_counter deps;
    chaindata_next_txid = (fun () -> Store_chaindata.next_txid deps.chaindata);
    exit_fatal = deps.exit_fatal;
  }

let run_account deps =
  Startup_account_shell.run {
    ledger_empty = (fun () -> Ledger.length deps.ledger = 0);
    observer_mode = deps.observer_mode;
    wallet_addr = deps.wallet.address;
    wallet_pub = deps.wallet.pub;
    wallet_present = (fun () -> Ledger.mem deps.ledger deps.wallet.address);
    consensus_port_configured = deps.consensus_port_configured;
    validators = deps.validators;
    add_account = (fun ~addr ~pub ~amount ->
      Ledger.add_account_with_pubkey deps.ledger addr amount pub);
    set_meta = (fun ~key ~value -> set_meta deps ~key ~value);
    flush_dirty = (fun () -> Lwt_main.run (Ledger.flush_dirty_lwt deps.ledger));
  }

let run_epoch deps =
  Startup_epoch_shell.run {
    last_epoch_meta = (fun () -> get_meta deps "last_epoch");
    saved_epochs = (fun () -> Node_rest_facade.list_saved_epochs deps.chaindata);
    set_current_epoch = (fun epoch ->
      set_meta deps ~key:"current_epoch" ~value:(string_of_int epoch));
  }

let run_store deps =
  run_store_integrity deps;
  run_epoch_tags deps

let run_node deps =
  let skip_recovery = deps.env "OCTRA_SKIP_RECOVERY" = Some "1" in
  let skip_reconcile = deps.env "OCTRA_SKIP_RECONCILE" = Some "1" in
  match recovery_override_error
    ~consensus_mode:deps.consensus_mode
    ~skip_recovery
    ~skip_reconcile with
  | Some reason ->
    Octra_log.fatal "init" "event = recovery_policy reason = %s" reason;
    deps.exit_fatal ();
    -1
  | None ->
    run_recovery deps;
    run_reconciliation deps;
    run_history deps;
    check_supply deps;
    check_emission deps;
    run_account deps;
    run_epoch deps