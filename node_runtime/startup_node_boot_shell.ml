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


module Ledger = Octra_core.Ledger
module Store_chaindata = Octra_core.Store_chaindata
module Store_irmin = Octra_core.Store_irmin
module Epochlog = Octra_core.Epochlog

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

let get_meta (deps : deps) key =
  run_s (Store_irmin.get_meta deps.store key)

let set_meta deps ~key ~value =
  run_s (Store_irmin.set_meta deps.store key value)

let get_store_meta (deps : store_deps) key =
  run_s (Store_irmin.get_meta deps.store key)

let chain_last_epoch deps () =
  Store_chaindata.get_last_epoch deps.chaindata
  |> last_epoch_id

let chain_last_epoch_or deps ~default () =
  Store_chaindata.get_last_epoch deps.chaindata
  |> last_epoch_or ~default

let irmin_stealth_counter deps () =
  get_meta deps "stealth_counter"
  |> Option.map (Startup_process_shell.parse_int64 ~default:0L)
  |> Option.value ~default:0L

let run_store_integrity (deps : store_deps) =
  Startup_store_shell.run_integrity {
    is_fresh_store = (fun () ->
      not (Sys.file_exists (Startup_store_shell.irmin_path deps.data_dir ^ "/store.pack")));
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
    flush_dirty = (fun () -> run_s (Ledger.flush_dirty_lwt deps.ledger));
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
  run_recovery deps;
  run_reconciliation deps;
  run_history deps;
  run_account deps;
  run_epoch deps