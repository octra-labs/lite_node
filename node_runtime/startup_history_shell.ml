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


type repair_kind =
  | Tx_loc
  | Txid_loc

type repair_deps = {
  kind : repair_kind;
  env_name : string;
  window : int;
  last_epoch : unit -> int option;
  repair :
    from_epoch:int ->
    to_epoch:int ->
    Octra_core.Store_chaindata.repair_stats;
}

type repair_windows = {
  tx_loc : int;
  txid_loc : int;
  strict : int;
}

type repair_pair_deps = {
  windows : repair_windows;
  last_epoch : unit -> int option;
  repair_tx_loc :
    from_epoch:int ->
    to_epoch:int ->
    Octra_core.Store_chaindata.repair_stats;
  repair_txid_loc :
    from_epoch:int ->
    to_epoch:int ->
    Octra_core.Store_chaindata.repair_stats;
}

type strict_deps = {
  window : int;
  last_epoch : unit -> int option;
  status_at : int -> Octra_core.Store_chaindata.epoch_index_status option;
  exit_fatal : unit -> unit;
}

type reindex_marker_deps = {
  marker_path : string;
  exists : string -> bool;
  exit_fatal : unit -> unit;
}

type startup_checks_deps = {
  int_value : string -> int -> int;
  last_epoch : unit -> int option;
  repair_tx_loc :
    from_epoch:int ->
    to_epoch:int ->
    Octra_core.Store_chaindata.repair_stats;
  repair_txid_loc :
    from_epoch:int ->
    to_epoch:int ->
    Octra_core.Store_chaindata.repair_stats;
  status_at : int -> Octra_core.Store_chaindata.epoch_index_status option;
  marker_path : string;
  marker_exists : string -> bool;
  irmin_stealth_counter : unit -> int64;
  chaindata_next_txid : unit -> int64;
  exit_fatal : unit -> unit;
}

type window =
  | Disabled of int
  | No_epochs
  | Window of {
      from_epoch : int;
      to_epoch : int;
    }

let repair_label = function
  | Tx_loc -> "TX_LOC STARTUP HEAL"
  | Txid_loc -> "TXID_LOC STARTUP HEAL"

let repair_windows ~int_value =
  let tx_loc = int_value "OCTRA_TX_HEAL_STARTUP_EPOCHS" 512 in
  let txid_loc = int_value "OCTRA_TXID_HEAL_STARTUP_EPOCHS" 512 in
  {
    tx_loc;
    txid_loc;
    strict =
      int_value "OCTRA_HISTORY_STRICT_STARTUP_EPOCHS"
        (max tx_loc txid_loc);
  }

let repair_window ~window ~last_epoch =
  if window <= 0 then
    Disabled window
  else
    match last_epoch with
    | None -> No_epochs
    | Some to_epoch ->
      Window {
        from_epoch = max 0 (to_epoch - window + 1);
        to_epoch;
      }

let log_repair_disabled label env_name window =
  Octra_log.warn "init" "%s disabled env = %s epochs = %d"
    label env_name window

let log_repair_stats label ~from_epoch ~to_epoch stats =
  if stats.Octra_core.Store_chaindata.repaired > 0 || stats.errors <> [] then
    Octra_log.warn "init"
      "%s epochs = %d..%d checked = %d repaired = %d errors = %d"
      label
      from_epoch
      to_epoch
      stats.checked
      stats.repaired
      (List.length stats.errors)
  else
    Octra_log.info "init"
      "%s clean epochs = %d..%d checked = %d"
      label from_epoch to_epoch stats.checked

let run_repair deps =
  let label = repair_label deps.kind in
  match repair_window ~window:deps.window ~last_epoch:(deps.last_epoch ()) with
  | Disabled window ->
    log_repair_disabled label deps.env_name window
  | No_epochs ->
    Octra_log.info "init" "%s skipped reason = no_chaindata_epochs" label
  | Window { from_epoch; to_epoch } ->
    deps.repair ~from_epoch ~to_epoch
    |> log_repair_stats label ~from_epoch ~to_epoch

let run_repair_pair deps =
  run_repair {
    kind = Tx_loc;
    env_name = "OCTRA_TX_HEAL_STARTUP_EPOCHS";
    window = deps.windows.tx_loc;
    last_epoch = deps.last_epoch;
    repair = deps.repair_tx_loc;
  };
  run_repair {
    kind = Txid_loc;
    env_name = "OCTRA_TXID_HEAL_STARTUP_EPOCHS";
    window = deps.windows.txid_loc;
    last_epoch = deps.last_epoch;
    repair = deps.repair_txid_loc;
  }

let strict_failures ~from_epoch ~to_epoch ~status_at =
  let failures = ref [] in
  for epoch = from_epoch to to_epoch do
    match status_at epoch with
    | Some status when
        not (Octra_core.Store_chaindata.epoch_index_status_ok status) ->
      failures := (epoch, status) :: !failures
    | _ -> ()
  done;
  List.rev !failures

let log_strict_failure
    ~from_epoch
    ~to_epoch
    (failures : (int * Octra_core.Store_chaindata.epoch_index_status) list) =
  let first_epoch, first_status = List.hd failures in
  Octra_log.fatal "init"
    "HISTORY STRICT VERIFY failed epochs = %d..%d failures = %d first_epoch = %d missing_epoch_meta = %b missing_txid_loc = %d missing_tx_loc = %d missing_addr_refs = %d malformed = %d"
    from_epoch
    to_epoch
    (List.length failures)
    first_epoch
    first_status.Octra_core.Store_chaindata.missing_epoch_meta
    first_status.missing_txid_loc
    first_status.missing_tx_loc
    first_status.missing_addr_refs
    first_status.malformed_records;
  List.iter
    (fun (epoch, (status : Octra_core.Store_chaindata.epoch_index_status)) ->
      match status.Octra_core.Store_chaindata.errors with
      | first :: _ ->
        Octra_log.fatal "init"
          "HISTORY STRICT VERIFY epoch = %d first_error = %s"
          epoch first
      | [] -> ())
    failures

let run_strict_verify deps =
  match repair_window ~window:deps.window ~last_epoch:(deps.last_epoch ()) with
  | Disabled window ->
    Octra_log.warn "init"
      "HISTORY STRICT VERIFY disabled env = OCTRA_HISTORY_STRICT_STARTUP_EPOCHS epochs = %d"
      window
  | No_epochs ->
    Octra_log.info "init"
      "HISTORY STRICT VERIFY skipped reason = no_chaindata_epochs"
  | Window { from_epoch; to_epoch } ->
    let failures = strict_failures ~from_epoch ~to_epoch ~status_at:deps.status_at in
    if failures = [] then
      Octra_log.info "init"
        "HISTORY STRICT VERIFY clean epochs = %d..%d"
        from_epoch to_epoch
    else begin
      log_strict_failure ~from_epoch ~to_epoch failures;
      deps.exit_fatal ()
    end

let run_reindex_marker_guard deps =
  if deps.exists deps.marker_path then begin
    Octra_log.stderr "\n[INIT FATAL] external reindex marker found at %s\n"
      deps.marker_path;
    deps.exit_fatal ()
  end

let log_stealth_counter ~irmin_stealth_counter ~chaindata_next_txid =
  if Int64.compare irmin_stealth_counter chaindata_next_txid > 0 then
    Octra_log.warn "init"
      "stealth_counter = %Ld chaindata_next_txid = %Ld relation = ahead action = check_gap_outputs"
      irmin_stealth_counter chaindata_next_txid
  else
    Octra_log.info "init"
      "stealth_counter = %Ld chaindata_next_txid = %Ld relation = ok"
      irmin_stealth_counter chaindata_next_txid

let run_startup_checks deps =
  let windows = repair_windows ~int_value:deps.int_value in
  run_repair_pair {
    windows;
    last_epoch = deps.last_epoch;
    repair_tx_loc = deps.repair_tx_loc;
    repair_txid_loc = deps.repair_txid_loc;
  };
  run_strict_verify {
    window = windows.strict;
    last_epoch = deps.last_epoch;
    status_at = deps.status_at;
    exit_fatal = deps.exit_fatal;
  };
  run_reindex_marker_guard {
    marker_path = deps.marker_path;
    exists = deps.marker_exists;
    exit_fatal = deps.exit_fatal;
  };
  log_stealth_counter
    ~irmin_stealth_counter:(deps.irmin_stealth_counter ())
    ~chaindata_next_txid:(deps.chaindata_next_txid ())