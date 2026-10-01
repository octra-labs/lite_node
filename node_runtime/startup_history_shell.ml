(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type strict_deps = {
  window : int;
  first_epoch : unit -> int;
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
  first_epoch : unit -> int;
  last_epoch : unit -> int option;
  status_at : int -> Octra_core.Store_chaindata.epoch_index_status option;
  marker_path : string;
  marker_exists : string -> bool;
  irmin_stealth_counter : unit -> int64;
  chaindata_next_txid : unit -> int64;
  exit_fatal : unit -> unit;
}

type window =
  | No_epochs
  | Window of { from_epoch : int; to_epoch : int }

let check_window ~window ~first_epoch ~last_epoch =
  match last_epoch with
  | None -> No_epochs
  | Some to_epoch when to_epoch < first_epoch -> No_epochs
  | Some to_epoch ->
      Window {
        from_epoch = max first_epoch (max 0 (to_epoch - max 1 window + 1));
        to_epoch;
      }

let strict_failures ~from_epoch ~to_epoch ~status_at =
  let failures = ref [] in
  for epoch = from_epoch to to_epoch do
    match status_at epoch with
    | Some status when Octra_core.Store_chaindata.epoch_index_status_ok status -> ()
    | status -> failures := (epoch, status) :: !failures
  done;
  List.rev !failures

let log_strict_failure ~from_epoch ~to_epoch failures =
  let first_epoch, _ = List.hd failures in
  Octra_log.fatal "init"
    "event = history_strict_verify status = failed epochs = %d..%d failures = %d first_epoch = %d"
    from_epoch to_epoch (List.length failures) first_epoch;
  List.iter (function
    | epoch, None ->
        Octra_log.fatal "init"
          "event = history_strict_verify_error epoch = %d reason = epoch_missing" epoch
    | epoch, Some (status : Octra_core.Store_chaindata.epoch_index_status) ->
        Octra_log.fatal "init"
          "event = history_strict_verify_error epoch = %d missing_epoch_meta = %b missing_txid_loc = %d missing_tx_loc = %d missing_addr_refs = %d malformed = %d first_error = %s"
          epoch status.missing_epoch_meta status.missing_txid_loc status.missing_tx_loc
          status.missing_addr_refs status.malformed_records
          (match status.errors with first :: _ -> first | [] -> "index_incomplete"))
    failures

let run_strict_verify (deps : strict_deps) =
  match check_window ~window:deps.window ~first_epoch:(deps.first_epoch ())
    ~last_epoch:(deps.last_epoch ()) with
  | No_epochs ->
      Octra_log.info "init"
        "event = history_strict_verify status = skipped reason = no_chaindata_epochs"
  | Window { from_epoch; to_epoch } ->
      let failures = strict_failures ~from_epoch ~to_epoch ~status_at:deps.status_at in
      if failures = [] then
        Octra_log.info "init"
          "event = history_strict_verify status = clean epochs = %d..%d"
          from_epoch to_epoch
      else begin
        log_strict_failure ~from_epoch ~to_epoch failures;
        deps.exit_fatal ();
        failwith "history startup refused"
      end

let run_reindex_marker_guard deps =
  if deps.exists deps.marker_path then begin
    Octra_log.fatal "init"
      "event = external_reindex_marker_found path = %s action = refuse_start"
      deps.marker_path;
    deps.exit_fatal ();
    failwith "history startup refused"
  end

let log_stealth_counter ~irmin_stealth_counter ~chaindata_next_txid =
  if Int64.compare irmin_stealth_counter chaindata_next_txid > 0 then
    Octra_log.warn "init"
      "event = stealth_counter irmin = %Ld chaindata_next_txid = %Ld relation = ahead action = check_gap_outputs"
      irmin_stealth_counter chaindata_next_txid
  else
    Octra_log.info "init"
      "event = stealth_counter irmin = %Ld chaindata_next_txid = %Ld relation = ok"
      irmin_stealth_counter chaindata_next_txid

let run_startup_checks deps =
  run_reindex_marker_guard {
    marker_path = deps.marker_path;
    exists = deps.marker_exists;
    exit_fatal = deps.exit_fatal;
  };
  run_strict_verify {
    window = max 1 (deps.int_value "OCTRA_HISTORY_STRICT_STARTUP_EPOCHS" 512);
    first_epoch = deps.first_epoch;
    last_epoch = deps.last_epoch;
    status_at = deps.status_at;
    exit_fatal = deps.exit_fatal;
  };
  log_stealth_counter
    ~irmin_stealth_counter:(deps.irmin_stealth_counter ())
    ~chaindata_next_txid:(deps.chaindata_next_txid ())