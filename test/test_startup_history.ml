(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module S = Octra_node_runtime.Startup_history_shell
exception Refused

let expect label condition = if not condition then failwith label

let status ?(ok = true) ?(errors = []) epoch_id =
  Octra_core.Store_chaindata.{
    epoch_id;
    expected_start_txid = 0L;
    expected_tx_count = 1;
    checked = 1;
    missing_epoch_meta = false;
    missing_txid_loc = (if ok then 0 else 1);
    missing_tx_loc = 0;
    missing_addr_refs = 0;
    malformed_records = 0;
    errors;
  }

let test_windows () =
  expect "zero window skipped latest epoch"
    (S.check_window ~window:0 ~first_epoch:0 ~last_epoch:(Some 9)
      = S.Window { from_epoch = 9; to_epoch = 9 });
  expect "empty history window"
    (S.check_window ~window:3 ~first_epoch:0 ~last_epoch:None = S.No_epochs);
  expect "recent window"
    (S.check_window ~window:3 ~first_epoch:0 ~last_epoch:(Some 9)
      = S.Window { from_epoch = 7; to_epoch = 9 });
  expect "snapshot window includes omitted prefix"
    (S.check_window ~window:512 ~first_epoch:8 ~last_epoch:(Some 9)
      = S.Window { from_epoch = 8; to_epoch = 9 });
  expect "snapshot with no local epochs"
    (S.check_window ~window:512 ~first_epoch:10 ~last_epoch:(Some 9) = S.No_epochs)

let test_failures () =
  match S.strict_failures ~from_epoch:4 ~to_epoch:6 ~status_at:(function
    | 4 -> None
    | 5 -> Some (status ~ok:false ~errors:["missing"] 5)
    | epoch -> Some (status epoch)) with
  | [4, None; 5, Some bad] -> expect "bad status" (bad.missing_txid_loc = 1)
  | _ -> failwith "missing and invalid epochs must both fail"

let test_refusal () =
  let refused = try
    S.run_strict_verify {
      window = 2;
      first_epoch = (fun () -> 0);
      last_epoch = (fun () -> Some 8);
      status_at = (function
        | 8 -> Some (status ~ok:false ~errors:["broken"] 8)
        | epoch -> Some (status epoch));
      exit_fatal = (fun () -> raise Refused);
    };
    false
  with Refused -> true in
  expect "strict refusal" refused

let run ~marker ~window ~status_at events =
  S.run_startup_checks {
    int_value = (fun name default ->
      expect "unexpected configuration lookup" (name = "OCTRA_HISTORY_STRICT_STARTUP_EPOCHS");
      expect "default history window" (default = 512);
      window);
    first_epoch = (fun () -> 0);
    last_epoch = (fun () -> Some 10);
    status_at = (fun epoch -> events := ("epoch" ^ string_of_int epoch) :: !events; status_at epoch);
    marker_path = "marker";
    marker_exists = (fun path -> expect "marker path" (path = "marker"); events := "marker" :: !events; marker);
    irmin_stealth_counter = (fun () -> events := "irmin" :: !events; 11L);
    chaindata_next_txid = (fun () -> events := "chaindata" :: !events; 12L);
    exit_fatal = (fun () -> raise Refused);
  }

let test_startup_order () =
  let events = ref [] in
  run ~marker:false ~window:1 ~status_at:(fun epoch -> Some (status epoch)) events;
  expect "marker not checked first" (List.hd (List.rev !events) = "marker");
  expect "wrong epoch check" (List.mem "epoch10" !events);
  expect "counter calls" (List.mem "irmin" !events && List.mem "chaindata" !events)

let test_marker_first () =
  let events = ref [] in
  let refused = try
    run ~marker:true ~window:512 ~status_at:(fun _ -> failwith "read after marker") events;
    false
  with Refused -> true in
  expect "marker refusal" refused;
  expect "effects after marker" (!events = ["marker"])

let test_window_zero () =
  let events = ref [] in
  let refused = try run ~marker:false ~window:0 ~status_at:(fun _ -> None) events; false
    with Refused -> true in
  expect "zero window disabled verification" refused;
  expect "counter read after failure" (not (List.mem "irmin" !events))

let test_returned_exit () =
  let refused = try
    S.run_reindex_marker_guard {
      marker_path = "marker"; exists = (fun _ -> true); exit_fatal = (fun () -> ());
    };
    false
  with Failure reason -> reason = "history startup refused" in
  expect "returning exit callback continued startup" refused

let () =
  List.iter (fun check -> check ()) [test_windows; test_failures; test_refusal;
    test_startup_order; test_marker_first; test_window_zero; test_returned_exit];
  print_endline "event = startup_history status = passed"