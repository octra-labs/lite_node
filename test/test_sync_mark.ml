(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Need = Octra_node_runtime.Sync_need
module Mark = Octra_node_runtime.Sync_mark

let expect label value =
  if not value then failwith ("sync mark: " ^ label)

let chain = "octra-test"

let setup need =
  let data_dir = Test_workspace.unique_dir "sync-mark" in
  expect "stored" (Mark.write ~data_dir ~chain need = Ok Mark.Stored);
  data_dir

let test_progress () =
  let need = Need.journal ~epoch:1514114 ~head:1514113 in
  List.iter
    (fun verified_head ->
      let data_dir = setup need in
      expect "verified progress"
        (Mark.consume_journal ~data_dir ~chain ~verified_head need = Ok ());
      expect "consumed" (Mark.read ~data_dir ~chain = Mark.Missing);
      expect "repeat"
        (Mark.consume_journal ~data_dir ~chain ~verified_head need = Ok ()))
    [1514113; 1514114; 1514115; 1516800]

let test_refusal () =
  let need = Need.journal ~epoch:7 ~head:6 in
  let cases = [
    need, need, chain, -1;
    need, need, chain, 5;
    need, need, chain, max_int;
    need, need, "another-chain", 7;
    need, Need.journal ~epoch:8 ~head:7, chain, 8;
    need, { need with epoch = 9 }, chain, 9;
    need, { need with head = -1; epoch = 0 }, chain, 0;
    need, { need with target = Some 9L }, chain, 9;
    Need.root ~epoch:7 ~head:6, Need.root ~epoch:7 ~head:6, chain, 7;
  ] in
  List.iter
    (fun (stored, expected, requested_chain, verified_head) ->
      let data_dir = setup stored in
      expect "refused"
        (Result.is_error
           (Mark.consume_journal ~data_dir ~chain:requested_chain
              ~verified_head expected));
      expect "preserved" (Mark.read ~data_dir ~chain = Mark.Ready stored))
    cases

let test_plan () =
  let need = Need.journal ~epoch:7 ~head:6 in
  List.iter
    (fun expected ->
      expect "invalid plan before disk access"
        (Mark.journal_ready ~verified_head:20 expected
         = Error "journal recovery plan is invalid"))
    [{ need with epoch = 9 }; { need with head = -1; epoch = 0 };
     { need with target = Some 9L }];
  expect "head error is precise"
    (Mark.journal_ready ~verified_head:(-1) need
     = Error "verified journal head is invalid");
  let data_dir = setup need in
  expect "old head waits"
    (Mark.finish_journal ~data_dir ~chain ~verified_head:5 need = Ok false);
  expect "waiting preserves marker" (Mark.read ~data_dir ~chain = Mark.Ready need);
  expect "old head cannot consume"
    (Mark.consume_journal ~data_dir ~chain ~verified_head:5 need
     = Error "verified journal head is below recovery head");
  expect "verified head completes"
    (Mark.finish_journal ~data_dir ~chain ~verified_head:7 need = Ok true);
  expect "restart reports temporary exit" (Mark.restart_code = 75)

let test_conflict () =
  let need = Need.journal ~epoch:7 ~head:6 in
  let data_dir = setup need in
  let conflict = Need.conflict ~epoch:7 ~head:6 in
  expect "conflict stored separately"
    (Mark.write ~data_dir ~chain conflict = Ok Mark.Stored);
  expect "conflict takes precedence"
    (Mark.read ~data_dir ~chain = Mark.Ready conflict);
  List.iter (fun verified_head ->
    expect "attestation does not resolve conflict"
      (Result.is_error
         (Mark.consume_journal ~data_dir ~chain ~verified_head need)))
    [6; 7; 20];
  expect "conflict cannot be consumed"
    (Result.is_error
      (Mark.consume_journal ~data_dir ~chain ~verified_head:20 conflict));
  expect "both markers remain"
    (Sys.file_exists (Mark.path data_dir)
     && Sys.file_exists (Mark.conflict_path data_dir));
  let legacy_dir = Test_workspace.unique_dir "sync-legacy" in
  let file = Mark.path (setup need) in
  let input = open_in_bin file in
  let raw = Fun.protect ~finally:(fun () -> close_in input)
      (fun () -> really_input_string input (in_channel_length input)) in
  let legacy = match Yojson.Safe.from_string raw with
    | `Assoc fields -> `Assoc (("schema", `String "octra_sync_need_v1")
        :: List.remove_assoc "schema" fields)
    | _ -> assert false
  in
  Unix.mkdir (Filename.dirname (Mark.path legacy_dir)) 0o750;
  let output = open_out_bin (Mark.path legacy_dir) in
  Fun.protect ~finally:(fun () -> close_out output) (fun () ->
    output_string output (Yojson.Safe.to_string legacy));
  expect "old reason is not inferred"
    (Mark.read ~data_dir:legacy_dir ~chain = Mark.Ready conflict);
  expect "new reason is preserved"
    (Mark.read ~data_dir:(Filename.dirname (Filename.dirname file)) ~chain
     = Mark.Ready need)

let test_range () =
  let need = Option.get (Need.lost ~head:100 ~target:5000L) in
  let data_dir = setup need in
  let consume verified_head limit =
    Mark.consume_range ~data_dir ~chain ~verified_head ~limit need in
  List.iter (fun (head, limit) ->
    expect "range held" (Result.is_error (consume head limit));
    expect "range retained" (Mark.read ~data_dir ~chain = Mark.Ready need))
    [100, 4096; 903, 4096; max_int, 4096; -1, 4096; 5000, -1];
  expect "changed plan refused"
    (Result.is_error (Mark.consume_range ~data_dir ~chain ~verified_head:5000
       ~limit:4096 { need with target = Some 5001L }));
  expect "window edge accepted" (consume 904 4096 = Ok ());
  expect "range consumed" (Mark.read ~data_dir ~chain = Mark.Missing);
  expect "range restart is idempotent" (consume 904 4096 = Ok ());
  let data_dir = setup need in
  let conflict = Need.conflict ~head:100 ~epoch:101 in
  expect "range conflict saved"
    (Mark.write ~data_dir ~chain conflict = Ok Mark.Stored);
  expect "range attestation preserves conflict"
    (Result.is_error (Mark.consume_range ~data_dir ~chain ~verified_head:5000
       ~limit:4096 need));
  expect "range conflict remains" (Mark.read ~data_dir ~chain = Mark.Ready conflict);
  let huge = { need with target = Some Int64.max_int } in
  expect "range arithmetic does not wrap"
    (not (Need.range_ready ~head:(max_int - 1) ~limit:4096 huge));
  expect "past target is complete" (Need.range_ready ~head:5001 ~limit:0 need);
  expect "root is not a range"
    (not (Need.range_ready ~head:5000 ~limit:4096
      (Need.root ~head:100 ~epoch:101)))

let test_root () =
  let root = Need.root ~head:100 ~epoch:101 in
  List.iter (fun (need, head, accepted) ->
    let data_dir = setup need in
    let result = Mark.consume_root ~data_dir ~chain ~verified_head:head ~limit:4096 need in
    expect "root consume decision" (Result.is_ok result = accepted);
    expect "root consume disk state"
      (Mark.read ~data_dir ~chain = if accepted then Mark.Missing else Mark.Ready need))
    [root, 100, false; root, 101, true;
      {root with target = Some 5000L}, 903, false;
      {root with target = Some 5000L}, 904, true;
      Option.get (Need.lost ~head:100 ~target:5000L), 5000, false]

let test_restore_cycle () =
  let module Boot = Octra_node_runtime.Consensus_driver_boot_shell in
  let module Mode = Octra_node_runtime.Consensus_mode in
  List.iter (fun cause -> List.iter (fun source ->
    let head = if source = "carry" then 3000 else 2000 in
    Test_workspace.with_dir "sync-cycle" (fun directory ->
      let command = String.concat " " (List.map Filename.quote ["python3"; "-B";
        Test_workspace.source "test/sync_cycle.py";
        Test_workspace.source "controls/lib"; directory; cause; source]) in
      expect "snapshot replacement" (Sys.command command = 0);
      let data_dir = Filename.concat directory "data" in
      let marked = Mark.read ~data_dir ~chain in
      let need = match marked with Mark.Ready need -> need | _ -> failwith "restored marker missing" in
      let base = Mode.of_inputs ~cli_observer:false ~env_mode:(Some "bft") in
      let recovery = Mode.with_need (Some need) base in
      expect "restored state cannot vote" (not recovery.voting_enabled && recovery.observer_enabled);
      expect "restored state starts network" (Boot.sync_plan ~head ~limit:4096 marked = Ok None);
      expect "old head remains held" (Boot.sync_plan ~head:100 ~limit:4096 marked = Ok (Some need));
      let exits = ref [] in
      let attest = Octra_node_runtime.Sync_attest.create ~data_dir ~chain
        ~getenv:(fun name -> if name = "OCTRA_CATCHUP_MAX_LAG" then Some "64" else None)
        ~set:(fun ~head:_ ~root:_ -> ()) ~exit:(fun code -> exits := code :: !exits)
        ~exit_error:(fun () -> failwith "attestation failed") (Some need) in
      attest ~head ~root:"checked";
      if need.target <> None then begin
        expect "configured limit preserves marker" (Mark.read ~data_dir ~chain = Mark.Ready need);
        expect "configured limit waits" (!exits = []);
        attest ~head:(Int64.to_int (Option.get need.target) - 64) ~root:"checked"
      end;
      attest ~head:5000 ~root:"checked";
      expect "one controlled restart" (!exits = [75]);
      expect "restart has no recovery marker" (Mark.read ~data_dir ~chain = Mark.Missing);
      let role = Mode.with_need None base in
      expect "restart restores vote role" role.voting_enabled))
      (if cause = "range" then ["disk"; "plan"; "conflict"] else ["disk"; "carry"; "conflict"]))
    ["range"; "root"]

let test_marker_progress () =
  let prior = Need.root ~epoch:101 ~head:100 in
  let next = Need.root ~epoch:905 ~head:904 in
  let data_dir = setup prior in
  expect "covered root replaces atomically"
    (Mark.write ~data_dir ~chain next = Ok Mark.Stored);
  expect "new need survives restart" (Mark.read ~data_dir ~chain = Mark.Ready next);
  expect "new need holds restored head"
    (Octra_node_runtime.Consensus_driver_boot_shell.sync_plan
      ~head:904 ~limit:4096 (Mark.read ~data_dir ~chain) = Ok (Some next));
  expect "repeat keeps same need" (Mark.write ~data_dir ~chain next = Ok (Mark.Present next));
  expect "earlier need cannot replace" (Mark.write ~data_dir ~chain prior = Ok (Mark.Present next));
  let range = Option.get (Need.lost ~head:100 ~target:5000L) in
  let data_dir = setup range in
  let carried = {next with Need.cause = Need.Range; target = Some 5000L} in
  expect "root carries range target"
    (Mark.write ~data_dir ~chain next = Ok Mark.Stored);
  expect "target persists across cause change" (Mark.read ~data_dir ~chain = Mark.Ready carried);
  expect "new root still holds old window"
    (not (Need.snapshot_ready ~head:905 ~limit:64 carried));
  expect "carried target prevents early consume"
    (Result.is_error (Mark.consume_range ~data_dir ~chain ~verified_head:905 ~limit:64 carried));
  expect "carried target allows checked consume"
    (Mark.consume_range ~data_dir ~chain ~verified_head:4936 ~limit:64 carried = Ok ());
  let data_dir = setup range in
  let smaller = Option.get (Need.lost ~head:904 ~target:4999L) in
  expect "smaller range cannot replace" (Mark.write ~data_dir ~chain smaller = Ok (Mark.Present range));
  let later = Option.get (Need.lost ~head:904 ~target:6000L) in
  expect "covered range replaces" (Mark.write ~data_dir ~chain later = Ok Mark.Stored);
  let conflict = Need.conflict ~epoch:905 ~head:904 in
  expect "conflict saved" (Mark.write ~data_dir ~chain conflict = Ok Mark.Stored);
  expect "conflict never replaced"
    (Mark.write ~data_dir ~chain (Need.root ~epoch:6001 ~head:6000) = Ok (Mark.Present conflict))

let test_attest_paths () =
  List.iter (fun cause ->
    let need = Option.map (fun kind -> Need.{cause = kind; epoch = 7; head = 6; target = None}) cause in
    let data_dir = match need with Some need -> setup need | None -> Test_workspace.unique_dir "sync-attest" in
    let calls = ref 0 and exits = ref [] and errors = ref 0 in
    let attest = Octra_node_runtime.Sync_attest.create ~data_dir ~chain ~getenv:(fun _ -> None)
      ~set:(fun ~head:_ ~root:_ -> incr calls) ~exit:(fun code -> exits := code :: !exits)
      ~exit_error:(fun () -> incr errors) need in
    attest ~head:5 ~root:"checked";
    expect "early attestation does not consume" (!calls = 1 && !exits = [] && !errors = 0);
    attest ~head:7 ~root:"checked";
    attest ~head:8 ~root:"checked";
    match cause with
    | Some Need.Journal ->
      expect "journal stops after one exit" (!calls = 2 && !exits = [75] && !errors = 0);
      expect "journal consumed on disk" (Mark.read ~data_dir ~chain = Mark.Missing)
    | _ ->
      expect "unconsumed attestation remains live" (!calls = 3 && !exits = [] && !errors = 0);
      expect "protected marker remains" (Mark.need (Mark.read ~data_dir ~chain) = Ok need))
    [None; Some Need.Conflict; Some Need.Journal];
  let need = Need.root ~epoch:7 ~head:6 in
  let data_dir = setup need in
  let calls = ref 0 and errors = ref 0 and exits = ref [] in
  let attest = Octra_node_runtime.Sync_attest.create ~data_dir ~chain ~getenv:(fun _ -> None)
    ~set:(fun ~head:_ ~root:_ -> incr calls) ~exit:(fun code -> exits := code :: !exits)
    ~exit_error:(fun () -> incr errors) (Some need) in
  let channel = open_out_bin (Mark.path data_dir) in
  output_string channel "invalid"; close_out channel;
  attest ~head:7 ~root:"checked";
  attest ~head:8 ~root:"checked";
  expect "metadata error exits once" (!calls = 1 && !errors = 1 && !exits = []);
  expect "invalid metadata preserved" (match Mark.read ~data_dir ~chain with Mark.Invalid _ -> true | _ -> false)

let run () =
  test_attest_paths ();
  List.iter (fun (text, expected) ->
    expect "history parser" (Need.history_limit ~retained:4096 (Some text) = expected))
    ["5000", 4096; "4096", 4096; "2000", 2000; "0", 1; "-1", 1;
     "invalid", 4096; "0x40", 4096; " 64", 4096; "+64", 64;
     "1_000", 4096; String.make 20 '9', 4096; String.make 20 '-', 4096];
  test_progress ();
  test_refusal ();
  test_plan ();
  test_conflict ();
  test_range ();
  test_root ();
  test_marker_progress ();
  test_restore_cycle ();
  print_endline "status = pass test = sync_mark"

let () =
  match Array.to_list Sys.argv with
  | [_; "--compat-write"; data_dir] ->
    let prior = Option.get (Need.lost ~head:100 ~target:5000L) in
    expect "compat initial marker" (Mark.write ~data_dir ~chain prior = Ok Mark.Stored);
    expect "compat carried marker"
      (Mark.write ~data_dir ~chain (Need.root ~epoch:905 ~head:904) = Ok Mark.Stored)
  | [_] -> run ()
  | _ -> failwith "sync mark arguments differ"