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

let () =
  test_progress ();
  test_refusal ();
  test_plan ();
  test_conflict ();
  print_endline "status = pass test = sync_mark"