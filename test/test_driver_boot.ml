(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module S = Octra_node_runtime.Consensus_driver_boot_shell
module D = Octra_node_runtime.Consensus_driver_read

let fail msg =
  failwith ("test_node_runtime_consensus_driver_boot_shell: " ^ msg)

let expect label cond =
  if not cond then fail label

let test_enabled () =
  expect "negative disabled" (not (S.enabled (-1)));
  expect "zero disabled" (not (S.enabled 0));
  expect "positive enabled" (S.enabled 1)

let test_validator_state_height () =
  let committed_head = ref 1272591 in
  let height () =
    S.validator_state_height
      ~committed_head_epoch:(fun () -> !committed_head)
  in
  expect "validator state uses committed head" (height () = 1272591L);
  committed_head := 1272592;
  expect "validator state follows committed head" (height () = 1272592L)

let reads () =
  D.{
    chain_id = "octra-test";
    get_epoch_json = (fun epoch -> Some (string_of_int epoch));
    epoch_time = (fun epoch -> Some (float_of_int epoch));
    get_tx_by_txid = (fun txid -> Some (Int64.to_string txid, "tx"));
    read_receipts = (fun epoch -> [string_of_int epoch]);
    root_to_raw32 = Fun.id;
    reward_source = (fun _ _ -> Error "unused");
    head_epoch = (fun () -> Some 12);
    lookup_bundle = (fun _ -> None);
    read_finality = (fun _ -> None);
  }

let test_committed_reads_open () =
  let guarded = S.committed_reads ~readable:(fun () -> true) (reads ()) in
  expect "epoch visible" (guarded.get_epoch_json 7 = Some "7");
  expect "time visible" (guarded.epoch_time 7 = Some 7.);
  expect "transaction visible" (guarded.get_tx_by_txid 7L = Some ("7", "tx"));
  expect "receipts visible" (guarded.read_receipts 7 = ["7"])

let test_committed_reads_closed () =
  let guarded = S.committed_reads ~readable:(fun () -> false) (reads ()) in
  expect "epoch hidden" (guarded.get_epoch_json 7 = None);
  expect "time hidden" (guarded.epoch_time 7 = None);
  expect "transaction hidden" (guarded.get_tx_by_txid 7L = None);
  expect "receipts hidden" (guarded.read_receipts 7 = []);
  expect "head stable" (guarded.head_epoch () = Some 12)

let test_sync_guard () =
  let module Need = Octra_node_runtime.Sync_need in
  let module Mark = Octra_node_runtime.Sync_mark in
  let root = Need.root ~epoch:7 ~head:6 in
  let cases = [
    6, Mark.Missing, Ok None;
    6, Mark.Ready (Need.journal ~epoch:7 ~head:6), Ok None;
    5, Mark.Ready (Need.journal ~epoch:7 ~head:6),
      Ok (Some (Need.journal ~epoch:7 ~head:6));
    6, Mark.Ready (Need.conflict ~epoch:7 ~head:6),
      Ok (Some (Need.conflict ~epoch:7 ~head:6));
    20, Mark.Ready (Need.conflict ~epoch:7 ~head:6),
      Ok (Some (Need.conflict ~epoch:7 ~head:6));
    6, Mark.Ready { Need.cause = Need.Range; epoch = 7; head = 6;
      target = Some 20L }, Ok None;
    5, Mark.Ready root, Ok (Some root);
    6, Mark.Ready root, Ok (Some root);
    7, Mark.Ready root, Ok None;
    8, Mark.Ready root, Ok None;
    6, Mark.Invalid "invalid marker", Error "invalid marker";
  ] in
  List.iter
    (fun (head, state, expected) ->
      expect "sync guard selects one action"
        (S.sync_plan ~head state = expected))
    cases

let test_seed_fault () =
  let module F = Octra_node_runtime.Sync_finality in
  let module N = Octra_node_runtime.Sync_need in
  expect "disk error is not conflict"
    (F.recovery ~head:12 (F.Journal "read failed") = None);
  expect "checkpoint mismatch needs root recovery"
    (F.recovery ~head:12 (F.Root "root differs")
     = Some (N.root ~epoch:13 ~head:12));
  expect "contradictory finality stays held"
    (F.recovery ~head:12 (F.Conflict "different block")
     = Some (N.conflict ~epoch:13 ~head:12));
  expect "invalid height cannot make marker"
    (F.recovery ~head:max_int (F.Conflict "different block") = None)

let () =
  test_enabled ();
  test_validator_state_height ();
  test_committed_reads_open ();
  test_committed_reads_closed ();
  test_sync_guard ();
  test_seed_fault ();
  print_endline "status = pass test = node_runtime_consensus_driver_boot_shell"