(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Phase = Octra_core.Recovery_phase
module Core = Octra_core
module Head = Core.Head_manifest
module Eic = Core.Epoch_index_commitment
module Wal = Core.Wal
module Journal = Core.Commit_journal
module Epoch = Core.Epochlog

let hash value = String.make 64 value
let expect label valid = if not valid then failwith label
let refused label input = expect label (Result.is_error (Phase.decide input))

let head_hash, head_root = Eic.next_root ~prev:Eic.genesis_root ~epoch_id:0
  [Eic.item ~txid:0L ~hash:(hash 'a')]

let head = Head.{schema_version; generation = 0; epoch_id = 0;
  state_root = Eic.folded_state_root ~ledger_state_root:(hash '1') ~epoch_index_root:head_root;
  ledger_state_root = Some (hash '1'); irmin_commit = Some (hash '2'); txid_hi = 0L;
  txlog_seg = Some 0; txlog_off = Some 100; epochlog_off = Some 100;
  commit_id = "first"; ts = 0.; quorum_cert_hash = None;
  epoch_index_hash = Some head_hash; epoch_index_root = Some head_root}

let stay = Phase.{head = Some head; irmin_epoch = 0; irmin_root = head.ledger_state_root;
  irmin_commit = head.irmin_commit; irmin_parent = None; chain_epoch = 0; marker = None;
  wal = []; journal = []; last = None; epoch_hash = head.epoch_index_hash;
  epoch_root = head.epoch_index_root; tx_position = 0, 100; epoch_offset = 100}

let entry = Wal.{epoch_id = 1; pre_state_root = hash '1'; post_state_root = hash '3';
  parent_commit = hash '2'; start_txid = 1L; tx_count = 0; finalized_by = "test";
  finalized_at = 0.; irmin_last_epoch_before = 0}

let next_hash, next_root = Eic.next_root ~prev:head_root ~epoch_id:1 []
let state_root = Eic.folded_state_root ~ledger_state_root:entry.post_state_root ~epoch_index_root:next_root
let prepare_record = Journal.Prepare {commit_id = "second"; prev_generation = 0; epoch_id = 1;
  planned_txid_hi = 0L; planned_state_root = state_root; ts = 0.}
let last = Epoch.{empty_epoch_header with id = 1; state_root; prev_state_root = head.state_root;
  start_txid = 1L; parent_commit = hash '2'}
let forward = Phase.{stay with irmin_epoch = 1; chain_epoch = 1;
  irmin_root = Some entry.post_state_root; irmin_commit = Some (hash '4');
  irmin_parent = Some (entry.parent_commit, entry.pre_state_root);
  wal = [entry]; journal = [prepare_record]; last = Some last;
  epoch_hash = Some next_hash; epoch_root = Some next_root; epoch_offset = 200}

let run () =
  List.iter (fun plan ->
    expect "resume changed the proved plan" (Phase.authorize Phase.Resume plan = Ok plan))
    [Phase.Empty []; Phase.Stay (head, []); Phase.Trim (head, []); Phase.Cut (head, entry, []); Phase.Publish head];
  expect "rollback accepted an empty store" (Result.is_error (Phase.authorize Phase.Rollback (Phase.Empty [])));
  expect "rollback accepted forward publication"
    (Result.is_error (Phase.authorize Phase.Rollback (Phase.Publish head)));
  List.iter (fun plan ->
    expect "rollback refused a completed or pending cut" (Phase.authorize Phase.Rollback plan = Ok plan))
    [Phase.Stay (head, []); Phase.Cut (head, entry, [])];
  expect "clean phase differs" (Phase.decide stay = Ok (Phase.Stay (head, [])));
  expect "txlog trim phase differs"
    (Phase.decide {stay with tx_position = 0, 110} = Ok (Phase.Trim (head, [])));
  expect "prepared-only retirement missing"
    (Phase.decide {stay with journal = [prepare_record]}
      = Ok (Phase.Stay (head, ["second"])));
  expect "cut retirement missing"
    (Phase.decide {stay with journal = [prepare_record]; wal = [entry]}
      = Ok (Phase.Cut (head, entry, ["second"])));
  let empty = Phase.{stay with head = None; irmin_epoch = -1; chain_epoch = -1;
    irmin_root = None; irmin_commit = None; tx_position = 0, Core.Txlog.header_size;
    epoch_offset = Epoch.header_size} in
  expect "empty phase differs" (Phase.decide empty = Ok (Phase.Empty []));
  List.iter (fun chain_epoch ->
    expect "cut phase differs" (Phase.decide {stay with chain_epoch; wal = [entry]}
      = Ok (Phase.Cut (head, entry, [])))) [0; 1];
  (match Phase.decide forward with
  | Ok (Phase.Publish actual) ->
    expect "publish root differs" (actual.state_root = state_root);
    expect "publish high-water differs" (actual.txid_hi = 0L);
    expect "publish identity differs" (actual.commit_id = "second" && actual.irmin_commit = Some (hash '4'))
  | _ -> failwith "valid forward refused");
  let mutations = [
    "root", {stay with irmin_root = Some (hash '0')};
    "commit", {stay with irmin_commit = Some (hash '0')};
    "journal_ahead", {stay with chain_epoch = 1};
    "tx_short", {stay with tx_position = 0, 90};
    "tx_identity", {stay with tx_position = 0, 110; head = Some {head with irmin_commit = None}};
    "tx_marker", {stay with tx_position = 0, 110;
      marker = Some Core.Epoch_commit_marker.{epoch_id = 1; phase = "wal_written"; ts = 0.}};
    "epoch_suffix", {stay with epoch_offset = 110};
    "irmin_behind", {stay with irmin_epoch = -1};
    "head_missing", {stay with head = None};
    "wal_missing", {forward with wal = []};
    "wal_multiple", {forward with wal = [entry; entry]};
    "wal_count", {forward with wal = [{entry with tx_count = -1}]};
    "wal_start", {forward with wal = [{entry with start_txid = 0L}]};
    "wal_epoch", {forward with wal = [{entry with epoch_id = 2}]};
    "wal_before", {forward with wal = [{entry with irmin_last_epoch_before = -1}]};
    "wal_pre", {forward with wal = [{entry with pre_state_root = hash '0'}]};
    "wal_post", {forward with wal = [{entry with post_state_root = hash '0'}]};
    "wal_parent", {forward with wal = [{entry with parent_commit = hash '0'}]};
    "parent_missing", {forward with irmin_parent = None};
    "parent_other", {forward with irmin_parent = Some (hash '0', hash '1')};
    "parent_root", {forward with irmin_parent = Some (hash '2', hash '0')};
    "journal_behind", {forward with chain_epoch = 0};
    "irmin_ahead", {forward with irmin_epoch = 2};
    "header_missing", {forward with last = None};
    "header_root", {forward with last = Some {last with state_root = hash '0'}};
    "header_previous", {forward with last = Some {last with prev_state_root = hash '0'}};
    "prepare_missing", {forward with journal = []};
    "prepare_multiple", {forward with journal = [prepare_record; prepare_record]};
    "prepare_aborted", {forward with journal = [prepare_record; Journal.Abort {
      commit_id = "second"; reason = "test"; ts = 0.}]};
    "cut_published", {stay with wal = [entry]; journal = [prepare_record; Journal.Commit {
      commit_id = "second"; generation = 1; ts = 0.}]};
    "stay_published", {stay with journal = [prepare_record; Journal.Commit {
      commit_id = "second"; generation = 1; ts = 0.}]};
    "index_hash_missing", {forward with epoch_hash = None};
    "index_root_missing", {forward with epoch_root = None};
    "tx_position", {forward with tx_position = 0, 0};
    "epoch_position", {forward with epoch_offset = 0};
  ] in
  List.iter (fun (label, input) -> refused label input) mutations;
  List.iter (fun phase ->
    refused "future marker" {stay with marker = Some {
      Core.Epoch_commit_marker.epoch_id = 99; phase; ts = 0.}})
    ["stage_batch_begin"; "wal_written"; "begin"; "chaindata_begin";
     "chaindata_committed"; "irmin_begin"; "irmin_committed"];
  Printf.printf "event = passed scope = recovery_phase negatives = %d\n%!" (List.length mutations + 7)

let () = run ()