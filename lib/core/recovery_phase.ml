(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Head = Head_manifest
module Epoch = Epochlog
module Marker = Epoch_commit_marker
module Journal = Commit_journal

type snapshot = {
  head : Head.t option;
  irmin_epoch : int;
  irmin_root : string option;
  irmin_commit : string option;
  irmin_parent : (string * string) option;
  chain_epoch : int;
  marker : Marker.marker option;
  wal : Wal.entry list;
  journal : Journal.record list;
  last : Epoch.epoch_header option;
  epoch_hash : string option;
  epoch_root : string option;
  tx_position : int * int;
  epoch_offset : int;
}

type plan =
  | Empty of string list
  | Stay of Head.t * string list
  | Trim of Head.t * string list
  | Cut of Head.t * Wal.entry * string list
  | Publish of Head.t

type request = Resume | Rollback

let authorize request plan =
  match request, plan with
  | Rollback, Publish _ -> Error "rollback refused: Irmin already committed the successor"
  | Rollback, Empty _ -> Error "rollback requires committed HEAD"
  | Resume, _ | Rollback, (Stay _ | Trim _ | Cut _) -> Ok plan

let ( let* ) = Result.bind
let require message condition = if condition then Ok () else Error message

let check_marker input head_epoch =
  match input.marker with
  | None -> Ok ()
  | Some marker ->
    let* () = require "commit marker epoch differs from recovery phase"
      (marker.epoch_id >= head_epoch
       && (marker.epoch_id = head_epoch
           || (head_epoch < max_int && marker.epoch_id = head_epoch + 1))) in
    require "Irmin commit marker epoch differs from current store"
      (marker.phase <> "irmin_committed" || marker.epoch_id = input.irmin_epoch)

let check_binding input head =
  let* () = require "Irmin epoch differs from HEAD" (input.irmin_epoch = head.Head.epoch_id) in
  let* () = require "Irmin root differs from HEAD"
    (input.irmin_root = Some (Head.ledger_state_root head)) in
  require "Irmin commit differs from HEAD"
    (match head.irmin_commit with None -> true | Some hash -> input.irmin_commit = Some hash)

let check_next head (entry : Wal.entry) =
  let* () = require "WAL epoch differs from HEAD successor"
    (head.Head.epoch_id < max_int && entry.Wal.epoch_id = head.epoch_id + 1
     && entry.irmin_last_epoch_before = head.epoch_id) in
  let* () = require "WAL transaction range differs from HEAD"
    (head.txid_hi >= -1L && head.txid_hi < Int64.max_int
     && entry.start_txid = Int64.succ head.txid_hi && entry.tx_count >= 0
     && Int64.of_int entry.tx_count <= Int64.sub Int64.max_int entry.start_txid) in
  let* () = require "WAL predecessor root differs from HEAD"
    (entry.pre_state_root = Head.ledger_state_root head) in
  require "WAL parent commit differs from HEAD"
    (match head.irmin_commit with None -> true | Some hash -> entry.parent_commit = hash)

let prepare input head epoch txid_hi state_root =
  let* attempts = Commit_attempt.read ~epoch ~generation:head.Head.generation input.journal in
  let* waiting = Commit_attempt.active attempts in
  match waiting with
  | Some entry ->
    let* () = require "commit prepare differs from recovered epoch"
      (entry.planned_txid_hi = txid_hi && entry.planned_state_root = state_root) in
    Ok entry.commit_id
  | None -> Error "forward recovery requires one matching commit prepare"

let retire input head =
  if head.Head.epoch_id = max_int then Ok []
  else
    let* attempts = Commit_attempt.read ~epoch:(head.epoch_id + 1)
      ~generation:head.generation input.journal in
    Commit_attempt.retire attempts

let forward input head (entry : Wal.entry) =
  let* () = check_next head entry in
  let* () = require "forward store epochs differ"
    (input.irmin_epoch = entry.epoch_id && input.chain_epoch = entry.epoch_id) in
  let* () = require "WAL post root differs from Irmin"
    (input.irmin_root = Some entry.post_state_root && input.irmin_commit <> None) in
  let* () = require "Irmin parent differs from WAL predecessor"
    (input.irmin_parent = Some (entry.parent_commit, entry.pre_state_root)) in
  let* header = match input.last with
    | Some header when header.Epoch.id = entry.epoch_id -> Ok header
    | _ -> Error "forward epoch header is missing" in
  let* () = require "WAL differs from epoch header"
    (header.start_txid = entry.start_txid && header.tx_count = entry.tx_count
     && header.prev_state_root = head.Head.state_root
     && header.parent_commit = entry.parent_commit) in
  let* state_root = match input.epoch_hash, input.epoch_root with
    | Some _, Some root -> Ok (Epoch_index_commitment.folded_state_root
        ~ledger_state_root:entry.post_state_root ~epoch_index_root:root)
    | None, None when head.epoch_index_root = None -> Ok entry.post_state_root
    | _ -> Error "forward epoch index commitment is incomplete" in
  let* () = require "forward epoch root differs from Irmin and index"
    (header.state_root = state_root) in
  let txid_hi = Int64.pred (Int64.add entry.start_txid (Int64.of_int entry.tx_count)) in
  let* commit_id = prepare input head entry.epoch_id txid_hi state_root in
  let segment, offset = input.tx_position in
  let* () = require "forward journal position is invalid"
    (segment >= 0 && offset >= Txlog.header_size && input.epoch_offset >= Epoch.header_size) in
  Ok (Publish Head.{schema_version; generation = entry.epoch_id; epoch_id = entry.epoch_id;
    state_root; ledger_state_root = Some entry.post_state_root;
    irmin_commit = input.irmin_commit; txid_hi; txlog_seg = Some segment;
    txlog_off = Some offset; epochlog_off = Some input.epoch_offset; commit_id;
    ts = header.finalized_at; quorum_cert_hash = None;
    epoch_index_hash = input.epoch_hash; epoch_index_root = input.epoch_root})

let decide input =
  match input.head with
  | None ->
    let* () = require "nonempty recovery requires HEAD"
      (input.irmin_epoch = -1 && input.chain_epoch = -1 && input.wal = []
       && input.marker = None && input.tx_position = (0, Txlog.header_size)
       && input.epoch_offset = Epoch.header_size) in
    let* attempts = Commit_attempt.initial input.journal in
    Ok (Empty attempts)
  | Some head ->
    let* () = check_marker input head.epoch_id in
    let* () = require "HEAD is ahead of Irmin" (head.epoch_id <= input.irmin_epoch) in
    let future = List.filter (fun (entry : Wal.entry) -> entry.epoch_id > head.epoch_id) input.wal in
    if input.irmin_epoch = head.epoch_id then begin
      let* () = check_binding input head in
      match future with
      | [] ->
        let* () = require "journal epoch differs from HEAD" (input.chain_epoch = head.epoch_id) in
        let* () = require "epoch journal suffix lacks WAL"
          (head.epochlog_off = Some input.epoch_offset) in
        let* attempts = retire input head in
        (match head.txlog_seg, head.txlog_off with
        | Some segment, Some offset when (segment, offset) = input.tx_position ->
          Ok (Stay (head, attempts))
        | Some segment, Some offset
          when segment >= 0 && offset >= Txlog.header_size
            && (segment, offset) < input.tx_position ->
          let* () = require "journal trim requires Irmin commit identity"
            (Option.is_some head.irmin_commit) in
          let* () = require "journal trim marker indicates later writes"
            (match input.marker with
             | None -> true
             | Some marker -> marker.phase = "stage_batch_begin") in
          let* () = require "journal trim has later commit attempts"
            (List.for_all (function
              | Journal.Prepare row -> head.epoch_id < max_int && row.epoch_id <= head.epoch_id + 1
              | Journal.Commit row -> row.generation <= head.generation
              | Journal.Abort _ -> true) input.journal) in
          Ok (Trim (head, attempts))
        | _ -> Error "journal position is missing or precedes HEAD")
      | [entry] ->
        let* () = check_next head entry in
        let* () = require "journal epoch differs from pending commit"
          (input.chain_epoch = head.epoch_id || input.chain_epoch = entry.epoch_id) in
        let* attempts = retire input head in
        Ok (Cut (head, entry, attempts))
      | _ -> Error "multiple commits extend HEAD"
    end else
      match future with
      | [entry] -> forward input head entry
      | _ -> Error "forward recovery requires one pending WAL"