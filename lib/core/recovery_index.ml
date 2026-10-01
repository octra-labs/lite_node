(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Index = Chaindata_index
module Head = Head_manifest
module Changes = Map.Make (String)

exception Refused of string

let refuse reason = raise (Refused reason)
let require reason valid = if not valid then refuse reason
let unwrap = function Ok value -> value | Error reason -> refuse reason

let fold mode txn map initial action =
  Lmdb.Cursor.go mode map ~txn (fun cursor ->
    let rec loop state pair =
      match pair with
      | None -> state
      | Some (key, value) ->
        loop (action state key value)
          (try Some (Lmdb.Cursor.next cursor) with Not_found -> None) in
    loop initial (try Some (Lmdb.Cursor.first cursor) with Not_found -> None))

let counts mode index txn head =
  let epoch, txid_hi = match head with
    | None -> -1, -1L | Some head -> head.Head.epoch_id, head.txid_hi in
  let hashes = fold mode txn index.Index.tx_loc 0 (fun total key bytes ->
    require ("transaction hash location length differs: " ^ key) (String.length bytes = 20);
    let segment, offset, length, entry_epoch = Index.decode_tx_loc bytes in
    require ("transaction hash location is invalid: " ^ key)
      (segment >= 0 && offset >= Txlog.header_size && length >= 8
       && length <= Txlog.max_record_len && entry_epoch >= 0
       && Index.encode_tx_loc ~seg_id:segment ~offset ~len:length ~epoch_id:entry_epoch = bytes);
    if entry_epoch <= epoch then total + 1 else total) in
  let references = fold mode txn index.txid_loc 0 (fun total txid bytes ->
    require "transaction reference is invalid" (txid >= 0L && String.length bytes = 16);
    let segment, offset, length = Index.decode_txid_loc bytes in
    require "transaction reference location is invalid"
      (segment >= 0 && offset >= Txlog.header_size && length >= 8
       && length <= Txlog.max_record_len
       && Index.encode_txid_loc ~seg_id:segment ~offset ~len:length = bytes);
    if txid <= txid_hi then total + 1 else total) in
  hashes, references

let scan mode index txn head ~txlog ~epochlog =
  let hashes, references = counts mode index txn head in
  let changes = ref Changes.empty in
  let read map key =
    try Some (Lmdb.Map.get map ~txn key) with Not_found -> None in
  let io = Index.{
    read_hash = (fun key -> match Changes.find_opt key !changes with
      | Some value -> Some value | None -> read index.tx_loc key);
    read_txid = read index.txid_loc;
    read_meta = read index.meta;
    write_hash = (fun key value -> changes := Changes.add key value !changes);
    hash_count = (fun () -> hashes + Changes.cardinal !changes);
  } in
  let epoch = match head with None -> -1 | Some head -> head.Head.epoch_id in
  let proof = unwrap (Committed_history.verify ~end_at_head:true io ~head ~max_epoch:epoch
    ~txlog ~epochlog) in
  require "retained transaction index counts differ"
    (hashes + Changes.cardinal !changes = references);
  proof, !changes

let inspect chain head =
  let index = chain.Store_chaindata.index in
  match Lmdb.Txn.go Lmdb.Ro index.env (fun txn ->
    let proof = fst (scan Lmdb.Ro index txn head ~txlog:chain.txlog ~epochlog:chain.epochlog) in
    let finish = Option.bind head (fun head ->
      match Lmdb.Map.get index.txid_loc ~txn head.Head.txid_hi with
      | bytes ->
        require "last transaction location length differs" (String.length bytes = 16);
        let segment, offset, length = Index.decode_txid_loc bytes in
        Some (segment, offset + 4 + length)
      | exception Not_found -> None) in
    proof, finish) with
  | Some result -> result
  | None -> refuse "index read transaction returned no result"

let commit chain head =
  let index = chain.Store_chaindata.index in
  require "recovery index is read-only" (not index.readonly);
  require "recovery index has an active batch" (index.batch = None);
  match Lmdb.Txn.go Lmdb.Rw index.env (fun txn ->
    let (checked, repaired, epochs, legacy), changes =
      scan Lmdb.Rw index txn head ~txlog:chain.txlog ~epochlog:chain.epochlog in
    Changes.iter (fun key value -> Lmdb.Map.set index.tx_loc ~txn key value) changes;
    List.iter (fun header ->
      Lmdb.Map.set index.epoch_meta ~txn (Int32.of_int header.Epochlog.id)
        (Epochlog.epoch_to_json header)) epochs;
    let epoch = match head with None -> -1 | Some head -> head.Head.epoch_id in
    List.iter (fun (key, value) -> Lmdb.Map.set index.meta ~txn key value)
      ["index_schema_version", "v2_ascii64_int32be";
       "repaired_upto_epoch", string_of_int epoch;
       "startup_storage_verifier", "verified_recovery_phase"];
    checked, repaired, legacy) with
  | Some proof -> proof
  | None -> refuse "recovery index transaction aborted"