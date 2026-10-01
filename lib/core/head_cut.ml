(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Index = Chaindata_index
module Head = Head_manifest

exception Invalid_cut of string

let require label condition = if not condition then raise (Invalid_cut label)

let verify ~head ~index ~txlog ~epochlog =
  try
    let result = Lmdb.Txn.go Lmdb.Ro index.Index.env (fun txn ->
      let read map key =
        try Some (Lmdb.Map.get map ~txn key) with Not_found -> None in
      let hashes = Lmdb.Cursor.go Lmdb.Ro index.tx_loc ~txn (fun cursor ->
        let rec count total pair =
          match pair with
          | None -> total
          | Some (_, bytes) ->
            require "transaction hash location length differs" (String.length bytes = 20);
            let segment, offset, length, epoch = Index.decode_tx_loc bytes in
            require "transaction hash location is invalid"
              (segment >= 0 && offset >= Txlog.header_size && length >= 8
               && length <= Txlog.max_record_len && epoch >= 0
               && Index.encode_tx_loc ~seg_id:segment ~offset ~len:length ~epoch_id:epoch = bytes);
            let total = if epoch <= head.Head.epoch_id then total + 1 else total in
            count total (try Some (Lmdb.Cursor.next cursor) with Not_found -> None) in
        count 0 (try Some (Lmdb.Cursor.first cursor) with Not_found -> None)) in
      let references = Lmdb.Cursor.go Lmdb.Ro index.txid_loc ~txn (fun cursor ->
        let rec count total pair =
          match pair with
          | None -> total
          | Some (txid, bytes) ->
            require "transaction reference is invalid" (txid >= 0L && String.length bytes = 16);
            let segment, offset, length = Index.decode_txid_loc bytes in
            require "transaction reference location is invalid"
              (segment >= 0 && offset >= Txlog.header_size && length >= 8
               && length <= Txlog.max_record_len
               && Index.encode_txid_loc ~seg_id:segment ~offset ~len:length = bytes);
            let total = if txid <= head.Head.txid_hi then total + 1 else total in
            count total (try Some (Lmdb.Cursor.next cursor) with Not_found -> None) in
        count 0 (try Some (Lmdb.Cursor.first cursor) with Not_found -> None)) in
      require "retained transaction index counts differ" (hashes = references);
      let io = Index.{read_hash = read index.tx_loc; read_txid = read index.txid_loc;
        read_meta = read index.meta;
        write_hash = (fun _ _ -> raise (Invalid_cut "committed transaction hash index is missing"));
        hash_count = (fun () -> hashes)} in
      Committed_history.verify ~end_at_head:true io ~head:(Some head)
        ~max_epoch:head.epoch_id ~txlog ~epochlog) in
    match result with
    | Some (Ok _) -> Ok ()
    | Some (Error reason) -> Error reason
    | None -> Error "index read transaction returned no result"
  with Invalid_cut reason -> Error reason