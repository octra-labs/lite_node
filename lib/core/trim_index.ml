(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Index = Chaindata_index
module Head = Head_manifest

exception Refused of string

let require reason valid = if not valid then raise (Refused reason)

let verify chain (head : Head.t) =
  let index = chain.Store_chaindata.index in
  require "journal trim has an active index batch" (index.batch = None);
  require "journal trim transaction sequence overflows" (head.txid_hi < Int64.max_int);
  match Lmdb.Txn.go Lmdb.Ro index.env (fun txn ->
    let get key = try Some (Lmdb.Map.get index.meta ~txn key) with Not_found -> None in
    let identity txid =
      let bytes = try Lmdb.Map.get index.txid_loc ~txn txid
        with Not_found -> raise (Refused "journal trim address transaction is missing") in
      require "journal trim address location is malformed" (String.length bytes = 16);
      let seg_id, offset, len = Index.decode_txid_loc bytes in
      let epoch, payload = Txlog.read_record chain.txlog ~seg_id ~offset ~len in
      let _, json = Store_chaindata.split_payload payload in
      let sender, recipient, extra =
        try Store_chaindata.parse_tx_identity json with
        | Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ ->
          raise (Refused (Printf.sprintf "journal trim address payload is invalid: txid = %Ld" txid)) in
      epoch, Store_chaindata.dedupe_addrs (sender :: recipient :: extra) in
    require "journal trim transaction sequence differs"
      (get "next_txid" = Some (Int64.to_string (Int64.succ head.txid_hi)));
    Aux_index.each index.txid_loc txn (fun txid _ ->
      require "journal trim has future transaction references" (txid >= 0L && txid <= head.txid_hi);
      let _, addresses = identity txid in
      Lmdb.Cursor.go Lmdb.Ro index.addr_tx ~txn (fun cursor ->
        List.iter (fun address ->
          require ("journal trim address reference is missing: " ^ address)
            (try Lmdb.Cursor.seek_dup cursor address txid; true with Not_found -> false)) addresses));
    Aux_index.each index.tx_loc txn (fun key bytes ->
      require ("journal trim hash location is malformed: " ^ key) (String.length bytes = 20);
      let _, _, _, epoch = Index.decode_tx_loc bytes in
      require ("journal trim has future hash reference: " ^ key) (epoch <= head.epoch_id));
    Aux_index.each index.addr_tx txn (fun address txid ->
      require ("journal trim has future address reference: " ^ address)
        (txid >= 0L && txid <= head.txid_hi);
      require ("journal trim address identity differs: " ^ address)
        (List.mem address (snd (identity txid))));
    Aux_index.each index.addr_recent txn (fun address bytes ->
      require ("journal trim has invalid recent reference: " ^ address)
        (match Index.decode_addr_recent bytes with
         | None -> false
         | Some rows -> List.for_all (fun (epoch, txid) ->
             epoch >= 0 && epoch <= head.epoch_id && txid >= 0L && txid <= head.txid_hi
             && let actual, addresses = identity txid in
                actual = epoch && List.mem address addresses) rows));
    Aux_index.each index.epoch_meta txn (fun epoch _ ->
      require "journal trim has future epoch metadata" (epoch >= 0l && Int32.to_int epoch <= head.epoch_id));
    let check_epoch key raw =
      require ("journal trim has invalid metadata: " ^ key)
        (match int_of_string_opt raw with Some epoch -> epoch >= 0 && epoch <= head.epoch_id | None -> false) in
    Aux_index.each index.meta txn (fun key value ->
      if key = "eic_latest_epoch" || key = "repaired_upto_epoch" then check_epoch key value;
      List.iter (fun prefix ->
        if String.starts_with ~prefix key then
          check_epoch key (String.sub key (String.length prefix) (String.length key - String.length prefix)))
        ["eic_epoch_hash:"; "eic_epoch_root:"]);
    require "journal trim latest index root differs"
      (match get "eic_latest_root", get "eic_latest_epoch", head.epoch_index_root with
       | None, None, None -> true
       | Some root, Some epoch, Some expected -> root = expected && epoch = string_of_int head.epoch_id
       | _ -> false);
    let auxiliary = Index.auxiliary index in
    let anchor = Some (Store_chaindata.head_anchor head) in
    require "journal trim requires auxiliary restoration"
      (match Aux_index.journal auxiliary txn with
       | None -> true
       | Some delta -> Aux_delta.decide ~head:anchor delta = Ok Aux_delta.Retire);
    Aux_index.verify auxiliary txn ~head:anchor) with
  | Some () -> ()
  | None -> raise (Refused "journal trim index transaction returned no result")