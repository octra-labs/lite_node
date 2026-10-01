(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Index = Octra_core.Chaindata_index

let expect message ok = if not ok then failwith message

let rows map =
  Lmdb.Cursor.go Lmdb.Ro map (fun cursor ->
    let rec loop acc pair = match pair with
      | None -> List.rev acc
      | Some row -> loop (row :: acc)
          (try Some (Lmdb.Cursor.next cursor) with Not_found -> None) in
    loop [] (try Some (Lmdb.Cursor.first cursor) with Not_found -> None))

let snapshot index =
  Marshal.to_string
    (rows index.Index.tx_loc, rows index.txid_loc, rows index.epoch_meta,
     rows index.addr_tx, rows index.addr_recent, rows index.meta) []

let prepare dir =
  let index = Index.open_index dir in
  Index.begin_write index;
  List.iter (fun epoch ->
    Index.buffer_tx index ~hash:(String.make 64 (if epoch = 0 then 'a' else 'b'))
      ~seg_id:0 ~offset:(16 + 100 * epoch) ~len:96 ~epoch_id:epoch
      ~txid:(Int64.of_int epoch) ~from_addr:"octA" ~to_addr:"octB" ~addrs:[];
    Index.buffer_epoch index epoch (string_of_int epoch)) [0; 1];
  Index.buffer_meta index "next_txid" "2";
  Index.buffer_meta index "repaired_upto_epoch" "1";
  Index.commit_write index;
  index

let invalid_index dir =
  let index = prepare dir in
  Fun.protect ~finally:(fun () -> Index.close index) (fun () ->
    ignore (Lmdb.Txn.go Lmdb.Rw index.env (fun txn ->
      Lmdb.Map.set index.tx_loc ~txn (String.make 64 'c') "x"));
    let before = snapshot index in
    let refused = match Index.cleanup_after_epoch index
      ~max_epoch:0 ~start_txid_inflight:1L ~tx_count_inflight:1 with
      | _ -> false
      | exception Index.Index_commit_failed _ -> true in
    expect "index cut accepted malformed location" refused;
    expect "index cut changed data after refusal" (snapshot index = before))

let run root =
  invalid_index (Filename.concat root "invalid");
  Printf.printf "case = invalid_index status = pass\n%!";
  let path = Filename.concat root "metadata" in
  let index = prepare path in
  let before = snapshot index in
  let refused = match Index.cleanup_after_epoch index ~metadata:["", "bad"]
    ~max_epoch:0 ~start_txid_inflight:1L ~tx_count_inflight:1 with
    | _ -> false
    | exception Index.Index_commit_failed _ -> true in
  expect "index cut accepted invalid metadata key" refused;
  expect "index cut published partial transaction" (snapshot index = before);
  Index.close index;
  let index = Index.open_index path in
  Fun.protect ~finally:(fun () -> Index.close index) (fun () ->
    expect "index cut persisted partial transaction" (snapshot index = before);
    let counts = Index.cleanup_after_epoch index
      ~metadata:["next_txid", "1"; "repaired_upto_epoch", "0"]
      ~max_epoch:0 ~start_txid_inflight:1L ~tx_count_inflight:1 in
    expect "index cut removed wrong rows" (counts = (1, 1, 2, 1));
    expect "index cut did not publish metadata"
      (Index.get_meta index "next_txid" = Some "1"
       && Index.get_meta index "repaired_upto_epoch" = Some "0");
    expect "index cut removed committed transaction"
      (Index.txid_loc_present index 0L && not (Index.txid_loc_present index 1L)));
  Printf.printf "case = index_atomic status = pass\n%!"

let () = Test_workspace.with_dir "index_cut" run