(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module R = Octra_core.Claim_archive
module S = Octra_core.Store_irmin
module C = Octra_core.Store_chaindata
module I = Octra_core.Chaindata_index
module E = Octra_core.Epoch_index_commitment
module T = Octra_core.Transaction

let unwrap = function Ok value -> value | Error reason -> failwith reason
let check name valid = if not valid then failwith name

type damage = Intact | Oversize | Lookup | Address | Missing

let run damage dir =
  let store = Lwt_main.run (S.open_store (Filename.concat dir "irmin_store")) in
  let path = Filename.concat dir "chaindata" in
  let archive = C.open_chaindata path in
  let before, after = Fun.protect ~finally:(fun () -> C.close archive) (fun () ->
    let pin epoch index_root next_txid =
      Lwt_main.run (S.set_meta store "last_epoch" (string_of_int epoch));
      Lwt_main.run (S.tag_epoch store epoch);
      let snapshot = Lwt_main.run (S.capture_read_snapshot store) |> unwrap in
      R.{epoch; index_root; next_txid; state_root = E.folded_state_root
        ~ledger_state_root:snapshot.state_root ~epoch_index_root:index_root}
    in
    let before = pin 0 E.genesis_root 0L in
    let tx = T.{from = "sender"; to_ = "receiver"; amount = Z.one;
      nonce = 1; ou = Z.one; timestamp = 1.0; signature = "";
      public_key = None; message = None; encrypted_data = None; op_type = Standard} in
    let hash = T.hash tx in
    C.begin_batch archive;
    C.save_tx archive ~hash ~epoch_id:1 ~from_addr:tx.from ~to_addr:tx.to_
      ~tx_json:(Yojson.Safe.to_string (T.to_yojson tx))
      ~op_type:"standard" ~encrypted_data:"" ~message:"";
    let epoch_hash, root = E.next_root ~prev:E.genesis_root ~epoch_id:1
      [E.item ~txid:0L ~hash] in
    let after = pin 1 root 1L in
    C.set_epoch archive {Octra_core.Epochlog.empty_epoch_header with
      id = 1; state_root = after.state_root; prev_state_root = before.state_root;
      start_txid = 0L; tx_count = 1};
    C.set_epoch_index_commitment archive ~epoch_id:1 ~epoch_hash ~root;
    C.commit_batch archive;
    let seg_id, offset, len = I.get_txid_loc_raw archive.index 0L |> Option.get in
    begin match damage with
    | Intact -> ()
    | Oversize -> I.repair_tx_full archive.index ~hash ~seg_id ~offset
        ~len:(C.max_txlog_record_len + 1) ~epoch_id:1 ~txid:0L
        ~from_addr:tx.from ~to_addr:tx.to_
    | Lookup -> I.repair_class_b archive.index ~hash ~seg_id ~offset:(offset + 1)
        ~len ~epoch_id:1 ~txid:0L ~from_addr:tx.from ~to_addr:tx.to_
    | Address -> I.remove_addr_tx_direct archive.index ~addr:tx.from ~txid:0L
    | Missing -> I.remove_txid_loc_direct archive.index 0L
    end;
    before, after)
  in
  let archive = C.open_chaindata ~readonly:true path in
  Fun.protect ~finally:(fun () -> C.close archive; Lwt_main.run (S.close store)) (fun () ->
    let allocated = Gc.allocated_bytes () in
    let result = R.read store archive ~before ~after ~max_txs:1 in
    let used = Gc.allocated_bytes () -. allocated in
    check "oversized record allocated before refusal" (used < 64_000_000.0);
    match damage, result with
    | Intact, Ok _ -> ()
    | Oversize, Error "history transaction location invalid" -> ()
    | Lookup, Error "history transaction lookup differs" -> ()
    | Address, Error "history address index incomplete" -> ()
    | Missing, Error "history transaction index missing" -> ()
    | _ -> failwith "history read result differs")

let () =
  List.iter (fun damage -> Test_workspace.with_dir "claim_limits" (run damage))
    [Intact; Oversize; Lookup; Address; Missing];
  Printf.printf "status = pass test = claim_limits\n%!"