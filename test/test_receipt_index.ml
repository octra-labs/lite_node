(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module T = Octra_core.Transaction
module R = Octra_core.Preverify_receipt
module W = Octra_core.Preverify_worker
module P = Octra_core.Preverify_commit
module L = Octra_core.Resource_lanes
module V = Octra_node_runtime.Consensus_bundle_validation
module C = Octra_consensus.C_types
module D = Octra_consensus.C_driver

let expect label value = if not value then failwith label
let get = function Ok value -> value | Error reason -> failwith reason

let tx ?(size = 32) nonce = T.{
  from = "oct_sender";
  to_ = "oct_receiver";
  amount = Z.zero;
  nonce;
  ou = Z.of_int 10_000;
  timestamp = 1.;
  signature = "signature";
  public_key = Some "public_key";
  message = Some (String.make size 'x');
  op_type = RecryptOp;
  encrypted_data = None;
}

let receipt tx = get (R.for_tx ~input_hash:(W.input_hash tx)
  ~output_hash:(W.output_hash tx "ok") ~ok:true ~reason:"" tx)

let duplicate key items =
  let rec loop seen = function
    | [] -> None
    | item :: rest ->
      let value = key item in
      if List.mem value seen then Some value else loop (value :: seen) rest in
  loop [] items

let prior gate txs =
  let ( let* ) = Result.bind in
  let* () = match duplicate T.hash txs with
    | None -> Ok () | Some key -> Error ("duplicate_tx:" ^ key) in
  let* () = match duplicate P.receipt_hash gate.P.receipts with
    | None -> Ok () | Some key -> Error ("duplicate_receipt:" ^ key) in
  let rec receipts = function
    | [] -> Ok ()
    | item :: rest ->
      let* () = R.validate item |> Result.map_error (fun reason -> "invalid_receipt:" ^ reason) in
      let* () = match List.find_opt (fun tx -> T.hash tx = item.R.tx_hash) txs with
        | None -> Error ("orphan_receipt:" ^ item.tx_hash)
        | Some tx when P.heavy tx -> Ok ()
        | Some _ -> Error ("receipt_for_light_tx:" ^ item.tx_hash) in
      receipts rest in
  let rec transactions used = function
    | [] -> Ok ()
    | tx :: rest when not (P.heavy tx) -> transactions used rest
    | tx :: rest ->
      let* item = P.find_receipt (T.hash tx) gate.receipts in
      let* () = P.check_receipt tx item in
      let* used = P.check_budget gate used tx in
      transactions used rest in
  let* () = receipts gate.receipts in
  transactions (List.map (fun lane -> lane, L.zero) L.all) txs

let cases () =
  let a, b = tx 1, tx 2 in
  let ra, rb = receipt a, receipt b in
  let light = {a with T.op_type = Standard} in
  let invalid = {ra with R.engine = "wrong"} in
  let rows = [
    [], [];
    [a; b], [ra; rb];
    [b; a], [ra; rb];
    [a; a], [invalid; invalid];
    [a; b; a; b], [ra];
    [a], [ra; ra];
    [a], [invalid; invalid];
    [a], [];
    [], [invalid];
    [a], [rb];
    [light], [{ra with R.tx_hash = T.hash light}];
    [a], [invalid];
    [a], [{ra with R.ok = false; reason = "denied"}];
    [a], [{ra with R.input_hash = String.make 64 '0'}];
    [a], [{ra with R.output_hash = String.make 64 '0'}];
    [a], [{ra with R.cost = {ra.cost with txs = 2}}];
    [a], [ra; rb];
    [a], [invalid; rb];
    [a], [rb; invalid];
  ] in
  let budgets = [L.default_budget; (fun lane -> { (L.default_budget lane) with max_txs = 0 })] in
  List.iter (fun budget -> List.iter (fun (txs, receipts) ->
    let gate = P.create ~budgets:budget receipts in
    expect "receipt result changed" (P.check gate txs = prior gate txs)) rows) budgets;
  expect "honest receipt refused" (P.check (P.create [ra; rb]) [a; b] = Ok ());
  print_endline "status = pass test = receipt_results"

let response txs receipts =
  let hashes = List.map T.hash txs in
  let receipts_json = List.map R.canonical receipts in
  let raw hex = Digestif.SHA256.(to_raw_string (of_hex hex)) in
  let header = C.{
    proto_version = proto_version_current;
    chain_id = "octra-test";
    epoch_id = 10L;
    prev_state_root = String.make 32 'p';
    tx_list_hash = Octra_net.Hash_domain.hash "octra:tx_list:v1" (String.concat "" hashes);
    receipt_root = Octra_consensus.C_hash.receipt_root receipts_json;
    proposed_state_root = String.make 32 's';
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = "oct_creator";
    txid_hi = 0L;
    ts = 1.;
  } in
  let record = D.{
    responder_addr = "oct_peer";
    tx_hashes = List.map raw hashes;
    txs_json = List.map (fun tx -> Yojson.Safe.to_string (T.to_yojson tx)) txs;
    receipts_json;
  } in
  header, hashes, record

let bundle_work () =
  let inputs = List.init 160 (fun n -> tx ~size:2048 (n + 1)) in
  let header, hashes, record = response inputs (List.map receipt inputs) in
  let bytes = List.fold_left (fun size s -> size + String.length s) 0
    (record.D.txs_json @ record.receipts_json) in
  expect "test bundle exceeds limit" (bytes < 8 * 1024 * 1024);
  let run label check =
    Gc.full_major ();
    let before = Gc.allocated_bytes () in
    let result = check () in
    let allocated = Gc.allocated_bytes () -. before in
    expect "bundle refusal changed" (result = Error "bundle preverify: lane_budget:fhe:tx_count");
    Printf.printf "test = %s bytes = %d allocated = %.0f\n%!" label bytes allocated;
    expect "receipt lookup repeats transaction serialization" (allocated < 64. *. 1024. *. 1024.) in
  run "proposal_work" (fun () -> V.proposal ~header ~expected_hashes:hashes record);
  run "finalized_work" (fun () -> V.finalized ~header record);
  let good = List.init 4 (fun n -> tx (n + 1)) in
  let header, hashes, record = response good (List.map receipt good) in
  expect "honest proposal refused" (Result.is_ok (V.proposal ~header ~expected_hashes:hashes record));
  expect "honest finalized refused" (Result.is_ok (V.finalized ~header record));
  print_endline "status = pass test = bundle_work"

let ledger_cases () =
  let module S = Octra_core.Store_irmin in
  let module Ledger = Octra_core.Ledger in
  let run = Lwt_main.run in
  Test_workspace.with_dir "receipt-index" (fun dir ->
    let store = run (S.open_store (Filename.concat dir "state")) in
    Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
      let ledger = Ledger.create store in
      let root = run (Ledger.hash ledger) in
      let a, b = tx 1, tx 2 in
      let wrong tx =
        let state = R.{pre_state_hash = String.make 64 '0';
          source_cipher_hash = String.make 64 '1';
          pvac_key_hash = String.make 64 '2'; transition_hash = None} in
        expect "test state matches root" (W.state_hash root <> state.pre_state_hash);
        get (R.for_tx_bound ~state ~ok:true ~reason:"" tx
          ~input_hash:(W.bound_input_hash tx state)
          ~output_hash:(W.bound_output_hash tx state "ok")) in
      let ra, rb = wrong a, wrong b in
      let check label gate txs expected =
        expect label (run (P.check_bound ledger gate txs) = expected) in
      check "honest state path refused" (P.create [receipt b; receipt a]) [a; b] (Ok ());
      let call = {a with T.op_type = CircleCall} in
      let circle = R.{snapshot_hash = W.state_hash root;
        circle_id = "oct5MrNfjiXFNRDLwsodn8Zm9hDKNGAYt3eQDCQ52bSpCHq";
        code_hash = String.make 64 '1'; stable_root = String.make 64 '2';
        public_reads_hash = String.make 64 '3'; context_hash = String.make 64 '4';
        transcript = []} in
      let linked = get (R.make_circle ~tx_hash:(T.hash call) ~circle ~ok:true ~reason:""
        ~input_hash:(W.circle_input_hash call circle)
        ~output_hash:(W.circle_output_hash call circle "ok") ~cost:(L.cost call)) in
      check "matching state refused" (P.create [linked]) [call] (Ok ());
      check "state order changed" (P.create [rb; ra]) [a; b]
        (Error ("receipt_state_mismatch:" ^ T.hash a));
      check "state reverse order changed" (P.create [ra; rb]) [b; a]
        (Error ("receipt_state_mismatch:" ^ T.hash b));
      check "state checked before structure" (P.create [ra]) [a; b]
        (Error ("missing_receipt:" ^ T.hash b));
      let budgets lane = { (L.default_budget lane) with max_txs = 0 } in
      check "state checked before budget" (P.create ~budgets [ra]) [a]
        (Error "lane_budget:fhe:tx_count");
      expect "receipt check changed ledger" (run (Ledger.hash ledger) = root)));
  print_endline "status = pass test = receipt_state"

let () = cases (); ledger_cases (); bundle_work ()