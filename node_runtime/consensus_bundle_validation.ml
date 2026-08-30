(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Transaction = Octra_core.Transaction
module C_types = Octra_consensus.C_types
module C_driver = Octra_consensus.C_driver

type accepted = {
  tx_hashes : string list;
  txs : Transaction.t list;
  receipts_json : string list;
  rejections : Octra_core.Tx_outcome.rejection list;
}

let parse_txs txs_json =
  List.fold_left
    (fun acc tx_json ->
       match acc with
       | Error _ -> acc
       | Ok lst ->
         try
           let json = Yojson.Safe.from_string tx_json in
           match Transaction.of_yojson json with
           | Ok tx -> Ok (tx :: lst)
           | Error e -> Error ("of_yojson: " ^ e)
         with exn ->
           Error (Printexc.to_string exn))
    (Ok [])
    txs_json
  |> Result.map List.rev

let receipt_root_matches header receipts_json =
  Octra_consensus.C_hash.receipt_root receipts_json = header.C_types.receipt_root

let tx_list_hash_matches header tx_hashes =
  let tx_list_hash =
    Octra_net.Hash_domain.hash "octra:tx_list:v1" (String.concat "" tx_hashes)
  in
  tx_list_hash = header.C_types.tx_list_hash

let artifacts receipts_json txs =
  match Octra_core.Tx_outcome.split receipts_json with
  | Error error -> Error ("bundle outcome: " ^ error)
  | Ok artifacts ->
    begin
      match
        Octra_core.Preverify_commit.check_strings artifacts.preverify txs
      with
      | Error error -> Error ("bundle preverify: " ^ error)
      | Ok () ->
        match
          Octra_core.Tx_outcome.merge
            ~confirmed:txs
            ~rejections:artifacts.rejections
        with
        | Error error -> Error ("bundle outcome: " ^ error)
        | Ok _ -> Ok artifacts
    end

let response_hashes r =
  List.map Text.hash32_hex r.C_driver.tx_hashes

let finalized ~header (r : C_driver.bundle_response_record) =
  if List.length r.tx_hashes <> List.length r.txs_json then
    Error "bundle length mismatch"
  else
    match parse_txs r.txs_json with
    | Error _ as e -> e
    | Ok txs ->
      let tx_hashes = response_hashes r in
      let recomputed = List.map Transaction.hash txs in
      if recomputed <> tx_hashes then
        Error "bundle tx hash mismatch"
      else if not (tx_list_hash_matches header tx_hashes) then
        Error "bundle tx_list_hash mismatch"
      else if not (receipt_root_matches header r.receipts_json) then
        Error "bundle receipt_root mismatch"
      else
        match artifacts r.receipts_json txs with
        | Error _ as e -> e
        | Ok artifacts ->
          Ok {
            tx_hashes;
            txs;
            receipts_json = r.receipts_json;
            rejections = artifacts.rejections;
          }

let proposal ~header ~expected_hashes (r : C_driver.bundle_response_record) =
  if List.length r.tx_hashes <> List.length r.txs_json then
    Error "bundle length mismatch"
  else if List.length r.tx_hashes <> List.length expected_hashes then
    Error "bundle expected length mismatch"
  else if not (receipt_root_matches header r.receipts_json) then
    Error "bundle receipt_root mismatch"
  else
    match parse_txs r.txs_json with
    | Error _ as e -> e
    | Ok txs ->
      let recomputed = List.map Transaction.hash txs in
      if recomputed <> expected_hashes then
        Error "bundle tx hash mismatch"
      else
        match artifacts r.receipts_json txs with
        | Error _ as e -> e
        | Ok artifacts ->
          Ok {
            tx_hashes = expected_hashes;
            txs;
            receipts_json = r.receipts_json;
            rejections = artifacts.rejections;
          }