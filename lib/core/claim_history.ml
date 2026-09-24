(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module T = Transaction
module A = Tx_archive
module PT = Crypto.PrivateTransferV4
module SC = Crypto.StealthClaimV5
module SA = Crypto.StealthAddress
module V = Pvac_verify_worker
module P = Pvac_verify_protocol

type entry = {
  epoch : int;
  index : int64;
  hash : string;
  json : string;
}

type key = {
  blob : string;
  hash : string;
  math : bool;
}

type checked = {
  id : int;
  sender : string;
  receiver : string;
  send_hash : string;
  claim_hash : string;
  commitment : string;
}

let ( let* ) = Result.bind

let require valid reason = if valid then Ok () else Error reason

let fields = function
  | `Assoc values ->
    let names = List.map fst values in
    let* () = require
      (List.length names = List.length (List.sort_uniq String.compare names))
      "duplicate history field" in
    Ok values
  | _ -> Error "history object required"

let text values name =
  match List.assoc_opt name values with
  | Some (`String value) -> Ok value
  | _ -> Error ("history string missing: " ^ name)

let number values name =
  match List.assoc_opt name values with
  | Some (`Int value) when value >= 0 -> Ok value
  | Some (`Intlit raw) ->
    begin match int_of_string_opt raw with
    | Some value when value >= 0 && string_of_int value = raw -> Ok value
    | _ -> Error ("history integer invalid: " ^ name)
    end
  | _ -> Error ("history integer missing: " ^ name)

let payload tx =
  match tx.T.encrypted_data with
  | None -> Error "history encrypted payload missing"
  | Some raw ->
    begin
      match Yojson.Safe.from_string raw with
      | json ->
        let* values = fields json in
        let* version = number values "version" in
        let* () = require (version = 5) "history payload version unsupported" in
        Ok json
      | exception Yojson.Json_error _ -> Error "history payload JSON invalid"
    end

let transaction (entry : entry) op =
  let* () = require (entry.epoch >= 0 && entry.index >= 0L)
    "history position invalid" in
  let* archived = A.decode ~hash:entry.hash ~json:entry.json in
  let* () = require (A.proof archived = A.Exact)
    "history hash does not authenticate encrypted payload" in
  let tx = A.tx archived in
  let* () = require (tx.T.op_type = op) "history operation differs" in
  Ok tx

let output_fields output (send : entry) (claim : entry) sender receiver
    (transfer : PT.t) (receipt : SC.t) =
  let* values = fields output in
  let* id = number values "id" in
  let* epoch = number values "epoch_id" in
  let* spent = number values "claimed" in
  let* () = require (id = receipt.output_id) "history output id differs" in
  let* () = require (epoch = send.epoch) "history output epoch differs" in
  let* () = require (spent = 1) "history output is not spent" in
  let expected = [
    "tx_hash", send.hash;
    "claim_tx_hash", claim.hash;
    "sender_addr", sender;
    "delta_cipher_stored", transfer.delta_cipher;
    "amount_commitment", transfer.amount_commitment;
    "claim_pub", transfer.claim_pub;
    "stealth_tag", transfer.stealth_tag;
    "eph_pub", transfer.eph_pub;
    "enc_amount", transfer.enc_amount;
  ] in
  let* () = List.fold_left (fun result (name, expected) ->
    let* () = result in
    let* actual = text values name in
    require (actual = expected) ("history output field differs: " ^ name))
    (Ok ()) expected in
  let* () = require
    (SA.verify_claim_secret ~claim_secret_hex:receipt.claim_secret
       ~claimer_addr:receiver ~stored_claim_pub_hex:transfer.claim_pub)
    "history claim ownership differs" in
  Ok id

let key_blob (key : key) =
  let* () = require (Pvac_registry.full_key_hash key.blob = key.hash)
    "history key hash differs" in
  Pvac_registry.validate_shape key.blob

let amount ~op entry (key : key) =
  let* () = require (op = T.EncryptOp || op = T.DecryptOp)
    "history operation has no public FHE amount" in
  let* tx = transaction entry op in
  let* () = require (tx.from = tx.to_)
    "history public FHE operation is not self-directed" in
  let* () = require
    (Z.leq tx.amount Denomination.max_supply)
    "history public amount exceeds supply range" in
  let* json = match tx.encrypted_data with
    | None -> Error "history encrypted payload missing"
    | Some raw ->
      begin match Yojson.Safe.from_string raw with
      | value -> Ok value
      | exception Yojson.Json_error _ -> Error "history payload JSON invalid"
      end in
  let* values = fields json in
  let* cipher = text values "cipher" in
  let* commitment = text values "amount_commitment" in
  let* proof = text values "zero_proof" in
  let* blinding = text values "blinding" in
  let* pubkey = key_blob key in
  let* () = V.result_sync ~math:key.math (P.Encrypt {
    pubkey; cipher; amount = tx.amount; proof; commitment; blinding; strict = true;
  }) in
  Ok tx.amount

let cell (key : key) ~cipher ~ciphertext_commitment ~proof ~amount_commitment =
  let* pubkey = key_blob key in
  V.verify_circle_cell_sync ~math:key.math ~strict:true ~pubkey ~cipher
    ~ciphertext_commitment ~proof_kind:P.Circle_bound_zero ~proof ~amount_commitment

let send_proof sender_key (transfer : PT.t) =
  let* () = cell sender_key ~cipher:transfer.delta_cipher
    ~ciphertext_commitment:transfer.commitment ~proof:transfer.send_zero_proof
    ~amount_commitment:transfer.amount_commitment in
  V.verify_range_sync ~math:sender_key.math ~strict:true
    ~pubkey:sender_key.blob ~cipher:transfer.delta_cipher
    ~proof:transfer.range_proof_delta

let sent entry key =
  let* tx = transaction entry T.StealthOp in
  let* () = require (tx.to_ = "stealth") "history stealth target differs" in
  let* json = payload tx in
  let* transfer = PT.of_json json in
  let* () = send_proof key transfer in
  Ok transfer.PT.amount_commitment

let unspent ~send ~claim ~before ~output =
  match before with
  | None -> require (send.epoch = claim.epoch) "history unspent output missing"
  | Some _ when send.epoch = claim.epoch ->
    Error "history output existed before creation epoch"
  | Some before ->
    let* prior = fields before in
    let* final = fields output in
    let* spent = number prior "claimed" in
    let* () = require (spent = 0) "history output was already spent" in
    let* () = require
      (match List.assoc_opt "claim_tx_hash" prior with
       | None | Some (`String "") -> true
       | _ -> false)
      "history unspent output has a spending hash" in
    let* () = List.fold_left (fun result name ->
      let* () = result in
      let* left = number prior name in
      let* right = number final name in
      require (left = right) ("history output origin changed: " ^ name))
      (Ok ()) ["id"; "epoch_id"] in
    List.fold_left (fun result name ->
      let* () = result in
      let* left = text prior name in
      let* right = text final name in
      require (left = right)
        ("history output origin changed: " ^ name)) (Ok ())
      ["tx_hash"; "sender_addr"; "delta_cipher_stored";
       "amount_commitment"; "claim_pub"; "stealth_tag"; "eph_pub"; "enc_amount"]

let verify ~send ~claim ~before_send ~before_claim ~output ~sender_key ~receiver_key =
  let* send_tx = transaction send T.StealthOp in
  let* claim_tx = transaction claim T.ClaimOp in
  let* () = require (send.epoch <= claim.epoch && send.index < claim.index)
    "history claim precedes output" in
  let* () = require (claim_tx.from = claim_tx.to_)
    "history claim must target its sender" in
  let* send_json = payload send_tx in
  let* transfer = PT.of_json send_json in
  let* claim_json = payload claim_tx in
  let* receipt = SC.of_json claim_json in
  let* id = output_fields output send claim send_tx.from claim_tx.from
    transfer receipt in
  let* () = require (send_tx.to_ = "stealth") "history stealth target differs" in
  let* () = require (before_send = None)
    "history output existed before creation epoch" in
  let* () = unspent ~send ~claim ~before:before_claim ~output in
  let* () = send_proof sender_key transfer in
  let* () = cell receiver_key ~cipher:receipt.claim_cipher
    ~ciphertext_commitment:receipt.commitment ~proof:receipt.zero_proof
    ~amount_commitment:transfer.amount_commitment in
  Ok {
    id;
    sender = send_tx.from;
    receiver = claim_tx.from;
    send_hash = send.hash;
    claim_hash = claim.hash;
    commitment = transfer.amount_commitment;
  }