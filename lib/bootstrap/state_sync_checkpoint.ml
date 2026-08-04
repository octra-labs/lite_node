(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type body = {
  chain_id : string;
  epoch : int64;
  state_root : string;
  ledger_state_root : string;
  txid_hi : int64;
  config_hash : string;
  validator_set_hash : string;
  quorum_cert_hash : string option;
  epoch_index_hash : string option;
  epoch_index_root : string option;
  created_at : int64;
  valid_until : int64;
}

type signature = {
  signer : string;
  signature : string;
}

let ( let* ) value f =
  match value with
  | Ok item -> f item
  | Error _ as error -> error

let ( >>= ) value f =
  let* item = value in
  f item

let protect f =
  try Ok (f ()) with exn -> Error (Printexc.to_string exn)

let raw_to_hex raw =
  let output = Bytes.create (String.length raw * 2) in
  let alphabet = "0123456789abcdef" in
  String.iteri (fun index byte ->
    let value = Char.code byte in
    Bytes.set output (index * 2) alphabet.[value lsr 4];
    Bytes.set output ((index * 2) + 1) alphabet.[value land 15]
  ) raw;
  Bytes.unsafe_to_string output

let lower_hex length value =
  String.length value = length
  && String.for_all (function
    | '0'..'9' | 'a'..'f' -> true
    | _ -> false
  ) value

let exact_fields expected fields =
  let actual = List.map fst fields |> List.sort String.compare in
  let expected = List.sort String.compare expected in
  if actual = expected then Ok () else Error "unexpected checkpoint fields"

let assoc fields name =
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> Error ("missing field " ^ name)

let string_value = function
  | `String value -> Ok value
  | _ -> Error "expected string"

let int64_value = function
  | `String value | `Intlit value -> protect (fun () -> Int64.of_string value)
  | `Int value -> Ok (Int64.of_int value)
  | _ -> Error "expected int64"

let option_string_value = function
  | `Null -> Ok None
  | `String value -> Ok (Some value)
  | _ -> Error "expected optional string"

let body_json body =
  `Assoc [
    "chain_id", `String body.chain_id;
    "epoch", `String (Int64.to_string body.epoch);
    "state_root", `String body.state_root;
    "ledger_state_root", `String body.ledger_state_root;
    "txid_hi", `String (Int64.to_string body.txid_hi);
    "config_hash", `String body.config_hash;
    "validator_set_hash", `String body.validator_set_hash;
    "quorum_cert_hash",
      (match body.quorum_cert_hash with Some value -> `String value | None -> `Null);
    "epoch_index_hash",
      (match body.epoch_index_hash with Some value -> `String value | None -> `Null);
    "epoch_index_root",
      (match body.epoch_index_root with Some value -> `String value | None -> `Null);
    "created_at", `String (Int64.to_string body.created_at);
    "valid_until", `String (Int64.to_string body.valid_until);
  ]

let signature_json signature =
  `Assoc [
    "signer", `String signature.signer;
    "signature", `String signature.signature;
  ]

let parse_body = function
  | `Assoc fields ->
      let names = [
        "chain_id"; "epoch"; "state_root"; "ledger_state_root"; "txid_hi";
        "config_hash"; "validator_set_hash"; "quorum_cert_hash";
        "epoch_index_hash"; "epoch_index_root"; "created_at"; "valid_until";
      ] in
      let* () = exact_fields names fields in
      let* chain_id = assoc fields "chain_id" >>= string_value in
      let* epoch = assoc fields "epoch" >>= int64_value in
      let* state_root = assoc fields "state_root" >>= string_value in
      let* ledger_state_root = assoc fields "ledger_state_root" >>= string_value in
      let* txid_hi = assoc fields "txid_hi" >>= int64_value in
      let* config_hash = assoc fields "config_hash" >>= string_value in
      let* validator_set_hash = assoc fields "validator_set_hash" >>= string_value in
      let* quorum_cert_hash = assoc fields "quorum_cert_hash" >>= option_string_value in
      let* epoch_index_hash = assoc fields "epoch_index_hash" >>= option_string_value in
      let* epoch_index_root = assoc fields "epoch_index_root" >>= option_string_value in
      let* created_at = assoc fields "created_at" >>= int64_value in
      let* valid_until = assoc fields "valid_until" >>= int64_value in
      Ok {
        chain_id;
        epoch;
        state_root;
        ledger_state_root;
        txid_hi;
        config_hash;
        validator_set_hash;
        quorum_cert_hash;
        epoch_index_hash;
        epoch_index_root;
        created_at;
        valid_until;
      }
  | _ -> Error "checkpoint must be an object"

let parse_signature = function
  | `Assoc fields ->
      let* () = exact_fields ["signer"; "signature"] fields in
      let* signer = assoc fields "signer" >>= string_value in
      let* signature = assoc fields "signature" >>= string_value in
      Ok { signer; signature }
  | _ -> Error "checkpoint signature must be an object"

let option_hash = function
  | None -> true
  | Some value -> lower_hex 64 value

let validate body =
  if body.chain_id = "" || String.length body.chain_id > 128 then Error "invalid chain id"
  else if Int64.compare body.epoch 0L < 0 then Error "negative checkpoint epoch"
  else if Int64.compare body.txid_hi 0L < 0 then Error "negative checkpoint txid"
  else if body.txid_hi = Int64.max_int then Error "checkpoint txid exceeds limit"
  else if not (lower_hex 64 body.state_root) then Error "invalid checkpoint state root"
  else if not (Octra_consensus.C_light_root.text_root_ok body.ledger_state_root) then
    Error "invalid checkpoint ledger root"
  else if not (lower_hex 64 body.config_hash) then Error "invalid checkpoint config hash"
  else if not (lower_hex 64 body.validator_set_hash) then
    Error "invalid checkpoint validator set hash"
  else if not (option_hash body.quorum_cert_hash) then Error "invalid checkpoint quorum hash"
  else if not (option_hash body.epoch_index_hash) then Error "invalid checkpoint index hash"
  else if not (option_hash body.epoch_index_root) then Error "invalid checkpoint index root"
  else if body.epoch_index_hash = None || body.epoch_index_root = None then
    Error "checkpoint index commitment is required"
  else if Int64.compare body.created_at 0L < 0 then Error "negative checkpoint creation time"
  else if Int64.compare body.valid_until body.created_at <= 0 then
    Error "checkpoint validity interval is empty"
  else if Int64.compare
    (Int64.sub body.valid_until body.created_at)
    2_678_400L > 0 then
    Error "checkpoint validity interval exceeds limit"
  else
    Ok ()

let same_finalized_state left right =
  left.chain_id = right.chain_id
  && left.epoch = right.epoch
  && left.state_root = right.state_root
  && left.ledger_state_root = right.ledger_state_root
  && left.txid_hi = right.txid_hi
  && left.config_hash = right.config_hash
  && left.validator_set_hash = right.validator_set_hash
  && left.epoch_index_hash = right.epoch_index_hash
  && left.epoch_index_root = right.epoch_index_root

let hash_raw body =
  let* () = validate body in
  Ok (Octra_net.Hash_domain.hash_encoded "octra:state_sync_checkpoint" (fun buffer ->
    Octra_net.Oce1.put_string buffer body.chain_id;
    Octra_net.Oce1.put_u64 buffer body.epoch;
    Octra_net.Oce1.put_hash32 buffer body.state_root;
    Octra_net.Oce1.put_string buffer body.ledger_state_root;
    Octra_net.Oce1.put_u64 buffer body.txid_hi;
    Octra_net.Oce1.put_hash32 buffer body.config_hash;
    Octra_net.Oce1.put_hash32 buffer body.validator_set_hash;
    Octra_net.Oce1.put_option Octra_net.Oce1.put_hash32 buffer body.quorum_cert_hash;
    Octra_net.Oce1.put_option Octra_net.Oce1.put_hash32 buffer body.epoch_index_hash;
    Octra_net.Oce1.put_option Octra_net.Oce1.put_hash32 buffer body.epoch_index_root;
    Octra_net.Oce1.put_u64 buffer body.created_at;
    Octra_net.Oce1.put_u64 buffer body.valid_until))

let hash body =
  let* raw = hash_raw body in
  Ok (raw_to_hex raw)

let canonical_base64 raw encoded =
  Base64.encode_exn raw = encoded

let raw_pubkey encoded =
  let* raw = protect (fun () -> Base64.decode_exn encoded) in
  if String.length raw <> 32 || not (canonical_base64 raw encoded) then
    Error "invalid signer public key"
  else if not (Octra_ed25519.safe raw) then
    Error "unsafe signer public key"
  else
    Ok raw

let raw_signature encoded =
  let* raw = protect (fun () -> Base64.decode_exn encoded) in
  if String.length raw <> 64 || not (canonical_base64 raw encoded) then
    Error "invalid checkpoint signature"
  else
    Ok raw

let make_signature ~wallet body =
  if not (Octra_core.Crypto.Address.verify_address_pubkey
    wallet.Octra_core.Crypto.Wallet.address wallet.pub) then
    Error "wallet address does not match public key"
  else
    let* private_raw = protect (fun () -> Base64.decode_exn wallet.priv) in
    if String.length private_raw < 32 then Error "invalid wallet private key"
    else
      let* message = hash_raw body in
      let signature =
        Octra_consensus.C_hash.sign_ed25519
          ~priv_raw:(String.sub private_raw 0 32)
          ~msg:message
        |> Base64.encode_exn
      in
      Ok { signer = wallet.address; signature }

let verify_signature ~signer_set ~message signature =
  match Octra_consensus.C_types.pubkey_of_addr signer_set signature.signer with
  | None -> Error "signature belongs to an unknown signer"
  | Some pubkey ->
      let* pubkey_raw = raw_pubkey pubkey in
      let* signature_raw = raw_signature signature.signature in
      if Octra_consensus.C_hash.verify_ed25519
        ~pubkey_raw
        ~msg:message
        ~signature:signature_raw then
        Ok ()
      else
        Error "invalid checkpoint signature"

let verify_quorum ~validator_set body signatures =
  let* () = validate body in
  let signers = List.map (fun item -> item.signer) signatures in
  if signers <> List.sort_uniq String.compare signers then
    Error "checkpoint signatures are not canonical"
  else
    let* message = hash_raw body in
    let* () =
      List.fold_left (fun state signature ->
        let* () = state in
        let* () = verify_signature ~signer_set:validator_set ~message signature in
        Ok ()
      ) (Ok ()) signatures
    in
    if not
      (Octra_consensus.C_types.has_quorum validator_set signers) then
      Error "checkpoint signature quorum missing"
    else
      Ok ()

let of_head ~chain_id ~config_hash ~validator_set_hash ~created_at ~valid_until
    (head : Octra_core.Head_manifest.t) =
  {
    chain_id;
    epoch = Int64.of_int head.epoch_id;
    state_root = head.state_root;
    ledger_state_root = Octra_core.Head_manifest.ledger_state_root head;
    txid_hi = head.txid_hi;
    config_hash;
    validator_set_hash;
    quorum_cert_hash = head.quorum_cert_hash;
    epoch_index_hash = head.epoch_index_hash;
    epoch_index_root = head.epoch_index_root;
    created_at;
    valid_until;
  }

let matches_head body (head : Octra_core.Head_manifest.t) =
  body.epoch = Int64.of_int head.epoch_id
  && body.state_root = head.state_root
  && body.ledger_state_root = Octra_core.Head_manifest.ledger_state_root head
  && body.txid_hi = head.txid_hi
  && body.quorum_cert_hash = head.quorum_cert_hash
  && body.epoch_index_hash = head.epoch_index_hash
  && body.epoch_index_root = head.epoch_index_root