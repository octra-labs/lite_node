(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module String_set = Set.Make (String)

type proof =
  | Exact
  | Signed
  | Signed_operation

type t = {
  hash : string;
  raw_json : string;
  json : Yojson.Safe.t;
  tx : Transaction.t;
  proof : proof;
}

let tx value = value.tx
let proof value = value.proof

let signed_json (tx : Transaction.t) =
  `Assoc [
    "from", `String tx.from;
    "to_", `String tx.to_;
    "amount", `String (Z.to_string tx.amount);
    "nonce", `Int tx.nonce;
    "ou", `String (Z.to_string tx.ou);
    "timestamp", `Float tx.timestamp;
  ]

let signed_bytes tx = Yojson.Safe.to_string (signed_json tx)

let signed_hash tx =
  signed_bytes tx
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex

let operation_json (tx : Transaction.t) =
  `Assoc [
    "from", `String tx.from;
    "to_", `String tx.to_;
    "amount", `String (Z.to_string tx.amount);
    "nonce", `Int tx.nonce;
    "ou", `String (Z.to_string tx.ou);
    "timestamp", `Float tx.timestamp;
    "op_type", `String (Transaction.op_type_to_string tx.op_type);
  ]

let operation_hash tx =
  operation_json tx
  |> Yojson.Safe.to_string
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex

let allowed_keys =
  String_set.of_list [
    "from";
    "to_";
    "amount";
    "nonce";
    "ou";
    "timestamp";
    "signature";
    "public_key";
    "message";
    "op_type";
  ]

let operation_allowed_keys =
  allowed_keys
  |> String_set.add "op_type"
  |> String_set.add "encrypted_data"

let required_keys =
  ["from"; "to_"; "amount"; "nonce"; "ou"; "timestamp"; "signature"]

let optional_string fields key =
  match List.assoc_opt key fields with
  | None
  | Some `Null
  | Some (`String _) -> true
  | Some _ -> false

let standard_op_type fields =
  match List.assoc_opt "op_type" fields with
  | None
  | Some `Null
  | Some (`String "standard") -> true
  | Some _ -> false

let signed_shape = function
  | `Assoc fields ->
      let keys = List.map fst fields in
      let unique = String_set.of_list keys in
      List.length keys = String_set.cardinal unique
      && String_set.subset unique allowed_keys
      && List.for_all (fun key -> String_set.mem key unique) required_keys
      && optional_string fields "public_key"
      && optional_string fields "message"
      && standard_op_type fields
  | _ -> false

let operation_shape = function
  | `Assoc fields ->
      let keys = List.map fst fields in
      let unique = String_set.of_list keys in
      List.length keys = String_set.cardinal unique
      && String_set.subset unique operation_allowed_keys
      && List.for_all
           (fun key -> String_set.mem key unique)
           ("op_type" :: required_keys)
      && optional_string fields "public_key"
      && optional_string fields "message"
      && optional_string fields "encrypted_data"
  | _ -> false

let retain_proved_fields tx =
  Transaction.{
    tx with
    public_key = None;
    message = None;
    encrypted_data = None;
  }

let retain_signed_fields tx =
  Transaction.{
    (retain_proved_fields tx) with
    op_type = Transaction.Standard;
  }

let decode ~hash ~json:raw_json =
  try
    let json = Yojson.Safe.from_string raw_json in
    match Tx_payload.decode_final json with
    | Error reason -> Error reason
    | Ok tx when Transaction.hash tx = hash ->
        if Yojson.Safe.to_string (Transaction.to_yojson tx) = raw_json then
          Ok { hash; raw_json; json; tx; proof = Exact }
        else
          Error "transaction encoding mismatch"
    | Ok tx when signed_hash tx = hash ->
        if signed_shape json then
          Ok {
            hash;
            raw_json;
            json;
            tx = retain_signed_fields tx;
            proof = Signed;
          }
        else
          Error "signed transaction shape is invalid"
    | Ok tx when operation_hash tx = hash ->
        if operation_shape json then
          Ok {
            hash;
            raw_json;
            json;
            tx = retain_proved_fields tx;
            proof = Signed_operation;
          }
        else
          Error "signed operation shape is invalid"
    | Ok _ -> Error "transaction hash mismatch"
  with exn -> Error (Printexc.to_string exn)

let carried_public_key = function
  | `Assoc fields ->
      begin
        match List.assoc_opt "public_key" fields with
        | Some (`String value) -> Some value
        | _ -> None
      end
  | _ -> None

let select_public_key ~public_key_of value =
  let trusted = public_key_of value.tx.from in
  let carried = carried_public_key value.json in
  match trusted, carried with
  | Some value, _
  | None, Some value -> Ok value
  | None, None -> Error "signed transaction public key is unavailable"

let proof_bytes value =
  match value.proof with
  | Exact -> value.raw_json
  | Signed -> signed_bytes value.tx
  | Signed_operation -> Yojson.Safe.to_string (operation_json value.tx)

let verify_signature public_key value =
  if not (Crypto.Address.verify_address_pubkey value.tx.from public_key) then
    Error "signed transaction public key does not match sender"
  else
    try
      let public_key = Base64.decode_exn public_key in
      let signature = Base64.decode_exn value.tx.signature in
      if not (Octra_ed25519.safe public_key) then
        Error "signed transaction public key is invalid"
      else if
        Octra_ed25519.verify
          ~pub:public_key
          ~msg:(proof_bytes value)
          signature
      then
        Ok ()
      else
        Error "signed transaction signature is invalid"
    with _ -> Error "signed transaction signature encoding is invalid"

let verify ~public_key_of value =
  match value.proof with
  | Exact -> Ok ()
  | Signed
  | Signed_operation ->
      begin
        match select_public_key ~public_key_of value with
        | Error _ as error -> error
        | Ok public_key -> verify_signature public_key value
      end

let visible_json value =
  match value.proof with
  | Exact -> value.raw_json
  | Signed
  | Signed_operation ->
      let tx = value.tx in
      let fields = [
        "from", `String tx.from;
        "to_", `String tx.to_;
        "amount", `String (Z.to_string tx.amount);
        "nonce", `Int tx.nonce;
        "ou", `String (Z.to_string tx.ou);
        "timestamp", `Float tx.timestamp;
        "signature", `String tx.signature;
      ] in
      let fields =
        match value.proof with
        | Signed_operation ->
            fields
            @ ["op_type", `String (Transaction.op_type_to_string tx.op_type)]
        | Exact
        | Signed -> fields
      in
      `Assoc fields
      |> Yojson.Safe.to_string