(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type context = {
  circle_id : string;
  caller_addr : string;
  key_id : string;
  intent_id : string;
  verb : string;
  proof_kind : string;
  policy_hash : string;
  ciphertext_hash : string;
  amount_commitment_hash : string;
}

type t = {
  version : string;
  verb : string;
  proof_kind : string;
  circle_id : string;
  caller_addr : string;
  key_id : string;
  intent_id : string;
  policy_hash : string;
  ciphertext_hash : string;
  amount_commitment_hash : string;
  signer_addr : string;
  signer_pubkey : string;
  signature : string;
}

let bind result next =
  match result with
  | Ok value -> next value
  | Error error -> Error error

let sha256 value =
  Digestif.SHA256.digest_string value
  |> Digestif.SHA256.to_hex

let rec canonical_json = function
  | `Assoc fields ->
    fields
    |> List.map (fun (name, value) -> name, canonical_json value)
    |> List.sort (fun (left, _) (right, _) -> String.compare left right)
    |> fun values -> `Assoc values
  | `List values -> `List (List.map canonical_json values)
  | value -> value

let canonical value =
  value
  |> canonical_json
  |> Yojson.Safe.to_string

let hash value =
  canonical value |> sha256

let hash_commitment encoded =
  match Base64.decode encoded with
  | Ok raw when String.length raw = 32 -> Ok (sha256 raw)
  | Ok _
  | Error _ -> Error "amount commitment is invalid"

let policy_hash policy =
  Circle_hfhe_policy.yojson_of_t policy
  |> hash

let required_fields =
  [
    "amount_commitment_hash";
    "caller_addr";
    "ciphertext_hash";
    "circle_id";
    "intent_id";
    "key_id";
    "policy_hash";
    "proof_kind";
    "signature";
    "signer_addr";
    "signer_pubkey";
    "verb";
    "version";
  ]

let unique_exact_fields fields =
  let names = List.map fst fields |> List.sort String.compare in
  names = required_fields

let string_field name fields =
  match List.assoc_opt name fields with
  | Some (`String value) -> Ok value
  | Some _ -> Error (name ^ " must be a string")
  | None -> Error (name ^ " is required")

let of_json = function
  | `Assoc fields when unique_exact_fields fields ->
    bind (string_field "version" fields) (fun version ->
    bind (string_field "verb" fields) (fun verb ->
    bind (string_field "proof_kind" fields) (fun proof_kind ->
    bind (string_field "circle_id" fields) (fun circle_id ->
    bind (string_field "caller_addr" fields) (fun caller_addr ->
    bind (string_field "key_id" fields) (fun key_id ->
    bind (string_field "intent_id" fields) (fun intent_id ->
    bind (string_field "policy_hash" fields) (fun policy_hash ->
    bind (string_field "ciphertext_hash" fields) (fun ciphertext_hash ->
    bind
      (string_field "amount_commitment_hash" fields)
      (fun amount_commitment_hash ->
    bind (string_field "signer_addr" fields) (fun signer_addr ->
    bind (string_field "signer_pubkey" fields) (fun signer_pubkey ->
    bind (string_field "signature" fields) (fun signature ->
      Ok {
        version;
        verb;
        proof_kind;
        circle_id;
        caller_addr;
        key_id;
        intent_id;
        policy_hash;
        ciphertext_hash;
        amount_commitment_hash;
        signer_addr;
        signer_pubkey;
        signature;
      })))))))))))))
  | `Assoc _ -> Error "proof receipt fields are invalid"
  | _ -> Error "proof receipt must be an object"

let frame domain fields =
  List.fold_left
    (fun value field ->
      value ^ "|" ^ string_of_int (String.length field) ^ ":" ^ field)
    domain
    fields

let context_fields (context : context) =
  [
    context.verb;
    context.proof_kind;
    context.circle_id;
    context.caller_addr;
    context.key_id;
    context.intent_id;
    context.policy_hash;
    context.ciphertext_hash;
    context.amount_commitment_hash;
  ]

let subject_v1 (context : context) =
  String.concat "|" ("octra_circle_hfhe_receipt_v1" :: context_fields context)

let subject_v2 (context : context) =
  frame "octra_circle_hfhe_receipt_v2" (context_fields context)

let safe_v1 (context : context) =
  context_fields context
  |> List.for_all (fun field -> not (String.contains field '|'))

let context_matches (context : context) (receipt : t) =
  receipt.verb = context.verb
  && receipt.proof_kind = context.proof_kind
  && receipt.circle_id = context.circle_id
  && receipt.caller_addr = context.caller_addr
  && receipt.key_id = context.key_id
  && receipt.intent_id = context.intent_id
  && receipt.policy_hash = context.policy_hash
  && receipt.ciphertext_hash = context.ciphertext_hash
  && receipt.amount_commitment_hash = context.amount_commitment_hash

let active_relay relays address =
  List.exists (String.equal address) relays

let signer_allowed (policy : Circle_hfhe_policy.t)
    ~owner ~caller ~active_relays signer =
  match policy.Circle_hfhe_policy.proof_receipt_signer_mode with
  | Circle_hfhe_policy.Deny -> false
  | Owner_only -> signer = owner
  | Caller_self -> signer = caller
  | Owner_or_caller -> signer = owner || signer = caller
  | Any_registered -> Crypto.Address.is_valid_address signer
  | Active_relay -> active_relay active_relays signer
  | Owner_or_active_relay ->
    signer = owner || active_relay active_relays signer

let class_allowed (policy : Circle_hfhe_policy.t) active_relays signer =
  match policy.Circle_hfhe_policy.proof_receipt_class with
  | Circle_hfhe_receipt_class.Detached ->
    not policy.require_receipt_transport_binding
    || active_relays <> []
  | Transport_bound -> active_relays <> []
  | Relay_witnessed -> active_relay active_relays signer

let signature_subject (context : context) (receipt : t) =
  match receipt.version with
  | "octra_circle_hfhe_receipt_v1" when safe_v1 context ->
    Ok (subject_v1 context)
  | "octra_circle_hfhe_receipt_v1" ->
    Error "legacy proof receipt context is ambiguous"
  | "octra_circle_hfhe_receipt_v2" ->
    Ok (subject_v2 context)
  | _ -> Error "invalid proof receipt version"

let verify_signature (context : context) (receipt : t) =
  if not (Crypto.Address.verify_address_pubkey receipt.signer_addr receipt.signer_pubkey) then
    Error "proof receipt signer binding invalid"
  else
    match Base64.decode receipt.signer_pubkey, Base64.decode receipt.signature with
    | Ok public_key, Ok signature
      when String.length public_key = 32
           && String.length signature = 64
           && Octra_ed25519.safe public_key ->
      bind (signature_subject context receipt) (fun subject ->
        if Octra_ed25519.verify ~pub:public_key ~msg:subject signature then Ok ()
        else Error "proof receipt signature verification failed")
    | _ -> Error "proof receipt signature encoding invalid"

let verify ~(context : context) ~policy ~owner ~active_relays json =
  bind (of_json json) (fun receipt ->
  if not (context_matches context receipt) then
    Error "proof receipt context mismatch"
  else if
    not
      (signer_allowed
         policy
         ~owner
         ~caller:context.caller_addr
         ~active_relays
         receipt.signer_addr)
  then
    Error "proof receipt signer is not allowed by policy"
  else if not (class_allowed policy active_relays receipt.signer_addr) then
    Error "proof receipt transport binding is invalid"
  else
    bind (verify_signature context receipt) (fun () -> Ok (hash json)))