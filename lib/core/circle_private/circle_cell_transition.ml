(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module P = Pvac_verify_protocol

let consensus_id = "circle_receipt:verify_apply:retry_tx"

type cell =
  | Balance of Circle_balance_cell.write_request
  | Register of Circle_register_cell.write_request

type t = {
  transition_hash : string;
  cell : cell;
  pubkey : string;
  cipher : string;
  ciphertext_commitment : string;
  proof_kind : P.circle_proof;
  proof : string;
  amount_commitment : string;
}

type proof_fields = {
  zero_proof : string option;
  range_proof : string option;
  amount_commitment : string option;
  proof_receipt : Yojson.Safe.t option;
  intent_id : string;
}

let bind result next =
  match result with
  | Ok value -> next value
  | Error error -> Error error

let sha256 value =
  Digestif.SHA256.digest_string value
  |> Digestif.SHA256.to_hex

let frame domain fields =
  List.fold_left
    (fun value field ->
      value ^ "|" ^ string_of_int (String.length field) ^ ":" ^ field)
    domain
    fields

let optional_string name fields =
  match List.assoc_opt name fields with
  | None
  | Some `Null -> Ok None
  | Some (`String value) when value <> "" -> Ok (Some value)
  | Some (`String _) -> Error (name ^ " must not be empty")
  | Some _ -> Error (name ^ " must be a string")

let optional_json name fields =
  match List.assoc_opt name fields with
  | None
  | Some `Null -> None
  | Some value -> Some value

let unique_fields fields =
  let names = List.map fst fields in
  List.length names = List.length (List.sort_uniq String.compare names)

let proof_fields_of_json = function
  | `Assoc fields when unique_fields fields ->
    bind (optional_string "zero_proof" fields) (fun zero_proof ->
    bind (optional_string "range_proof" fields) (fun range_proof ->
    bind
      (optional_string "amount_commitment" fields)
      (fun amount_commitment ->
    bind (optional_string "intent_id" fields) (fun intent_id ->
      Ok {
        zero_proof;
        range_proof;
        amount_commitment;
        proof_receipt = optional_json "proof_receipt" fields;
        intent_id = Option.value ~default:"" intent_id;
      }))))
  | `Assoc _ -> Error "circle cell payload has duplicate fields"
  | _ -> Error "circle cell payload must be an object"

let transaction_json tx =
  match tx.Transaction.message with
  | None -> Error "circle cell requires message payload"
  | Some raw ->
    try Ok (Yojson.Safe.from_string raw)
    with _ -> Error "circle cell payload is invalid json"

let resolve_cell tx json =
  match tx.Transaction.op_type with
  | Transaction.CircleBalanceCellPut ->
    bind (Circle_balance_cell.put_payload_of_yojson json) (fun payload ->
    bind (Circle_balance_cell.resolve_put_payload payload) (fun request ->
      Ok (Balance request)))
  | Transaction.CircleRegisterCellPut ->
    bind (Circle_register_cell.put_payload_of_yojson json) (fun payload ->
    bind (Circle_register_cell.resolve_put_payload payload) (fun request ->
      Ok (Register request)))
  | _ -> Error "transaction is not a circle cell write"

let proof_kind = function
  | Balance request ->
    Option.value
      ~default:Circle_hfhe_proof.No_proof
      request.Circle_balance_cell.cell.proof_kind
  | Register request ->
    Option.value
      ~default:Circle_hfhe_proof.No_proof
      request.Circle_register_cell.cell.proof_kind

let proof_receipt_hash = function
  | Balance request -> request.Circle_balance_cell.cell.proof_receipt_hash
  | Register request -> request.Circle_register_cell.cell.proof_receipt_hash

let ciphertext_commitment = function
  | Balance request -> request.Circle_balance_cell.cell.ciphertext_commitment
  | Register request -> request.Circle_register_cell.cell.ciphertext_commitment

let state_ref = function
  | Balance request -> request.Circle_balance_cell.state_ref
  | Register request -> request.Circle_register_cell.state_ref

let key_id = function
  | Balance request -> request.Circle_balance_cell.key_id
  | Register request -> request.Circle_register_cell.key_id

let balance_amount_commitment = function
  | Balance request -> request.Circle_balance_cell.cell.amount_commitment
  | Register _ -> None

let receipt_required = function
  | Circle_hfhe_proof.Zero_receipt_v1
  | Range_receipt_v1
  | Bound_zero_receipt_v1 -> true
  | No_proof
  | Range_v1
  | Bound_zero_v1 -> false

let relay_mode = function
  | Circle_hfhe_policy.Active_relay
  | Owner_or_active_relay -> true
  | Deny
  | Owner_only
  | Caller_self
  | Owner_or_caller
  | Any_registered -> false

let active_relay relays address =
  List.exists (String.equal address) relays

let mode_allows mode ~owner ~caller ~active_relays =
  match mode with
  | Circle_hfhe_policy.Deny -> false
  | Owner_only -> caller = owner
  | Caller_self -> true
  | Owner_or_caller -> caller = owner
  | Any_registered -> Crypto.Address.is_valid_address caller
  | Active_relay -> active_relay active_relays caller
  | Owner_or_active_relay ->
    caller = owner || active_relay active_relays caller

let receipt_transport_required (policy : Circle_hfhe_policy.t) kind =
  receipt_required kind
  &&
  (policy.Circle_hfhe_policy.require_receipt_transport_binding
   || policy.proof_receipt_class <> Circle_hfhe_receipt_class.Detached
   || relay_mode policy.proof_receipt_signer_mode)

let active_relays store circle_id current_epoch
    (policy : Circle_hfhe_policy.t) kind intent_id =
  let required =
    relay_mode policy.Circle_hfhe_policy.encrypt_mode
    || receipt_transport_required policy kind
  in
  if not required then Lwt.return_ok []
  else if intent_id = "" then Lwt.return_error "intent_id is required by circle hfhe policy"
  else
    let open Lwt.Syntax in
    let* transport = Circle_policy_store.load_transport_policy store circle_id in
    let* claims =
      Circle_transport_state.active_outbox_claims
        store
        circle_id
        intent_id
        current_epoch
    in
    if not (Circle_transport_state.claim_ready ~transport_policy:transport claims) then
      Lwt.return_error "circle outbox relay quorum is not ready"
    else
      claims
      |> List.map (fun (claim : Circles.relay_claim) -> claim.relay_id)
      |> List.sort_uniq String.compare
      |> Lwt.return_ok

let key_state store circle_id current_epoch key_id =
  let open Lwt.Syntax in
  let* policy = Circle_policy_store.load_key_policy store circle_id key_id in
  Lwt.return (Circle_key_state.classify policy (Int64.of_int current_epoch))

let decoded_ciphertext tx =
  match tx.Transaction.encrypted_data with
  | None -> Error "circle cell ciphertext is required"
  | Some encoded ->
    begin
      match Base64.decode encoded with
      | Ok raw when raw <> "" -> Ok (encoded, raw)
      | Ok _
      | Error _ -> Error "circle cell ciphertext is invalid"
    end

let source_cipher_hash tx =
  bind (decoded_ciphertext tx) (fun (_, raw) -> Ok (sha256 raw))

let normalized_commitment name = function
  | Some value -> Ok value
  | None -> Error (name ^ " is required")

let proof_plan cell kind fields =
  let amount =
    match balance_amount_commitment cell with
    | Some value -> Some value
    | None -> fields.amount_commitment
  in
  match kind with
  | Circle_hfhe_proof.No_proof ->
    if
      fields.zero_proof <> None
      || fields.range_proof <> None
      || fields.proof_receipt <> None
    then
      Error "proof fields require a circle hfhe proof policy"
    else
      Ok (P.Circle_none, "", "", "")
  | Zero_receipt_v1 ->
    begin
      match cell, fields.zero_proof, fields.range_proof, fields.proof_receipt with
      | Balance _, _, _, _ ->
        Error "zero receipt does not bind a balance cell amount commitment"
      | Register _, Some proof, None, Some _ ->
        Ok (P.Circle_zero, proof, "", "")
      | _ -> Error "zero proof and proof receipt are required"
    end
  | Bound_zero_v1
  | Bound_zero_receipt_v1 ->
    begin
      match fields.zero_proof, fields.range_proof, amount with
      | Some proof, None, Some commitment ->
        Ok (P.Circle_bound_zero, proof, commitment, commitment)
      | _ -> Error "zero proof and amount commitment are required"
    end
  | Range_v1
  | Range_receipt_v1 ->
    begin
      match fields.zero_proof, fields.range_proof, amount with
      | None, Some proof, Some commitment ->
        Ok (P.Circle_range, proof, commitment, commitment)
      | _ -> Error "range proof and amount commitment are required"
    end

let receipt_hash_for_plan ~context ~policy ~owner ~active_relays cell fields =
  match receipt_required (proof_kind cell), proof_receipt_hash cell, fields.proof_receipt with
  | false, None, None -> Ok ""
  | false, _, _ -> Error "proof receipt requires a receipt proof kind"
  | true, Some expected, Some receipt ->
    bind
      (Circle_hfhe_receipt.verify
         ~context
         ~policy
         ~owner
         ~active_relays
         receipt)
      (fun actual ->
        if actual = expected then Ok actual
        else Error "proof receipt hash mismatch")
  | true, _, _ -> Error "proof receipt and proof receipt hash are required"

let transition_hash tx cell policy_hash key_state kind ciphertext_hash
    ciphertext_commitment amount_commitment proof receipt_hash intent_id =
  frame
    "octra:circle_cell_transition:v1"
    [
      Transaction.hash tx;
      Transaction.op_type_to_string tx.Transaction.op_type;
      state_ref cell;
      key_id cell;
      policy_hash;
      Circle_key_state.string_of_t key_state;
      Circle_hfhe_proof.string_of_t kind;
      ciphertext_hash;
      ciphertext_commitment;
      amount_commitment;
      sha256 proof;
      receipt_hash;
      intent_id;
    ]
  |> sha256

let prepare ~store ~ledger ~current_epoch tx =
  let open Lwt.Syntax in
  match transaction_json tx with
  | Error error -> Lwt.return_error error
  | Ok json ->
    begin
      match resolve_cell tx json, proof_fields_of_json json, decoded_ciphertext tx with
      | Error error, _, _
      | _, Error error, _
      | _, _, Error error -> Lwt.return_error error
      | Ok cell, Ok fields, Ok (ciphertext_b64, ciphertext_raw) ->
        let kind = proof_kind cell in
        let* info = Store_irmin.get_circle_info store tx.Transaction.to_ in
        begin
          match info with
          | None -> Lwt.return_error "circle does not exist"
          | Some info when info.Circles.owner <> tx.from ->
            Lwt.return_error "only the owner can modify circle private state cells"
          | Some info when info.resource_mode <> Circles.Sealed_read ->
            Lwt.return_error "private state cells require sealed_read circle mode"
          | Some info ->
            let* policy = Circle_policy_store.load_hfhe_policy store tx.to_ in
            if kind <> policy.Circle_hfhe_policy.encrypt_proof then
              Lwt.return_error "circle cell proof kind does not match hfhe policy"
            else
              let* relays =
                active_relays
                  store
                  tx.to_
                  current_epoch
                  policy
                  kind
                  fields.intent_id
              in
              begin
                match relays with
                | Error error -> Lwt.return_error error
                | Ok active_relays ->
                  if
                    not
                      (mode_allows
                         policy.encrypt_mode
                         ~owner:info.owner
                         ~caller:tx.from
                         ~active_relays)
                  then
                    Lwt.return_error "circle hfhe policy denied encryption"
                  else
                    let* key_state = key_state store tx.to_ current_epoch (key_id cell) in
                    if
                      policy.require_live_key_policy
                      && not (Circle_key_state.live key_state)
                    then
                      Lwt.return_error "circle key policy is not live"
                    else
                      let* bound = Ledger.pvac_key_is_bound ledger tx.from in
                      if not bound then
                        Lwt.return_error "pvac key is not consensus bound"
                      else
                        let* pubkey = Ledger.get_pvac_pubkey ledger tx.from in
                        begin
                          match
                            pubkey,
                            normalized_commitment
                              "ciphertext commitment"
                              (ciphertext_commitment cell),
                            proof_plan cell kind fields
                          with
                          | None, _, _ -> Lwt.return_error "pvac pubkey is unavailable"
                          | _, Error error, _
                          | _, _, Error error -> Lwt.return_error error
                          | Some pubkey, Ok ciphertext_commitment,
                            Ok (worker_kind, proof, worker_amount, receipt_amount) ->
                            let policy_digest = Circle_hfhe_receipt.policy_hash policy in
                            let ciphertext_hash = sha256 ciphertext_raw in
                            let amount_commitment_hash =
                              if receipt_amount = "" then Ok ""
                              else Circle_hfhe_receipt.hash_commitment receipt_amount
                            in
                            begin
                              match amount_commitment_hash with
                              | Error error -> Lwt.return_error error
                              | Ok amount_commitment_hash ->
                                let context = Circle_hfhe_receipt.{
                                  circle_id = tx.to_;
                                  caller_addr = tx.from;
                                  key_id = key_id cell;
                                  intent_id = fields.intent_id;
                                  verb = "encrypt";
                                  proof_kind = Circle_hfhe_proof.string_of_t kind;
                                  policy_hash = policy_digest;
                                  ciphertext_hash;
                                  amount_commitment_hash;
                                } in
                                begin
                                  match
                                    receipt_hash_for_plan
                                      ~context
                                      ~policy
                                      ~owner:info.owner
                                      ~active_relays
                                      cell
                                      fields
                                  with
                                  | Error error -> Lwt.return_error error
                                  | Ok receipt_hash ->
                                    let transition_hash =
                                      transition_hash
                                        tx
                                        cell
                                        policy_digest
                                        key_state
                                        kind
                                        ciphertext_hash
                                        ciphertext_commitment
                                        worker_amount
                                        proof
                                        receipt_hash
                                        fields.intent_id
                                    in
                                    Lwt.return_ok {
                                      transition_hash;
                                      cell;
                                      pubkey;
                                      cipher = Crypto.FheBalance.prefix ^ ciphertext_b64;
                                      ciphertext_commitment;
                                      proof_kind = worker_kind;
                                      proof;
                                      amount_commitment = worker_amount;
                                    }
                                end
                            end
                        end
              end
        end
    end

let verify_classified plan =
  Pvac_verify_worker.classified_result
    (P.Circle_cell {
       pubkey = plan.pubkey;
       cipher = plan.cipher;
       ciphertext_commitment = plan.ciphertext_commitment;
       proof_kind = plan.proof_kind;
       proof = plan.proof;
       amount_commitment = plan.amount_commitment;
     })

let verify plan =
  let open Lwt.Syntax in
  let* result = verify_classified plan in
  Lwt.return
    (Result.map_error
       Pvac_verify_worker.verification_failure_message
       result)