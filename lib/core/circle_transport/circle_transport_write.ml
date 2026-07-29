(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type transport_policy_plan = {
  policy : Circle_transport_policy.t;
}

type open_plan = {
  intent : Circles.outbox_intent;
}

type claim_plan = {
  claim : Circles.relay_claim;
  status : Circles.outbox_status;
}

type cancel_plan = {
  claim_resolution : Circles.outbox_resolution;
  outbox_resolution : Circles.outbox_resolution option;
  status : Circles.outbox_status;
}

type ingress_plan = {
  packet : Circles.ingress_packet;
  resolution : Circles.outbox_resolution;
  status : Circles.outbox_status;
}

let transport_policy_put payload =
  let relay_allowlist =
    Option.value ~default:[] payload.Circle_transport_policy.relay_allowlist in
  if not (List.for_all Crypto.Address.is_valid_address relay_allowlist) then
    Error ("invalid_circle_relay_allowlist", "invalid_circle_relay_allowlist must contain valid octra addresses")
  else
    match payload.max_claim_window_epochs, payload.max_response_bytes with
    | Some value, _ when Int64.compare value 0L <= 0 ->
      Error ("invalid_circle_claim_window", "max_claim_window_epochs must be positive")
    | _, Some value when Int64.compare value 0L <= 0 ->
      Error ("invalid_circle_response_limit", "max_response_bytes must be positive")
    | _ ->
      let policy = Circle_transport_policy.resolve_put_payload payload in
      match Circle_transport_policy.validate policy with
      | Error e -> Error e
      | Ok () -> Ok { policy }

let open_envelope ~current_epoch (payload : Circles.outbox_open_payload) =
  if String.length payload.intent_id = 0 then
    Error ("invalid_circle_intent_id", "intent_id must not be empty")
  else if String.length payload.relay_policy_hash <> 64 then
    Error ("invalid_circle_relay_policy_hash", "relay_policy_hash must be a 64-char hex string")
  else if String.length payload.payload_hash <> 64 then
    Error ("invalid_circle_payload_hash", "payload_hash must be a 64-char hex string")
  else if Int64.compare payload.expiry_epoch (Int64.of_int current_epoch) <= 0 then
    Error ("invalid_circle_expiry_epoch", "expiry_epoch must be in the future")
  else
    Ok ()

let open_intent
    ~current_epoch
    ~(transport_policy : Circle_transport_policy.t)
    ~delivery_key_deliverable
    ~existing_intent
    (payload : Circles.outbox_open_payload) =
  match open_envelope ~current_epoch payload with
  | Error e -> Error e
  | Ok () ->
  if not delivery_key_deliverable then
    Error ("circle_delivery_key_inactive", "delivery key is not deliverable before intent expiry")
  else if
    match transport_policy.max_response_bytes with
    | Some max_response_bytes ->
      Int64.compare payload.max_response_bytes max_response_bytes > 0
    | None ->
      false
  then
    Error ("circle_transport_policy_violation", "max_response_bytes exceeds circle transport policy")
  else if existing_intent then
    Error ("circle_outbox_exists", "outbox intent already exists")
  else
    Ok {
      intent = {
        Circles.intent_id = payload.intent_id;
        created_epoch = Int64.of_int current_epoch;
        expiry_epoch = payload.expiry_epoch;
        relay_policy_hash = payload.relay_policy_hash;
        payload_hash = payload.payload_hash;
        ciphertext_blob_hash = payload.ciphertext_blob_hash;
        delivery_key_id = payload.delivery_key_id;
        max_response_bytes = payload.max_response_bytes;
        fee_budget = payload.fee_budget;
        route_hint = payload.route_hint;
        callback_policy_hash = payload.callback_policy_hash;
      };
    }

let relay_claim_envelope ~sender ~current_epoch (claim : Circles.relay_claim) =
  if String.length claim.intent_id = 0 then
    Error ("invalid_circle_intent_id", "intent_id must not be empty")
  else if sender <> claim.relay_id then
    Error ("circle_relay_claim_sender_mismatch", "claim relay_id must match tx sender")
  else if Int64.compare claim.claim_epoch (Int64.of_int current_epoch) <> 0 then
    Error ("circle_relay_claim_epoch_invalid", "claim_epoch must match the current epoch")
  else
    Ok ()

let relay_claim
    ~current_epoch
    ~transport_policy
    ~intent
    ~status
    ~delivery_key_deliverable
    ~existing_claims
    ~active_claims
    claim =
  match
    Circle_transport_machine.claim_admissible
      ~current_epoch
      ~transport_policy
      ~intent
      ~status
      ~delivery_key_deliverable
      ~existing_claims
      ~active_claims
      claim
  with
  | Error e -> Error e
  | Ok () ->
    Ok {
      claim;
      status = Circles.Claimed;
    }

let relay_cancel_envelope ~sender ~current_epoch (cancel : Circles.relay_cancel) =
  if String.length cancel.intent_id = 0 then
    Error ("invalid_circle_intent_id", "intent_id must not be empty")
  else if sender <> cancel.relay_id then
    Error ("circle_relay_cancel_sender_mismatch", "cancel relay_id must match tx sender")
  else if Int64.compare cancel.cancel_epoch (Int64.of_int current_epoch) <> 0 then
    Error ("circle_relay_cancel_epoch_invalid", "cancel_epoch must match the current epoch")
  else
    Ok ()

let relay_cancel
    ~current_epoch
    ~intent
    ~status
    ~(delivery_key_status : Circle_transport_state.delivery_key_status)
    ~existing_claims
    ~active_claims
    (cancel : Circles.relay_cancel) =
  match
    Circle_transport_machine.relay_cancel_admissible
      ~current_epoch
      ~intent
      ~status
      ~delivery_key_state:delivery_key_status.state
      ~existing_claims
      ~active_claims
      cancel
  with
  | Error e -> Error e
  | Ok () ->
    let claim_resolution =
      Circle_transport_resolution.make
        ?related_key_id:delivery_key_status.key_id
        ~actor_id:cancel.relay_id
        ~intent_id:cancel.intent_id
        ~resolved_epoch:(Int64.of_int current_epoch)
        cancel.reason in
    let remaining_active_claims =
      active_claims
      |> List.filter
           (fun (claim : Circles.relay_claim) ->
             not (String.equal claim.relay_id cancel.relay_id)) in
    let outbox_resolution =
      match remaining_active_claims with
      | [] -> Some claim_resolution
      | _ -> None in
    let status =
      match remaining_active_claims with
      | [] -> claim_resolution.status
      | _ -> Circles.Claimed in
    Ok {
      claim_resolution;
      outbox_resolution;
      status;
    }

let ingress_envelope ~sender (payload : Circles.ingress_commit_payload) =
  if String.length payload.intent_id = 0 then
    Error ("invalid_circle_intent_id", "intent_id must not be empty")
  else if sender <> payload.relay_id then
    Error ("circle_ingress_sender_mismatch", "ingress relay_id must match tx sender")
  else if String.length payload.response_payload_hash <> 64 then
    Error ("invalid_circle_response_payload_hash", "response_payload_hash must be a 64-char hex string")
  else if Int64.compare payload.response_size_bytes 0L < 0 then
    Error ("invalid_circle_response_size_bytes", "response_size_bytes must not be negative")
  else
    Ok ()

let ingress_commit
    ~current_epoch
    ~(transport_policy : Circle_transport_policy.t)
    ~(intent : Circles.outbox_intent)
    ~status
    ~(delivery_key_status : Circle_transport_state.delivery_key_status)
    ~active_claims
    ~existing_packet
    (payload : Circles.ingress_commit_payload) =
  if not (Circle_transport_policy.relay_allowed transport_policy payload.relay_id) then
    Error ("circle_transport_policy_violation", "relay is not allowed by circle transport policy")
  else if not (Circle_key_state.live delivery_key_status.state) then
    Error ("circle_delivery_key_inactive", "outbox delivery key is not live")
  else if
    transport_policy.require_response_ciphertext
    && Option.is_none payload.response_ciphertext_blob_hash
  then
    Error ("circle_transport_policy_violation", "circle transport policy requires response ciphertext")
  else if
    transport_policy.require_external_receipt
    && Option.is_none payload.external_receipt_hash
  then
    Error ("circle_transport_policy_violation", "circle transport policy requires external receipt")
  else if not (Circle_transport_policy.result_code_allowed transport_policy payload.result_code) then
    Error ("circle_transport_policy_violation", "result_code is not allowed by circle transport policy")
  else if Int64.compare payload.response_size_bytes intent.max_response_bytes > 0 then
    Error ("circle_transport_policy_violation", "response_size_bytes exceeds outbox intent limit")
  else if not (Circle_transport_policy.response_size_allowed transport_policy payload.response_size_bytes) then
    Error ("circle_transport_policy_violation", "response_size_bytes exceeds circle transport policy")
  else
    match
      Circle_transport_machine.ingress_admissible
        ~current_epoch
        ~transport_policy
        ~intent
        ~status
        ~active_claims
        payload
    with
    | Error e -> Error e
    | Ok () ->
      if existing_packet then
        Error ("circle_ingress_packet_exists", "ingress packet already exists")
      else
        let packet : Circles.ingress_packet = {
          intent_id = payload.intent_id;
          relay_id = payload.relay_id;
          ingress_nonce = payload.ingress_nonce;
          result_code = payload.result_code;
          response_payload_hash = payload.response_payload_hash;
          response_size_bytes = payload.response_size_bytes;
          response_ciphertext_blob_hash = payload.response_ciphertext_blob_hash;
          external_receipt_hash = payload.external_receipt_hash;
          signature = payload.signature;
        } in
        let resolution =
          Circle_transport_resolution.make
            ?related_key_id:delivery_key_status.key_id
            ~actor_id:payload.relay_id
            ~intent_id:payload.intent_id
            ~resolved_epoch:(Int64.of_int current_epoch)
            Circles.Fulfilled_delivery in
        Ok {
          packet;
          resolution;
          status = Circles.Fulfilled;
        }