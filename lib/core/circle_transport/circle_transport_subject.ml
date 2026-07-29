(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let relay_claim_subject ~circle_id ~(claim : Circles.relay_claim) =
  String.concat "|"
    [
      "octra_circle_relay_claim";
      circle_id;
      claim.intent_id;
      claim.relay_id;
      Int64.to_string claim.claim_epoch;
      Int64.to_string claim.claim_expiry_epoch;
    ]

let relay_cancel_subject ~circle_id ~(cancel : Circles.relay_cancel) =
  String.concat "|"
    [
      "octra_circle_relay_cancel";
      circle_id;
      cancel.intent_id;
      cancel.relay_id;
      Int64.to_string cancel.cancel_epoch;
      Circles.string_of_outbox_resolution_reason cancel.reason;
      Option.value ~default:"" cancel.related_key_id;
    ]

let ingress_packet_subject ~circle_id ~(packet : Circles.ingress_packet) =
  String.concat "|"
    [
      "octra_circle_ingress_commit";
      circle_id;
      packet.intent_id;
      packet.relay_id;
      Int64.to_string packet.ingress_nonce;
      string_of_int packet.result_code;
      packet.response_payload_hash;
      Int64.to_string packet.response_size_bytes;
      Option.value ~default:"" packet.response_ciphertext_blob_hash;
      Option.value ~default:"" packet.external_receipt_hash;
    ]

let ingress_payload_subject ~circle_id (payload : Circles.ingress_commit_payload) =
  ingress_packet_subject
    ~circle_id
    ~packet:{
      Circles.intent_id = payload.intent_id;
      relay_id = payload.relay_id;
      ingress_nonce = payload.ingress_nonce;
      result_code = payload.result_code;
      response_payload_hash = payload.response_payload_hash;
      response_size_bytes = payload.response_size_bytes;
      response_ciphertext_blob_hash = payload.response_ciphertext_blob_hash;
      external_receipt_hash = payload.external_receipt_hash;
      signature = payload.signature;
    }