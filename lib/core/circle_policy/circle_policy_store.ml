(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let read_first_inline_value store circle_id keys =
  let rec loop = function
    | [] -> Lwt.return_none
    | key :: rest ->
      let open Lwt.Syntax in
      let* value_opt = Store_irmin.read_circle_stable_inline_value store circle_id key in
      begin
        match value_opt with
        | Some _ -> Lwt.return value_opt
        | None -> loop rest
      end
  in
  loop keys

let load_transport_policy store circle_id =
  let open Lwt.Syntax in
  let* relay_mode_raw =
    read_first_inline_value store circle_id [Circle_transport_policy.relay_mode_key] in
  let* lease_class_raw =
    read_first_inline_value store circle_id [Circle_transport_policy.lease_class_key] in
  let* claim_strategy_raw =
    read_first_inline_value store circle_id [Circle_transport_policy.claim_strategy_key] in
  let* claim_topology_raw =
    read_first_inline_value store circle_id [Circle_transport_policy.claim_topology_key] in
  let* quorum_mode_raw =
    read_first_inline_value store circle_id [Circle_transport_policy.quorum_mode_key] in
  let* ingress_strategy_raw =
    read_first_inline_value store circle_id [Circle_transport_policy.ingress_strategy_key] in
  let* quorum_threshold_raw =
    read_first_inline_value store circle_id [Circle_transport_policy.quorum_threshold_key] in
  let* quorum_weight_threshold_raw =
    read_first_inline_value store circle_id [Circle_transport_policy.quorum_weight_threshold_key] in
  let* max_active_claims_raw =
    read_first_inline_value store circle_id [Circle_transport_policy.max_active_claims_key] in
  let* relay_allowlist_raw =
    read_first_inline_value store circle_id [Circle_transport_policy.relay_allowlist_key] in
  let* relay_weights_raw =
    read_first_inline_value store circle_id [Circle_transport_policy.relay_weights_key] in
  let* max_claim_window_raw =
    read_first_inline_value store circle_id [Circle_transport_policy.max_claim_window_key] in
  let* max_response_bytes_raw =
    read_first_inline_value store circle_id [Circle_transport_policy.max_response_bytes_key] in
  let* require_response_ciphertext_raw =
    read_first_inline_value store circle_id [Circle_transport_policy.require_response_ciphertext_key] in
  let* require_external_receipt_raw =
    read_first_inline_value store circle_id [Circle_transport_policy.require_external_receipt_key] in
  let* accepted_result_codes_raw =
    read_first_inline_value store circle_id [Circle_transport_policy.accepted_result_codes_key] in
  Lwt.return
    (Circle_transport_policy.of_stored_values
       ~relay_mode_raw
       ~lease_class_raw
       ~claim_strategy_raw
       ~claim_topology_raw
       ~quorum_mode_raw
       ~ingress_strategy_raw
       ~quorum_threshold_raw
       ~quorum_weight_threshold_raw
       ~max_active_claims_raw
       ~relay_allowlist_raw
       ~relay_weights_raw
       ~max_claim_window_raw
       ~max_response_bytes_raw
       ~require_response_ciphertext_raw
       ~require_external_receipt_raw
       ~accepted_result_codes_raw)

let load_hfhe_policy store circle_id =
  let open Lwt.Syntax in
  let* load_pk_mode_raw =
    read_first_inline_value store circle_id [Circle_hfhe_policy.load_pk_mode_key] in
  let* encrypt_mode_raw =
    read_first_inline_value store circle_id [Circle_hfhe_policy.encrypt_mode_key] in
  let* decrypt_mode_raw =
    read_first_inline_value store circle_id [Circle_hfhe_policy.decrypt_mode_key] in
  let* cipher_arithmetic_mode_raw =
    read_first_inline_value store circle_id [Circle_hfhe_policy.cipher_arithmetic_mode_key] in
  let* commit_mode_raw =
    read_first_inline_value store circle_id [Circle_hfhe_policy.commit_mode_key] in
  let* pedersen_mode_raw =
    read_first_inline_value store circle_id [Circle_hfhe_policy.pedersen_mode_key] in
  let* cipher_serde_mode_raw =
    read_first_inline_value store circle_id [Circle_hfhe_policy.cipher_serde_mode_key] in
  let* pubkey_serde_mode_raw =
    read_first_inline_value store circle_id [Circle_hfhe_policy.pubkey_serde_mode_key] in
  let* verify_zero_mode_raw =
    read_first_inline_value store circle_id [Circle_hfhe_policy.verify_zero_mode_key] in
  let* verify_range_mode_raw =
    read_first_inline_value store circle_id [Circle_hfhe_policy.verify_range_mode_key] in
  let* verify_bound_mode_raw =
    read_first_inline_value store circle_id [Circle_hfhe_policy.verify_bound_mode_key] in
  let* proof_receipt_signer_mode_raw =
    read_first_inline_value store circle_id [Circle_hfhe_policy.proof_receipt_signer_mode_key] in
  let* proof_receipt_class_raw =
    read_first_inline_value store circle_id [Circle_hfhe_policy.proof_receipt_class_key] in
  let* pk_allowlist_raw =
    read_first_inline_value store circle_id [Circle_hfhe_policy.pk_allowlist_key] in
  let* require_live_key_policy_raw =
    read_first_inline_value store circle_id [Circle_hfhe_policy.require_live_key_policy_key] in
  let* require_receipt_transport_binding_raw =
    read_first_inline_value store circle_id [Circle_hfhe_policy.require_receipt_transport_binding_key] in
  let* encrypt_proof_raw =
    read_first_inline_value store circle_id [Circle_hfhe_policy.encrypt_proof_key] in
  let* decrypt_proof_raw =
    read_first_inline_value store circle_id [Circle_hfhe_policy.decrypt_proof_key] in
  Lwt.return
    (Circle_hfhe_policy.of_stored_values
       ~load_pk_mode_raw
       ~encrypt_mode_raw
       ~decrypt_mode_raw
       ~cipher_arithmetic_mode_raw
       ~commit_mode_raw
       ~pedersen_mode_raw
       ~cipher_serde_mode_raw
       ~pubkey_serde_mode_raw
       ~verify_zero_mode_raw
       ~verify_range_mode_raw
       ~verify_bound_mode_raw
       ~proof_receipt_signer_mode_raw
       ~proof_receipt_class_raw
       ~pk_allowlist_raw
       ~require_live_key_policy_raw
       ~require_receipt_transport_binding_raw
       ~encrypt_proof_raw
       ~decrypt_proof_raw)

let load_key_policy store circle_id key_id =
  let open Lwt.Syntax in
  let* activate_after_raw =
    read_first_inline_value store circle_id [Circle_key_policy.activate_after_key key_id] in
  let* expire_after_raw =
    read_first_inline_value store circle_id [Circle_key_policy.expire_after_key key_id] in
  let* revoked_raw =
    read_first_inline_value store circle_id [Circle_key_policy.revoked_key key_id] in
  let* erased_raw =
    read_first_inline_value store circle_id [Circle_key_policy.erased_key key_id] in
  Lwt.return
    (Circle_key_policy.of_stored_values
       ~activate_after_raw
       ~expire_after_raw
       ~revoked_raw
       ~erased_raw)