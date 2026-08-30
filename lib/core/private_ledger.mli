(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type kat =
  | Kat_missing
  | Kat_current
  | Kat_mismatch

type failure = {
  tag : string;
  reason : string;
  user_reason : string;
}

exception Worker_retry of string

type failure_action = Reject | Retry

type field_policy =
  | First_field
  | Unique_fields

val field_policy_of_mode : Rule_graph.mode -> field_policy

type balance_plan = {
  current_cipher : string;
  next_cipher : string;
}

type key_switch_plan = {
  old_key_hash : string;
  new_key_hash : string;
  new_pubkey : string;
  new_cipher : string option;
  source_cipher : string;
}

type key_switch_artifact

type private_artifact

type key_switch_apply = {
  old_key_hash : string;
  new_key_hash : string;
  migrated_cipher : bool;
}

type key_switch_rejection = {
  failure : failure;
  consume_nonce : bool;
}

type key_switch_outcome =
  | Key_switch_applied of key_switch_apply
  | Key_switch_rejected of key_switch_rejection

type stealth_plan = {
  stealth_current_cipher : string;
  stealth_next_cipher : string;
  stealth_delta_cipher : string;
  stealth_range_proof_delta : string;
  stealth_range_proof_balance : string;
  stealth_tag : string;
  stealth_eph_pub : string;
  stealth_enc_amount : string;
  stealth_claim_pub : string;
  stealth_amount_commitment : string;
}

type stealth_range = {
  delta_ok : bool;
  balance_ok : bool;
}

type claim_plan = {
  claim_output_id : int;
  claim_cipher : string;
}

type prepared =
  | Prepared_encrypt of balance_plan
  | Prepared_decrypt of balance_plan
  | Prepared_key_switch of key_switch_plan
  | Prepared_stealth of stealth_plan
  | Prepared_claim of claim_plan * balance_plan

type private_rejection = {
  private_error : failure;
  private_preverify_reason : string;
}

type key_switch_binding =
  | Key_switch_bound of prepared
  | Key_switch_source_changed
  | Key_switch_artifact_invalid of failure

type private_binding =
  | Private_bound of prepared
  | Private_source_changed
  | Private_artifact_invalid of private_rejection

val hash_prepared : prepared -> string

val key_bound_stealth_output_marker : string

val stealth_output_is_key_bound :
  Ledger_types.stealth_output ->
  bool

val claim_gate :
  string ->
  Ledger_types.stealth_output ->
  Crypto.StealthClaimV5.t ->
  (unit, failure) result

val kat_state : Ledger.t -> string -> kat

val backfill_kat : Ledger.t -> string -> unit

val encrypt_plan :
  field_policy:field_policy ->
  ?worker_priority:Compute_pool.priority ->
  ?result_policy:Private_result_policy.t ->
  Ledger.t ->
  Transaction.t ->
  (balance_plan, failure) result Lwt.t

val prepare_encrypt_plan :
  field_policy:field_policy ->
  ?result_policy:Private_result_policy.t ->
  Ledger.t ->
  Transaction.t ->
  (balance_plan, failure) result Lwt.t

val decrypt_plan :
  field_policy:field_policy ->
  ?worker_priority:Compute_pool.priority ->
  ?result_policy:Private_result_policy.t ->
  Ledger.t ->
  Transaction.t ->
  (balance_plan, failure) result Lwt.t

val prepare_decrypt_plan :
  field_policy:field_policy ->
  ?result_policy:Private_result_policy.t ->
  Ledger.t ->
  Transaction.t ->
  (balance_plan, failure) result Lwt.t

val apply_encrypt_plan :
  Ledger.t ->
  Transaction.t ->
  balance_plan ->
  (balance_plan, failure) result Lwt.t

val apply_decrypt_plan :
  Ledger.t ->
  Transaction.t ->
  balance_plan ->
  (balance_plan, failure) result Lwt.t

val apply_encrypt :
  field_policy:field_policy ->
  ?result_policy:Private_result_policy.t ->
  Ledger.t ->
  Transaction.t ->
  (balance_plan, failure) result Lwt.t

val apply_decrypt :
  field_policy:field_policy ->
  ?result_policy:Private_result_policy.t ->
  Ledger.t ->
  Transaction.t ->
  (balance_plan, failure) result Lwt.t

val key_switch_plan :
  field_policy:field_policy ->
  ?legacy_public_replay:Pvac_legacy_public_replay.decision ->
  Ledger.t ->
  Transaction.t ->
  (key_switch_plan, failure) result Lwt.t

val preverify_key_switch_artifact :
  field_policy:field_policy ->
  ?worker_priority:Compute_pool.priority ->
  Ledger.t ->
  Transaction.t ->
  (key_switch_artifact, failure) result Lwt.t

val key_switch_failure_retryable : failure -> bool

val private_failure_retryable : failure -> bool

val failure_action : failure -> failure_action

val bind_key_switch_artifact :
  field_policy:field_policy ->
  Ledger.t ->
  Transaction.t ->
  key_switch_artifact ->
  key_switch_binding Lwt.t

val preverify_private_artifact :
  field_policy:field_policy ->
  ?worker_priority:Compute_pool.priority ->
  ?result_policy:Private_result_policy.t ->
  Ledger.t ->
  Transaction.t ->
  (private_artifact, failure) result Lwt.t

val verify_private :
  field_policy:field_policy ->
  ?worker_priority:Compute_pool.priority ->
  ?result_policy:Private_result_policy.t ->
  Ledger.t ->
  Transaction.t ->
  (prepared, private_rejection) result Lwt.t

val bind_private_artifact :
  field_policy:field_policy ->
  ?result_policy:Private_result_policy.t ->
  Ledger.t ->
  Transaction.t ->
  private_artifact ->
  private_binding Lwt.t

val prepare_key_switch_plan :
  field_policy:field_policy ->
  Ledger.t ->
  Transaction.t ->
  (key_switch_plan, failure) result Lwt.t

val key_switch_requests_legacy_public_migration :
  field_policy:field_policy ->
  Transaction.t ->
  bool

val key_switch_requests_historical_owner_proof :
  field_policy:field_policy ->
  Transaction.t ->
  bool

val key_switch_requests_legacy_audit :
  field_policy:field_policy ->
  Transaction.t ->
  bool

val apply_key_switch :
  field_policy:field_policy ->
  ?legacy_public_replay:Pvac_legacy_public_replay.decision ->
  Ledger.t ->
  Transaction.t ->
  key_switch_outcome Lwt.t

val apply_key_switch_plan :
  Ledger.t ->
  Transaction.t ->
  key_switch_plan ->
  key_switch_outcome Lwt.t

val stealth_plan :
  field_policy:field_policy ->
  ?worker_priority:Compute_pool.priority ->
  ?result_policy:Private_result_policy.t ->
  Ledger.t ->
  Transaction.t ->
  (stealth_plan, failure) result Lwt.t

val prepare_stealth_plan :
  field_policy:field_policy ->
  ?worker_priority:Compute_pool.priority ->
  ?result_policy:Private_result_policy.t ->
  Ledger.t ->
  Transaction.t ->
  (stealth_plan, failure) result Lwt.t

val stealth_inline_range :
  ?worker_priority:Compute_pool.priority ->
  Ledger.t ->
  Transaction.t ->
  stealth_plan ->
  (stealth_range, failure) result Lwt.t

val stealth_accept_range :
  stealth_range ->
  (unit, failure) result

val stealth_binding :
  field_policy:field_policy ->
  ?worker_priority:Compute_pool.priority ->
  Ledger.t ->
  Transaction.t ->
  stealth_plan ->
  (unit, failure) result Lwt.t

val claim_plan :
  field_policy:field_policy ->
  ?worker_priority:Compute_pool.priority ->
  Ledger.t ->
  Transaction.t ->
  (claim_plan, failure) result Lwt.t

val prepare_claim_plan :
  field_policy:field_policy ->
  Ledger.t ->
  Transaction.t ->
  (claim_plan, failure) result Lwt.t

val claim_balance_plan :
  ?worker_priority:Compute_pool.priority ->
  ?result_policy:Private_result_policy.t ->
  Ledger.t ->
  Transaction.t ->
  claim_plan ->
  (balance_plan, failure) result Lwt.t