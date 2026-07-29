(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type reason =
  | Disabled
  | Capacity
  | ChainMismatch
  | EpochTooOld
  | EpochTooNew
  | MissingChallenge
  | Malformed
  | BadScore
  | UnknownNode
  | BadSignature
  | ConflictingIdentity

type decision =
  | Accept
  | Reject of reason
  | Quarantine of reason

type window = {
  current_epoch : int64;
  fraud_window : int64;
  future_window : int64;
}

type evidence = {
  node_id : string;
  epoch_id : int64;
  reason : reason;
  attestation_id : string;
  observed_epoch : int64;
}

type pool = {
  accepted : (string, Resource_attestations.attestation) Hashtbl.t;
  identity_index : (string, string) Hashtbl.t;
  rejected : (string, evidence) Hashtbl.t;
  quarantined : (string, evidence) Hashtbl.t;
}

let max_accepted = 4_096
let max_evidence = 4_096

let create_pool () =
  {
    accepted = Hashtbl.create 256;
    identity_index = Hashtbl.create 256;
    rejected = Hashtbl.create 64;
    quarantined = Hashtbl.create 64;
  }

let reason_to_string = function
  | Disabled -> "disabled"
  | Capacity -> "capacity"
  | ChainMismatch -> "chain_mismatch"
  | EpochTooOld -> "epoch_too_old"
  | EpochTooNew -> "epoch_too_new"
  | MissingChallenge -> "missing_challenge"
  | Malformed -> "malformed"
  | BadScore -> "bad_score"
  | UnknownNode -> "unknown_node"
  | BadSignature -> "bad_signature"
  | ConflictingIdentity -> "conflicting_identity"

let decision_to_string = function
  | Accept -> "accept"
  | Reject reason -> "reject:" ^ reason_to_string reason
  | Quarantine reason -> "quarantine:" ^ reason_to_string reason

let identity_key (attestation : Resource_attestations.attestation) =
  String.concat "|"
    [
      attestation.node_id;
      Int64.to_string attestation.epoch_id;
      string_of_int (Resource_attestations.attestation_kind_to_u8 attestation.kind);
    ]

let evidence_id evidence =
  Octra_net.Hash_domain.hash_encoded "octra:resource_admission_evidence:v1" (fun buf ->
    Octra_net.Oce1.put_string buf evidence.node_id;
    Octra_net.Oce1.put_u64 buf evidence.epoch_id;
    Octra_net.Oce1.put_string buf (reason_to_string evidence.reason);
    Octra_net.Oce1.put_hash32 buf evidence.attestation_id;
    Octra_net.Oce1.put_u64 buf evidence.observed_epoch)

let evidence ~window reason (attestation : Resource_attestations.attestation) =
  {
    node_id = attestation.node_id;
    epoch_id = attestation.epoch_id;
    reason;
    attestation_id = Resource_attestations.attestation_id attestation;
    observed_epoch = window.current_epoch;
  }

let classify
    ~chain_id
    ~challenge
    ~window
    ~pubkey_of_node
    pool
    (attestation : Resource_attestations.attestation) =
  let attestation_id = Resource_attestations.attestation_id attestation in
  if Hashtbl.mem pool.accepted attestation_id then Accept
  else if Hashtbl.length pool.accepted >= max_accepted then Reject Capacity
  else if attestation.chain_id <> chain_id then Reject ChainMismatch
  else if attestation.epoch_id < Int64.sub window.current_epoch window.fraud_window then Reject EpochTooOld
  else if attestation.epoch_id > Int64.add window.current_epoch window.future_window then Reject EpochTooNew
  else if not (Resource_attestations.is_well_formed attestation) then Reject Malformed
  else if attestation.score <> Resource_attestations.attestation_score ~challenge attestation then Reject BadScore
  else
    match pubkey_of_node attestation.node_id with
    | None -> Quarantine UnknownNode
    | Some pubkey
      when not (Resource_attestations.verify_attestation_signature ~pubkey_raw:pubkey attestation) ->
        Reject BadSignature
    | Some _ ->
        match Hashtbl.find_opt pool.identity_index (identity_key attestation) with
        | Some existing_id when existing_id <> attestation_id -> Quarantine ConflictingIdentity
        | _ -> Accept

let remember pool ~window decision attestation =
  let attestation_id = Resource_attestations.attestation_id attestation in
  match decision with
  | Accept ->
      Hashtbl.replace pool.accepted attestation_id attestation;
      Hashtbl.replace pool.identity_index (identity_key attestation) attestation_id
  | Reject reason ->
      let record = evidence ~window reason attestation in
      if Hashtbl.length pool.rejected < max_evidence then
        Hashtbl.replace pool.rejected (evidence_id record) record
  | Quarantine reason ->
      let record = evidence ~window reason attestation in
      if Hashtbl.length pool.quarantined < max_evidence then
        Hashtbl.replace pool.quarantined (evidence_id record) record

let ingest ~chain_id ~challenge ~window ~pubkey_of_node pool attestation =
  let decision = classify ~chain_id ~challenge ~window ~pubkey_of_node pool attestation in
  remember pool ~window decision attestation;
  decision

let accepted pool =
  Hashtbl.fold (fun _ attestation acc -> attestation :: acc) pool.accepted []

let rejected pool =
  Hashtbl.fold (fun _ evidence acc -> evidence :: acc) pool.rejected []

let quarantined pool =
  Hashtbl.fold (fun _ evidence acc -> evidence :: acc) pool.quarantined []