(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type commitment = {
  chain_id : string;
  offer_id : string;
  commit_epoch : int64;
  node_id : string;
  graph_root : string;
  model_root : string;
  program_root : string;
  executor_root : string;
  nonce_hash : string;
  signature : string;
}

type reveal = {
  chain_id : string;
  offer_id : string;
  commit_epoch : int64;
  reveal_epoch : int64;
  node_id : string;
  graph_root : string;
  model_root : string;
  program_root : string;
  executor_root : string;
  nonce : string;
  evidence_root : string;
  signature : string;
}

type reject =
  | Malformed
  | Chain_mismatch
  | Offer_mismatch
  | Epoch_mismatch
  | Identity_mismatch
  | Model_mismatch
  | Program_mismatch
  | Nonce_mismatch
  | Commitment_signature
  | Reveal_signature
  | Evidence

type member = {
  node_id : string;
  graph_root : string;
  model_root : string;
  program_root : string;
  executor_root : string;
  evidence_root : string;
  score : string;
}

type selection = {
  offer_id : string;
  target_epoch : int64;
  challenge : string;
  members : member list;
  root : string;
}

val nonce_hash : string -> string
val reveal_is_well_formed : reveal -> bool
val commitment_sign_bytes : commitment -> string
val commitment_id : commitment -> string
val reveal_sign_bytes : commitment -> reveal -> string
val reveal_id : commitment -> reveal -> string
val challenge : chain_id:string -> source_epoch:int64 -> finality_hash:string -> string
val validate :
  chain_id:string ->
  target_epoch:int64 ->
  minimum_delay:int64 ->
  verify_commitment_signature:(commitment -> bool) ->
  verify_reveal_signature:(commitment -> reveal -> bool) ->
  verify_evidence:(reveal -> bool) ->
  commitment ->
  reveal ->
  (reveal, reject) result
val select :
  chain_id:string ->
  offer_id:string ->
  target_epoch:int64 ->
  challenge:string ->
  minimum_delay:int64 ->
  committee_size:int ->
  verify_commitment_signature:(commitment -> bool) ->
  verify_reveal_signature:(commitment -> reveal -> bool) ->
  verify_evidence:(reveal -> bool) ->
  commitments:commitment list ->
  reveals:reveal list ->
  selection