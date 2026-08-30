(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

type slash_record = {
  id : string;
  offender : string;
  evidence_epoch : int64;
  applied_epoch : int64;
  amount : Z.t;
}

type bond_payload = {
  consensus_pubkey_b64 : string;
  proof_b64 : string;
}

type ready_payload = {
  consensus_pubkey_b64 : string;
  head_epoch : int64;
  state_root : string;
}

val meta_key : string
val escrow_address : string
val empty : t
val candidates : t -> Validator_admission.candidate list
val slashes : t -> slash_record list
val bond_message :
  chain_id:string ->
  address:string ->
  amount:Z.t ->
  nonce:int ->
  pubkey:string ->
  string
val verify_bond_proof :
  chain_id:string ->
  address:string ->
  amount:Z.t ->
  nonce:int ->
  bond_payload ->
  (string, string) result
val find : string -> t -> Validator_admission.candidate option
val apply_bond :
  Validator_admission.parameters ->
  chain_id:string ->
  epoch:int64 ->
  address:string ->
  sender_pubkey_b64:string ->
  amount:Z.t ->
  nonce:int ->
  bond_payload ->
  t ->
  (t, string) result
val mark_ready :
  epoch:int64 ->
  address:string ->
  consensus_pubkey_b64:string ->
  t ->
  (t, string) result
val request_exit :
  epoch:int64 ->
  address:string ->
  t ->
  (t, string) result
val withdraw :
  Validator_admission.parameters ->
  current_epoch:int64 ->
  active_addresses:string list ->
  address:string ->
  t ->
  ((t * Z.t), string) result
val apply_slash :
  current_epoch:int64 ->
  evidence_epochs:int64 ->
  Validator_evidence.verified ->
  t ->
  ((t * Z.t), string) result
val snapshot :
  Validator_admission.parameters ->
  activate_epoch:int64 ->
  t ->
  (Validator_admission.snapshot, string) result
val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result
val to_string : t -> string
val of_string : string -> (t, string) result
val bond_payload_of_message : string option -> (bond_payload, string) result
val ready_payload_of_message : string option -> (ready_payload, string) result