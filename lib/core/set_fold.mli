(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

type cfg = {
  window : int64;
  challenge : int64;
  rejoin_span : int64;
  pulse_gap : int64;
  cadence : int64;
  delay : int64;
  max_members : int;
  minimum : Validator_participation.minimum;
}

type final = {
  epoch : int64;
  proposal_id : string;
  set_hash : string;
}

type proof = {
  vote : Octra_consensus.C_types.vote;
  commit : Octra_consensus.C_types.parent_commit;
}

type counts = {
  live : int;
  shadow : int;
  allowed : int;
}

type cap_mode = Reject | Prune

val meta_key : string
val standard : cfg
val participating : cfg
val empty : t
val validate_cfg : cfg -> (unit, string) result
val consensus_id : cfg -> string
val replacement_limit : cfg -> int64
val note_set :
  cfg ->
  epoch:int64 ->
  active:string list ->
  t ->
  (t, string) result
val lock :
  cfg ->
  active:string list ->
  t ->
  ((t * bool), string) result
val seats : t -> int option
val delay : cfg -> at:int64 -> t -> (t, string) result
val note_final :
  ?cap_mode:cap_mode ->
  cfg ->
  at:int64 ->
  active:string list ->
  final:final ->
  signers:string list ->
  t ->
  (t, string) result
val note_pulse :
  ?cap_mode:cap_mode ->
  cfg ->
  epoch:int64 ->
  active:bool ->
  address:string ->
  t ->
  (t, string) result
val apply_proof :
  ?cap_mode:cap_mode ->
  cfg ->
  chain_id:string ->
  epoch:int64 ->
  active:bool ->
  address:string ->
  proof ->
  t ->
  (t, string) result
val allows :
  cfg ->
  start:int64 ->
  source:int64 ->
  address:string ->
  t ->
  bool
val filter :
  cfg ->
  start:int64 ->
  source:int64 ->
  Validator_admission.candidate list ->
  t ->
  Validator_admission.candidate list * counts
val decode_proof :
  vote_b64:string ->
  commit_b64:string ->
  (proof, string) result
val read_parent :
  chain_id:string ->
  Octra_consensus.C_types.parent_commit ->
  ((final * string list * string list), string) result
val advance :
  ?cap_mode:cap_mode ->
  cfg ->
  chain_id:string ->
  start:int64 ->
  at:int64 ->
  parent:Octra_consensus.C_types.parent_commit option ->
  t ->
  ((t * string option * bool), string) result
val proof_of_message : string option -> (proof option, string) result
val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result
val to_string : t -> string
val of_string : string -> (t, string) result