(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type accelerator =
  | Cpu
  | Cuda
  | Metal
  | Rocm

type t = {
  chain_id : string;
  observed_epoch : int64;
  valid_until : int64;
  validator_set_root : string;
  node_id : string;
  circle_id : string;
  model_epoch : int64;
  model_state_root : string;
  graph_root : string;
  model_root : string;
  program_root : string;
  executor_root : string;
  evidence_root : string;
  accelerator : accelerator;
  lanes : int;
  memory_bytes : int64;
  max_request_bytes : int;
  max_response_bytes : int;
  signature : string;
}

type reject =
  | Malformed
  | Chain_mismatch
  | Epoch_mismatch
  | Not_validator
  | Signature
  | Evidence

val accelerator_name : accelerator -> string
val accelerator_code : accelerator -> int
val accelerator_of_code : int -> accelerator
val sign_bytes : t -> string
val id : t -> string
val validate :
  chain_id:string ->
  current_epoch:int64 ->
  max_ttl:int64 ->
  is_validator:(string -> bool) ->
  verify_signature:(t -> bool) ->
  verify_evidence:(t -> bool) ->
  t ->
  (t, reject) result