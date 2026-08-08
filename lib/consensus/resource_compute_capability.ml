(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let max_chain_id_bytes = 128
let max_node_id_bytes = 128
let max_lanes = 8
let max_memory_bytes = 1_099_511_627_776L
let max_request_bytes = 1_048_576
let max_response_bytes = 1_048_576

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

let accelerator_code = function
  | Cpu -> 0
  | Cuda -> 1
  | Metal -> 2
  | Rocm -> 3

let accelerator_name = function
  | Cpu -> "cpu"
  | Cuda -> "cuda"
  | Metal -> "metal"
  | Rocm -> "rocm"

let accelerator_of_code = function
  | 0 -> Cpu
  | 1 -> Cuda
  | 2 -> Metal
  | 3 -> Rocm
  | _ -> invalid_arg "resource compute accelerator"

let hash32 value =
  String.length value = 32

let bounded_non_empty limit value =
  let length = String.length value in
  length > 0 && length <= limit

let is_well_formed capability =
  bounded_non_empty max_chain_id_bytes capability.chain_id
  && capability.observed_epoch >= 0L
  && capability.valid_until >= capability.observed_epoch
  && hash32 capability.validator_set_root
  && bounded_non_empty max_node_id_bytes capability.node_id
  && bounded_non_empty max_node_id_bytes capability.circle_id
  && capability.model_epoch >= 0L
  && hash32 capability.model_state_root
  && hash32 capability.graph_root
  && hash32 capability.model_root
  && hash32 capability.program_root
  && hash32 capability.executor_root
  && hash32 capability.evidence_root
  && capability.lanes > 0
  && capability.lanes <= max_lanes
  && capability.memory_bytes > 0L
  && capability.memory_bytes <= max_memory_bytes
  && capability.max_request_bytes > 0
  && capability.max_request_bytes <= max_request_bytes
  && capability.max_response_bytes > 0
  && capability.max_response_bytes <= max_response_bytes
  && String.length capability.signature = 64

let sign_bytes capability =
  if not (is_well_formed capability) then
    invalid_arg "resource compute capability";
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_capability" (fun buf ->
    Octra_net.Oce1.put_string buf capability.chain_id;
    Octra_net.Oce1.put_u64 buf capability.observed_epoch;
    Octra_net.Oce1.put_u64 buf capability.valid_until;
    Octra_net.Oce1.put_hash32 buf capability.validator_set_root;
    Octra_net.Oce1.put_string buf capability.node_id;
    Octra_net.Oce1.put_string buf capability.circle_id;
    Octra_net.Oce1.put_u64 buf capability.model_epoch;
    Octra_net.Oce1.put_hash32 buf capability.model_state_root;
    Octra_net.Oce1.put_hash32 buf capability.graph_root;
    Octra_net.Oce1.put_hash32 buf capability.model_root;
    Octra_net.Oce1.put_hash32 buf capability.program_root;
    Octra_net.Oce1.put_hash32 buf capability.executor_root;
    Octra_net.Oce1.put_hash32 buf capability.evidence_root;
    Octra_net.Oce1.put_u8 buf (accelerator_code capability.accelerator);
    Octra_net.Oce1.put_u16 buf capability.lanes;
    Octra_net.Oce1.put_u64 buf capability.memory_bytes;
    Octra_net.Oce1.put_u32_int buf capability.max_request_bytes;
    Octra_net.Oce1.put_u32_int buf capability.max_response_bytes)

let id capability =
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_capability_id" (fun buf ->
    Octra_net.Oce1.put_hash32 buf (sign_bytes capability);
    Octra_net.Oce1.put_sig64 buf capability.signature)

let protected_bool evaluate =
  try evaluate () with _ -> false

let validate
    ~chain_id
    ~current_epoch
    ~max_ttl
    ~is_validator
    ~verify_signature
    ~verify_evidence
    capability =
  if not (is_well_formed capability) then
    Error Malformed
  else if capability.chain_id <> chain_id then
    Error Chain_mismatch
  else if
    max_ttl <= 0L
    || current_epoch < capability.observed_epoch
    || current_epoch > capability.valid_until
    || Int64.sub capability.valid_until capability.observed_epoch > max_ttl
  then
    Error Epoch_mismatch
  else if not (protected_bool (fun () -> is_validator capability.node_id)) then
    Error Not_validator
  else if not (protected_bool (fun () -> verify_signature capability)) then
    Error Signature
  else if not (protected_bool (fun () -> verify_evidence capability)) then
    Error Evidence
  else
    Ok capability