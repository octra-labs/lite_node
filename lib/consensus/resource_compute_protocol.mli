(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type offer = {
  chain_id : string;
  offered_epoch : int64;
  expires_epoch : int64;
  coordinator : string;
  validator_set_root : string;
  circle_id : string;
  model_epoch : int64;
  model_state_root : string;
  graph_root : string;
  model_root : string;
  program_root : string;
  executor_root : string;
  min_memory_bytes : int64;
  request_bytes : int;
  response_bytes : int;
  signature : string;
}

type call = {
  request : Resource_compute_certificate.request;
  method_name : string;
  params_json : string;
  caller_public_key : string;
  caller_signature : string;
  coordinator : string;
  signature : string;
}

type result = {
  vote : Resource_compute_certificate.vote;
  output_json : string;
}

type message =
  | Offer of offer
  | Capability of Resource_compute_capability.t
  | Commitment of Resource_compute_selection.commitment
  | Reveal of Resource_compute_selection.reveal
  | Call of call
  | Result of result

val offer_sign_bytes : offer -> string
val offer_id : offer -> string
val input_hash : method_name:string -> params_json:string -> string
val intent_sign_bytes : call -> string
val call_sign_bytes : call -> string
val output_hash : string -> string
val offer_shape : offer -> bool
val call_shape : call -> bool
val result_shape : result -> bool
val encode : message -> string
val decode : string -> message