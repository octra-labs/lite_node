(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type cancellation = {
  chain_id : string;
  caller : string;
  caller_public_key : string;
  session_id : string;
  caller_signature : string;
  until : int64 option;
}

type cancel_round = {
  cancel : cancellation;
  origin : string;
  epoch : int64;
  expires : int64;
  round : int;
  signature : string;
}

type cancel_ack = {
  delivery : cancel_round;
  origin_key : string;
  node_id : string;
  node_key : string;
  epoch : int64;
  signature : string;
}

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

type header = private {
  request : Resource_compute_certificate.request;
  valid_until : int64 option;
  caller_public_key : string;
  caller_signature : string;
  coordinator : string;
  signature : string;
}

type call = {
  request : Resource_compute_certificate.request;
  valid_until : int64 option;
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

type fetch = {
  call : call;
  target : string;
  epoch_id : int64;
  expires_epoch : int64;
  round : int;
  signature : string;
}

type message =
  | Offer of offer
  | Capability of Resource_compute_capability.t
  | Commitment of Resource_compute_selection.commitment
  | Reveal of Resource_compute_selection.reveal
  | Call of call
  | Result of result
  | Cancel of cancellation
  | Cancel_round of cancel_round
  | Cancel_ack of cancel_ack
  | Fetch of fetch
  | Answer of fetch * result

val max_members : int
val max_records : int
val reveal_delay : int64
val offer_ttl : int64
val call_ttl : int64
val capacity_ttl : int64
val fetch_rounds : int
val fetch_span : int64
val fetch_shape : fetch -> bool
val fetch_sign_bytes : fetch -> string

val cancel_shape : cancellation -> bool
val cancel_sign_bytes : cancellation -> string
val cancel_rounds : int
val cancel_span : int64
val cancel_round_shape : cancel_round -> bool
val cancel_round_sign : cancel_round -> string
val cancel_ack_shape : cancel_ack -> bool
val cancel_ack_sign : cancel_ack -> string

val offer_sign_bytes : offer -> string
val offer_id : offer -> string
val input_hash : method_name:string -> params_json:string -> string
val intent_sign_bytes : call -> string
val call_sign_bytes : call -> string
val header : call -> (header, string) Stdlib.result
val header_intent : header -> string
val header_sign : header -> string
val header_id : header -> string
val intent_id : call -> string
val encode_header : header -> string
val decode_header : string -> (header, string) Stdlib.result
val output_hash : string -> string
val offer_shape : offer -> bool
val call_shape : call -> bool
val result_shape : result -> bool
val encode : message -> string
val decode : string -> message