(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Capability = Resource_compute_capability
module Selection = Resource_compute_selection
module Certificate = Resource_compute_certificate

let max_chain_id_bytes = 128
let max_node_id_bytes = 128
let max_circle_id_bytes = 128
let max_method_bytes = 64
let max_params_bytes = 65_536
let max_output_bytes = 262_144
let max_nonce_bytes = 128

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
  request : Certificate.request;
  method_name : string;
  params_json : string;
  caller_public_key : string;
  caller_signature : string;
  coordinator : string;
  signature : string;
}

type result = {
  vote : Certificate.vote;
  output_json : string;
}

type message =
  | Offer of offer
  | Capability of Capability.t
  | Commitment of Selection.commitment
  | Reveal of Selection.reveal
  | Call of call
  | Result of result

let hash32 value =
  String.length value = 32

let signature64 value =
  String.length value = 64

let bounded_non_empty limit value =
  let length = String.length value in
  length > 0 && length <= limit

let offer_shape offer =
  bounded_non_empty max_chain_id_bytes offer.chain_id
  && offer.offered_epoch >= 0L
  && offer.expires_epoch >= offer.offered_epoch
  && bounded_non_empty max_node_id_bytes offer.coordinator
  && hash32 offer.validator_set_root
  && bounded_non_empty max_circle_id_bytes offer.circle_id
  && offer.model_epoch >= 0L
  && hash32 offer.model_state_root
  && hash32 offer.graph_root
  && hash32 offer.model_root
  && hash32 offer.program_root
  && hash32 offer.executor_root
  && offer.min_memory_bytes > 0L
  && offer.request_bytes > 0
  && offer.request_bytes <= 1_048_576
  && offer.response_bytes > 0
  && offer.response_bytes <= 1_048_576
  && signature64 offer.signature

let offer_sign_bytes offer =
  if not (offer_shape offer) then
    invalid_arg "resource compute offer";
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_offer" (fun buf ->
    Octra_net.Oce1.put_string buf offer.chain_id;
    Octra_net.Oce1.put_u64 buf offer.offered_epoch;
    Octra_net.Oce1.put_u64 buf offer.expires_epoch;
    Octra_net.Oce1.put_string buf offer.coordinator;
    Octra_net.Oce1.put_hash32 buf offer.validator_set_root;
    Octra_net.Oce1.put_string buf offer.circle_id;
    Octra_net.Oce1.put_u64 buf offer.model_epoch;
    Octra_net.Oce1.put_hash32 buf offer.model_state_root;
    Octra_net.Oce1.put_hash32 buf offer.graph_root;
    Octra_net.Oce1.put_hash32 buf offer.model_root;
    Octra_net.Oce1.put_hash32 buf offer.program_root;
    Octra_net.Oce1.put_hash32 buf offer.executor_root;
    Octra_net.Oce1.put_u64 buf offer.min_memory_bytes;
    Octra_net.Oce1.put_u32_int buf offer.request_bytes;
    Octra_net.Oce1.put_u32_int buf offer.response_bytes)

let offer_id offer =
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_offer_id" (fun buf ->
    Octra_net.Oce1.put_hash32 buf (offer_sign_bytes offer);
    Octra_net.Oce1.put_sig64 buf offer.signature)

let input_hash ~method_name ~params_json =
  if
    not (bounded_non_empty max_method_bytes method_name)
    || String.length params_json > max_params_bytes
  then
    invalid_arg "resource compute input";
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_input" (fun buf ->
    Octra_net.Oce1.put_string buf method_name;
    Octra_net.Oce1.put_string buf params_json)

let call_shape call =
  bounded_non_empty max_method_bytes call.method_name
  && String.length call.params_json <= max_params_bytes
  && hash32 call.caller_public_key
  && signature64 call.caller_signature
  && bounded_non_empty max_node_id_bytes call.coordinator
  && signature64 call.signature
  && call.request.input_hash = input_hash ~method_name:call.method_name ~params_json:call.params_json

let intent_sign_bytes call =
  if not (call_shape call) then
    invalid_arg "resource compute call";
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_intent" (fun buf ->
    Octra_net.Oce1.put_string buf call.request.chain_id;
    Octra_net.Oce1.put_string buf call.request.circle_id;
    Octra_net.Oce1.put_u64 buf call.request.model_epoch;
    Octra_net.Oce1.put_hash32 buf call.request.model_state_root;
    Octra_net.Oce1.put_hash32 buf call.request.graph_root;
    Octra_net.Oce1.put_hash32 buf call.request.model_root;
    Octra_net.Oce1.put_hash32 buf call.request.program_root;
    Octra_net.Oce1.put_hash32 buf call.request.executor_root;
    Octra_net.Oce1.put_string buf call.request.caller;
    Octra_net.Oce1.put_hash32 buf call.caller_public_key;
    Octra_net.Oce1.put_hash32 buf call.request.session_id;
    Octra_net.Oce1.put_u64 buf call.request.sequence;
    Octra_net.Oce1.put_hash32 buf call.request.input_hash;
    Octra_net.Oce1.put_u32_int buf call.request.input_bytes;
    Octra_net.Oce1.put_u32_int buf call.request.max_output_bytes)

let call_sign_bytes call =
  if not (call_shape call) then
    invalid_arg "resource compute call";
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_call" (fun buf ->
    Octra_net.Oce1.put_hash32 buf (Certificate.request_id call.request);
    Octra_net.Oce1.put_string buf call.coordinator;
    Octra_net.Oce1.put_hash32 buf (intent_sign_bytes call);
    Octra_net.Oce1.put_sig64 buf call.caller_signature)

let output_hash output_json =
  if String.length output_json > max_output_bytes then
    invalid_arg "resource compute output";
  Octra_net.Hash_domain.hash "octra:resource_compute_output" output_json

let result_shape result =
  String.length result.output_json <= max_output_bytes
  && result.vote.output_hash = output_hash result.output_json

let put_request buf (request : Certificate.request) =
  Octra_net.Oce1.put_string buf request.Certificate.chain_id;
  Octra_net.Oce1.put_u64 buf request.epoch_id;
  Octra_net.Oce1.put_u64 buf request.expires_epoch;
  Octra_net.Oce1.put_hash32 buf request.validator_set_root;
  Octra_net.Oce1.put_hash32 buf request.selection_root;
  Octra_net.Oce1.put_string buf request.circle_id;
  Octra_net.Oce1.put_hash32 buf request.state_root;
  Octra_net.Oce1.put_u64 buf request.model_epoch;
  Octra_net.Oce1.put_hash32 buf request.model_state_root;
  Octra_net.Oce1.put_hash32 buf request.graph_root;
  Octra_net.Oce1.put_hash32 buf request.model_root;
  Octra_net.Oce1.put_hash32 buf request.program_root;
  Octra_net.Oce1.put_hash32 buf request.executor_root;
  Octra_net.Oce1.put_string buf request.caller;
  Octra_net.Oce1.put_hash32 buf request.session_id;
  Octra_net.Oce1.put_u64 buf request.sequence;
  Octra_net.Oce1.put_hash32 buf request.input_hash;
  Octra_net.Oce1.put_u32_int buf request.input_bytes;
  Octra_net.Oce1.put_u32_int buf request.max_output_bytes

let get_request cursor =
  let chain_id = Octra_net.Oce1.get_string_bounded ~max:max_chain_id_bytes cursor in
  let epoch_id = Octra_net.Oce1.get_u64 cursor in
  let expires_epoch = Octra_net.Oce1.get_u64 cursor in
  let validator_set_root = Octra_net.Oce1.get_hash32 cursor in
  let selection_root = Octra_net.Oce1.get_hash32 cursor in
  let circle_id = Octra_net.Oce1.get_string_bounded ~max:max_circle_id_bytes cursor in
  let state_root = Octra_net.Oce1.get_hash32 cursor in
  let model_epoch = Octra_net.Oce1.get_u64 cursor in
  let model_state_root = Octra_net.Oce1.get_hash32 cursor in
  let graph_root = Octra_net.Oce1.get_hash32 cursor in
  let model_root = Octra_net.Oce1.get_hash32 cursor in
  let program_root = Octra_net.Oce1.get_hash32 cursor in
  let executor_root = Octra_net.Oce1.get_hash32 cursor in
  let caller = Octra_net.Oce1.get_string_bounded ~max:max_node_id_bytes cursor in
  let session_id = Octra_net.Oce1.get_hash32 cursor in
  let sequence = Octra_net.Oce1.get_u64 cursor in
  let input_hash = Octra_net.Oce1.get_hash32 cursor in
  let input_bytes = Octra_net.Oce1.get_u32_int cursor in
  let max_output_bytes = Octra_net.Oce1.get_u32_int cursor in
  ({
    chain_id = chain_id;
    epoch_id = epoch_id;
    expires_epoch = expires_epoch;
    validator_set_root = validator_set_root;
    selection_root = selection_root;
    circle_id = circle_id;
    state_root = state_root;
    model_epoch = model_epoch;
    model_state_root = model_state_root;
    graph_root = graph_root;
    model_root = model_root;
    program_root = program_root;
    executor_root = executor_root;
    caller = caller;
    session_id = session_id;
    sequence = sequence;
    input_hash = input_hash;
    input_bytes = input_bytes;
    max_output_bytes = max_output_bytes;
  } : Certificate.request)

let put_vote buf (vote : Certificate.vote) =
  Octra_net.Oce1.put_hash32 buf vote.Certificate.request_id;
  Octra_net.Oce1.put_string buf vote.node_id;
  Octra_net.Oce1.put_hash32 buf vote.output_hash;
  Octra_net.Oce1.put_hash32 buf vote.trace_root;
  Octra_net.Oce1.put_u32_int buf vote.output_bytes;
  Octra_net.Oce1.put_u64 buf vote.steps;
  Octra_net.Oce1.put_u64 buf vote.operations;
  Octra_net.Oce1.put_sig64 buf vote.signature

let get_vote cursor =
  let request_id = Octra_net.Oce1.get_hash32 cursor in
  let node_id = Octra_net.Oce1.get_string_bounded ~max:max_node_id_bytes cursor in
  let output_hash = Octra_net.Oce1.get_hash32 cursor in
  let trace_root = Octra_net.Oce1.get_hash32 cursor in
  let output_bytes = Octra_net.Oce1.get_u32_int cursor in
  let steps = Octra_net.Oce1.get_u64 cursor in
  let operations = Octra_net.Oce1.get_u64 cursor in
  let signature = Octra_net.Oce1.get_sig64 cursor in
  ({
    request_id = request_id;
    node_id = node_id;
    output_hash = output_hash;
    trace_root = trace_root;
    output_bytes = output_bytes;
    steps = steps;
    operations = operations;
    signature = signature;
  } : Certificate.vote)

let put_offer buf (offer : offer) =
  Octra_net.Oce1.put_string buf offer.chain_id;
  Octra_net.Oce1.put_u64 buf offer.offered_epoch;
  Octra_net.Oce1.put_u64 buf offer.expires_epoch;
  Octra_net.Oce1.put_string buf offer.coordinator;
  Octra_net.Oce1.put_hash32 buf offer.validator_set_root;
  Octra_net.Oce1.put_string buf offer.circle_id;
  Octra_net.Oce1.put_u64 buf offer.model_epoch;
  Octra_net.Oce1.put_hash32 buf offer.model_state_root;
  Octra_net.Oce1.put_hash32 buf offer.graph_root;
  Octra_net.Oce1.put_hash32 buf offer.model_root;
  Octra_net.Oce1.put_hash32 buf offer.program_root;
  Octra_net.Oce1.put_hash32 buf offer.executor_root;
  Octra_net.Oce1.put_u64 buf offer.min_memory_bytes;
  Octra_net.Oce1.put_u32_int buf offer.request_bytes;
  Octra_net.Oce1.put_u32_int buf offer.response_bytes;
  Octra_net.Oce1.put_sig64 buf offer.signature

let get_offer cursor =
  let chain_id = Octra_net.Oce1.get_string_bounded ~max:max_chain_id_bytes cursor in
  let offered_epoch = Octra_net.Oce1.get_u64 cursor in
  let expires_epoch = Octra_net.Oce1.get_u64 cursor in
  let coordinator = Octra_net.Oce1.get_string_bounded ~max:max_node_id_bytes cursor in
  let validator_set_root = Octra_net.Oce1.get_hash32 cursor in
  let circle_id = Octra_net.Oce1.get_string_bounded ~max:max_circle_id_bytes cursor in
  let model_epoch = Octra_net.Oce1.get_u64 cursor in
  let model_state_root = Octra_net.Oce1.get_hash32 cursor in
  let graph_root = Octra_net.Oce1.get_hash32 cursor in
  let model_root = Octra_net.Oce1.get_hash32 cursor in
  let program_root = Octra_net.Oce1.get_hash32 cursor in
  let executor_root = Octra_net.Oce1.get_hash32 cursor in
  let min_memory_bytes = Octra_net.Oce1.get_u64 cursor in
  let request_bytes = Octra_net.Oce1.get_u32_int cursor in
  let response_bytes = Octra_net.Oce1.get_u32_int cursor in
  let signature = Octra_net.Oce1.get_sig64 cursor in
  {
    chain_id;
    offered_epoch;
    expires_epoch;
    coordinator;
    validator_set_root;
    circle_id;
    model_epoch;
    model_state_root;
    graph_root;
    model_root;
    program_root;
    executor_root;
    min_memory_bytes;
    request_bytes;
    response_bytes;
    signature;
  }

let put_capability buf (capability : Capability.t) =
  Octra_net.Oce1.put_string buf capability.Capability.chain_id;
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
  Octra_net.Oce1.put_u8 buf (Capability.accelerator_code capability.accelerator);
  Octra_net.Oce1.put_u16 buf capability.lanes;
  Octra_net.Oce1.put_u64 buf capability.memory_bytes;
  Octra_net.Oce1.put_u32_int buf capability.max_request_bytes;
  Octra_net.Oce1.put_u32_int buf capability.max_response_bytes;
  Octra_net.Oce1.put_sig64 buf capability.signature

let get_capability cursor =
  let chain_id = Octra_net.Oce1.get_string_bounded ~max:max_chain_id_bytes cursor in
  let observed_epoch = Octra_net.Oce1.get_u64 cursor in
  let valid_until = Octra_net.Oce1.get_u64 cursor in
  let validator_set_root = Octra_net.Oce1.get_hash32 cursor in
  let node_id = Octra_net.Oce1.get_string_bounded ~max:max_node_id_bytes cursor in
  let circle_id = Octra_net.Oce1.get_string_bounded ~max:max_circle_id_bytes cursor in
  let model_epoch = Octra_net.Oce1.get_u64 cursor in
  let model_state_root = Octra_net.Oce1.get_hash32 cursor in
  let graph_root = Octra_net.Oce1.get_hash32 cursor in
  let model_root = Octra_net.Oce1.get_hash32 cursor in
  let program_root = Octra_net.Oce1.get_hash32 cursor in
  let executor_root = Octra_net.Oce1.get_hash32 cursor in
  let evidence_root = Octra_net.Oce1.get_hash32 cursor in
  let accelerator = Capability.accelerator_of_code (Octra_net.Oce1.get_u8 cursor) in
  let lanes = Octra_net.Oce1.get_u16 cursor in
  let memory_bytes = Octra_net.Oce1.get_u64 cursor in
  let max_request_bytes = Octra_net.Oce1.get_u32_int cursor in
  let max_response_bytes = Octra_net.Oce1.get_u32_int cursor in
  let signature = Octra_net.Oce1.get_sig64 cursor in
  ({
    chain_id = chain_id;
    observed_epoch = observed_epoch;
    valid_until = valid_until;
    validator_set_root = validator_set_root;
    node_id = node_id;
    circle_id = circle_id;
    model_epoch = model_epoch;
    model_state_root = model_state_root;
    graph_root = graph_root;
    model_root = model_root;
    program_root = program_root;
    executor_root = executor_root;
    evidence_root = evidence_root;
    accelerator = accelerator;
    lanes = lanes;
    memory_bytes = memory_bytes;
    max_request_bytes = max_request_bytes;
    max_response_bytes = max_response_bytes;
    signature = signature;
  } : Capability.t)

let put_commitment buf (commitment : Selection.commitment) =
  Octra_net.Oce1.put_string buf commitment.Selection.chain_id;
  Octra_net.Oce1.put_hash32 buf commitment.offer_id;
  Octra_net.Oce1.put_u64 buf commitment.commit_epoch;
  Octra_net.Oce1.put_string buf commitment.node_id;
  Octra_net.Oce1.put_hash32 buf commitment.graph_root;
  Octra_net.Oce1.put_hash32 buf commitment.model_root;
  Octra_net.Oce1.put_hash32 buf commitment.program_root;
  Octra_net.Oce1.put_hash32 buf commitment.executor_root;
  Octra_net.Oce1.put_hash32 buf commitment.nonce_hash;
  Octra_net.Oce1.put_sig64 buf commitment.signature

let get_commitment cursor =
  let chain_id = Octra_net.Oce1.get_string_bounded ~max:max_chain_id_bytes cursor in
  let offer_id = Octra_net.Oce1.get_hash32 cursor in
  let commit_epoch = Octra_net.Oce1.get_u64 cursor in
  let node_id = Octra_net.Oce1.get_string_bounded ~max:max_node_id_bytes cursor in
  let graph_root = Octra_net.Oce1.get_hash32 cursor in
  let model_root = Octra_net.Oce1.get_hash32 cursor in
  let program_root = Octra_net.Oce1.get_hash32 cursor in
  let executor_root = Octra_net.Oce1.get_hash32 cursor in
  let nonce_hash = Octra_net.Oce1.get_hash32 cursor in
  let signature = Octra_net.Oce1.get_sig64 cursor in
  ({
    chain_id = chain_id;
    offer_id = offer_id;
    commit_epoch = commit_epoch;
    node_id = node_id;
    graph_root = graph_root;
    model_root = model_root;
    program_root = program_root;
    executor_root = executor_root;
    nonce_hash = nonce_hash;
    signature = signature;
  } : Selection.commitment)

let put_reveal buf (reveal : Selection.reveal) =
  Octra_net.Oce1.put_string buf reveal.Selection.chain_id;
  Octra_net.Oce1.put_hash32 buf reveal.offer_id;
  Octra_net.Oce1.put_u64 buf reveal.commit_epoch;
  Octra_net.Oce1.put_u64 buf reveal.reveal_epoch;
  Octra_net.Oce1.put_string buf reveal.node_id;
  Octra_net.Oce1.put_hash32 buf reveal.graph_root;
  Octra_net.Oce1.put_hash32 buf reveal.model_root;
  Octra_net.Oce1.put_hash32 buf reveal.program_root;
  Octra_net.Oce1.put_hash32 buf reveal.executor_root;
  Octra_net.Oce1.put_string buf reveal.nonce;
  Octra_net.Oce1.put_hash32 buf reveal.evidence_root;
  Octra_net.Oce1.put_sig64 buf reveal.signature

let get_reveal cursor =
  let chain_id = Octra_net.Oce1.get_string_bounded ~max:max_chain_id_bytes cursor in
  let offer_id = Octra_net.Oce1.get_hash32 cursor in
  let commit_epoch = Octra_net.Oce1.get_u64 cursor in
  let reveal_epoch = Octra_net.Oce1.get_u64 cursor in
  let node_id = Octra_net.Oce1.get_string_bounded ~max:max_node_id_bytes cursor in
  let graph_root = Octra_net.Oce1.get_hash32 cursor in
  let model_root = Octra_net.Oce1.get_hash32 cursor in
  let program_root = Octra_net.Oce1.get_hash32 cursor in
  let executor_root = Octra_net.Oce1.get_hash32 cursor in
  let nonce = Octra_net.Oce1.get_string_bounded ~max:max_nonce_bytes cursor in
  let evidence_root = Octra_net.Oce1.get_hash32 cursor in
  let signature = Octra_net.Oce1.get_sig64 cursor in
  ({
    chain_id = chain_id;
    offer_id = offer_id;
    commit_epoch = commit_epoch;
    reveal_epoch = reveal_epoch;
    node_id = node_id;
    graph_root = graph_root;
    model_root = model_root;
    program_root = program_root;
    executor_root = executor_root;
    nonce = nonce;
    evidence_root = evidence_root;
    signature = signature;
  } : Selection.reveal)

let encode message =
  Octra_net.Oce1.encode (fun buf ->
    match message with
    | Offer offer ->
      Octra_net.Oce1.put_u8 buf 1;
      put_offer buf offer
    | Capability capability ->
      Octra_net.Oce1.put_u8 buf 2;
      put_capability buf capability
    | Commitment commitment ->
      Octra_net.Oce1.put_u8 buf 3;
      put_commitment buf commitment
    | Reveal reveal ->
      Octra_net.Oce1.put_u8 buf 4;
      put_reveal buf reveal
    | Call call ->
      Octra_net.Oce1.put_u8 buf 5;
      put_request buf call.request;
      Octra_net.Oce1.put_string buf call.method_name;
      Octra_net.Oce1.put_string buf call.params_json;
      Octra_net.Oce1.put_hash32 buf call.caller_public_key;
      Octra_net.Oce1.put_sig64 buf call.caller_signature;
      Octra_net.Oce1.put_string buf call.coordinator;
      Octra_net.Oce1.put_sig64 buf call.signature
    | Result result ->
      Octra_net.Oce1.put_u8 buf 6;
      put_vote buf result.vote;
      Octra_net.Oce1.put_string buf result.output_json)

let decode payload =
  Octra_net.Oce1.decode
    (fun cursor ->
      match Octra_net.Oce1.get_u8 cursor with
      | 1 -> Offer (get_offer cursor)
      | 2 -> Capability (get_capability cursor)
      | 3 -> Commitment (get_commitment cursor)
      | 4 -> Reveal (get_reveal cursor)
      | 5 ->
        let request = get_request cursor in
        let method_name = Octra_net.Oce1.get_string_bounded ~max:max_method_bytes cursor in
        let params_json = Octra_net.Oce1.get_string_bounded ~max:max_params_bytes cursor in
        let caller_public_key = Octra_net.Oce1.get_hash32 cursor in
        let caller_signature = Octra_net.Oce1.get_sig64 cursor in
        let coordinator = Octra_net.Oce1.get_string_bounded ~max:max_node_id_bytes cursor in
        let signature = Octra_net.Oce1.get_sig64 cursor in
        Call {
          request;
          method_name;
          params_json;
          caller_public_key;
          caller_signature;
          coordinator;
          signature;
        }
      | 6 ->
        let vote = get_vote cursor in
        let output_json = Octra_net.Oce1.get_string_bounded ~max:max_output_bytes cursor in
        Result {
          vote;
          output_json;
        }
      | _ -> failwith "bad resource compute message tag")
    payload