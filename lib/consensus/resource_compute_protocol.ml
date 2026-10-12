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
let max_members = 5
let max_records = 256
let reveal_delay = 1L
let offer_ttl = 16L
let call_ttl = 64L
let capacity_ttl = 16L

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

let cancel_rounds = 64
let cancel_span = 64L

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

type header = {
  request : Resource_compute_certificate.request;
  valid_until : int64 option;
  caller_public_key : string;
  caller_signature : string;
  coordinator : string;
  signature : string;
}

type call = {
  request : Certificate.request;
  valid_until : int64 option;
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

type fetch = {
  call : call;
  target : string;
  epoch_id : int64;
  expires_epoch : int64;
  round : int;
  signature : string;
}

let fetch_rounds = 8
let fetch_span = 4L

type message =
  | Offer of offer
  | Capability of Capability.t
  | Commitment of Selection.commitment
  | Reveal of Selection.reveal
  | Call of call
  | Result of result
  | Cancel of cancellation
  | Cancel_round of cancel_round
  | Cancel_ack of cancel_ack
  | Fetch of fetch
  | Answer of fetch * result

let hash32 value =
  String.length value = 32

let signature64 value =
  String.length value = 64

let non_empty limit value =
  let length = String.length value in
  length > 0 && length <= limit

let cancel_shape (cancel : cancellation) =
  non_empty max_chain_id_bytes cancel.chain_id
  && non_empty max_node_id_bytes cancel.caller
  && hash32 cancel.caller_public_key
  && hash32 cancel.session_id
  && signature64 cancel.caller_signature
  && (match cancel.until with None -> true | Some until -> until >= 0L)

let cancel_sign_bytes (cancel : cancellation) =
  if not (cancel_shape cancel) then invalid_arg "resource compute cancellation";
  let domain = match cancel.until with None -> "octra:resource_compute_cancel"
    | Some _ -> "octra:compute_cancel_until" in
  Octra_net.Hash_domain.hash_encoded domain (fun buffer ->
    Octra_net.Oce1.put_string buffer cancel.chain_id;
    Octra_net.Oce1.put_string buffer cancel.caller;
    Octra_net.Oce1.put_hash32 buffer cancel.caller_public_key;
    Octra_net.Oce1.put_hash32 buffer cancel.session_id;
    Option.iter (Octra_net.Oce1.put_u64 buffer) cancel.until)

let cancel_round_shape value =
  cancel_shape value.cancel && non_empty max_node_id_bytes value.origin
  && value.epoch >= 0L && value.expires >= value.epoch
  && Int64.sub value.expires value.epoch <= cancel_span
  && value.round >= 1 && value.round <= cancel_rounds
  && signature64 value.signature
  && (match value.cancel.until with None -> true | Some until -> value.expires = until)

let cancel_round_sign value =
  if not (cancel_round_shape value) then invalid_arg "compute cancel round shape";
  Octra_net.Hash_domain.hash_encoded "octra:compute_cancel_round" (fun buffer ->
    Octra_net.Oce1.put_hash32 buffer (cancel_sign_bytes value.cancel);
    Octra_net.Oce1.put_sig64 buffer value.cancel.caller_signature;
    Octra_net.Oce1.put_string buffer value.origin;
    Octra_net.Oce1.put_u64 buffer value.epoch;
    Octra_net.Oce1.put_u64 buffer value.expires;
    Octra_net.Oce1.put_u8 buffer value.round)

let cancel_ack_shape (value : cancel_ack) =
  cancel_round_shape value.delivery && non_empty max_node_id_bytes value.node_id
  && hash32 value.origin_key && hash32 value.node_key
  && value.epoch >= value.delivery.epoch && value.epoch <= value.delivery.expires
  && signature64 value.signature

let cancel_ack_sign (value : cancel_ack) =
  if not (cancel_ack_shape value) then invalid_arg "compute cancel ack shape";
  Octra_net.Hash_domain.hash_encoded "octra:compute_cancel_ack" (fun buffer ->
    Octra_net.Oce1.put_hash32 buffer (cancel_round_sign value.delivery);
    Octra_net.Oce1.put_sig64 buffer value.delivery.signature;
    Octra_net.Oce1.put_hash32 buffer value.origin_key;
    Octra_net.Oce1.put_string buffer value.node_id;
    Octra_net.Oce1.put_hash32 buffer value.node_key;
    Octra_net.Oce1.put_u64 buffer value.epoch)

let offer_shape (offer : offer) =
  non_empty max_chain_id_bytes offer.chain_id
  && offer.offered_epoch >= 0L
  && offer.expires_epoch >= offer.offered_epoch
  && non_empty max_node_id_bytes offer.coordinator
  && hash32 offer.validator_set_root
  && non_empty max_circle_id_bytes offer.circle_id
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
    not (non_empty max_method_bytes method_name)
    || String.length params_json > max_params_bytes
  then
    invalid_arg "resource compute input";
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_input" (fun buf ->
    Octra_net.Oce1.put_string buf method_name;
    Octra_net.Oce1.put_string buf params_json)

let header_fields (call : call) : header = {
  request = call.request; valid_until = call.valid_until;
  caller_public_key = call.caller_public_key; caller_signature = call.caller_signature;
  coordinator = call.coordinator; signature = call.signature;
}

let terms_shape (call : header) =
  Certificate.fee_valid ~fuel:call.request.fuel_limit
    ~price:call.request.unit_price ~cap:call.request.fee_cap
  && (match call.request.unit_price with None -> true | Some _ -> Option.is_some call.request.memory_bytes)
  && (match call.request.memory_bytes with None -> true | Some bytes ->
    Option.is_some call.request.guest_bytes && bytes > 0 && bytes <= Certificate.max_memory_bytes)
  && (match call.valid_until with None -> true | Some epoch ->
    epoch >= 0L && call.request.expires_epoch <= epoch)
  && (match call.request.fuel_limit with None -> true | Some fuel ->
    Option.is_some call.valid_until && fuel > 0 && fuel <= 2_000_000_000)
  && (match call.request.guest_bytes with None -> true | Some bytes ->
    Option.is_some call.request.fuel_limit && Option.is_some call.valid_until
    && bytes >= 65_536 && bytes <= 67_108_864 && bytes mod 65_536 = 0)
  && hash32 call.caller_public_key
  && signature64 call.caller_signature
  && non_empty max_node_id_bytes call.coordinator
  && signature64 call.signature

let call_shape call =
  non_empty max_method_bytes call.method_name
  && terms_shape (header_fields call)
  && String.length call.params_json <= max_params_bytes
  && call.request.input_hash = input_hash ~method_name:call.method_name ~params_json:call.params_json

let header_shape call = terms_shape call && Certificate.request_shape call.request

let header call =
  if call_shape call && Certificate.request_shape call.request then Ok (header_fields call)
  else Error "compute header shape"

let intent_body (call : header) =
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

let intent_digest (call : header) =
  let digest = intent_body call in
  let digest = match call.valid_until with
    | None -> digest
    | Some epoch ->
      Octra_net.Hash_domain.hash_encoded "octra:compute_intent_expiry" (fun buf ->
        Octra_net.Oce1.put_hash32 buf digest;
        Octra_net.Oce1.put_u64 buf epoch) in
  let digest = match call.request.fuel_limit with
  | None -> digest
  | Some fuel ->
    Octra_net.Hash_domain.hash_encoded "octra:compute_intent_fuel" (fun buf ->
      Octra_net.Oce1.put_hash32 buf digest;
      Octra_net.Oce1.put_u32_int buf fuel) in
  let digest = match call.request.guest_bytes with
  | None -> digest
  | Some bytes ->
    Octra_net.Hash_domain.hash_encoded "octra:compute_intent_guest" (fun buf ->
      Octra_net.Oce1.put_hash32 buf digest;
      Octra_net.Oce1.put_u32_int buf bytes) in
  let digest = match call.request.memory_bytes with
  | None -> digest
  | Some bytes ->
    Octra_net.Hash_domain.hash_encoded "octra:compute_intent_memory" (fun buf ->
      Octra_net.Oce1.put_hash32 buf digest;
      Octra_net.Oce1.put_u32_int buf bytes) in
  match call.request.unit_price, call.request.fee_cap with
  | Some price, Some cap ->
    Octra_net.Hash_domain.hash_encoded "octra:compute_intent_fee" (fun buf ->
      Octra_net.Oce1.put_hash32 buf digest;
      Octra_net.Oce1.put_u64 buf price;
      Octra_net.Oce1.put_u64 buf cap)
  | _ -> digest

let call_digest (call : header) =
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_call" (fun buf ->
    Octra_net.Oce1.put_hash32 buf (Certificate.request_id call.request);
    Octra_net.Oce1.put_string buf call.coordinator;
    Octra_net.Oce1.put_hash32 buf (intent_digest call);
    Octra_net.Oce1.put_sig64 buf call.caller_signature)

let intent_sign_bytes call =
  if not (call_shape call) then invalid_arg "resource compute call";
  intent_digest (header_fields call)

let call_sign_bytes call =
  if not (call_shape call) then invalid_arg "resource compute call";
  call_digest (header_fields call)

let header_intent value =
  if not (header_shape value) then invalid_arg "compute header shape";
  intent_digest value

let header_sign value =
  if not (header_shape value) then invalid_arg "compute header shape";
  call_digest value

let signed_id digest signature =
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_intent_id" (fun buffer ->
    Octra_net.Oce1.put_hash32 buffer digest;
    Octra_net.Oce1.put_sig64 buffer signature)

let header_id (value : header) =
  signed_id (header_intent value) value.caller_signature

let intent_id call =
  signed_id (intent_sign_bytes call) call.caller_signature

let fetch_shape (fetch : fetch) =
  call_shape fetch.call && Certificate.request_shape fetch.call.request
  && non_empty max_node_id_bytes fetch.target
  && fetch.epoch_id >= fetch.call.request.epoch_id
  && fetch.expires_epoch >= fetch.epoch_id
  && fetch.expires_epoch <= fetch.call.request.expires_epoch
  && Int64.sub fetch.expires_epoch fetch.epoch_id <= fetch_span
  && fetch.round >= 0 && fetch.round < fetch_rounds
  && signature64 fetch.signature

let fetch_sign_bytes fetch =
  if not (fetch_shape fetch) then invalid_arg "compute fetch shape";
  Octra_net.Hash_domain.hash_encoded "octra:compute_fetch" (fun buf ->
    Octra_net.Oce1.put_hash32 buf (call_sign_bytes fetch.call);
    Octra_net.Oce1.put_sig64 buf fetch.call.signature;
    Octra_net.Oce1.put_string buf fetch.target;
    Octra_net.Oce1.put_u64 buf fetch.epoch_id;
    Octra_net.Oce1.put_u64 buf fetch.expires_epoch;
    Octra_net.Oce1.put_u8 buf fetch.round)

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

let get_request ?(fuel_limit = None) ?(guest_bytes = None) ?(memory_bytes = None)
    ?(unit_price = None) ?(fee_cap = None) cursor =
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
    unit_price;
    fee_cap;
    memory_bytes;
    guest_bytes;
    fuel_limit;
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

let get_memory cursor =
  Int64.to_int (Int64.logand (Int64.of_int32 (Octra_net.Oce1.get_u32 cursor)) 0xffff_ffffL)

let encode_header (value : header) =
  if not (header_shape value) then invalid_arg "compute header shape";
  let open Octra_net.Oce1 in
  encode (fun buffer ->
    put_string buffer "compute_header";
    put_option put_u64 buffer value.valid_until;
    put_option put_u32_int buffer value.request.fuel_limit;
    put_option put_u32_int buffer value.request.guest_bytes;
    put_option put_u32_int buffer value.request.memory_bytes;
    put_option put_u64 buffer value.request.unit_price;
    put_option put_u64 buffer value.request.fee_cap;
    put_request buffer value.request;
    put_hash32 buffer value.caller_public_key;
    put_sig64 buffer value.caller_signature;
    put_string buffer value.coordinator;
    put_sig64 buffer value.signature)

let decode_header raw =
  if String.length raw > 2048 then Error "compute header size"
  else try
    let open Octra_net.Oce1 in
    decode (fun cursor ->
      if get_string_bounded ~max:14 cursor <> "compute_header" then
        invalid_arg "compute header format";
      let valid_until = get_option get_u64 cursor in
      let fuel_limit = get_option get_u32_int cursor in
      let guest_bytes = get_option get_u32_int cursor in
      let memory_bytes = get_option get_memory cursor in
      let unit_price = get_option get_u64 cursor in
      let fee_cap = get_option get_u64 cursor in
      let request = get_request ~fuel_limit ~guest_bytes ~memory_bytes ~unit_price ~fee_cap cursor in
      let caller_public_key = get_hash32 cursor in
      let caller_signature = get_sig64 cursor in
      let coordinator = get_string_bounded ~max:max_node_id_bytes cursor in
      let signature = get_sig64 cursor in
      let value = {request; valid_until; caller_public_key; caller_signature; coordinator; signature} in
      if header_shape value then Ok value else Error "compute header shape") raw
  with
  | (Out_of_memory | Stack_overflow) as error -> raise error
  | _ -> Error "compute header encoding"

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

let put_call buf (call : call) =
  put_request buf call.request;
  Octra_net.Oce1.put_string buf call.method_name;
  Octra_net.Oce1.put_string buf call.params_json;
  Octra_net.Oce1.put_hash32 buf call.caller_public_key;
  Octra_net.Oce1.put_sig64 buf call.caller_signature;
  Octra_net.Oce1.put_string buf call.coordinator;
  Octra_net.Oce1.put_sig64 buf call.signature

let get_call ?(timed = false) ?(metered = false) ?(pages = false) ?(memory = false)
    ?(priced = false) cursor =
  let valid_until = if timed then Some (Octra_net.Oce1.get_u64 cursor) else None in
  let fuel_limit = if metered then Some (Octra_net.Oce1.get_u32_int cursor) else None in
  let guest_bytes = if pages then Some (Octra_net.Oce1.get_u32_int cursor) else None in
  let memory_bytes = if memory then Some (get_memory cursor) else None in
  let unit_price = if priced then Some (Octra_net.Oce1.get_u64 cursor) else None in
  let fee_cap = if priced then Some (Octra_net.Oce1.get_u64 cursor) else None in
  let request = get_request ~fuel_limit ~guest_bytes ~memory_bytes ~unit_price ~fee_cap cursor in
  let method_name = Octra_net.Oce1.get_string_bounded ~max:max_method_bytes cursor in
  let params_json = Octra_net.Oce1.get_string_bounded ~max:max_params_bytes cursor in
  let caller_public_key = Octra_net.Oce1.get_hash32 cursor in
  let caller_signature = Octra_net.Oce1.get_sig64 cursor in
  let coordinator = Octra_net.Oce1.get_string_bounded ~max:max_node_id_bytes cursor in
  let signature = Octra_net.Oce1.get_sig64 cursor in
  {request; valid_until; method_name; params_json; caller_public_key; caller_signature; coordinator; signature}

let put_expiry buf call =
  (match call.valid_until with None -> () | Some epoch -> Octra_net.Oce1.put_u64 buf epoch);
  (match call.request.fuel_limit with None -> () | Some fuel -> Octra_net.Oce1.put_u32_int buf fuel);
  (match call.request.guest_bytes with None -> () | Some bytes -> Octra_net.Oce1.put_u32_int buf bytes);
  (match call.request.memory_bytes with None -> () | Some bytes -> Octra_net.Oce1.put_u32_int buf bytes);
  (match call.request.unit_price with None -> () | Some price -> Octra_net.Oce1.put_u64 buf price);
  match call.request.fee_cap with None -> () | Some cap -> Octra_net.Oce1.put_u64 buf cap

let put_fetch buf fetch =
  if not (fetch_shape fetch) then invalid_arg "compute fetch shape";
  put_expiry buf fetch.call;
  put_call buf fetch.call;
  Octra_net.Oce1.put_string buf fetch.target;
  Octra_net.Oce1.put_u64 buf fetch.epoch_id;
  Octra_net.Oce1.put_u64 buf fetch.expires_epoch;
  Octra_net.Oce1.put_u8 buf fetch.round;
  Octra_net.Oce1.put_sig64 buf fetch.signature

let get_fetch ?(timed = false) ?(metered = false) ?(pages = false) ?(memory = false)
    ?(priced = false) cursor =
  let call = get_call ~timed ~metered ~pages ~memory ~priced cursor in
  let target = Octra_net.Oce1.get_string_bounded ~max:max_node_id_bytes cursor in
  let epoch_id = Octra_net.Oce1.get_u64 cursor in
  let expires_epoch = Octra_net.Oce1.get_u64 cursor in
  let round = Octra_net.Oce1.get_u8 cursor in
  let signature = Octra_net.Oce1.get_sig64 cursor in
  let fetch = {call; target; epoch_id; expires_epoch; round; signature} in
  if not (fetch_shape fetch) then invalid_arg "compute fetch shape";
  fetch

let put_cancel buffer (cancel : cancellation) =
  if not (cancel_shape cancel) then invalid_arg "resource compute cancellation";
  Octra_net.Oce1.put_string buffer cancel.chain_id;
  Octra_net.Oce1.put_string buffer cancel.caller;
  Octra_net.Oce1.put_hash32 buffer cancel.caller_public_key;
  Octra_net.Oce1.put_hash32 buffer cancel.session_id;
  Octra_net.Oce1.put_sig64 buffer cancel.caller_signature;
  Option.iter (Octra_net.Oce1.put_u64 buffer) cancel.until

let get_cancel ?(timed = false) cursor =
  let chain_id = Octra_net.Oce1.get_string_bounded ~max:max_chain_id_bytes cursor in
  let caller = Octra_net.Oce1.get_string_bounded ~max:max_node_id_bytes cursor in
  let caller_public_key = Octra_net.Oce1.get_hash32 cursor in
  let session_id = Octra_net.Oce1.get_hash32 cursor in
  let caller_signature = Octra_net.Oce1.get_sig64 cursor in
  let until = if timed then Some (Octra_net.Oce1.get_u64 cursor) else None in
  {chain_id; caller; caller_public_key; session_id; caller_signature; until}

let put_cancel_round buffer (value : cancel_round) =
  if not (cancel_round_shape value) then invalid_arg "compute cancel round shape";
  put_cancel buffer value.cancel;
  Octra_net.Oce1.put_string buffer value.origin;
  Octra_net.Oce1.put_u64 buffer value.epoch;
  Octra_net.Oce1.put_u64 buffer value.expires;
  Octra_net.Oce1.put_u8 buffer value.round;
  Octra_net.Oce1.put_sig64 buffer value.signature

let get_cancel_round ?(timed = false) cursor =
  let cancel = get_cancel ~timed cursor in
  let origin = Octra_net.Oce1.get_string_bounded ~max:max_node_id_bytes cursor in
  let epoch = Octra_net.Oce1.get_u64 cursor in
  let expires = Octra_net.Oce1.get_u64 cursor in
  let round = Octra_net.Oce1.get_u8 cursor in
  let signature = Octra_net.Oce1.get_sig64 cursor in
  let value = {cancel; origin; epoch; expires; round; signature} in
  if not (cancel_round_shape value) then invalid_arg "compute cancel round shape";
  value

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
      Octra_net.Oce1.put_u8 buf (if Option.is_some call.request.unit_price then 22
        else if Option.is_some call.request.memory_bytes then 19
        else if Option.is_some call.request.guest_bytes then 16
        else if Option.is_some call.request.fuel_limit then 13
        else if Option.is_some call.valid_until then 10 else 5);
      put_expiry buf call;
      put_call buf call
    | Result result ->
      Octra_net.Oce1.put_u8 buf 6;
      put_vote buf result.vote;
      Octra_net.Oce1.put_string buf result.output_json
    | Cancel cancel ->
      Octra_net.Oce1.put_u8 buf (if cancel.until = None then 7 else 27);
      put_cancel buf cancel
    | Cancel_round value ->
      Octra_net.Oce1.put_u8 buf (if value.cancel.until = None then 25 else 28);
      put_cancel_round buf value
    | Cancel_ack value ->
      if not (cancel_ack_shape value) then invalid_arg "compute cancel ack shape";
      Octra_net.Oce1.put_u8 buf (if value.delivery.cancel.until = None then 26 else 29);
      put_cancel_round buf value.delivery;
      Octra_net.Oce1.put_hash32 buf value.origin_key;
      Octra_net.Oce1.put_string buf value.node_id;
      Octra_net.Oce1.put_hash32 buf value.node_key;
      Octra_net.Oce1.put_u64 buf value.epoch;
      Octra_net.Oce1.put_sig64 buf value.signature
    | Fetch fetch ->
      Octra_net.Oce1.put_u8 buf (if Option.is_some fetch.call.request.unit_price then 23
        else if Option.is_some fetch.call.request.memory_bytes then 20
        else if Option.is_some fetch.call.request.guest_bytes then 17
        else if Option.is_some fetch.call.request.fuel_limit then 14
        else if Option.is_some fetch.call.valid_until then 11 else 8);
      put_fetch buf fetch
    | Answer (fetch, result) ->
      Octra_net.Oce1.put_u8 buf (if Option.is_some fetch.call.request.unit_price then 24
        else if Option.is_some fetch.call.request.memory_bytes then 21
        else if Option.is_some fetch.call.request.guest_bytes then 18
        else if Option.is_some fetch.call.request.fuel_limit then 15
        else if Option.is_some fetch.call.valid_until then 12 else 9);
      put_fetch buf fetch;
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
      | 5 -> Call (get_call cursor)
      | 10 -> Call (get_call ~timed:true cursor)
      | 13 -> Call (get_call ~timed:true ~metered:true cursor)
      | 16 -> Call (get_call ~timed:true ~metered:true ~pages:true cursor)
      | 19 -> Call (get_call ~timed:true ~metered:true ~pages:true ~memory:true cursor)
      | 22 -> Call (get_call ~timed:true ~metered:true ~pages:true ~memory:true ~priced:true cursor)
      | 6 ->
        let vote = get_vote cursor in
        let output_json = Octra_net.Oce1.get_string_bounded ~max:max_output_bytes cursor in
        Result {
          vote;
          output_json;
        }
      | 7 -> Cancel (get_cancel cursor)
      | 27 -> Cancel (get_cancel ~timed:true cursor)
      | 25 -> Cancel_round (get_cancel_round cursor)
      | 28 -> Cancel_round (get_cancel_round ~timed:true cursor)
      | (26 | 29) as tag ->
        let delivery = get_cancel_round ~timed:(tag = 29) cursor in
        let origin_key = Octra_net.Oce1.get_hash32 cursor in
        let node_id = Octra_net.Oce1.get_string_bounded ~max:max_node_id_bytes cursor in
        let node_key = Octra_net.Oce1.get_hash32 cursor in
        let epoch = Octra_net.Oce1.get_u64 cursor in
        let signature = Octra_net.Oce1.get_sig64 cursor in
        let value = {delivery; origin_key; node_id; node_key; epoch; signature} in
        if not (cancel_ack_shape value) then invalid_arg "compute cancel ack shape";
        Cancel_ack value
      | 8 -> Fetch (get_fetch cursor)
      | 11 -> Fetch (get_fetch ~timed:true cursor)
      | 14 -> Fetch (get_fetch ~timed:true ~metered:true cursor)
      | 17 -> Fetch (get_fetch ~timed:true ~metered:true ~pages:true cursor)
      | 20 -> Fetch (get_fetch ~timed:true ~metered:true ~pages:true ~memory:true cursor)
      | 23 -> Fetch (get_fetch ~timed:true ~metered:true ~pages:true ~memory:true ~priced:true cursor)
      | (9 | 12 | 15 | 18 | 21 | 24) as tag ->
        let fetch = get_fetch ~timed:(tag <> 9) ~metered:(tag >= 15) ~pages:(tag >= 18)
          ~memory:(tag >= 21) ~priced:(tag = 24) cursor in
        let vote = get_vote cursor in
        let output_json = Octra_net.Oce1.get_string_bounded ~max:max_output_bytes cursor in
        Answer (fetch, {vote; output_json})
      | _ -> failwith "bad resource compute message tag")
    payload