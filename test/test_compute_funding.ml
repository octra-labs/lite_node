(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Funding = Octra_core.Compute_funding
module Protocol = Octra_consensus.Resource_compute_protocol
module Certificate = Octra_consensus.Resource_compute_certificate
module Ed = Mirage_crypto_ec.Ed25519

let expect name value = if not value then failwith ("compute funding: " ^ name)
let get = function Ok value -> value | Error error -> failwith error
let key seed = match Ed.priv_of_octets (String.make 32 seed) with
  | Ok value -> value | Error _ -> failwith "test signing key"
let public key = Ed.pub_to_octets (Ed.pub_of_priv key)
let address key = Octra_core.Crypto.Address.address_from_pubkey (Base64.encode_exn (public key))
let sign key message = Ed.sign ~key message

let payer = key 'p'
let coordinator = key 'c'
let target = address (key 't')
let hash byte = String.make 32 byte

let signed call =
  let call = {call with Protocol.caller_signature = sign payer (Protocol.intent_sign_bytes call)} in
  {call with Protocol.signature = sign coordinator (Protocol.call_sign_bytes call)}

let call () =
  let request = Certificate.{unit_price = Some 2L; fee_cap = Some 10_000L;
    memory_bytes = Some 131_072; guest_bytes = Some 65_536; fuel_limit = Some 5000;
    chain_id = "compute-funding-test"; epoch_id = 42L; expires_epoch = 50L;
    validator_set_root = hash 'v'; selection_root = hash 's'; circle_id = target;
    state_root = hash 'r'; model_epoch = 40L; model_state_root = hash 'm';
    graph_root = hash 'g'; model_root = hash 'w'; program_root = hash 'p'; executor_root = hash 'e';
    caller = address payer; session_id = hash 'i'; sequence = 3L;
    input_hash = Protocol.input_hash ~method_name:"predict" ~params_json:"[]";
    input_bytes = 2; max_output_bytes = 256} in
  signed Protocol.{request; valid_until = Some 50L; method_name = "predict";
    params_json = "[]"; caller_public_key = public payer; caller_signature = String.make 64 '\000';
    coordinator = address coordinator; signature = String.make 64 '\000'}

let permission call =
  let terms = Funding.prepare ~call ~target ~nonce:7L |> get in
  Funding.authorize terms ~signature:(sign payer (Funding.sign_bytes terms)) |> get

let verify ?(chain_id = "compute-funding-test") ?(epoch = 42L) ?(nonce = 7L)
    ?(target = target) call value =
  Funding.verify ~chain_id ~epoch ~nonce ~target ~call value

let test_signature () =
  let call = call () in
  let terms = Funding.prepare ~call ~target ~nonce:7L |> get in
  expect "amount differs" (terms.amount = 10_000L);
  expect "request signature accepted for funding"
    (Result.is_error (Funding.authorize terms ~signature:call.caller_signature));
  expect "coordinator signature accepted for funding"
    (Result.is_error (Funding.authorize terms ~signature:call.signature));
  expect "other signer accepted"
    (Result.is_error (Funding.authorize terms ~signature:(sign coordinator (Funding.sign_bytes terms))));
  expect "empty signature accepted" (Result.is_error (Funding.authorize terms ~signature:""));
  let value = permission call in
  expect "valid funding refused" (verify call value = Ok 10_000L);
  expect "expiry endpoint refused" (verify ~epoch:50L call value = Ok 10_000L);
  expect "future funding accepted" (Result.is_error (verify ~epoch:41L call value));
  expect "expired funding accepted" (Result.is_error (verify ~epoch:51L call value));
  expect "wrong chain accepted" (Result.is_error (verify ~chain_id:"elsewhere" call value));
  expect "wrong nonce accepted" (Result.is_error (verify ~nonce:8L call value));
  expect "wrong target accepted" (Result.is_error (verify ~target:(address coordinator) call value));
  expect "zero nonce accepted" (Result.is_error (Funding.prepare ~call ~target ~nonce:0L));
  expect "negative nonce accepted" (Result.is_error (Funding.prepare ~call ~target ~nonce:Int64.min_int));
  expect "invalid target accepted" (Result.is_error (Funding.prepare ~call ~target:"bad" ~nonce:7L));
  let invalid = {call with caller_public_key = public coordinator} in
  expect "payer key mismatch accepted" (Result.is_error (Funding.prepare ~call:invalid ~target ~nonce:7L));
  let invalid = {call with caller_signature = String.make 64 '\000'} in
  expect "unsigned call accepted" (Result.is_error (Funding.prepare ~call:invalid ~target ~nonce:7L))

let test_scope () =
  let call = call () in
  let value = permission call in
  let request = call.request in
  let changes = Certificate.[
    {request with chain_id = "other-chain"}; {request with epoch_id = 43L};
    {request with expires_epoch = 49L}; {request with validator_set_root = hash 'x'};
    {request with selection_root = hash 'x'}; {request with circle_id = address coordinator};
    {request with state_root = hash 'x'}; {request with model_epoch = 41L};
    {request with model_state_root = hash 'x'}; {request with graph_root = hash 'x'};
    {request with model_root = hash 'x'}; {request with program_root = hash 'x'};
    {request with executor_root = hash 'x'}; {request with session_id = hash 'x'};
    {request with sequence = 4L}; {request with max_output_bytes = 257};
    {request with fuel_limit = Some 4999}; {request with guest_bytes = Some 131072};
    {request with memory_bytes = Some 262144}; {request with unit_price = Some 1L};
    {request with fee_cap = Some 10001L}; {request with input_bytes = 3}] in
  List.iter (fun request ->
    let changed = signed {call with request} in
    expect "changed request accepted" (Result.is_error (verify changed value))) changes;
  List.iter (fun changed ->
    expect "changed call accepted" (Result.is_error (verify (signed changed) value)))
    [{call with valid_until = Some 51L}; {call with coordinator = address payer};
     {call with method_name = "other"; request = {request with
       input_hash = Protocol.input_hash ~method_name:"other" ~params_json:"[]"}};
     {call with params_json = "[1]"; request = {request with input_bytes = 3;
       input_hash = Protocol.input_hash ~method_name:"predict" ~params_json:"[1]"}}];
  expect "changed coordinator signature accepted"
    (Result.is_error (verify {call with signature = String.make 64 '\000'} value));
  let unpriced = {call with request = {request with unit_price = None; fee_cap = None}} |> signed in
  expect "unpriced call accepted" (Result.is_error (Funding.prepare ~call:unpriced ~target ~nonce:7L));
  let high = {call with request = {request with fee_cap = Some Int64.max_int}} |> signed in
  let terms = Funding.prepare ~call:high ~target ~nonce:Int64.max_int |> get in
  let high_value = Funding.authorize terms ~signature:(sign payer (Funding.sign_bytes terms)) |> get in
  expect "large valid cap refused" (verify ~nonce:Int64.max_int high high_value = Ok Int64.max_int)

let test_wire () =
  let call = call () in
  let value = permission call in
  let raw = Funding.encode value in
  let decoded = Funding.decode raw |> get in
  expect "wire changed" (Funding.encode decoded = raw);
  expect "decoded scope differs" (verify call decoded = Ok 10_000L);
  for length = 0 to String.length raw - 1 do
    expect "truncated permission accepted" (Result.is_error (Funding.decode (String.sub raw 0 length)))
  done;
  for index = 0 to String.length raw - 1 do
    let bytes = Bytes.of_string raw in
    Bytes.set bytes index (Char.chr (Char.code (Bytes.get bytes index) lxor 1));
    expect "modified permission accepted" (Result.is_error (Funding.decode (Bytes.to_string bytes)))
  done;
  expect "extra bytes accepted" (Result.is_error (Funding.decode (raw ^ "x")));
  expect "large permission accepted" (Result.is_error (Funding.decode (String.make 1025 'x')))

let test_header () =
  let call = call () in
  let header = Protocol.header call |> get in
  expect "header intent differs" (Protocol.header_intent header = Protocol.intent_sign_bytes call);
  expect "header call differs" (Protocol.header_sign header = Protocol.call_sign_bytes call);
  let raw = Protocol.encode_header header in
  let decoded = Protocol.decode_header raw |> get in
  expect "header round trip differs" (decoded = header);
  expect "header oversized" (String.length raw < 2048);
  let terms = Funding.prepare_header ~header:decoded ~target ~nonce:7L |> get in
  expect "header permission differs" (terms = (Funding.prepare ~call ~target ~nonce:7L |> get));
  let value = permission call in
  let verify header = Funding.verify_header ~chain_id:call.request.chain_id
    ~epoch:42L ~nonce:7L ~target ~header value in
  expect "header permission refused" (verify decoded = Ok 10_000L);
  for length = 0 to String.length raw - 1 do
    expect "short header accepted" (Result.is_error (Protocol.decode_header (String.sub raw 0 length)))
  done;
  for index = 0 to String.length raw - 1 do
    let bytes = Bytes.of_string raw in
    Bytes.set bytes index (Char.chr (Char.code (Bytes.get bytes index) lxor 1));
    match Protocol.decode_header (Bytes.to_string bytes) with
    | Error _ -> ()
    | Ok header -> expect "altered header funded" (Result.is_error (verify header))
  done;
  expect "extra header bytes accepted" (Result.is_error (Protocol.decode_header (raw ^ "x")));
  expect "large header accepted" (Result.is_error (Protocol.decode_header (String.make 2049 'x')));
  let request = call.request in
  let variants = [
    {call with valid_until = None; request = {request with unit_price = None; fee_cap = None;
      memory_bytes = None; guest_bytes = None; fuel_limit = None}};
    {call with request = {request with unit_price = None; fee_cap = None;
      memory_bytes = None; guest_bytes = None; fuel_limit = None}};
    {call with request = {request with unit_price = None; fee_cap = None;
      memory_bytes = None; guest_bytes = None}};
    {call with request = {request with unit_price = None; fee_cap = None; memory_bytes = None}};
    {call with request = {request with unit_price = None; fee_cap = None}};
    {call with request = {request with memory_bytes = Some Certificate.max_memory_bytes}}] in
  List.iter (fun call ->
    let call = signed call in
    let header = Protocol.header call |> get in
    let decoded = Protocol.decode_header (Protocol.encode_header header) |> get in
    expect "header optional fields changed" (header = decoded);
    expect "optional intent bytes changed" (Protocol.header_intent decoded = Protocol.intent_sign_bytes call);
    expect "optional call bytes changed" (Protocol.header_sign decoded = Protocol.call_sign_bytes call)) variants;
  expect "body mismatch became header"
    (Result.is_error (Protocol.header {call with params_json = "[1]"}));
  expect "missing method became header"
    (Result.is_error (Protocol.header {call with method_name = ""}))

let () =
  test_signature ();
  test_scope ();
  test_wire ();
  test_header ();
  print_endline "status = pass test = compute_funding"