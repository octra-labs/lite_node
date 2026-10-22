(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Certificate = Octra_consensus.Resource_compute_certificate
module Config = Octra_node_runtime.Resource_compute_provider_config
module Engine = Octra_node_runtime.Resource_compute_provider_engine
module Protocol = Octra_consensus.Resource_compute_protocol
module Provider_rpc = Octra_node_runtime.Resource_compute_provider_rpc
module Rpc = Octra_node_runtime.Resource_compute_rpc
module Selection = Octra_consensus.Resource_compute_selection
module Service = Octra_node_runtime.Resource_compute_service
module Record = Octra_node_runtime.Compute_record
module Read = Octra_node_runtime.Compute_read
module Funding = Octra_core.Compute_funding
module Store = Octra_core.Store_irmin
module Ledger = Octra_core.Ledger

type identity = {
  address : string;
  private_key : Mirage_crypto_ec.Ed25519.priv;
  public_key : string;
}

type node = {
  identity : identity;
  service : Service.t;
  disk : Octra_core.Compute_history.t;
}

type cut = Keep | At_plan | At_call | At_vote | Lost | Cold

let fail message =
  failwith ("test_resource_compute_service: " ^ message)

let raw_hash label =
  Octra_net.Hash_domain.hash "test:resource_compute_service" label

let hex_hash label =
  Octra_node_runtime.Text.raw_to_hex (raw_hash label)

let identity () =
  let private_key, public_key = Mirage_crypto_ec.Ed25519.generate () in
  let public_key = Mirage_crypto_ec.Ed25519.pub_to_octets public_key in
  let address =
    public_key
    |> Base64.encode_exn
    |> Octra_core.Crypto.Address.address_from_pubkey in
  { address; private_key; public_key }

let sign identity message =
  Mirage_crypto_ec.Ed25519.sign ~key:identity.private_key message

let read_request node caller epoch job_id =
  let read = Read.{chain_id = "octra-resource-compute-service-test";
    node_id = node.address; caller = caller.address; caller_public_key = caller.public_key;
    job_id = Octra_node_runtime.Text.hex_to_string job_id;
    epoch_id = epoch; expires_epoch = Int64.add epoch Read.max_epochs;
    caller_signature = String.make 64 '\000'} in
  {read with caller_signature = sign caller (Read.sign_bytes read)}

let read_params node caller epoch job_id =
  let read = read_request node caller epoch job_id in
  `Assoc [
    "chain_id", `String read.chain_id;
    "node_id", `String read.node_id;
    "caller", `String read.caller;
    "caller_public_key", `String (Base64.encode_exn read.caller_public_key);
    "caller_signature", `String (Base64.encode_exn read.caller_signature);
    "job_id", `String job_id;
    "epoch_id", `String (Int64.to_string read.epoch_id);
    "expires_epoch", `String (Int64.to_string read.expires_epoch)]

let test_read_scope () =
  Mirage_crypto_rng_unix.use_default ();
  let node = identity () in
  let caller = identity () in
  let other = identity () in
  let read = read_request node caller 100L (hex_hash "read job") in
  let verify epoch value = Read.verify ~chain_id:read.chain_id
    ~node_id:node.address ~epoch value in
  if not (verify 100L read && verify 116L read) then fail "signed read interval";
  let forged = {read with caller = other.address} in
  let borrowed = {read with caller_public_key = other.public_key} in
  let impostor = {borrowed with caller_signature = sign other (Read.sign_bytes borrowed)} in
  let changed = [forged; borrowed; impostor; {read with job_id = raw_hash "another job"};
    {read with node_id = other.address}; {read with chain_id = "another chain"};
    {read with epoch_id = 99L}; {read with expires_epoch = 115L};
    {read with expires_epoch = 117L}; {read with caller_signature = String.make 64 '\000'}] in
  List.iter (fun value -> if verify 100L value then fail "changed read accepted") changed;
  if verify 99L read || verify 117L read then fail "read time accepted";
  let other_scope = {read with node_id = other.address} in
  let other_scope = {other_scope with caller_signature = sign caller (Read.sign_bytes other_scope)} in
  if verify 100L other_scope then fail "signed read accepted at another node";
  let other_chain = {read with chain_id = "another chain"} in
  let other_chain = {other_chain with caller_signature = sign caller (Read.sign_bytes other_chain)} in
  if verify 100L other_chain then fail "signed read accepted on another chain";
  List.iter (fun value -> if Read.shape value then fail "invalid read shape accepted") [
    {read with chain_id = ""}; {read with chain_id = String.make 129 'a'};
    {read with node_id = "invalid"}; {read with caller = "invalid"};
    {read with caller_public_key = String.make 31 '\000'};
    {read with caller_signature = String.make 63 '\000'};
    {read with job_id = String.make 31 '\000'};
    {read with epoch_id = -1L}; {read with expires_epoch = 99L}];
  let edge = {read with epoch_id = Int64.sub Int64.max_int Read.max_epochs;
    expires_epoch = Int64.max_int} in
  let edge = {edge with caller_signature = sign caller (Read.sign_bytes edge)} in
  if not (verify Int64.max_int edge) then fail "read maximum epoch refused"

let test_read_vector () =
  let private_key = match Mirage_crypto_ec.Ed25519.priv_of_octets (String.make 32 '\001') with
    | Ok key -> key
    | Error _ -> fail "read vector key" in
  let read = Read.{chain_id = "octra-test-chain";
    node_id = "oct89dkqqjMw9HTi1pqHbANSNwioHuMhmpTCXapTmfUr1EW";
    caller = "oct4XmjKEd9A96KhoMX94zWJmd28dcPisbWGYWtad1dQ9v5";
    caller_public_key = Base64.decode_exn "iojj3XQJ8ZX9UtstPLpdcspnCb8dlBIb83SIAbQPb1w=";
    job_id = String.make 32 '\119'; epoch_id = 50L; expires_epoch = 58L;
    caller_signature = String.make 64 '\000'} in
  let digest = Read.sign_bytes read in
  if Octra_node_runtime.Text.raw_to_hex digest <>
      "b63b9775604376eccec2a811141a5eb6339b1c85b2826cd2ca7477b8361e096d" then
    fail "read vector digest";
  let caller_signature = Mirage_crypto_ec.Ed25519.sign ~key:private_key digest in
  if Base64.encode_exn caller_signature <>
      "d5NVrCVIUPhG620vWNWaSSu9q/qnzYezU1w/9S85JqA296IvVI2HHo4DA6EkOsLFBlsIYAXi5WWms93jtMSzDA==" then
    fail "read vector signature";
  if not (Read.verify ~chain_id:read.chain_id ~node_id:read.node_id
      ~epoch:50L {read with caller_signature}) then fail "read vector verification"

let history data_dir identity =
  let path = Filename.concat data_dir identity.address in
  (try Unix.mkdir path 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  Octra_core.Compute_history.open_db
    ~chain_id:"octra-resource-compute-service-test" ~node_id:identity.address path

let cancel_work disk caller session =
  if Octra_core.Compute_history.known_job disk ~caller:caller.address ~session <> Ok true then
    if Octra_core.Compute_history.reserve disk ~caller:caller.address ~session
        ~scope:"cancel work" ~sequence:1L ~request:(raw_hash "cancel work") <> Ok true then
      fail "cancel work setup"

let now () = Int64.of_float (Unix.gettimeofday () *. 1000.)

let limits = Config.{
  accelerator = Cpu;
  lanes = 1;
  memory_bytes = 4_294_967_296L;
  max_request_bytes = 65_536;
  max_response_bytes = 262_144;
  timeout_seconds = 300;
}

let program_raw =
  "resource-compute-program"

let program_root =
  Digestif.SHA256.(to_hex (digest_string program_raw))

let graph_root =
  hex_hash "graph"

let model_root =
  hex_hash "model"

let executor_root =
  hex_hash "executor"

let evidence_root =
  hex_hash "evidence"

let circle_id =
  "oct1111111111111111111111111111111111111111BURN"

let model_epoch =
  90L

let fund_roots = Hashtbl.create 16

let state_root epoch =
  match Hashtbl.find_opt fund_roots epoch with
  | Some root -> root
  | None -> raw_hash ("state:" ^ Int64.to_string epoch)

let no_funding _ _ = Lwt.return_ok None

let engine ?(output = "{\"tokens\":[198]}") ?(check = fun _ -> ()) execution_count executed =
  let deps = Engine.{
    program = (fun ~epoch_id:_ ~state_root:_ ~circle_id ->
      Lwt.return
        (Ok Engine.{
           circle_id;
           code_b64 = Base64.encode_exn program_raw;
           code_hash = program_root;
           runtime = "wasm_v1";
         }));
    storage = (fun ~epoch_id:_ ~state_root:_ ~circle_id:_ ->
      Lwt.return (Ok []));
    load_model = (fun ~cancel:_ request ->
      Lwt.return
        (Ok Engine.{
           snapshots = [request.Provider_rpc.model_epoch, request.model_state_root];
           circle_id = request.circle_id;
           graph_root = request.graph_root;
           model_root = request.model_root;
           program_root = request.program_root;
           executor_root = request.executor_root;
           cache_key = Octra_core.Resource_compute_graph.cache_key
             ~circle_id:request.circle_id ~graph_root:request.graph_root;
           cache_root = hex_hash "cache";
           entries = 291;
           bytes = 409_000_000L;
         }));
    drop_model = (fun _ -> Lwt.return (Ok ()));
    hold_model = (fun _ -> Ok (Octra_core.Circle_wasm_native.lease_new ()));
    self_test = (fun () ->
      Lwt.return
        (Ok Provider_rpc.{
           executor_root;
           evidence_root;
           profile = "q24-v1";
         }));
    execute = (fun ~model:_ ~program:_ ~storage:_ request ->
      check request;
      incr execution_count;
      Lwt_condition.broadcast executed ();
      Lwt.return
        (Ok Engine.{
           output_json = output;
           trace_root = hex_hash "trace";
           steps = 20_000L;
           operations = 1_200L;
           effort_used = 20_000;
         }));
  } in
  Engine.create ~limits ~deps

let field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let status_name json =
  match field "status" json with
  | Some (`String value) -> value
  | _ -> fail "status field"

let intent_id call =
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_intent_id" (fun buffer ->
    Octra_net.Oce1.put_hash32 buffer (Protocol.intent_sign_bytes call);
    Octra_net.Oce1.put_sig64 buffer call.Protocol.caller_signature)

let build_request ?(unit_price = 2L) ?(fee_cap = 10_000_000L)
    ?(memory_bytes = 67_108_864) ?(guest_bytes = 65_536)
    ?(fuel_limit = 5_000_000) ?valid_until validator_set coordinator caller head_epoch session_id sequence =
  let valid_until = Option.value valid_until ~default:(Int64.add head_epoch 64L) in
  let params_json = "[\"15496,11,995\",32]" in
  let certificate_request = Certificate.{
    unit_price = Some unit_price;
    fee_cap = Some fee_cap;
    memory_bytes = Some memory_bytes;
    guest_bytes = Some guest_bytes;
    fuel_limit = Some fuel_limit;
    chain_id = "octra-resource-compute-service-test";
    epoch_id = head_epoch;
    expires_epoch = valid_until;
    validator_set_root = Octra_consensus.C_config.validator_set_hash validator_set;
    selection_root = String.make 32 '\000';
    circle_id;
    state_root = state_root head_epoch;
    model_epoch;
    model_state_root = state_root model_epoch;
    graph_root = Octra_node_runtime.Text.hex_to_string graph_root;
    model_root = Octra_node_runtime.Text.hex_to_string model_root;
    program_root = Octra_node_runtime.Text.hex_to_string program_root;
    executor_root = Octra_node_runtime.Text.hex_to_string executor_root;
    caller = caller.address;
    session_id;
    sequence;
    input_hash = Protocol.input_hash ~method_name:"complete" ~params_json;
    input_bytes = String.length "complete" + String.length params_json;
    max_output_bytes = 4096;
  } in
  let unsigned_call = Protocol.{
    request = certificate_request;
    valid_until = Some certificate_request.expires_epoch;
    method_name = "complete";
    params_json;
    caller_public_key = caller.public_key;
    caller_signature = String.make 64 '\000';
    coordinator = coordinator.address;
    signature = String.make 64 '\000';
  } in
  let caller_signature = sign caller (Protocol.intent_sign_bytes unsigned_call) in
  `Assoc [
    "unit_price", `String (Int64.to_string unit_price);
    "fee_cap", `String (Int64.to_string fee_cap);
    "memory_bytes", `Int memory_bytes;
    "guest_bytes", `Int guest_bytes;
    "fuel_limit", `Int fuel_limit;
    "valid_until", `String (Int64.to_string certificate_request.expires_epoch);
    "caller", `String caller.address;
    "caller_public_key", `String (Base64.encode_exn caller.public_key);
    "caller_signature", `String (Base64.encode_exn caller_signature);
    "circle_id", `String circle_id;
    "executor_root", `String executor_root;
    "graph_root", `String graph_root;
    "max_output_bytes", `Int 4096;
    "method_name", `String "complete";
    "model_epoch", `String (Int64.to_string model_epoch);
    "model_root", `String model_root;
    "model_state_root", `String (Octra_node_runtime.Text.raw_to_hex (state_root model_epoch));
    "params_json", `String params_json;
    "program_root", `String program_root;
    "sequence", `String (Int64.to_string sequence);
    "session_id", `String (Octra_node_runtime.Text.raw_to_hex session_id);
  ]

let cancellation_params ?until caller session_id =
  let caller_signature =
    Protocol.cancel_sign_bytes Protocol.{chain_id = "octra-resource-compute-service-test";
      caller = caller.address; caller_public_key = caller.public_key; session_id; until;
      caller_signature = String.make 64 '\000'}
    |> sign caller in
  `Assoc ((match until with None -> [] | Some epoch -> ["valid_until", `String (Int64.to_string epoch)]) @ [
    "caller", `String caller.address;
    "caller_public_key", `String (Base64.encode_exn caller.public_key);
    "caller_signature", `String (Base64.encode_exn caller_signature);
    "session_id", `String (Octra_node_runtime.Text.raw_to_hex session_id);
  ])

let signed_cancel ?until ?(chain_id = "octra-resource-compute-service-test") caller session_id =
  let cancel = Protocol.{chain_id; caller = caller.address;
    caller_public_key = caller.public_key; session_id;
    caller_signature = String.make 64 '\000'; until} in
  {cancel with caller_signature = sign caller (Protocol.cancel_sign_bytes cancel)}

let signed_round ?(epoch = 100L) ?(expires = 164L) ?(round = 2) origin cancel =
  let value = Protocol.{cancel; origin = origin.address; epoch; expires; round;
    signature = String.make 64 '\000'} in
  {value with signature = sign origin (Protocol.cancel_round_sign value)}

let signed_ack origin who delivery =
  let ack = Protocol.{delivery; origin_key = origin.public_key; node_id = who.address;
    node_key = who.public_key; epoch = delivery.epoch; signature = String.make 64 '\000'} in
  {ack with signature = sign who (Protocol.cancel_ack_sign ack)}

let test_cancel_wire () =
  Mirage_crypto_rng_unix.use_default ();
  if Protocol.cancel_rounds <> Octra_core.Compute_history.notice_rounds then
    fail "cancel round limits differ";
  let caller = identity () in
  let cancel = signed_cancel caller (raw_hash "wire cancel") in
  let original = Octra_net.Hash_domain.hash_encoded "octra:resource_compute_cancel"
    (fun buffer ->
      Octra_net.Oce1.put_string buffer cancel.chain_id;
      Octra_net.Oce1.put_string buffer cancel.caller;
      Octra_net.Oce1.put_hash32 buffer cancel.caller_public_key;
      Octra_net.Oce1.put_hash32 buffer cancel.session_id) in
  if Protocol.cancel_sign_bytes cancel <> original then fail "cancel signed bytes changed";
  let wire = Protocol.encode (Protocol.Cancel cancel) in
  if Protocol.decode wire <> Protocol.Cancel cancel then fail "cancel codec roundtrip";
  let origin = identity () in
  let peer = identity () in
  let timed = signed_cancel ~until:164L caller cancel.session_id in
  if Protocol.cancel_sign_bytes timed = Protocol.cancel_sign_bytes cancel then fail "cancel deadline not signed";
  let changed = {timed with until = Some 165L} in
  if Octra_ed25519.verify ~pub:caller.public_key ~msg:(Protocol.cancel_sign_bytes changed)
      timed.caller_signature then fail "cancel deadline can be changed";
  let round = signed_round origin timed in
  let ack = signed_ack origin peer round in
  List.iter (fun value ->
    if Protocol.decode (Protocol.encode value) <> value then fail "timed cancel codec")
    [Protocol.Cancel timed; Protocol.Cancel_round round; Protocol.Cancel_ack ack];
  let before = Octra_net.Oce1.encode (fun buffer ->
    Octra_net.Oce1.put_u8 buffer 7;
    Octra_net.Oce1.put_string buffer cancel.chain_id;
    Octra_net.Oce1.put_string buffer cancel.caller;
    Octra_net.Oce1.put_hash32 buffer cancel.caller_public_key;
    Octra_net.Oce1.put_hash32 buffer cancel.session_id;
    Octra_net.Oce1.put_sig64 buffer cancel.caller_signature) in
  if wire <> before then fail "cancel original wire changed";
  List.iter (fun payload ->
    let refused = try ignore (Protocol.decode payload); false with _ -> true in
    if not refused then fail "cancel codec accepted incomplete or extra bytes")
    [String.sub wire 0 (String.length wire - 1); wire ^ "x"];
  let refused = try
    ignore (Protocol.encode (Protocol.Cancel {cancel with caller = String.make 129 'a'}));
    false with _ -> true in
  if not refused then fail "cancel codec caller limit";
  let origin = identity () in
  let value = signed_round origin cancel in
  let wire = Protocol.encode (Protocol.Cancel_round value) in
  if Protocol.decode wire <> Protocol.Cancel_round value then fail "cancel round codec";
  List.iter (fun changed ->
    if Protocol.cancel_round_sign changed = Protocol.cancel_round_sign value then
      fail "cancel round unsigned field")
    [{value with origin = caller.address}; {value with epoch = 101L};
     {value with expires = 163L}; {value with round = 3};
     {value with cancel = signed_cancel caller (raw_hash "other session")}];
  List.iter (fun payload ->
    if (try ignore (Protocol.decode payload); true with _ -> false) then
      fail "cancel round malformed accepted")
    [String.sub wire 0 (String.length wire - 1); wire ^ "x"];
  List.iter (fun value ->
    if (try ignore (Protocol.encode (Protocol.Cancel_round value)); true with _ -> false) then
      fail "cancel round range accepted")
    [{value with round = 0}; {value with round = 65}; {value with epoch = -1L};
     {value with expires = 99L}; {value with expires = 165L}];
  let ack = signed_ack origin caller value in
  let wire = Protocol.encode (Protocol.Cancel_ack ack) in
  if Protocol.decode wire <> Protocol.Cancel_ack ack then fail "cancel ack codec";
  List.iter (fun changed ->
    if Protocol.cancel_ack_sign changed = Protocol.cancel_ack_sign ack then
      fail "cancel ack unsigned field")
    [{ack with node_id = origin.address}; {ack with epoch = 101L};
     {ack with origin_key = caller.public_key}; {ack with node_key = origin.public_key};
     {ack with delivery = signed_round ~round:3 origin cancel}];
  List.iter (fun wire ->
    if (try ignore (Protocol.decode wire); true with _ -> false) then
      fail "cancel ack malformed accepted")
    [String.sub wire 0 (String.length wire - 1); wire ^ "x"]

let test_usage (call : Protocol.call) =
  let request = {call.request with fuel_limit = Some 2000} in
  let nodes = List.init 5 (fun _ -> identity ()) in
  let vote request node steps =
    let value = Certificate.{request_id = Certificate.request_id request;
      node_id = node.address; output_hash = Protocol.output_hash "{}";
      trace_root = raw_hash "usage"; output_bytes = 2; steps; operations = 1L;
      signature = String.make 64 '\000'} in
    {value with signature = sign node (Certificate.vote_sign_bytes value)} in
  let decide request votes = Certificate.decide ~chain_id:request.Certificate.chain_id
    ~current_epoch:request.epoch_id ~max_ttl:64L
    ~selected:(List.map (fun node -> node.address) nodes) ~signer_floor:4 ~weight_floor:(Z.of_int 4)
    ~weight_of:(fun _ -> Some Z.one) ~verify_signature:(fun value ->
      let node = List.find (fun node -> node.address = value.Certificate.node_id) nodes in
      Octra_ed25519.verify ~pub:node.public_key ~msg:(Certificate.vote_sign_bytes value) value.signature)
    request votes in
  let votes = List.map (fun node -> vote request node 1234L) nodes in
  let accepted = match decide request votes with
    | Certificate.Agreed value when value.steps = 1234L -> value
    | _ -> fail "agreed usage absent" in
  begin match decide request (List.rev votes) with
  | Certificate.Agreed value when value.root = accepted.root -> ()
  | _ -> fail "usage result order differs"
  end;
  let refused values = match decide request values with
    | Certificate.Insufficient -> () | _ -> fail "invalid usage reached quorum" in
  refused (List.mapi (fun index node -> vote request node (if index < 3 then 1234L else 1235L)) nodes);
  refused (List.map (fun node -> vote request node 2001L) nodes);
  refused (List.map (fun (value : Certificate.vote) -> {value with steps = 1235L}) votes);
  let first = List.hd nodes in
  refused (List.init 5 (fun _ -> vote request first 1234L));
  refused [List.hd votes; vote request first 1235L;
    List.nth votes 1; List.nth votes 2; List.nth votes 3];
  let prior = {request with unit_price = None; fee_cap = None;
    memory_bytes = None; guest_bytes = None; fuel_limit = None} in
  begin match decide prior (List.map (fun node -> vote prior node 2001L) nodes) with
  | Certificate.Agreed _ -> () | _ -> fail "prior usage rules changed"
  end

let test_fetch_wire signer (call : Protocol.call) =
  test_usage call;
  let old_call = {call with valid_until = None;
    request = {call.request with unit_price = None; fee_cap = None;
      memory_bytes = None; guest_bytes = None; fuel_limit = None}} in
  let address = "oct1111111111111111111111111111111111111111BURN" in
  let vector = {call with valid_until = Some 58L; caller_public_key = String.make 32 '\102';
    method_name = "complete"; params_json = "[\"15496,11,995\",32]";
    request = {call.request with unit_price = None; fee_cap = None;
      memory_bytes = None; guest_bytes = None; fuel_limit = None; chain_id = "octra-test-chain"; circle_id = address;
      caller = address; epoch_id = 50L; expires_epoch = 58L; model_epoch = 42L;
      model_state_root = String.make 32 '\017'; graph_root = String.make 32 '\034';
      model_root = String.make 32 '\051'; program_root = String.make 32 '\068';
      executor_root = String.make 32 '\085'; session_id = String.make 32 '\119';
      validator_set_root = String.make 32 '\021'; selection_root = String.make 32 '\022';
      state_root = String.make 32 '\023';
      sequence = 9L; max_output_bytes = 4096}} in
  if Octra_node_runtime.Text.raw_to_hex (Protocol.intent_sign_bytes vector) <>
      "6c9dbd5dcff9e30a9acb601db59b9e72d9661948784f66d822eae4ddd32f3186" then
    fail "timed intent vector differs";
  if Octra_node_runtime.Text.raw_to_hex (Protocol.intent_sign_bytes {vector with valid_until = None}) <>
      "a4c359b4b9df68eb57b43a6362298712b3c522fb8e0840a6bb0dcc92e932f3b1" then
    fail "existing intent vector differs";
  let metered = {vector with request = {vector.request with fuel_limit = Some 2_000_000_000}} in
  if Octra_node_runtime.Text.raw_to_hex (Protocol.intent_sign_bytes metered) <>
      "4d7cc5c2db90e51368e03f40acf9a7640ee0c4f3a3c735bda59d382a212a7242" then
    fail "fuel intent vector differs";
  if Octra_node_runtime.Text.raw_to_hex (Certificate.request_id metered.request) <>
      "95b5f1274ce60d3053e69785a2bc56228571b2c62866aa997ddabdf6e9c2d411" then
    fail "fuel request vector differs";
  let paged = {metered with request = {metered.request with guest_bytes = Some 67_108_864}} in
  List.iter (fun bytes ->
    let call = {paged with request = {paged.request with guest_bytes = Some bytes}} in
    if Protocol.call_shape call || Certificate.request_shape call.request then
      fail "invalid guest shape accepted") [0; 65_535; 65_537; 67_174_400];
  if Octra_node_runtime.Text.raw_to_hex (Protocol.intent_sign_bytes paged) <>
      "4510144758f8b7ec82598ac0130a6ae59b05868c1b9dc2445489caa10bf3ce86" then
    fail "guest intent vector differs";
  if Octra_node_runtime.Text.raw_to_hex (Certificate.request_id paged.request) <>
      "9113b6f1a8bb68da4de8edaf1400b78c7f6e945260d92bd24d3d2c286e028088" then
    fail "guest request vector differs";
  let changed = {paged with request = {paged.request with guest_bytes = Some 65_536}} in
  if Protocol.intent_sign_bytes changed = Protocol.intent_sign_bytes paged
     || Certificate.request_id changed.request = Certificate.request_id paged.request then
    fail "guest bytes not signed";
  let memory = {paged with request = {paged.request with memory_bytes = Some 1_073_741_824}} in
  if Octra_node_runtime.Text.raw_to_hex (Protocol.intent_sign_bytes memory) <>
      "d9a101df7e3828f2f9357adfe2b9a299821e3bf64ceccca453f8f94cea3dd204"
     || Octra_node_runtime.Text.raw_to_hex (Certificate.request_id memory.request) <>
      "794c0e22e7d59407ae4077c13ba49267f5834cf02c2efc66bf283c547df1fafb" then
    fail "memory signed vectors differ";
  List.iter (fun bytes ->
    let changed = {memory with request = {memory.request with memory_bytes = Some bytes}} in
    if Protocol.call_shape changed || Certificate.request_shape changed.request then
      fail "invalid memory shape accepted") [0; -1; Certificate.max_memory_bytes + 1];
  let changed = {memory with request = {memory.request with memory_bytes = Some 1}} in
  if Protocol.intent_sign_bytes changed = Protocol.intent_sign_bytes memory
     || Certificate.request_id changed.request = Certificate.request_id memory.request then
    fail "memory limit not signed";
  let changed = {memory with request = {memory.request with guest_bytes = None}} in
  if Protocol.call_shape changed || Certificate.request_shape changed.request then
    fail "memory without pages accepted";
  if Protocol.decode (Protocol.encode (Protocol.Call paged)) <> Protocol.Call paged
     || Protocol.decode (Protocol.encode (Protocol.Call memory)) <> Protocol.Call memory then
    fail "memory call codec differs";
  let maximum = {memory with request = {memory.request with
    memory_bytes = Some Certificate.max_memory_bytes}} in
  if Protocol.decode (Protocol.encode (Protocol.Call maximum)) <> Protocol.Call maximum then
    fail "unsigned memory maximum changed";
  let priced = {memory with request = {memory.request with
    unit_price = Some 2L; fee_cap = Some 4_000_000_000L}} in
  if Octra_node_runtime.Text.raw_to_hex (Protocol.intent_sign_bytes priced) <>
      "4f24d261390292444c92631ff66356193fa98ede4b2c54bf0279ff7ee5e0363e"
     || Octra_node_runtime.Text.raw_to_hex (Certificate.request_id priced.request) <>
      "d67dd52bb926ee3a8fdb81731975b85476861d884bc35a8290025e82210e5610" then
    fail "fee signed vectors differ";
  List.iter (fun (price, cap) ->
    let changed = {priced with request = {priced.request with unit_price = price; fee_cap = cap}} in
    if Protocol.call_shape changed || Certificate.request_shape changed.request then
      fail "invalid fee terms accepted")
    [None, Some 1L; Some 1L, None; Some 0L, Some 4_000_000_000L;
     Some (-1L), Some 4_000_000_000L; Some 2L, Some 3_999_999_999L;
     Some Int64.max_int, Some Int64.max_int];
  List.iter (fun changed ->
    if Protocol.intent_sign_bytes changed = Protocol.intent_sign_bytes priced
       || Certificate.request_id changed.request = Certificate.request_id priced.request then
      fail "fee terms not signed")
    [{priced with request = {priced.request with unit_price = Some 1L}};
     {priced with request = {priced.request with fee_cap = Some 4_000_000_001L}}];
  let maximum = {priced with request = {priced.request with fuel_limit = Some 1;
    unit_price = Some Int64.max_int; fee_cap = Some Int64.max_int}} in
  if Protocol.decode (Protocol.encode (Protocol.Call maximum)) <> Protocol.Call maximum
     || Protocol.decode (Protocol.encode (Protocol.Call priced)) <> Protocol.Call priced then
    fail "fee call codec differs";
  if Octra_node_runtime.Text.raw_to_hex (Certificate.request_id vector.request) <>
      "be336e4df14570a2cb2933f4410da71c5bc624f89e9debf9573a7bff1419019f" then
    fail "historical request vector differs";
  let changed = {metered with request = {metered.request with fuel_limit = Some 1}} in
  if Protocol.intent_sign_bytes changed = Protocol.intent_sign_bytes metered
     || Certificate.request_id changed.request = Certificate.request_id metered.request then
    fail "fuel not signed";
  let legacy = Octra_net.Oce1.encode (fun buf ->
    let request = call.request in
    let module Wire = Octra_net.Oce1 in
    Wire.put_u8 buf 5;
    Wire.put_string buf request.chain_id;
    Wire.put_u64 buf request.epoch_id;
    Wire.put_u64 buf request.expires_epoch;
    Wire.put_hash32 buf request.validator_set_root;
    Wire.put_hash32 buf request.selection_root;
    Wire.put_string buf request.circle_id;
    Wire.put_hash32 buf request.state_root;
    Wire.put_u64 buf request.model_epoch;
    Wire.put_hash32 buf request.model_state_root;
    Wire.put_hash32 buf request.graph_root;
    Wire.put_hash32 buf request.model_root;
    Wire.put_hash32 buf request.program_root;
    Wire.put_hash32 buf request.executor_root;
    Wire.put_string buf request.caller;
    Wire.put_hash32 buf request.session_id;
    Wire.put_u64 buf request.sequence;
    Wire.put_hash32 buf request.input_hash;
    Wire.put_u32_int buf request.input_bytes;
    Wire.put_u32_int buf request.max_output_bytes;
    Wire.put_string buf call.method_name;
    Wire.put_string buf call.params_json;
    Wire.put_hash32 buf call.caller_public_key;
    Wire.put_sig64 buf call.caller_signature;
    Wire.put_string buf call.coordinator;
    Wire.put_sig64 buf call.signature) in
  if legacy <> Protocol.encode (Protocol.Call old_call) then fail "existing call bytes changed";
  if Protocol.decode legacy <> Protocol.Call old_call then fail "existing call decode differs";
  if Protocol.decode (Protocol.encode (Protocol.Call call)) <> Protocol.Call call then
    fail "metered call decode differs";
  let timed = {call with request = {call.request with unit_price = None; fee_cap = None;
    memory_bytes = None; guest_bytes = None; fuel_limit = None}} in
  if Protocol.decode (Protocol.encode (Protocol.Call timed)) <> Protocol.Call timed then
    fail "timed call decode differs";
  let fuel = {call with request = {call.request with unit_price = None; fee_cap = None;
    memory_bytes = None; guest_bytes = None}} in
  if Protocol.decode (Protocol.encode (Protocol.Call fuel)) <> Protocol.Call fuel then
    fail "fuel call decode differs";
  let pages = {call with request = {call.request with unit_price = None; fee_cap = None; memory_bytes = None}} in
  let memory = {call with request = {call.request with unit_price = None; fee_cap = None}} in
  if Protocol.decode (Protocol.encode (Protocol.Call memory)) <> Protocol.Call memory then
    fail "historical memory codec differs";
  let fetch = Protocol.{call; target = signer.address; epoch_id = call.request.epoch_id;
    expires_epoch = Int64.add call.request.epoch_id 4L; round = 0;
    signature = String.make 64 '\000'} in
  let digest = Protocol.fetch_sign_bytes fetch in
  let fetch = {fetch with signature = sign signer digest} in
  let differs value =
    if Protocol.fetch_shape value && Protocol.fetch_sign_bytes value = digest then
      fail "fetch signed scope omitted" in
  List.iter differs [
    {fetch with target = (identity ()).address};
    {fetch with epoch_id = Int64.succ fetch.epoch_id};
    {fetch with expires_epoch = Int64.pred fetch.expires_epoch};
    {fetch with round = 1};
    {fetch with call = {call with signature = String.make 64 '\000'}}];
  List.iter (fun value -> if Protocol.fetch_shape value then fail "fetch shape accepted") [
    {fetch with target = String.make 129 'x'};
    {fetch with round = -1}; {fetch with round = Protocol.fetch_rounds};
    {fetch with epoch_id = -1L};
    {fetch with expires_epoch = Int64.add fetch.epoch_id 5L}];
  let vote = Certificate.{request_id = Certificate.request_id call.request;
    node_id = signer.address; output_hash = Protocol.output_hash "{}";
    trace_root = raw_hash "fetch trace"; output_bytes = 2; steps = 1L; operations = 1L;
    signature = String.make 64 '\000'} in
  let result = Protocol.{vote = {vote with signature = sign signer (Certificate.vote_sign_bytes vote)};
    output_json = "{}"} in
  List.iter (fun message ->
    let wire = Protocol.encode message in
    if Protocol.decode wire <> message then fail "fetch wire differs";
    List.iter (fun broken ->
      match Protocol.decode broken with
      | _ -> fail "fetch wire accepted extra or missing data"
      | exception _ -> ()) [wire ^ "x"; String.sub wire 0 (String.length wire - 1)])
    [Protocol.Call call; Protocol.Call old_call;
     Protocol.Fetch fetch; Protocol.Answer (fetch, result);
     Protocol.Fetch {fetch with call = timed};
     Protocol.Answer ({fetch with call = timed}, result);
     Protocol.Fetch {fetch with call = fuel};
     Protocol.Answer ({fetch with call = fuel}, result);
     Protocol.Fetch {fetch with call = pages};
     Protocol.Answer ({fetch with call = pages}, result);
     Protocol.Fetch {fetch with call = memory};
     Protocol.Answer ({fetch with call = memory}, result);
     Protocol.Fetch {fetch with call = old_call};
     Protocol.Answer ({fetch with call = old_call}, result)];
  let big = {call with method_name = String.make 64 'm'; params_json = String.make 65_536 'p'} in
  let big = {big with request = {big.request with input_bytes = 65_600;
    input_hash = Protocol.input_hash ~method_name:big.method_name ~params_json:big.params_json}} in
  let output_json = String.make 262_144 'x' in
  let result = Protocol.{vote = {vote with output_bytes = String.length output_json;
    output_hash = Protocol.output_hash output_json}; output_json} in
  let wire = Protocol.encode (Protocol.Answer ({fetch with call = big}, result)) in
  if String.length wire > Octra_net.P2p_frame.max_payload_size Octra_net.P2p_frame.msg_resource_compute then
    fail "fetch answer exceeds transport";
  ignore (Protocol.decode wire)

let advance_head ?(first = 101L) ?(last = 112L) head =
  let open Lwt.Syntax in
  let rec loop epoch =
    if epoch > last then Lwt.return_unit
    else
      let* () = Lwt_unix.sleep 0.1 in
      head := epoch;
      loop (Int64.succ epoch) in
  loop first

let rec await_finished node caller epoch job_id attempts =
  if attempts <= 0 then fail "committee result timeout";
  let open Lwt.Syntax in
  let* response =
    Rpc.status node.service (read_params node.identity caller !epoch job_id) in
  match response with
  | Error error -> fail error.Octra_core.Rpc.message
  | Ok json when status_name json = "finished" -> Lwt.return json
  | Ok json when status_name json = "refused" ->
    begin
      match field "reason" json with
      | Some (`String reason) -> fail reason
      | _ -> fail "committee request refused"
    end
  | Ok _ ->
    let* () = Lwt_unix.sleep 0.005 in
    await_finished node caller epoch job_id (attempts - 1)

let test_cancel_slots data_dir =
  Mirage_crypto_rng_unix.use_default ();
  let open Lwt.Syntax in
  let owner = Thread.id (Thread.self ()) in
  let owned action () =
    if Thread.id (Thread.self ()) <> owner then fail "service policy read outside owner";
    action () in
  let signer = identity () in
  let caller = identity () in
  let peers = List.init 8 (fun _ -> identity ()) in
  let validator_set = Octra_consensus.C_types.make_validator_set
    [Octra_consensus.C_types.{address = signer.address; pubkey = signer.public_key}] in
  let epoch = ref 100L in
  let send_blocked = ref false in
  let send_expired = ref false in
  let clock = ref (now ()) in
  let relays = ref 0 in
  let disk = history data_dir signer in
  let service = Service.create ~provider:None ~history:(Ok disk)
    ~deps:Service.{
    chain_id = "octra-resource-compute-service-test";
    node_id = signer.address;
    sign = sign signer;
    validator_set = (fun () -> validator_set);
    head = (fun () -> Some {epoch_id = !epoch; state_root = state_root !epoch});
    state_root_at = (fun height -> Some (state_root height));
    funding = no_funding;
    self_test = (fun () -> Lwt.return (Error "unused"));
    nonce = (fun () -> raw_hash "nonce");
    now = owned (fun () -> !clock); cancel_rounds = owned (fun () -> true);
    send = (fun ~peer:_ _ -> Lwt.return_unit);
    broadcast = (fun payload -> match Protocol.decode payload with
      | Protocol.Cancel _ | Protocol.Cancel_round _ when not !send_blocked -> Lwt.return_unit
      | _ -> fst (Lwt.task ()));
    relay = (fun ~source:_ _ -> incr relays; Lwt.return_unit);
    sleep = (fun seconds ->
      if !send_blocked && seconds = 2.0 then begin
        send_expired := true;
        Lwt.pause ()
      end else Lwt_unix.sleep seconds);
  } in
  let cancel name expected owner session =
    if expected then cancel_work disk owner session;
    let until = match Octra_core.Compute_history.cancel_deadline disk ~caller:owner.address ~session with
      | Ok value -> value | Error _ -> None in
    let* result = Rpc.cancel service (cancellation_params ?until owner session) in
    if Result.is_ok result <> expected then fail name;
    Lwt.return_unit in
  let entry index = raw_hash ("quota:" ^ string_of_int index) in
  let request owner session = build_request validator_set signer owner !epoch session 1L in
  Lwt.finalize (fun () ->
    let unknown = cancellation_params caller (entry 2000) in
    let* () = Lwt_list.iter_s (fun params ->
      let+ refused = Rpc.cancel service params in
      if Result.is_ok refused then fail "unknown session cancellation accepted")
      [unknown; `List [unknown]] in
    if Octra_core.Compute_history.cancelled disk ~caller:caller.address
        ~session:(entry 2000) <> Ok false then fail "unknown cancellation persisted";
    if Octra_core.Compute_history.cancel disk ~caller:caller.address
        ~session:(entry 2001) <> Ok true then fail "remote cancel setup";
    let* refused = Rpc.cancel service (cancellation_params caller (entry 2001)) in
    if Result.is_ok refused then fail "remote cancellation authorized local notice";
    let invalid = match cancellation_params caller (entry 0) with
      | `Assoc fields -> `Assoc (("caller_signature", `String
          (Base64.encode_exn (String.make 64 '\000')))
          :: List.remove_assoc "caller_signature" fields)
      | _ -> assert false in
    let* refused = Rpc.cancel service invalid in
    if Result.is_ok refused then fail "unsigned cancellation persisted";
    let* () = Lwt_list.iter_s (fun index ->
      let session = entry (3000 + index) in
      let* () = Service.on_message service ~source:signer.address
        (Protocol.encode (Protocol.Cancel (signed_cancel caller session))) in
      let* () = Service.on_message service ~source:signer.address
        (Protocol.encode (Protocol.Cancel (signed_cancel ~until:164L caller session))) in
      if Octra_core.Compute_history.cancelled disk ~caller:caller.address ~session <> Ok false then
        fail "unknown legacy cancel kept a permanent row";
      Lwt.return_unit) (List.init Octra_core.Compute_history.send_limit Fun.id) in
    if !relays <> 0 then fail "unknown bare cancel forwarded";
    let urgent = entry 0 in
    let other = List.hd peers in
    cancel_work disk other urgent;
    let* () = Service.on_message service ~source:signer.address
      (Protocol.encode (Protocol.Cancel (signed_cancel other urgent))) in
    if !relays <> 1 then fail "unknown cancels consumed control quota";
    let* () = Lwt_list.iter_s (fun index ->
      cancel "caller cancellation admitted" true caller (entry index)) (List.init 32 Fun.id) in
    let* () = cancel "caller cancellation quota" false caller (entry 32) in
    let* () = cancel "caller cancellation repeat" true caller (entry 0) in
    let* () = cancel "other caller cancellation" true (List.hd peers) (entry 0) in
    let* refused = Rpc.submit service (request caller (entry 33)) in
    if Result.is_ok refused then fail "caller job lacked cancellation slot";
    epoch := 164L;
    let* () = cancel "caller quota retained at expiry" false caller (entry 32) in
    epoch := 165L;
    let* refused = Rpc.submit service (request caller (entry 0)) in
    if Result.is_ok refused then fail "expired cache revived cancelled session";
    let* () = Lwt_list.iter_s (fun index ->
      cancel "expired caller slots released" true caller (entry index)) (List.init 31 Fun.id) in
    let pending = request caller (entry 33) in
    let* submitted = Rpc.submit service pending in
    if Result.is_error submitted then fail "last caller slot reserved";
    let* duplicate = Rpc.submit service pending in
    if duplicate <> submitted then fail "reserved job not idempotent";
    let* active = Service.active_jobs service in
    if active <> 1 then fail "reserved job not active";
    let* () = cancel "caller reservation consumed" false caller (entry 32) in
    let* () = cancel "reserved caller cancellation" true caller (entry 33) in
    let* active = Service.active_jobs service in
    if active <> 0 then fail "reserved caller job still active";
    epoch := 230L;
    let pending = request caller (entry 1000) in
    let* submitted = Rpc.submit service pending in
    if Result.is_error submitted then fail "global reservation job refused";
    let* () = Lwt_list.iter_s (fun index ->
      cancel "global cancellation admitted" true (List.nth peers (index / 32))
        (entry index)) (List.init 255 Fun.id) in
    let* () = cancel "global reservation consumed" false (List.nth peers 7) (entry 255) in
    let* () = cancel "reservation lost on duplicate" true (List.hd peers) (entry 0) in
    let* duplicate = Rpc.submit service pending in
    if duplicate <> submitted then fail "full reservation changed job identity";
    let* refused = Rpc.submit service (request (identity ()) (entry 1001)) in
    if Result.is_ok refused then fail "global job lacked cancellation slot";
    let* () = cancel "reserved global cancellation" true caller (entry 1000) in
    let* active = Service.active_jobs service in
    if active <> 0 then fail "reserved global job still active";
    let* () = cancel "global cancellation limit" false (identity ()) (entry 1002) in
    let* () = cancel "global cancellation repeat" true caller (entry 1000) in
    let* refused = Rpc.submit service pending in
    if Result.is_ok refused then fail "reserved cancellation lost";
    epoch := 294L;
    let* () = cancel "global quota retained at expiry" false (identity ()) (entry 1003) in
    epoch := 295L;
    let* () = cancel "global quota released after expiry" true caller (entry 1004) in
    send_blocked := true;
    clock := Int64.add !clock 3000L;
    cancel_work disk caller (entry 1007);
    let* cancelled = Rpc.cancel service (cancellation_params caller (entry 1007)) in
    begin match cancelled with
    | Ok response when field "scope" response = Some (`String "local") -> ()
    | _ -> fail "cancel send scope"
    end;
    if not !send_expired then fail "cancel send deadline absent";
    let* refused = Rpc.submit service (request caller (entry 1007)) in
    if Result.is_ok refused then fail "cancel send failure revived session";
    send_blocked := false;
    epoch := 350L;
    let owner = identity () in
    let session = entry 4000 in
    let old = request owner session in
    let* started = Rpc.submit service old in
    if Result.is_error started then fail "timed cancellation job refused";
    let* refused = Rpc.cancel service (cancellation_params owner session) in
    if Result.is_ok refused then fail "unpaid cancellation omitted deadline";
    let until = 414L in
    let* stopped = Rpc.cancel service (cancellation_params ~until owner session) in
    if Result.is_error stopped then fail "timed cancellation refused";
    epoch := until;
    let* refused = Rpc.submit service (request owner session) in
    if Result.is_ok refused then fail "timed cancellation ended early";
    epoch := Int64.succ until;
    let* old_read = Rpc.submit service old in
    let* active = Service.active_jobs service in
    if active <> 0 then fail "expired signed request ran";
    let* () = match old_read with
      | Error _ -> Lwt.return_unit
      | Ok value ->
        let id = match field "job_id" value with Some (`String id) -> id | _ -> fail "old job id" in
        let+ status = Rpc.status service (read_params signer owner !epoch id) in
        (match status with Ok value when status_name value = "refused" -> ()
        | _ -> fail "expired job returned active status") in
    let* fresh = Rpc.submit service (request owner session) in
    if Result.is_error fresh then fail "unpaid session not reusable";
    let* refused = Rpc.cancel service (cancellation_params ~until owner session) in
    if Result.is_ok refused then fail "old cancellation stopped new request";
    let* stopped = Rpc.cancel service (cancellation_params ~until:479L owner session) in
    if Result.is_error stopped then fail "new cancellation refused";
    Octra_core.Compute_history.close disk;
    epoch := 500L;
    let* () = cancel "closed history cancellation" false caller (entry 1005) in
    let* refused = Rpc.submit service (request caller (entry 1006)) in
    if Result.is_ok refused then fail "closed history accepted work";
    Lwt.return_unit)
    (fun () -> Service.shutdown service)

let test_cancel_delivery data_dir =
  let open Lwt.Syntax in
  Mirage_crypto_rng_unix.use_default ();
  let sender = identity () in
  let receiver = identity () in
  let caller = identity () in
  let session = raw_hash "cancel delivery" in
  let validator_set = Octra_consensus.C_types.make_validator_set
    (List.map (fun (identity : identity) -> Octra_consensus.C_types.{
      address = identity.address; pubkey = identity.public_key}) [sender; receiver]) in
  let nodes = ref [] in
  let dropped = ref true in
  let rounds = ref false in
  let clock = ref (now ()) in
  let sent = ref [] in
  let delivered, signal = Lwt.wait () in
  let make identity =
    let disk = history data_dir identity in
    let service = Service.create ~provider:None ~history:(Ok disk) ~deps:Service.{
      chain_id = "octra-resource-compute-service-test";
      node_id = identity.address;
      sign = sign identity;
      validator_set = (fun () -> validator_set);
      head = (fun () -> Some {epoch_id = 100L; state_root = state_root 100L});
      state_root_at = (fun epoch -> Some (state_root epoch));
      funding = no_funding;
      self_test = (fun () -> Lwt.return (Error "unused"));
      nonce = (fun () -> raw_hash "nonce");
      sleep = Lwt_unix.sleep;
      now = (fun () -> !clock); cancel_rounds = (fun () -> !rounds);
      send = (fun ~peer wire -> match List.assoc_opt peer !nodes with
        | Some service -> Service.on_message service ~source:identity.address wire
        | None -> Lwt.return_unit);
      broadcast = (fun wire ->
        sent := wire :: !sent;
        if !dropped then Lwt.return_unit else
        let* () = Lwt_list.iter_s (fun (address, service) ->
          if address = identity.address then Lwt.return_unit
          else Service.on_message service ~source:identity.address wire) !nodes in
        if Lwt.is_sleeping delivered then Lwt.wakeup_later signal ();
        Lwt.return_unit);
      relay = (fun ~source:_ _ -> Lwt.return_unit);
    } in
    service, disk in
  let first, first_disk = make sender in
  let other, other_disk = make receiver in
  nodes := [sender.address, first; receiver.address, other];
  cancel_work first_disk caller session;
  cancel_work other_disk caller session;
  Lwt.finalize (fun () ->
    let* result = Rpc.cancel first (cancellation_params caller session) in
    if Result.is_error result then fail "cancel delivery start";
    if List.length !sent <> 1 then fail "cancel first send missing";
    if Octra_core.Compute_history.cancelled other_disk
        ~caller:caller.address ~session <> Ok false then fail "dropped cancel delivered";
    begin match Octra_core.Compute_history.pending_notices first_disk ~epoch:100L with
    | Ok [row] when row.round = 1 -> ()
    | _ -> fail "cancel send not durably claimed"
    end;
    clock := Int64.add !clock 6000L;
    let* repeat = Rpc.cancel first (cancellation_params caller session) in
    if Result.is_error repeat || List.length !sent <> 1 then fail "prior mode sent cancel round";
    let delivery = signed_round sender (signed_cancel caller session) in
    let* () = Service.on_message other ~source:sender.address
      (Protocol.encode (Protocol.Cancel_round delivery)) in
    if Octra_core.Compute_history.cancelled other_disk ~caller:caller.address ~session <> Ok false then
      fail "prior mode accepted cancel round";
    rounds := true;
    let original = List.hd !sent in
    let* () = Service.shutdown first in
    nodes := [receiver.address, other];
    let restarted, disk = make sender in
    nodes := (sender.address, restarted) :: !nodes;
    dropped := false;
    let* () = Lwt.pick [Lwt.protected delivered;
      (let* () = Lwt_unix.sleep 8.0 in fail "cancel restart did not retry")] in
    let original = Protocol.decode original in
    let same wire = match original, Protocol.decode wire with
      | Protocol.Cancel before, Protocol.Cancel after -> before = after
      | Protocol.Cancel before, Protocol.Cancel_round after -> before = after.cancel
      | Protocol.Cancel before, Protocol.Cancel_ack after -> before = after.delivery.cancel
      | _ -> false in
    if List.length !sent < 2 || not (List.for_all same !sent) then
      fail "cancel retry changed authorization";
    if Octra_core.Compute_history.cancelled other_disk
        ~caller:caller.address ~session <> Ok true then fail "retry not committed at receiver";
    begin match Octra_core.Compute_history.pending_notices disk ~epoch:100L with
    | Ok [row] when row.round >= 2 -> ()
    | _ -> fail "cancel restart reset send counter"
    end;
    let request = build_request validator_set receiver caller 100L session 2L in
    let* refused = Rpc.submit other request in
    if Result.is_ok refused then fail "cancelled receiver restarted work";
    Lwt.return_unit)
    (fun () -> Lwt_list.iter_s (fun (_, service) -> Service.shutdown service) !nodes)

let test_cancel_hops data_dir =
  let open Lwt.Syntax in
  Mirage_crypto_rng_unix.use_default ();
  let sender = identity () in
  let relay = identity () in
  let receiver = identity () in
  let caller = identity () in
  let session = raw_hash "cancel hops" in
  let validators = Octra_consensus.C_types.make_validator_set
    (List.map (fun (who : identity) -> Octra_consensus.C_types.{
      address = who.address; pubkey = who.public_key}) [sender; relay; receiver]) in
  let nodes = ref [] in
  let blocked = ref true in
  let forwarded = ref [] in
  let clock = ref 0L in
  let held, wake_held = Lwt.wait () in
  let received, wake_received = Lwt.wait () in
  let deliver origin wire =
    let target = if origin = sender.address then relay.address
      else if origin = relay.address then receiver.address else sender.address in
    forwarded := (origin, wire) :: !forwarded;
    if origin = relay.address && !blocked then begin
      (match Protocol.decode wire with
       | Protocol.Cancel_round value when value.round = 2 && Lwt.is_sleeping held ->
         Lwt.wakeup_later wake_held wire
       | _ -> ());
      Lwt.return_unit
    end else
      match List.assoc_opt target !nodes with
      | None -> fail "cancel hop target missing"
      | Some (service, _) ->
        let* () = Service.on_message service ~source:origin wire in
        if target = receiver.address && Lwt.is_sleeping received then
          Lwt.wakeup_later wake_received ();
        Lwt.return_unit in
  let make (who : identity) =
    let disk = history data_dir who in
    let service = Service.create ~provider:None ~history:(Ok disk) ~deps:Service.{
      chain_id = "octra-resource-compute-service-test"; node_id = who.address;
      sign = sign who; validator_set = (fun () -> validators);
      head = (fun () -> Some {epoch_id = 100L; state_root = state_root 100L});
      state_root_at = (fun epoch -> Some (state_root epoch));
      funding = no_funding;
      self_test = (fun () -> Lwt.return (Error "unused"));
      nonce = (fun () -> raw_hash "nonce"); sleep = Lwt_unix.sleep;
      now = (fun () -> Int64.add (now ()) !clock); cancel_rounds = (fun () -> true);
      send = (fun ~peer wire -> match List.assoc_opt peer !nodes with
        | Some (service, _) -> Service.on_message service ~source:who.address wire
        | None -> Lwt.return_unit);
      broadcast = deliver who.address;
      relay = (fun ~source:_ -> deliver who.address);
    } in
    service, disk in
  let first, first_disk = make sender in
  let middle, middle_disk = make relay in
  let last, disk = make receiver in
  nodes := [sender.address, (first, first_disk); relay.address, (middle, middle_disk);
    receiver.address, (last, disk)];
  cancel_work first_disk caller session;
  cancel_work disk caller session;
  Lwt.finalize (fun () ->
    let* result = Rpc.cancel first (cancellation_params caller session) in
    if Result.is_error result then fail "cancel hop start";
    let* wire = Lwt.pick [Lwt.protected held;
      (let* () = Lwt_unix.sleep 8.0 in fail "cancel second round not sent")] in
    if Octra_core.Compute_history.cancelled disk ~caller:caller.address ~session <> Ok false then
      fail "cancel hop loss not exercised";
    let* () = Service.shutdown middle in
    let restarted, middle_disk = make relay in
    nodes := (relay.address, (restarted, middle_disk)) :: List.remove_assoc relay.address !nodes;
    let before = List.length !forwarded in
    let* () = Service.on_message restarted ~source:sender.address wire in
    if List.length !forwarded <> before then fail "cancel relay repeat after restart";
    let fresh = signed_cancel caller (raw_hash "bad cancel hop") in
    let valid = signed_round sender fresh in
    let values = [
      {valid with signature = String.make 64 '\000'};
      {valid with round = 3};
      signed_round ~epoch:101L sender fresh;
      signed_round ~epoch:0L ~expires:64L sender fresh;
      signed_round (identity ()) fresh;
      signed_round sender {fresh with caller_signature = String.make 64 '\000'};
      signed_round sender (signed_cancel ~chain_id:"other-chain" caller fresh.session_id);
    ] in
    let* () = Lwt_list.iter_s (fun value ->
      Service.on_message restarted ~source:sender.address
        (Protocol.encode (Protocol.Cancel_round value))) values in
    if List.length !forwarded <> before then fail "invalid cancel round relayed";
    if Octra_core.Compute_history.cancelled middle_disk
        ~caller:caller.address ~session:fresh.session_id <> Ok false then
      fail "invalid cancel round closed session";
    blocked := false;
    let* () = Lwt.pick [Lwt.protected received;
      (let* () = Lwt_unix.sleep 8.0 in fail "cancel did not cross restarted relay")] in
    if Octra_core.Compute_history.cancelled disk ~caller:caller.address ~session <> Ok true then
      fail "cancel hop not committed";
    let deliveries = List.filter (fun (_, wire) -> match Protocol.decode wire with
      | Protocol.Cancel _ | Protocol.Cancel_round _ -> true | _ -> false) !forwarded in
    if List.length deliveries > 10 then fail "cancel relay cycle";
    List.iter (fun entry ->
      if List.length (List.filter ((=) entry) !forwarded) > 2 then
        fail "cancel receipt relay cycle") !forwarded;
    let* result = Rpc.submit last (build_request validators receiver caller 100L session 2L) in
    if Result.is_ok result then fail "cancel hop permitted new work";
    let* () = Lwt_list.iter_s (fun index ->
      let auth = signed_cancel ~until:164L caller (raw_hash ("path " ^ string_of_int index)) in
      Lwt_list.iter_s (fun round ->
        clock := Int64.add !clock 3000L;
        Service.on_message restarted ~source:sender.address
          (Protocol.encode (Protocol.Cancel_round (signed_round ~round sender auth))))
        (List.init 63 (fun index -> index + 2))) (List.init 17 Fun.id) in
    let fresh = raw_hash "path after retries" in
    clock := Int64.add !clock 3000L;
    let before = List.length !forwarded in
    let* () = Service.on_message restarted ~source:sender.address
      (Protocol.encode (Protocol.Cancel_round (signed_round sender (signed_cancel caller fresh)))) in
    if List.length !forwarded <= before then
      fail "cancel retry paths exhausted";
    if Octra_core.Compute_history.cancelled middle_disk ~caller:caller.address ~session:fresh <> Ok false then
      fail "unknown relay left permanent closure";
    Lwt.return_unit)
    (fun () -> Lwt_list.iter_s (fun (_, (service, _)) -> Service.shutdown service) !nodes)

let test_cancel_receipts data_dir =
  let open Lwt.Syntax in
  Mirage_crypto_rng_unix.use_default ();
  let sender = identity () in
  let peer = identity () in
  let caller = identity () in
  let stranger = identity () in
  let session = raw_hash "cancel receipts" in
  let validators peers = Octra_consensus.C_types.make_validator_set
    (List.map (fun (who : identity) -> Octra_consensus.C_types.{
      address = who.address; pubkey = who.public_key}) peers) in
  let current = ref (validators [sender; peer]) in
  let nodes = ref [] in
  let dropped, wake_dropped = Lwt.wait () in
  let delivered, wake_delivered = Lwt.wait () in
  let drop = ref true in
  let deliver owner wire =
    let target = if owner = sender.address then peer.address else sender.address in
    match Protocol.decode wire with
    | Protocol.Cancel _ -> Lwt.return_unit
    | Protocol.Cancel_ack _ when !drop ->
      if Lwt.is_sleeping dropped then Lwt.wakeup_later wake_dropped wire;
      Lwt.return_unit
    | message ->
      let service = fst (List.assoc target !nodes) in
      let* () = Service.on_message service ~source:owner wire in
      (match message with
       | Protocol.Cancel_ack _ when Lwt.is_sleeping delivered ->
         Lwt.wakeup_later wake_delivered wire
       | _ -> ());
      Lwt.return_unit in
  let make (who : identity) =
    let disk = history data_dir who in
    let service = Service.create ~provider:None ~history:(Ok disk) ~deps:Service.{
      chain_id = "octra-resource-compute-service-test"; node_id = who.address;
      sign = sign who; validator_set = (fun () -> !current);
      head = (fun () -> Some {epoch_id = 100L; state_root = state_root 100L});
      state_root_at = (fun epoch -> Some (state_root epoch));
      funding = no_funding;
      self_test = (fun () -> Lwt.return (Error "unused"));
      nonce = (fun () -> raw_hash "nonce"); sleep = Lwt_unix.sleep;
      now; cancel_rounds = (fun () -> true);
      send = (fun ~peer:target wire ->
        let expected = if who.address = sender.address then peer.address else sender.address in
        if target <> expected then fail "cancel receipt path differs";
        deliver who.address wire);
      broadcast = deliver who.address; relay = (fun ~source:_ _ -> Lwt.return_unit);
    } in service, disk in
  let first, first_disk = make sender in
  let second, second_disk = make peer in
  nodes := [sender.address, (first, first_disk); peer.address, (second, second_disk)];
  cancel_work first_disk caller session;
  cancel_work second_disk caller session;
  let auth = signed_cancel caller session in
  let request = Service.{caller = caller.address; caller_public_key = caller.public_key;
    caller_signature = auth.caller_signature; session_id = session; until = auth.until} in
  let read service =
    let+ result = Service.cancel_receipts service request in
    Result.map (List.filter (fun (ack : Protocol.cancel_ack) -> ack.node_id <> sender.address)) result in
  Lwt.finalize (fun () ->
    let* result = Rpc.cancel first (cancellation_params caller session) in
    if Result.is_error result then fail "cancel receipt start";
    let* lost = Lwt.pick [Lwt.protected dropped;
      (let* () = Lwt_unix.sleep 8.0 in fail "cancel receipt not issued")] in
    let* before = read first in
    if before <> Ok [] then fail "lost receipt reported delivered";
    let* local = Service.cancel_receipts first request in
    (match local with
     | Ok [ack] when ack.node_id = sender.address && ack.node_key = sender.public_key -> ()
     | _ -> fail "local cancellation receipt absent");
    let* () = Service.shutdown second in
    let second, second_disk = make peer in
    nodes := (peer.address, (second, second_disk)) :: List.remove_assoc peer.address !nodes;
    drop := false;
    let* wire = Lwt.pick [Lwt.protected delivered;
      (let* () = Lwt_unix.sleep 8.0 in fail "cancel receipt not retried")] in
    let ack = match Protocol.decode wire with
      | Protocol.Cancel_ack value -> value
      | _ -> fail "cancel receipt message" in
    let lost = match Protocol.decode lost with
      | Protocol.Cancel_ack value -> value | _ -> fail "lost receipt message" in
    if ack.delivery.round <= lost.delivery.round then fail "cancel receipt round not advanced";
    let* before = read first in
    if before <> Ok [ack] then fail "cancel receipt not stored";
    let other = signed_ack sender sender ack.delivery in
    let invalid = [
      {other with signature = String.make 64 '\000'};
      {other with node_key = peer.public_key};
      signed_ack sender stranger ack.delivery;
      signed_ack sender sender (signed_round ~round:64 sender auth);
      signed_ack sender sender (signed_round ~expires:163L sender auth);
      signed_ack sender sender (signed_round sender
        {auth with caller_signature = String.make 64 '\000'});
      signed_ack sender sender (signed_round sender
        (signed_cancel ~chain_id:"other-chain" caller session));
    ] in
    let* () = Lwt_list.iter_s (fun value -> Service.on_message first ~source:peer.address
      (Protocol.encode (Protocol.Cancel_ack value))) invalid in
    let* after = read first in
    if after <> before then fail "invalid receipt stored";
    let* refused = Service.cancel_receipts first {request with
      caller_signature = String.make 64 '\000'} in
    if Result.is_ok refused then fail "unsigned receipt read";
    let* () = Service.shutdown first in
    let first, first_disk = make sender in
    nodes := (sender.address, (first, first_disk)) :: List.remove_assoc sender.address !nodes;
    current := validators [stranger];
    let* restored = read first in
    if restored <> before then fail "cancel receipts lost on restart or set change";
    let* repeated = Rpc.cancel first (cancellation_params caller session) in
    (match repeated with
     | Ok response when field "receipt_scope" response = Some (`String "session_closed")
         && (match field "receipts" response with Some (`List [_; _]) -> true | _ -> false) -> ()
     | _ -> fail "cancel receipt RPC differs");
    Lwt.return_unit)
    (fun () -> Lwt_list.iter_s (fun (_, (service, _)) -> Service.shutdown service) !nodes)

let test_cancel_pace data_dir =
  let open Lwt.Syntax in
  let sender = identity () in
  let relay = identity () in
  let peer = identity () in
  let caller = identity () in
  let session = raw_hash "cancel pace" in
  let validators = Octra_consensus.C_types.make_validator_set
    (List.map (fun (who : identity) -> Octra_consensus.C_types.{
      address = who.address; pubkey = who.public_key}) [sender; relay; peer]) in
  let clock = ref 100L in
  let sent = ref [] in
  let record wire = sent := wire :: !sent; Lwt.return_unit in
  let make () =
    let disk = history data_dir relay in
    let service = Service.create ~provider:None ~history:(Ok disk) ~deps:Service.{
      chain_id = "octra-resource-compute-service-test"; node_id = relay.address;
      sign = sign relay; validator_set = (fun () -> validators);
      head = (fun () -> Some {epoch_id = 100L; state_root = state_root 100L});
      state_root_at = (fun epoch -> Some (state_root epoch)); funding = no_funding;
      self_test = (fun () -> Lwt.return_error "unused"); nonce = (fun () -> raw_hash "nonce");
      sleep = Lwt_unix.sleep; now = (fun () -> !clock); cancel_rounds = (fun () -> true);
      send = (fun ~peer:_ -> record); broadcast = record; relay = (fun ~source:_ -> record)} in
    service, disk in
  let service, disk = make () in
  cancel_work disk caller session;
  let current = ref service in
  let auth = signed_cancel caller session in
  let deliver source message =
    Service.on_message !current ~source (Protocol.encode message) in
  Lwt.finalize (fun () ->
    let* () = Lwt_list.iter_s (fun index ->
      deliver caller.address (Protocol.Cancel (signed_cancel caller
        (raw_hash ("bare " ^ string_of_int index))))) (List.init 64 Fun.id) in
    if !sent <> [] then fail "unknown bare cancellation used transit";
    let* () = Lwt_list.iter_s (fun index ->
      let unknown = signed_cancel caller (raw_hash ("unknown ack " ^ string_of_int index)) in
      let delivery = signed_round peer unknown in
      let* () = deliver peer.address (Protocol.Cancel_round delivery) in
      let* () = deliver sender.address (Protocol.Cancel_ack (signed_ack peer sender delivery)) in
      if Octra_core.Compute_history.cancelled disk ~caller:caller.address
          ~session:unknown.session_id <> Ok false then fail "unknown receipt closed session";
      Lwt.return_unit) (List.init 32 Fun.id) in
    if List.length !sent <> Octra_core.Compute_history.transit_limit then
      fail "unknown receipt relay differs";
    sent := [];
    let* () = Lwt_list.iter_s (fun round ->
      deliver sender.address (Protocol.Cancel_round (signed_round ~round sender auth)))
      (List.init 10 (fun index -> index + 2)) in
    if List.length !sent <> 20 then fail (Printf.sprintf
      "cancel forwarding or receipt absent count = %d" (List.length !sent));
    let* () = Lwt_list.iter_s (fun round ->
      deliver peer.address (Protocol.Cancel_ack (signed_ack sender peer
        (signed_round ~round sender auth)))) (List.init 10 (fun index -> index + 2)) in
    if List.length !sent <> 30 then fail "cancel receipt forwarding absent";
    let* () = Lwt_list.iter_s (fun round ->
      deliver sender.address (Protocol.Cancel_round (signed_round ~round sender auth)))
      (List.init 20 (fun index -> index + 12)) in
    if List.length !sent <> 32 then fail "cancel output quota exceeded";
    let fresh = raw_hash "cancel while output full" in
    cancel_work disk caller fresh;
    let pending = Protocol.Cancel_round (signed_round sender (signed_cancel caller fresh)) in
    let* () = deliver sender.address pending in
    if List.length !sent <> 32 || Octra_core.Compute_history.cancelled disk
        ~caller:caller.address ~session:fresh <> Ok true then
      fail "full output prevented local cancellation";
    let receiver = Octra_net.P2p_frame_budget.create ~now:0.
      ~peer_class:Octra_net.P2p_frame_budget.Validator in
    List.iter (fun payload ->
      if Octra_net.P2p_frame_budget.admit receiver ~now:0.1
          ~msg_type:Octra_net.P2p_frame.msg_resource_compute ~bytes:(String.length payload)
          <> Octra_net.P2p_frame_budget.Accept then fail "cancel output closed peer") !sent;
    let* () = Service.shutdown !current in
    let restarted, _ = make () in
    current := restarted;
    clock := 60_099L;
    let* () = deliver sender.address (Protocol.Cancel_round (signed_round ~round:32 sender auth)) in
    if List.length !sent <> 32 then fail "cancel restart reset quota";
    clock := 60_100L;
    let* () = deliver sender.address (Protocol.Cancel_round (signed_round ~round:32 sender auth)) in
    if List.length !sent <> 34 then fail "cancel output did not resume";
    let* () = deliver sender.address pending in
    if List.length !sent <> 36 then fail "pending cancellation did not resume";
    Lwt.return_unit) (fun () -> Service.shutdown !current)

let test_cancel_saved data_dir =
  let open Lwt.Syntax in
  Mirage_crypto_rng_unix.use_default ();
  let signer = identity () in
  let caller = identity () in
  let validator_set = Octra_consensus.C_types.make_validator_set
    [Octra_consensus.C_types.{address = signer.address; pubkey = signer.public_key}] in
  let disk = history data_dir signer in
  for index = 0 to Octra_core.Compute_history.caller_notices - 1 do
    let session = raw_hash ("saved cancel " ^ string_of_int index) in
    let body = Protocol.encode (Protocol.Cancel (signed_cancel caller session)) in
    if Octra_core.Compute_history.queue_cancel disk ~epoch:100L ~expires:164L
        ~caller:caller.address ~session ~body <> Ok true then fail "saved cancel setup"
  done;
  let sent = ref 0 in
  let service = Service.create ~provider:None ~history:(Ok disk) ~deps:Service.{
    chain_id = "octra-resource-compute-service-test"; node_id = signer.address;
    sign = sign signer; validator_set = (fun () -> validator_set);
    head = (fun () -> Some {epoch_id = 100L; state_root = state_root 100L});
    state_root_at = (fun epoch -> Some (state_root epoch));
    funding = no_funding;
    self_test = (fun () -> Lwt.return (Error "unused"));
    nonce = (fun () -> raw_hash "nonce"); sleep = Lwt_unix.sleep;
    now; cancel_rounds = (fun () -> true); send = (fun ~peer:_ _ -> Lwt.return_unit);
    broadcast = (fun _ -> incr sent; Lwt.return_unit);
    relay = (fun ~source:_ _ -> Lwt.return_unit);
  } in
  Lwt.finalize (fun () ->
    let session = raw_hash "another session" in
    let* result = Rpc.submit service (build_request validator_set signer caller 100L session 1L) in
    if Result.is_ok result then fail "saved cancel quota ignored by new work";
    let* result = Rpc.cancel service (cancellation_params caller session) in
    if Result.is_ok result || !sent <> 0 then fail "saved cancel quota ignored by cancel";
    Lwt.return_unit) (fun () -> Service.shutdown service)

let test_cancel_scope data_dir =
  let open Lwt.Syntax in
  Lwt_list.iter_s (fun other_chain ->
    let signer = identity () in
    let caller = identity () in
    let validator_set = Octra_consensus.C_types.make_validator_set
      [Octra_consensus.C_types.{address = signer.address; pubkey = signer.public_key}] in
    let session = raw_hash "cancel scope" in
    let valid = signed_cancel caller session in
    let bad = if other_chain then signed_cancel ~chain_id:"other-chain" caller session
      else {valid with Protocol.caller_signature = String.make 64 '\000'} in
    let disk = history data_dir signer in
    if Octra_core.Compute_history.queue_cancel disk ~epoch:100L ~expires:164L
        ~caller:caller.address ~session ~body:(Protocol.encode (Protocol.Cancel bad)) <> Ok true then
      fail "cancel scope setup";
    let sent = ref 0 in
    let service = Service.create ~provider:None ~history:(Ok disk) ~deps:Service.{
      chain_id = "octra-resource-compute-service-test"; node_id = signer.address;
      sign = sign signer; validator_set = (fun () -> validator_set);
      head = (fun () -> Some {epoch_id = 100L; state_root = state_root 100L});
      state_root_at = (fun epoch -> Some (state_root epoch));
      funding = no_funding;
      self_test = (fun () -> Lwt.return (Error "unused"));
      nonce = (fun () -> raw_hash "nonce"); sleep = Lwt_unix.sleep;
      now; cancel_rounds = (fun () -> true); send = (fun ~peer:_ _ -> Lwt.return_unit);
      broadcast = (fun _ -> incr sent; Lwt.return_unit);
      relay = (fun ~source:_ _ -> Lwt.return_unit);
    } in
    Lwt.finalize (fun () ->
      let* result = Rpc.cancel service (cancellation_params caller session) in
      if Result.is_ok result then fail "invalid saved cancel accepted";
      let* () = Lwt_unix.sleep 3.1 in
      if !sent <> 0 then fail "invalid saved cancel sent";
      begin match Octra_core.Compute_history.pending_notices disk ~epoch:100L with
      | Ok [row] when row.round = 0 -> ()
      | _ -> fail "invalid saved cancel claimed"
      end;
      Lwt.return_unit) (fun () -> Service.shutdown service)) [false; true]

let test_cancel_priority data_dir =
  Mirage_crypto_rng_unix.use_default ();
  let open Lwt.Syntax in
  let signer = identity () in
  let caller = identity () in
  let validator_set = Octra_consensus.C_types.make_validator_set
    [Octra_consensus.C_types.{address = signer.address; pubkey = signer.public_key}] in
  let entered, enter = Lwt.wait () in
  let waiting, release = Lwt.wait () in
  let relayed = ref [] in
  let disk = history data_dir signer in
  cancel_work disk caller (raw_hash "priority");
  let service = Service.create
    ~provider:(Some Service.{limits; engine = engine (ref 0) (Lwt_condition.create ())})
    ~history:(Ok disk)
    ~deps:Service.{
      chain_id = "octra-resource-compute-service-test";
      node_id = signer.address;
      sign = sign signer;
      validator_set = (fun () -> validator_set);
      head = (fun () -> Some {epoch_id = 100L; state_root = state_root 100L});
      state_root_at = (fun height -> Some (state_root height));
      funding = no_funding;
      self_test = (fun () -> Lwt.wakeup_later enter (); waiting);
      nonce = (fun () -> raw_hash "priority nonce");
      broadcast = (fun _ -> Lwt.return_unit);
      now; cancel_rounds = (fun () -> true); send = (fun ~peer:_ _ -> Lwt.return_unit);
      relay = (fun ~source:_ payload -> relayed := payload :: !relayed; Lwt.return_unit);
      sleep = Lwt_unix.sleep;
    } in
  let offer = Protocol.{
    chain_id = "octra-resource-compute-service-test";
    offered_epoch = 100L; expires_epoch = 116L; coordinator = signer.address;
    validator_set_root = Octra_consensus.C_config.validator_set_hash validator_set;
    circle_id; model_epoch; model_state_root = state_root model_epoch;
    graph_root = Octra_node_runtime.Text.hex_to_string graph_root;
    model_root = Octra_node_runtime.Text.hex_to_string model_root;
    program_root = Octra_node_runtime.Text.hex_to_string program_root;
    executor_root = Octra_node_runtime.Text.hex_to_string executor_root;
    min_memory_bytes = 1L; request_bytes = 1; response_bytes = 4096;
    signature = String.make 64 '\000';
  } in
  let offer = {offer with signature = sign signer (Protocol.offer_sign_bytes offer)} in
  Lwt.finalize (fun () ->
    let offered = Service.on_message service ~source:signer.address
      (Protocol.encode (Protocol.Offer offer)) in
    let* () = entered in
    let queued = List.init 512 (fun _ -> Service.active_jobs service) in
    let wire = Protocol.encode (Protocol.Cancel (signed_cancel caller (raw_hash "priority"))) in
    let cancelled = Service.on_message service ~source:signer.address wire in
    Lwt.wakeup_later release (Error "test self check released");
    let* () = cancelled in
    let* () = offered in
    let* _ = Lwt.all queued in
    if not (List.mem wire !relayed) then fail "cancel dropped behind work queue";
    Lwt.return_unit)
    (fun () ->
      if Lwt.is_sleeping waiting then Lwt.wakeup_later release (Error "test stopped");
      Service.shutdown service)

let test_offer_expiry data_dir =
  Mirage_crypto_rng_unix.use_default ();
  let open Lwt.Syntax in
  let signer = identity () in
  let validator_set = Octra_consensus.C_types.make_validator_set
    [Octra_consensus.C_types.{address = signer.address; pubkey = signer.public_key}] in
  let head = ref (Some 100L) in
  let clock = ref 0L in
  let relayed = ref [] in
  let service = Service.create ~provider:None ~history:(Ok (history data_dir signer))
    ~deps:Service.{
    chain_id = "octra-resource-compute-service-test";
    node_id = signer.address;
    sign = sign signer;
    validator_set = (fun () -> validator_set);
    head = (fun () -> Option.map
      (fun epoch_id -> {epoch_id; state_root = state_root epoch_id}) !head);
    state_root_at = (fun epoch -> Some (state_root epoch));
    funding = no_funding;
    self_test = (fun () -> Lwt.return (Error "unused"));
    nonce = (fun () -> raw_hash "nonce");
    broadcast = (fun _ -> Lwt.return_unit);
    now = (fun () -> Int64.add (now ()) !clock);
    cancel_rounds = (fun () -> true); send = (fun ~peer:_ _ -> Lwt.return_unit);
    relay = (fun ~source:_ payload ->
      relayed := payload :: !relayed;
      Lwt.return_unit);
    sleep = Lwt_unix.sleep;
  } in
  let offer epoch index =
    let unsigned = Protocol.{
      chain_id = "octra-resource-compute-service-test";
      offered_epoch = epoch;
      expires_epoch = Int64.add epoch 16L;
      coordinator = signer.address;
      validator_set_root = Octra_consensus.C_config.validator_set_hash validator_set;
      circle_id;
      model_epoch;
      model_state_root = state_root model_epoch;
      graph_root = Octra_node_runtime.Text.hex_to_string graph_root;
      model_root = Octra_node_runtime.Text.hex_to_string model_root;
      program_root = Octra_node_runtime.Text.hex_to_string program_root;
      executor_root = Octra_node_runtime.Text.hex_to_string executor_root;
      min_memory_bytes = 1L;
      request_bytes = index + 1;
      response_bytes = 4096;
      signature = String.make 64 '\000';
    } in
    {unsigned with signature = sign signer (Protocol.offer_sign_bytes unsigned)} in
  let send name expected message =
    let payload = Protocol.encode message in
    let before = List.length !relayed in
    let* () = Service.on_message service ~source:signer.address payload in
    if (List.length !relayed = before + 1) <> expected then fail name;
    Lwt.return_unit in
  let commitment (offer : Protocol.offer) index =
    let unsigned = Selection.{
      chain_id = offer.chain_id;
      offer_id = Protocol.offer_id offer;
      commit_epoch = offer.offered_epoch;
      node_id = signer.address;
      graph_root = offer.graph_root;
      model_root = offer.model_root;
      program_root = offer.program_root;
      executor_root = offer.executor_root;
      nonce_hash = raw_hash (string_of_int index);
      signature = String.make 64 '\000';
    } in
    Protocol.Commitment {unsigned with
      signature = sign signer (Selection.commitment_sign_bytes unsigned)} in
  Lwt.finalize (fun () ->
    let valid = offer 100L 200 in
    let* () = send "offer signature" false
      (Protocol.Offer {valid with signature = String.make 64 '\000'}) in
    let other = {valid with chain_id = "other-chain"} in
    let* () = send "offer chain" false (Protocol.Offer {other with
      signature = sign signer (Protocol.offer_sign_bytes other)}) in
    let* () = Lwt_list.iter_s (fun index ->
      send "offer acceptance" true (Protocol.Offer (offer 100L index)))
      (List.init 63 Fun.id) in
    head := Some 101L;
    let live = offer 101L 63 in
    let* () = send "last offer slot" true (Protocol.Offer live) in
    head := Some 116L;
    let* () = send "live offer limit" false (Protocol.Offer (offer 116L 64)) in
    head := None;
    let* () = send "offer without head" false (Protocol.Offer (offer 116L 65)) in
    head := Some 116L;
    let* () = send "unknown head preserves offers" false
      (Protocol.Offer (offer 116L 66)) in
    head := Some 117L;
    let next = offer 117L 67 in
    let* () = send "expired offers release capacity" true (Protocol.Offer next) in
    let* () = send "live offer preserved" true (commitment live 0) in
    let* () = send "expired offer refused" false (commitment (offer 100L 0) 1) in
    let* () = send "duplicate offer refused" false (Protocol.Offer next) in
    clock := Int64.add !clock 60_001L;
    let* () = Lwt_list.iter_s (fun index ->
      send "reused offer capacity" true (Protocol.Offer (offer 117L (index + 68))))
      (List.init 62 Fun.id) in
    let* () = send "reused offer limit" false (Protocol.Offer (offer 117L 130)) in
    head := Some 134L;
    send "second offer expiry" true (Protocol.Offer (offer 134L 131)))
    (fun () -> Service.shutdown service)

let test_request_expiry ~fund ~present ~next_window service caller signer call head relayed executions =
  let open Lwt.Syntax in
  let before = !executions in
  let call_at index expiry =
    let unsigned = Protocol.{call with request = {
      call.request with
      expires_epoch = expiry;
      max_output_bytes = 8192 + index;
    }} in
    let authorized = {unsigned with
      caller_signature = sign caller (Protocol.intent_sign_bytes unsigned)} in
    {authorized with signature = sign signer (Protocol.call_sign_bytes authorized)} in
  let send name expected message =
    let payload = Protocol.encode message in
    let count () = List.length (List.filter (( = ) payload) !relayed) in
    let old = count () in
    let* () = Service.on_message service ~source:signer.address payload in
    if (count () = old + 1) <> expected then fail name;
    Lwt.return_unit in
  let vote ?(output_json = "{}") ?output_bytes ?(steps = 1L) (call : Protocol.call) =
    let unsigned = Certificate.{
      request_id = Certificate.request_id call.request;
      node_id = signer.address;
      output_hash = Protocol.output_hash output_json;
      trace_root = raw_hash "expiry trace";
      output_bytes = Option.value output_bytes ~default:(String.length output_json);
      steps;
      operations = 1L;
      signature = String.make 64 '\000';
    } in
    Protocol.Result {vote = {unsigned with
      signature = sign signer (Certificate.vote_sign_bytes unsigned)}; output_json} in
  let epoch = Int64.succ !head in
  let expiry = Int64.succ epoch in
  let later = Int64.succ expiry in
  let occupied = List.filter_map (fun wire -> match Protocol.decode wire with
    | Protocol.Call value when value.request.expires_epoch >= epoch ->
      Some (Certificate.request_id value.request)
    | _ -> None) present |> List.sort_uniq String.compare |> List.length in
  if occupied >= 127 then fail "request expiry setup capacity";
  let remaining = 127 - occupied in
  let batch = List.init remaining (fun index -> call_at index epoch) in
  let live = call_at remaining expiry in
  let overflow = call_at (remaining + 1) later in
  let next = call_at (remaining + 2) later in
  let* () = fund epoch (batch @ [live; overflow; next]) in
  next_window ();
  let* () = Lwt_list.iteri_s (fun index call ->
    if index > 0 && index mod Octra_core.Compute_history.forward_limit = 0 then next_window ();
    send "request acceptance" true (Protocol.Call call)) batch in
  let* () = send "last request slot" true (Protocol.Call live) in
  let* () = send "live request limit" false (Protocol.Call overflow) in
  let* () = fund expiry [] in
  let* () = send "expired requests release capacity" true (Protocol.Call next) in
  let* () = send "result byte count differs" false
    (vote ~output_json:"\"data\"" ~output_bytes:2 live) in
  let* () = send "result exceeds signed limit" false
    (vote ~output_json:("\"" ^ String.make live.request.max_output_bytes 'x' ^ "\"") live) in
  let* () = send "result usage excessive" false
    (vote ~steps:(Int64.succ (Int64.of_int (Option.get live.request.fuel_limit))) live) in
  let* () = send "live request preserved" true (vote live) in
  let* () = send "expired result refused" false (vote (List.hd batch)) in
  let* () = send "duplicate call refused" false (Protocol.Call next) in
  let* () = send "duplicate result refused" false (vote live) in
  let* () = send "new request result" true (vote next) in
  if !executions <> before then fail "request expiry reset sequence guard";
  Lwt.return_unit

let test_remote_cancel ~advance ~calls nodes validator_set caller head session executions =
  let open Lwt.Syntax in
  let remote = List.nth nodes 1 in
  let epoch = Int64.add !head 65L in
  let* () = advance epoch epoch in
  let before = !executions in
  let local = List.hd nodes in
  let* refused = Rpc.submit local.service
    (build_request validator_set local.identity caller epoch session 2L) in
  (match refused with
  | Error error when error.Octra_core.Rpc.code = -32040
      && error.data = Some (`String "resource compute sequence already issued") -> ()
  | _ -> fail "issued sequence was not refused precisely");
  let new_session = raw_hash "fresh remote session" in
  let params = build_request validator_set remote.identity caller epoch new_session 1L in
  let* submitted = Rpc.submit remote.service params in
  let job_id = match submitted with
    | Error error -> fail error.Octra_core.Rpc.message
    | Ok json ->
      match field "job_id" json with
      | Some (`String value) -> value
      | _ -> fail "remote job id" in
  let progress = advance (Int64.succ epoch) (Int64.add epoch 12L) in
  let* _ = await_finished remote caller head job_id 1000 in
  let* () = progress in
  let call = List.find (fun (call : Protocol.call) ->
    call.request.session_id = new_session) !calls in
  let payload = Protocol.encode (Protocol.Call call) in
  let* () = Lwt_list.iter_s (fun node ->
    Service.on_message node.service ~source:remote.identity.address payload) nodes in
  let rec complete attempts =
    if !executions = before + List.length nodes then Lwt.return_unit
    else if attempts = 0 || !executions > before + List.length nodes then
      fail "cancelled session blocked new session"
    else let* () = Lwt_unix.sleep 0.005 in complete (attempts - 1) in
  complete 1000

let reveal_transport ~busy origin (record : Record.t) =
  let open Lwt.Syntax in
  let module Swarm = Octra_net.P2p_swarm in
  let module Conn = Octra_net.P2p_conn in
  let swarm = Swarm.create {listen_port = 0; chain_id = record.call.request.chain_id;
    node_id = origin.address; node_addr = origin.address; pubkey_raw = origin.public_key;
    consensus_config_hash = raw_hash "config"; binary_hash = raw_hash "binary";
    require_binary_hash = false; upgrade_plan = None; profile_plan = [];
    allowed_pubkeys = []; bootstrap_peers = []; max_peers = 4; sign_fn = sign origin;
    best_epoch_fn = (fun () -> record.call.request.epoch_id);
    best_root_fn = (fun () -> record.call.request.state_root)} in
  let connect name =
    let+ fd = Lwt_unix.openfile "/dev/null" [Unix.O_RDONLY] 0 in
    let conn = Conn.create ~peer_class:Octra_net.P2p_frame_budget.Validator fd
      ~peer_id:name ~addr:name ~direction:Conn.Inbound in
    Hashtbl.add swarm.peers name conn;
    conn in
  let frame = Octra_net.P2p_frame.{msg_type = msg_resource_compute; payload = "occupied"} in
  let* () = Swarm.broadcast swarm frame in
  let* absent = Lwt.catch
    (fun () -> let+ () = Swarm.broadcast ~required:true swarm frame in false)
    (function Failure reason when reason = "broadcast queues unavailable" -> Lwt.return_true
      | error -> Lwt.fail error) in
  if not absent then fail "absent peers reported delivery";
  let* gone = connect "gone" in
  let* () = Lwt_mvar.put gone.write_queue frame in
  let pending = Conn.enqueue gone frame in
  let* () = Conn.close gone in
  let* sent = pending in
  if sent then fail "closed queue reported delivery";
  Hashtbl.remove swarm.peers "gone";
  let* slow = connect "slow" in
  let* fast = connect "fast" in
  let* () = Lwt_mvar.put slow.write_queue
    Octra_net.P2p_frame.{msg_type = msg_resource_compute; payload = "occupied"} in
  let* () = if busy then Lwt_mvar.put fast.write_queue
    Octra_net.P2p_frame.{msg_type = msg_resource_compute; payload = "occupied"}
    else Lwt.return_unit in
  let send payload =
    let frame = Octra_net.P2p_frame.{msg_type = msg_resource_compute; payload} in
    let* () = Swarm.broadcast ~required:true swarm frame in
    let+ received = Lwt_mvar.take fast.write_queue in
    if received <> frame then fail "reveal transport changed frame" in
  let close () = Lwt.join [Conn.close slow; Conn.close fast] in
  let release () = Lwt.map (fun _ -> ()) (Lwt_mvar.take fast.write_queue) in
  Lwt.return (send, release, close)

let test_reveal_send ?(slow = false) ?(drop = false) ?(busy = false) ?(reserve = false) data_dir origin peers (record : Record.t) ~queued ~fault ~cancelled ~held =
  let open Lwt.Syntax in
  Printf.eprintf "event = reveal_case queued = %b fault = %b cancelled = %b held = %b slow = %b drop = %b busy = %b reserve = %b\n%!"
    queued fault cancelled held slow drop busy reserve;
  let root = Filename.concat data_dir (Printf.sprintf "reveal_%b_%b_%b_%b_%b_%b_%b_%b_%s"
    queued fault cancelled held slow drop busy reserve origin.address) in
  Unix.mkdir root 0o700;
  let disk = history root origin in
  let sent = ref [] in
  let failed = ref fault in
  let faulted, signal_fault = Lwt.wait () in
  let pulse = Lwt_condition.create () in
  let clock = ref (now ()) in
  let inflight = ref None in
  let owner = Thread.id (Thread.self ()) in
  let abort_claim = ref false in
  let aborted = ref false in
  let barrier, release = Lwt.wait () in
  let blocked = ref false in
  let* transport, unblock, close = if slow then reveal_transport ~busy origin record
    else Lwt.return ((fun _ -> Lwt.return_unit), (fun () -> Lwt.return_unit),
      (fun () -> Lwt.return_unit)) in
  let save wire = match Protocol.decode wire with
    | Protocol.Reveal _ when !failed ->
      if Lwt.is_sleeping faulted then Lwt.wakeup_later signal_fault ();
      Lwt.fail_with "reveal transport fault"
    | Protocol.Reveal _ when held && not !blocked ->
      blocked := true;
      let* () = barrier in
      if drop then Lwt.fail_with "held reveal send failed"
      else (sent := wire :: !sent; Lwt.return_unit)
    | Protocol.Reveal _ ->
      let* () = transport wire in
      sent := wire :: !sent;
      Lwt.return_unit
    | _ -> sent := wire :: !sent; Lwt.return_unit in
  let validators = Option.get record.validators in
  let capability = List.hd record.capabilities in
  let service = Service.create ~provider:None ~history:(Ok disk) ~deps:Service.{
    chain_id = record.call.request.chain_id; node_id = origin.address; sign = sign origin;
    validator_set = (fun () -> validators);
    head = (fun () -> Some {epoch_id = record.call.request.epoch_id;
      state_root = record.call.request.state_root});
    state_root_at = (fun epoch -> Some (if epoch = record.offer.model_epoch
      then record.offer.model_state_root else state_root epoch));
    funding = no_funding;
    self_test = (fun () -> Lwt.return_ok Provider_rpc.{
      executor_root = Octra_node_runtime.Text.raw_to_hex capability.executor_root;
      evidence_root = Octra_node_runtime.Text.raw_to_hex capability.evidence_root;
      profile = "q24-v1"});
    nonce = (fun () -> raw_hash "nonce");
    sleep = (fun seconds -> if seconds = 3.0 then Lwt_condition.wait pulse else Lwt_unix.sleep seconds);
    now = (fun () ->
      if Thread.id (Thread.self ()) <> owner then fail "reveal clock read outside owner";
      if !abort_claim then begin
        abort_claim := false;
        Option.iter Lwt.cancel !inflight
      end;
      !clock); cancel_rounds = (fun () -> true);
    send = (fun ~peer:_ -> save); broadcast = save;
    relay = (fun ~source:_ wire ->
      (match Protocol.decode wire with
        | Protocol.Commitment _ when cancelled -> abort_claim := true
        | _ -> ());
      save wire)} in
  let send value = Service.on_message service ~source:origin.address (Protocol.encode value) in
  let reveal = if reserve then List.find (fun (reveal : Selection.reveal) ->
    reveal.node_id = origin.address) record.reveals else List.hd record.reveals in
  let commitment = List.find (fun (value : Selection.commitment) ->
    value.node_id = reveal.node_id) record.commitments in
  Lwt.finalize (fun () ->
    let* () = Lwt_list.iter_s (fun index ->
      let session = raw_hash ("transit:" ^ string_of_int index) in
      send (Protocol.Cancel (signed_cancel origin session)))
      (List.init (2 * Octra_core.Compute_history.transit_limit) Fun.id) in
    let* () = send (Protocol.Offer record.offer) in
    let* () = Lwt_list.iter_s (fun value -> send (Protocol.Capability value)) record.capabilities in
    let* () = if not reserve then Lwt.return_unit else begin
      let foreign = List.find (fun (value : Selection.reveal) -> value.node_id <> origin.address)
        record.reveals in
      let commitment = List.find (fun (value : Selection.commitment) ->
        value.node_id = foreign.node_id) record.commitments in
      let* () = send (Protocol.Commitment commitment) in
      let rec fill () = match Octra_core.Compute_history.claim_send ~local:false disk ~now:!clock with
        | Ok true -> fill () | Ok false -> () | Error reason -> fail reason in
      fill ();
      send (Protocol.Reveal foreign)
    end in
    let before = List.length !sent in
    let forged = {reveal with signature = String.make 64 '\000'} in
    let* () = send (Protocol.Reveal forged) in
    if List.length !sent <> before then fail "unsigned reveal forwarded before commitment";
    let* () = send (Protocol.Reveal reveal) in
    if List.length !sent <> before then fail "unverified reveal forwarded before commitment";
    if queued then begin
      let used = 1 + List.length record.capabilities in
      for _ = used + 1 to Octra_core.Compute_history.send_limit - 1 do
        if Octra_core.Compute_history.claim_send disk ~now:!clock <> Ok true then
          fail "reveal quota setup"
      done
    end;
    let pending = send (Protocol.Commitment commitment) in
    inflight := Some pending;
    let* () = Lwt.catch (fun () -> pending)
      (function Lwt.Canceled when cancelled -> aborted := true; Lwt.return_unit
        | error -> Lwt.fail error) in
    let* () = if reserve then send (Protocol.Reveal reveal) else Lwt.return_unit in
    if cancelled && not !aborted then fail "reveal claim was not cancelled";
    let wire = Protocol.encode (Protocol.Reveal reveal) in
    let* () = if not busy then Lwt.return_unit else begin
      let* () = Lwt_unix.sleep 2.2 in
      if List.mem wire !sent then fail "full queues marked reveal delivered";
      let* () = unblock () in
      clock := Int64.add !clock 3001L;
      Lwt_condition.broadcast pulse ();
      Lwt.return_unit
    end in
    let* () = if not fault then Lwt.return_unit else begin
      let* () = faulted in
      let* () = Lwt.pause () in
      let+ _ = Service.active_jobs service in ()
    end in
    if queued || fault then begin
      if List.mem wire !sent then fail "blocked reveal was sent";
      failed := false;
      clock := Int64.add !clock 60_000L;
      Lwt_condition.broadcast pulse ()
    end;
    let rec wait wire attempts =
      if List.mem wire !sent then Lwt.return_unit
      else if attempts = 0 then fail (Printf.sprintf
        "queued reveal never retried queued = %b fault = %b held = %b slow = %b drop = %b busy = %b"
        queued fault held slow drop busy)
      else let* () = Lwt_unix.sleep 0.005 in wait wire (attempts - 1) in
    let next () =
      let signer = List.find (fun (who : identity) -> who.address = reveal.node_id) peers in
      let nonce = raw_hash "next reveal" in
      let next = {commitment with nonce_hash = Selection.nonce_hash nonce} in
      let next = {next with signature = sign signer (Selection.commitment_sign_bytes next)} in
      let proof = {reveal with nonce} in
      let proof = {proof with signature = sign signer (Selection.reveal_sign_bytes next proof)} in
      let* () = send (Protocol.Commitment next) in
      let* () = send (Protocol.Reveal proof) in
      Lwt.return (Protocol.encode (Protocol.Reveal proof)) in
    let* pending = if not held then Lwt.return_none else begin
      if not !blocked then fail "first reveal was not held";
      let* pending = next () in
      if List.mem pending !sent then fail "second reveal bypassed sender";
      Lwt.wakeup_later release ();
      Lwt.return_some pending
    end in
    let* () = if drop then Lwt.return_unit else wait wire (if slow || reserve then 800 else 100) in
    let* () = match pending with None -> Lwt.return_unit | Some value -> wait value 100 in
    if List.length (List.filter (( = ) wire) !sent) <> (if drop then 0 else 1) then
      fail "ordered reveal delivery lost";
    let before = List.length !sent in
    let* () = send (Protocol.Reveal forged) in
    if List.length !sent <> before then fail "unsigned reveal forwarded after commitment";
    let* () = send (Protocol.Reveal reveal) in
    if List.length !sent <> before then fail "verified reveal sent twice";
    let* () = match pending with
      | Some _ -> Lwt.return_unit
      | None -> let* value = next () in wait value (if slow then 800 else 100) in
    Lwt.return_unit) (fun () ->
      let* () = Service.shutdown service in close ())

let test_funding_cancel data_dir origin caller (record : Record.t) =
  let open Lwt.Syntax in
  let module History = Octra_core.Compute_history in
  let root = Filename.concat data_dir ("funding_cancel_" ^ origin.address) in
  Unix.mkdir root 0o700;
  let disk = history root origin in
  let call = record.call in
  let session = call.request.session_id in
  let validators = Option.get record.validators in
  let current = ref None in
  let read = ref false in
  let cancel_wire = Protocol.encode (Protocol.Cancel
    (signed_cancel ~until:call.request.expires_epoch caller session)) in
  let service = Service.create ~provider:None ~history:(Ok disk) ~deps:Service.{
    chain_id = call.request.chain_id; node_id = origin.address; sign = sign origin;
    validator_set = (fun () -> validators);
    head = (fun () -> Some {epoch_id = call.request.epoch_id;
      state_root = call.request.state_root});
    state_root_at = (fun epoch -> Some (if epoch = call.request.epoch_id then call.request.state_root
      else if epoch = call.request.model_epoch then call.request.model_state_root else state_root epoch));
    funding = (fun _ _ ->
      read := true;
      let* () = Service.on_message (Option.get !current) ~source:origin.address cancel_wire in
      if History.cancelled disk ~caller:caller.address ~session <> Ok true then
        fail "inflight cancellation not recorded";
      Lwt.return_ok None);
    self_test = (fun () -> Lwt.return (Ok Provider_rpc.{executor_root; evidence_root; profile = "q24-v1"}));
    nonce = (fun () -> raw_hash "nonce");
    sleep = Lwt_unix.sleep; now; cancel_rounds = (fun () -> true);
    send = (fun ~peer:_ _ -> Lwt.return_unit); broadcast = (fun _ -> Lwt.return_unit);
    relay = (fun ~source:_ _ -> Lwt.return_unit)} in
  current := Some service;
  Lwt.finalize (fun () ->
    let* () = Service.on_message service ~source:origin.address cancel_wire in
    if History.cancelled disk ~caller:caller.address ~session <> Ok false then
      fail "unknown cancellation occupied history";
    let messages = Protocol.Offer record.offer ::
      List.map (fun item -> Protocol.Capability item) record.capabilities @
      List.map (fun item -> Protocol.Commitment item) record.commitments @
      List.map (fun item -> Protocol.Reveal item) record.reveals @ [Protocol.Call call] in
    let* () = Lwt_list.iter_s (fun message -> Service.on_message service ~source:origin.address
      (Protocol.encode message)) messages in
    if not !read then fail "funding cancellation path not exercised";
    if History.reserve disk ~caller:caller.address ~session ~sequence:call.request.sequence
        ~request:(Certificate.request_id call.request) <> Ok false then
      fail "inflight cancellation allowed execution";
    Lwt.return_unit) (fun () -> Service.shutdown service)

let test_funding_status data_dir origin caller (record : Record.t) =
  let open Lwt.Syntax in
  let module History = Octra_core.Compute_history in
  let root = Filename.concat data_dir ("funding_wait_" ^ origin.address) in
  Unix.mkdir root 0o700;
  let disk = history root origin in
  let call = record.call in
  let session = call.request.session_id in
  let id = Protocol.intent_id call in
  let body = Record.encode {record with results = []} in
  let validators = Option.get record.validators in
  let scope = History.{sequence = call.request.sequence;
    request = Certificate.request_id call.request; body = Record.encode_scope call validators} in
  if History.ensure disk ~caller:caller.address ~session <> Ok true
     || History.plan_job disk ~caller:caller.address ~session ~id ~body <> Ok true
     || History.wait_job ~scope ~room:(Record.job_space {record with results = []}
          (List.map (fun value -> value.Octra_consensus.C_types.address) validators.validators))
          disk ~caller:caller.address ~session ~id ~before:body ~body <> Ok true then
    fail "funding status setup";
  let epoch = ref call.request.epoch_id in
  let traffic = ref 0 in
  let make disk = Service.create ~provider:None ~history:(Ok disk) ~deps:Service.{
    chain_id = call.request.chain_id; node_id = origin.address; sign = sign origin;
    validator_set = (fun () -> validators);
    head = (fun () -> Some {epoch_id = !epoch; state_root = state_root !epoch});
    state_root_at = (fun value -> Some (state_root value));
    funding = no_funding;
    self_test = (fun () -> fail "funding wait started self test");
    nonce = (fun () -> raw_hash "funding nonce"); sleep = Lwt_unix.sleep;
    now; cancel_rounds = (fun () -> true); send = (fun ~peer:_ _ -> Lwt.return_unit);
    broadcast = (fun _ -> incr traffic; Lwt.return_unit);
    relay = (fun ~source:_ _ -> incr traffic; Lwt.return_unit)} in
  let service = ref (make disk) in
  let check expected =
    let* reply = Rpc.status !service (read_params origin caller !epoch
      (Octra_node_runtime.Text.raw_to_hex id)) in
    begin match reply with
    | Ok value when status_name value = expected -> ()
    | _ -> fail "funding status differs"
    end;
    let+ active = Service.active_jobs !service in
    if active <> 0 || !traffic <> 0 then fail "funding wait dispatched work" in
  let* () = check "funding" in
  let* () = check "funding" in
  let* () = Service.shutdown !service in
  service := make (history root origin);
  let* () = check "funding" in
  epoch := Int64.succ call.request.expires_epoch;
  let* () = check "refused" in
  Service.shutdown !service

let test_cancel_selection data_dir origin caller peers (record : Record.t) =
  let open Lwt.Syntax in
  let module History = Octra_core.Compute_history in
  let root = Filename.concat data_dir ("cancel_selection_" ^ origin.address) in
  Unix.mkdir root 0o700;
  let disk = history root origin in
  let session = record.call.request.session_id in
  let id = intent_id record.call in
  let job_id = Octra_node_runtime.Text.raw_to_hex id in
  let body = Record.encode {record with results = []} in
  if History.ensure disk ~caller:caller.address ~session <> Ok true
     || History.plan_job disk ~caller:caller.address ~session ~id ~body <> Ok true
     || History.advance_job disk ~caller:caller.address ~session ~id
          ~before:(Planned body) ~after:(Issued body) <> Ok true then
    fail "cancel selected job setup";
  let head = ref record.call.request.epoch_id in
  let expires = Int64.add !head 64L in
  let auth = signed_cancel caller session in
  let cancel = Protocol.encode (Protocol.Cancel auth) in
  if History.queue_cancel disk ~epoch:!head ~expires ~caller:caller.address ~session ~body:cancel
      <> Ok true then fail "cancel selected notice setup";
  ignore (History.claim_notice ~target:(caller.address, session) disk ~epoch:!head);
  let round = match History.claim_notice ~target:(caller.address, session) disk ~epoch:!head with
    | Ok (Some notice) -> notice.round | _ -> fail "cancel selected round setup" in
  let delivery = signed_round ~epoch:!head ~expires ~round origin auth in
  let validators = Option.get record.validators in
  let* () = match peers with
    | first :: second :: _ ->
      let one = signed_ack origin first delivery in
      let two = signed_ack origin second delivery in
      let selected = Record.select_cancels ~call:record.call ~validators ~members:[first.address] in
      if selected [two; one] <> [one] then fail "unselected validator receipt counted";
      if selected [{one with node_key = second.public_key};
          {one with origin_key = second.public_key}] <> [] then
        fail "wrong receipt key counted";
      Lwt.return_unit
    | _ -> Lwt.return_unit in
  let store who =
    let ack = signed_ack origin who delivery in
    if History.confirm_cancel disk ~epoch:!head ~expires ~round ~caller:caller.address
        ~session ~cancel ~node:who.address ~body:(Protocol.encode (Protocol.Cancel_ack ack))
       <> Ok true then fail "cancel selected receipt setup" in
  let stranger = identity () in
  store stranger;
  let current = ref (Octra_consensus.C_types.make_validator_set
    [Octra_consensus.C_types.{address = stranger.address; pubkey = stranger.public_key}]) in
  let traffic = ref 0 in
  let make disk = Service.create ~provider:None ~history:(Ok disk) ~deps:Service.{
    chain_id = "octra-resource-compute-service-test"; node_id = origin.address;
    sign = sign origin; validator_set = (fun () -> !current);
    head = (fun () -> Some {epoch_id = !head; state_root = state_root !head});
    state_root_at = (fun epoch -> Some (state_root epoch));
    funding = no_funding;
    self_test = (fun () -> Lwt.return (Ok Provider_rpc.{executor_root; evidence_root; profile = "q24-v1"}));
    nonce = (fun () -> raw_hash "nonce"); sleep = Lwt_unix.sleep;
    now; cancel_rounds = (fun () -> true); send = (fun ~peer:_ _ -> Lwt.return_unit);
    broadcast = (fun _ -> incr traffic; Lwt.return_unit);
    relay = (fun ~source:_ _ -> incr traffic; Lwt.return_unit)} in
  let service = ref (make disk) in
  let selected = List.map (fun (peer : identity) -> peer.address) peers |> List.sort String.compare in
  let read () = Rpc.status !service (read_params origin caller !head job_id) in
  let check state missing count =
    let* result = read () in
    match result with
    | Ok result when status_name result = "refused" ->
      begin match field "cancellation" result with
      | Some value when field "status" value = Some (`String state)
          && field "scope" value = Some (`String "session_closed")
          && field "expires_epoch" value = Some
            (if state = "unavailable" then `Null else `String (Int64.to_string expires))
          && field "members" value = Some (`List (List.map (fun node -> `String node) selected))
          && field "missing" value = Some (`List (List.map (fun node -> `String node) missing))
          && field "selection_root" value = Some (`String
            (Octra_node_runtime.Text.raw_to_hex record.call.request.selection_root))
          && (match field "receipts" value with Some (`List values) -> List.length values = count
            | _ -> false) -> Lwt.return result
      | _ -> fail ("cancel selection status = " ^ state ^ " response = " ^ Yojson.Safe.to_string result)
      end
    | _ -> fail "cancel selection response" in
  let check_unverified name bytes issued =
    let path = Filename.concat root name in
    Unix.mkdir path 0o700;
    let saved = history path origin in
    if History.ensure saved ~caller:caller.address ~session <> Ok true
       || History.plan_job saved ~caller:caller.address ~session ~id ~body:bytes <> Ok true then
      fail "cancel unverified setup";
    if issued && History.advance_job saved ~caller:caller.address ~session ~id
        ~before:(Planned bytes) ~after:(Issued bytes) <> Ok true then
      fail "cancel unverified issued setup";
    if History.queue_cancel saved ~epoch:!head ~expires ~caller:caller.address ~session ~body:cancel
       <> Ok true then fail "cancel unverified notice setup";
    let node = make saved in
    Lwt.finalize (fun () ->
      let* result = Rpc.status node (read_params origin caller !head job_id) in
      match result with
      | Ok value when status_name value = "refused" && field "cancellation" value = None ->
        Lwt.return_unit
      | _ -> fail "cancel unverified selection accepted")
      (fun () -> Service.shutdown node) in
  Lwt.finalize (fun () ->
    let* () = check_unverified "no_set" (Record.encode {record with validators = None}) true in
    let altered = {record.call with request = {record.call.request with selection_root = raw_hash "wrong set"}} in
    let altered = {altered with signature = sign origin (Protocol.call_sign_bytes altered)} in
    let* () = check_unverified "wrong_selection" (Record.encode {record with call = altered}) true in
    let* () = check_unverified "not_issued" body false in
    let* _ = check "partial" selected 0 in
    let last = List.hd peers in
    List.iter store (List.tl peers);
    let* _ = check "partial" [last.address] (List.length peers - 1) in
    store last;
    let* complete = check "complete" [] (List.length peers) in
    let* unsigned = Service.status !service
      {(read_request origin caller !head job_id) with caller_signature = String.make 64 '\000'} in
    if Result.is_ok unsigned then fail "cancel status unsigned read";
    let* wrong = Rpc.status !service (read_params origin stranger !head job_id) in
    if Result.is_ok wrong then fail "cancel status caller mismatch";
    let* () = Service.shutdown !service in
    let reopened = history root origin in
    service := make reopened;
    let* restored = check "complete" [] (List.length peers) in
    if restored <> complete then fail "cancel status restart changed receipts";
    head := Int64.succ expires;
    let* _ = check "unavailable" selected 0 in
    if History.cancelled reopened ~caller:caller.address ~session <> Ok true then
      fail "cancel status expiry opened session";
    Lwt.return_unit)
    (fun () -> Service.shutdown !service)

let test_cancel_keys ?(retain = false) ?(many = false) data_dir origin caller peers (record : Record.t) =
  let open Lwt.Syntax in
  let module History = Octra_core.Compute_history in
  let validators = Option.get record.validators in
  let call = record.call in
  let session = call.request.session_id in
  let name = if many then "cancel_job_keys" else if retain then "cancel_peer_keys" else "cancel_keys" in
  let root = Filename.concat (Filename.concat data_dir origin.address) name in
  Unix.mkdir root 0o700;
  let scope = History.{sequence = call.request.sequence;
    request = Certificate.request_id call.request; body = Record.encode_scope call validators} in
  let decoded, keys = Record.decode_scope scope.body in
  if decoded <> call || keys <> validators then fail "cancel key encoding differs";
  let reject raw = match Record.decode_scope raw with
    | _ -> fail "cancel key encoding accepted change"
    | exception (Invalid_argument _ | Failure _) -> () in
  reject (scope.body ^ "x");
  reject (Octra_net.Oce1.encode (fun buffer ->
    Octra_net.Oce1.put_string buffer (Protocol.encode (Protocol.Call
      {call with signature = String.make 64 '\000'}));
    Octra_net.Oce1.put_string buffer (Record.encode_read call validators)));
  let stranger = identity () in
  let members people = Octra_consensus.C_types.make_validator_set
    (List.map (fun (who : identity) -> Octra_consensus.C_types.{address = who.address; pubkey = who.public_key})
      people) in
  let current = Octra_consensus.C_types.make_validator_set
    (List.map (fun (who : identity) -> Octra_consensus.C_types.{address = who.address; pubkey = who.public_key})
      (if retain then [origin; stranger] else [stranger])) in
  let head = ref 100L in
  let nodes = ref [] in
  let sent = ref [] in
  let drop = ref true in
  let held, wake_held = Lwt.wait () in
  let executions = ref 0 in
  let deliver source wire =
    sent := wire :: !sent;
    match Protocol.decode wire with
    | Protocol.Cancel _ -> Lwt.return_unit
    | Protocol.Cancel_round delivery when !drop ->
      if Lwt.is_sleeping held then Lwt.wakeup_later wake_held delivery;
      Lwt.return_unit
    | _ -> Lwt_list.iter_s (fun (who, (service, _)) ->
        if who = source then Lwt.return_unit else Service.on_message service ~source wire) !nodes in
  let make ?(path = root) ?(network = true) who =
    let disk = history path who in
    let service = Service.create ~history:(Ok disk)
      ~provider:(Some Service.{limits; engine = engine executions (Lwt_condition.create ())})
      ~deps:Service.{chain_id = call.request.chain_id; node_id = who.address; sign = sign who;
        validator_set = (fun () -> current);
        head = (fun () -> Some {epoch_id = !head; state_root = state_root !head});
        state_root_at = (fun epoch -> Some (state_root epoch));
        funding = no_funding;
        self_test = (fun () -> Lwt.return (Error "unused"));
        nonce = (fun () -> raw_hash "nonce"); sleep = Lwt_unix.sleep;
        now; cancel_rounds = (fun () -> true);
        send = (fun ~peer wire ->
          sent := wire :: !sent;
          match List.assoc_opt peer !nodes with
          | Some (service, _) when network -> Service.on_message service ~source:who.address wire
          | _ -> Lwt.return_unit);
        broadcast = (if network then deliver who.address else fun wire -> sent := wire :: !sent; Lwt.return_unit);
        relay = (fun ~source:_ _ -> Lwt.return_unit)} in
    service, disk in
  List.iter (fun who ->
    let disk = history root who in
    if History.reserve disk ~scope:scope.body ~caller:caller.address ~session
        ~sequence:scope.sequence ~request:scope.request <> Ok true then fail "cancel key reserve";
    if History.saved disk ~caller:caller.address ~session ~sequence:scope.sequence
        ~request:scope.request <> Ok None then fail "cancel key test has completed result";
    History.close disk) peers;
  if many then begin
    let store people signer validators sequence =
      let next = Protocol.{call with coordinator = signer.address;
        request = {call.request with sequence;
          validator_set_root = Octra_consensus.C_config.validator_set_hash validators}} in
      let next = {next with caller_signature = sign caller (Protocol.intent_sign_bytes next)} in
      let next = {next with signature = sign signer (Protocol.call_sign_bytes next)} in
      let body = Record.encode_scope next validators in
      List.iter (fun who ->
        let disk = history root who in
        if History.reserve disk ~scope:body ~caller:caller.address ~session
            ~sequence ~request:(Certificate.request_id next.request) <> Ok true then
          fail "cancel later scope reserve";
        History.close disk) people in
    store peers stranger (members (stranger :: peers)) (Int64.succ scope.sequence);
    store [origin] origin (members [origin; stranger]) (Int64.add scope.sequence 2L)
  end;
  nodes := List.map (fun who -> who.address, make who) peers;
  let first () = fst (List.assoc origin.address !nodes) in
  let auth = signed_cancel caller session in
  let round = signed_round origin auth in
  let check_unknown name value delivery =
    let path = Filename.concat root name in
    Unix.mkdir path 0o700;
    let peer = List.hd peers in
    let disk = history path peer in
    Option.iter (fun (value : History.scope) ->
      if History.reserve disk ~scope:value.body ~caller:caller.address ~session
          ~sequence:value.sequence ~request:value.request <> Ok true then fail "cancel bad scope setup") value;
    History.close disk;
    let service, disk = make ~path ~network:false peer in
    Lwt.finalize (fun () ->
      let count = List.length !sent in
      let* () = Service.on_message service ~source:origin.address
        (Protocol.encode (Protocol.Cancel_round delivery)) in
      if History.cancelled disk ~caller:caller.address ~session <> Ok false
         || List.length !sent <> count || !executions <> 0 then fail "cancel unknown scope trusted";
      Lwt.return_unit) (fun () -> Service.shutdown service) in
  Lwt.finalize (fun () ->
    let* () = if retain then Lwt.return_unit else begin
    let* () = check_unknown "absent" None round in
    let* () = check_unknown "request" (Some {scope with request = raw_hash "other"}) round in
    let* () = check_unknown "signature" (Some {scope with body = Record.encode_scope
      {call with signature = String.make 64 '\000'} validators}) round in
    let* () = check_unknown "caller" (Some {scope with body = Record.encode_scope
      {call with caller_signature = String.make 64 '\000'} validators}) round in
    let* () = check_unknown "sequence" (Some {scope with sequence = Int64.succ scope.sequence}) round in
    let* () = check_unknown "session" (Some scope)
      (signed_round origin (signed_cancel caller (raw_hash "other session"))) in
    let* () = check_unknown "origin" (Some scope) (signed_round (identity ()) auth) in
    check_unknown "unsigned" (Some scope) {round with signature = String.make 64 '\000'}
    end in
    let* started = Rpc.cancel (first ()) (cancellation_params caller session) in
    if Result.is_error started then fail "cancel old origin start";
    let* delivery = Lwt.pick [Lwt.protected held;
      (let* () = Lwt_unix.sleep 8.0 in fail "cancel old origin did not send")] in
    List.iter (fun (who, (_, disk)) ->
      if who <> origin.address && History.cancelled disk ~caller:caller.address ~session <> Ok false then
        fail "cancel old origin loss not exercised") !nodes;
    let* () = Lwt_list.iter_s (fun (_, (service, _)) -> Service.shutdown service) !nodes in
    nodes := List.map (fun who -> who.address, make who) peers;
    drop := false;
    let read () = Service.cancel_receipts (first ()) Service.{caller = caller.address;
      caller_public_key = caller.public_key; caller_signature = auth.caller_signature; session_id = session;
      until = auth.until} in
    let rec wait attempts =
      let* receipts = read () in
      match receipts with
      | Ok values when List.length values = List.length peers -> Lwt.return values
      | _ when attempts > 0 -> let* () = Lwt_unix.sleep 0.02 in wait (attempts - 1)
      | _ -> fail "cancel old keys did not restore delivery" in
    let* receipts = wait 450 in
    List.iter (fun (ack : Protocol.cancel_ack) ->
      if ack.origin_key <> origin.public_key
         || ack.node_key <> (List.find (fun who -> who.address = ack.node_id) peers).public_key then
        fail "cancel original keys changed";
      if ack.node_id <> origin.address && ack.delivery.round <= delivery.round then
        fail "cancel old origin round reused") receipts;
    List.iter (fun (_, (_, disk)) ->
      if History.cancelled disk ~caller:caller.address ~session <> Ok true then
        fail "cancel old keys not committed") !nodes;
    let count = List.length !sent in
    let* () = Lwt_list.iter_s (fun (_, (service, _)) ->
      Service.on_message service ~source:origin.address (Protocol.encode (Protocol.Call call))) !nodes in
    let* () = if retain then Lwt.return_unit else
      let+ submitted = Rpc.submit (first ())
        (build_request validators origin caller !head (raw_hash "fresh session") 1L) in
      if Result.is_ok submitted then fail "removed origin started new work" in
    if !executions <> 0 || List.length !sent <> count then
      fail "cancel keys permitted new work";
    let* () = Lwt_list.iter_s (fun (_, (service, _)) -> Service.shutdown service) !nodes in
    nodes := List.map (fun who -> who.address, make who) peers;
    let* restored = read () in
    if restored <> Ok receipts then fail "cancel old keys receipts not durable";
    head := 165L;
    let count = List.length !sent in
    let* () = Lwt_list.iter_s (fun (_, (service, _)) ->
      Service.on_message service ~source:origin.address
        (Protocol.encode (Protocol.Cancel_round (signed_round ~round:64 origin auth)))) !nodes in
    if List.length !sent <> count then fail "cancel old keys extended expiry";
    List.iter (fun (_, (_, disk)) ->
      if History.cancelled disk ~caller:caller.address ~session <> Ok true then
        fail "cancel key expiry reopened session") !nodes;
    Lwt.return_unit)
    (fun () -> Lwt_list.iter_s (fun (_, (service, _)) -> Service.shutdown service) !nodes)

let run ?(span = 64L) ?(inject_duplicate_offer = false) ?(cancel_on_offer = false)
    ?(output = "{\"tokens\":[198]}")
    ?(restart = false) ?(cancel_restart = false) ?(fetch_restart = false)
    ?(rotate = false) ?(cut = Keep) ?(funding_wait = false) data_dir validator_count =
  Mirage_crypto_rng_unix.use_default ();
  Printf.eprintf "event = service_case cut = %s restart = %b fetch = %b rotate = %b\n%!"
    (match cut with Keep -> "keep" | At_plan -> "plan" | At_call -> "call"
      | At_vote -> "vote" | Lost -> "lost" | Cold -> "cold") restart fetch_restart rotate;
  let open Lwt.Syntax in
  Hashtbl.clear fund_roots;
  let identities = List.init validator_count (fun _ -> identity ()) in
  let caller = identity () in
  let validators =
    List.map
      (fun (value : identity) ->
        Octra_consensus.C_types.{
          address = value.address;
          pubkey = value.public_key;
        })
      identities in
  let validator_set = Octra_consensus.C_types.make_validator_set validators in
  let active_set = ref validator_set in
  let head_epoch = ref 100L in
  let head_available = ref true in
  let roots_available = ref true in
  let funding_open = ref (not funding_wait) in
  let requested = ref None in
  let late_seen = ref false in
  let late_check = ref false in
  let late_bad = ref false in
  let check_wait = ref (fun () -> Lwt.return_true) in
  let slow_read = ref false in
  let live_reads = ref 0 in
  let peak_reads = ref 0 in
  let clock = ref 0L in
  let now () = Int64.add (now ()) !clock in
  let fund_lock = Lwt_mutex.create () in
  let index_root epoch = hex_hash ("compute index " ^ Int64.to_string epoch) in
  let fund_root epoch root =
    Octra_core.Epoch_index_commitment.folded_state_root
      ~ledger_state_root:root ~epoch_index_root:(index_root epoch)
    |> Octra_node_runtime.Text.hex_to_string in
  let* fund_store = Store.open_store (Filename.concat data_dir ("funds_" ^ caller.address)) in
  let fund_ledger = Ledger.create_from_store fund_store in
  let get = function Ok value -> value | Error reason -> fail reason in
  Ledger.add_account fund_ledger caller.address (Z.of_string "1000000000000") |> get;
  let* () = Ledger.flush_dirty_lwt fund_ledger in
  let commit_funds epoch headers =
    let* () = Store.begin_epoch_batch fund_store in
    Ledger.begin_journal fund_ledger |> get;
    let* () = Lwt_list.iter_s (fun (header : Protocol.header) ->
      let module Tx = Octra_core.Transaction in
      let account = Ledger.find fund_ledger caller.address in
      let terms = Funding.prepare_header ~header ~target:Funding.escrow
        ~nonce:(Int64.of_int (account.nonce + 1)) |> get in
      let permission = Funding.authorize terms ~signature:(sign caller (Funding.sign_bytes terms)) |> get in
      let unsigned = Tx.{from = caller.address; to_ = Funding.escrow; amount = Z.of_int64 terms.amount;
        nonce = account.nonce + 1; ou = Z.of_int 10_000; timestamp = Int64.to_float epoch;
        public_key = Some (Base64.encode_exn caller.public_key); signature = "";
        encrypted_data = None; message = Some (Funding.encode_request ~header permission);
        op_type = ComputeFund} in
      let tx = Tx.sign_with_privkey unsigned
        (Base64.encode_exn (Mirage_crypto_ec.Ed25519.priv_to_octets caller.private_key))
        |> Tx.to_yojson |> Octra_core.Tx_payload.decode_admit |> get in
      if not (Tx.verify tx (Option.get tx.public_key)) then fail "funding signature refused";
      let+ funded = Funding.hold_header ~ledger:fund_ledger ~chain_id:header.request.chain_id
        ~epoch ~fee:tx.ou ~header permission in
      let _, fee = get funded in
      Ledger.credit fund_ledger header.coordinator fee |> get) headers in
    let* () = Store.set_meta fund_store "last_epoch" (Int64.to_string epoch) in
    let* () = Store.commit_epoch_batch fund_store "compute test funding" in
    Ledger.commit_journal fund_ledger |> get;
    let* () = Store.tag_epoch fund_store (Int64.to_int epoch) in
    let* binding = Store.epoch_binding fund_store (Int64.to_int epoch) in
    Hashtbl.replace fund_roots epoch (fund_root epoch (get binding).root);
    head_epoch := epoch;
    Lwt.return_unit in
  let fund epoch calls = Lwt_mutex.with_lock fund_lock (fun () ->
    commit_funds epoch (List.map (fun call -> Protocol.header call |> get) calls)) in
  let rec advance_funds epoch last =
    if epoch > last then Lwt.return_unit else
    let* () = Lwt_unix.sleep 0.1 in
    let* () = Lwt_mutex.with_lock fund_lock (fun () ->
      if epoch <= !head_epoch then Lwt.return_unit else commit_funds epoch []) in
    advance_funds (Int64.succ epoch) last in
  let observe_ready head (header : Protocol.header) =
    requested := Some header;
    if not !funding_open then Lwt.return_ok None
    else Lwt_mutex.with_lock fund_lock (fun () ->
      let* () = if not !late_check then Lwt.return_unit else
        let+ pending = !check_wait () in
        if not pending then late_bad := true;
        late_check := false in
      let* found = Funding.read fund_store ~id:(Certificate.request_id header.request) in
      let* () = match found with
        | Error reason -> Lwt.fail_with reason
        | Ok (Some _) -> Lwt.return_unit
        | Ok None when header.request.caller <> caller.address -> Lwt.return_unit
        | Ok None ->
          let epoch = Int64.succ !head_epoch in
          commit_funds epoch [header] in
      if not (Hashtbl.mem fund_roots head.Service.epoch_id) then Lwt.return_ok None
      else
        let* observed = Funding.observe ~chain_id:header.request.chain_id ~epoch:head.epoch_id
          ~index_root:(index_root head.epoch_id)
          ~state_root:(Octra_node_runtime.Text.raw_to_hex head.state_root) ~header fund_store in
        match observed with
        | Ok (Some _) when funding_wait && not !late_seen ->
          late_seen := true;
          late_check := true;
          let+ () = commit_funds (Int64.succ !head_epoch) [] in
          observed
        | _ -> Lwt.return observed) in
  let observe head header =
    incr live_reads;
    peak_reads := max !peak_reads !live_reads;
    Lwt.finalize (fun () ->
      let* () = if funding_wait && !funding_open && not !slow_read then begin
        slow_read := true;
        Lwt.no_cancel (Lwt_unix.sleep 2.25)
      end else Lwt.return_unit in
      observe_ready head header)
      (fun () -> decr live_reads; Lwt.return_unit) in
  let nodes = ref [] in
  let execution_count = ref 0 in
  let executed = Lwt_condition.create () in
  let held_commitments = Hashtbl.create 8 in
  let released_commitments = Hashtbl.create 8 in
  let observed_offers = Hashtbl.create 8 in
  let duplicate_injected = ref false in
  let reorder = ref true in
  let relayed = ref [] in
  let calls = ref [] in
  let stop_request = ref None in
  let stopped, stop_reply = Lwt.wait () in
  let cut_at = cut in
  let cut = ref cut in
  let reached, reach = Lwt.wait () in
  let held_results = ref [] in
  let reads = ref [] in
  let answers = ref [] in
  let dropped = Hashtbl.create 8 in
  let first_vote = ref true in
  let mark () = if Lwt.is_sleeping reached then Lwt.wakeup_later reach () in
  let deliver_now sender excluded payload =
    !nodes
    |> List.filter (fun node ->
      node.identity.address <> sender
      && node.identity.address <> excluded)
    |> Lwt_list.iter_s (fun node ->
      Service.on_message node.service ~source:sender payload) in
  let deliver sender excluded payload =
    relayed := payload :: !relayed;
    match Protocol.decode payload with
    | Protocol.Offer _ when !cut = At_plan ->
      mark ();
      fst (Lwt.wait ())
    | Protocol.Result _ when !cut = At_call || !cut = At_vote || !cut = Lost || !cut = Cold ->
      held_results := payload :: !held_results;
      if (!cut = Lost || !cut = Cold)
          && List.length (List.sort_uniq String.compare !held_results) >= validator_count then
        mark ();
      if !cut = At_vote && !first_vote && sender <> (List.hd identities).address then begin
        first_vote := false;
        let* () = deliver_now sender excluded payload in
        let rec started () =
          if !execution_count >= validator_count then Lwt.return_unit
          else let* () = Lwt_condition.wait executed in started () in
        let* () = started () in
        mark ();
        Lwt.return_unit
      end else Lwt.return_unit
    | Protocol.Fetch fetch ->
      reads := fetch :: !reads;
      deliver_now sender excluded payload
    | Protocol.Answer (fetch, result) ->
      answers := (fetch, result) :: !answers;
      if (cut_at = Lost || cut_at = Cold) && sender = fetch.target
          && not (Hashtbl.mem dropped fetch.target) then begin
        Hashtbl.add dropped fetch.target ();
        Lwt.return_unit
      end else deliver_now sender excluded payload
    | Protocol.Offer offer ->
      Hashtbl.replace observed_offers (Protocol.offer_id offer) ();
      begin
        match !stop_request with
        | None -> ()
        | Some params ->
          stop_request := None;
          Lwt.on_any (Rpc.cancel (List.hd !nodes).service params)
            (Lwt.wakeup_later stop_reply) (Lwt.wakeup_later_exn stop_reply)
      end;
      let* () = deliver_now sender excluded payload in
      if
        not inject_duplicate_offer
        || !duplicate_injected
        || validator_count < 2
      then Lwt.return_unit
      else begin
        duplicate_injected := true;
        let peer = List.nth identities 1 in
        let unsigned = Protocol.{
          offer with
          coordinator = peer.address;
          response_bytes = offer.response_bytes + 1;
          signature = String.make 64 '\000';
        } in
        let duplicate = Protocol.{
          unsigned with
          signature = sign peer (Protocol.offer_sign_bytes unsigned);
        } in
        Protocol.encode (Protocol.Offer duplicate)
        |> deliver_now peer.address ""
      end
    | Protocol.Commitment commitment
      when !reorder && commitment.Selection.node_id = sender ->
      let key = sender, commitment.offer_id in
      if Hashtbl.mem released_commitments key then deliver_now sender excluded payload
      else begin
        Hashtbl.replace held_commitments key payload;
        Lwt.return_unit
      end
    | Protocol.Reveal reveal when !reorder && reveal.Selection.node_id = sender ->
      let* () = deliver_now sender excluded payload in
      begin
        let key = sender, reveal.offer_id in
        match Hashtbl.find_opt held_commitments key with
        | None when Hashtbl.mem released_commitments key -> Lwt.return_unit
        | None -> fail "held commitment missing"
        | Some commitment ->
          Hashtbl.remove held_commitments key;
          Hashtbl.replace released_commitments key commitment;
          deliver_now sender excluded commitment
      end
    | Protocol.Call call ->
      calls := call :: !calls;
      let* () = deliver_now sender excluded payload in
      if !cut = At_call then mark ();
      Lwt.return_unit
    | _ -> deliver_now sender excluded payload in
  let make_node ?(root = data_dir) ?(detached = false) ?(counter = execution_count)
      ?(funding = observe)
      ?(traffic = ref 0) identity =
    let disk = history root identity in
    let check (request : Provider_rpc.execute) =
      match Octra_core.Compute_history.scope disk ~caller:request.caller
          ~session:(Octra_node_runtime.Text.hex_to_string request.session_id) with
      | Ok (Some scope) when Octra_node_runtime.Text.raw_to_hex scope.request = request.request_id ->
        let call, saved = Record.decode_scope scope.body in
        if call.request.sequence <> scope.sequence || saved <> validator_set then
          fail "executor key scope differs";
        let vote = Certificate.{request_id = request_id call.request;
          node_id = identity.address; output_hash = String.make 32 '\000';
          trace_root = String.make 32 '\000';
          output_bytes = call.request.max_output_bytes; steps = 0L; operations = 0L;
          signature = String.make 64 '\000'} in
        let result = Protocol.{vote; output_json = String.make call.request.max_output_bytes 'x'} in
        let size = String.length (Protocol.encode (Protocol.Result result))
          + String.length (Record.encode_read call saved) in
        if Record.reply_space call saved identity.address <> size then
          fail "executor reply allocation differs"
      | _ -> fail "executor started before key scope commit" in
    let provider = Service.{ limits; engine = engine ~output ~check counter executed } in
    let service =
      Service.create
        ~history:(Ok disk)
        ~deps:Service.{
          chain_id = "octra-resource-compute-service-test";
          node_id = identity.address;
          sign = sign identity;
          validator_set = (fun () -> !active_set);
          head = (fun () ->
            if not !head_available then None else Some Service.{
              epoch_id = !head_epoch;
              state_root = state_root !head_epoch;
            });
          state_root_at = (fun epoch ->
            if !roots_available && epoch >= 0L && epoch <= !head_epoch then Some (state_root epoch)
            else None);
          funding;
          self_test = (fun () ->
            Lwt.return
              (Ok Provider_rpc.{
                 executor_root;
                 evidence_root;
                 profile = "q24-v1";
               }));
          nonce = (fun () -> Mirage_crypto_rng.generate 32);
          now; cancel_rounds = (fun () -> true);
          send = (fun ~peer payload ->
            incr traffic;
            if detached then (relayed := payload :: !relayed; Lwt.return_unit)
            else match List.find_opt (fun node -> node.identity.address = peer) !nodes with
              | Some node -> Service.on_message node.service ~source:identity.address payload
              | None -> Lwt.return_unit);
          broadcast = (fun payload ->
            incr traffic;
            if detached then (relayed := payload :: !relayed; Lwt.return_unit)
            else deliver identity.address "" payload);
          relay = (fun ~source payload ->
            incr traffic;
            if detached then (relayed := payload :: !relayed; Lwt.return_unit)
            else deliver identity.address source payload);
          sleep = Lwt_unix.sleep;
        }
        ~provider:(Some provider) in
    { identity; service; disk } in
  nodes := List.map (fun identity -> make_node identity) identities;
  let coordinator = List.hd !nodes in
  let session_id = raw_hash "session" in
  let params =
    build_request
      ~valid_until:(Int64.add !head_epoch span)
      validator_set
      coordinator.identity
      caller
      !head_epoch
      session_id
      1L in
  let open Lwt.Syntax in
  let* () = if span <> 12L then Lwt.return_unit else
    let refuse name params =
      let* result = Rpc.submit coordinator.service params in
      if Result.is_ok result then fail name;
      if !calls <> [] || Hashtbl.length observed_offers <> 0 then
        fail "refused expiry started work";
      Lwt.return_unit in
    let* () = refuse "expired intent accepted"
      (build_request ~valid_until:(Int64.pred !head_epoch)
        validator_set coordinator.identity caller !head_epoch session_id 1L) in
    let* () = refuse "distant intent accepted"
      (build_request ~valid_until:(Int64.add !head_epoch 65L)
        validator_set coordinator.identity caller !head_epoch session_id 1L) in
    let missing = match params with
      | `Assoc fields -> `Assoc (List.remove_assoc "valid_until" fields)
      | _ -> assert false in
    let* () = refuse "untimed intent accepted" missing in
    let fields = match params with `Assoc fields -> fields | _ -> assert false in
    let* () = Lwt_list.iter_s (fun name ->
      refuse "missing fee term accepted" (`Assoc (List.remove_assoc name fields)))
      ["unit_price"; "fee_cap"] in
    let* () = Lwt_list.iter_s (fun value ->
      refuse "invalid fee value accepted"
        (`Assoc (("unit_price", value) :: List.remove_assoc "unit_price" fields)))
      [`Int 2; `String "0"; `String "-1"; `String "+2"; `String "02";
       `String "0x2"; `String "9223372036854775808"] in
    let* () = refuse "fee cap below fuel accepted"
      (`Assoc (("fee_cap", `String "9999999") :: List.remove_assoc "fee_cap" fields)) in
    let* () = refuse "unmetered intent accepted"
      (`Assoc (List.remove_assoc "fuel_limit" fields)) in
    let* () = Lwt_list.iter_s (fun fuel ->
      refuse "invalid fuel intent accepted"
        (`Assoc (("fuel_limit", fuel) :: List.remove_assoc "fuel_limit" fields)))
      [`Int 0; `Int (-1); `Int 5_000_001; `Int 2_000_000_001; `String "10"] in
    let* () = refuse "missing memory limit accepted"
      (`Assoc (List.remove_assoc "memory_bytes" fields)) in
    let* () = Lwt_list.iter_s (fun bytes ->
      refuse "invalid memory limit accepted"
        (`Assoc (("memory_bytes", bytes) :: List.remove_assoc "memory_bytes" fields)))
      [`Int 0; `Int (-1); `Int 67_174_400; `Int (Certificate.max_memory_bytes + 1); `String "65536"] in
    let* () = refuse "missing guest limit accepted"
      (`Assoc (List.remove_assoc "guest_bytes" fields)) in
    Lwt_list.iter_s (fun bytes ->
      refuse "invalid guest limit accepted"
        (`Assoc (("guest_bytes", bytes) :: List.remove_assoc "guest_bytes" fields)))
      [`Int 0; `Int 65_535; `Int 65_537; `Int 67_174_400; `String "65536"] in
  let* submitted = Rpc.submit coordinator.service params in
  let job_id =
    match submitted with
    | Error error -> fail error.Octra_core.Rpc.message
    | Ok json ->
      begin
        match field "job_id" json with
        | Some (`String value) -> value
        | _ -> fail "job id"
      end in
  let head_progress = advance_funds 101L 112L in
  Lwt.async (fun () -> head_progress);
  let* coordinator = if not funding_wait then Lwt.return coordinator else begin
    let rec wait () =
      let* status = Rpc.status coordinator.service
        (read_params coordinator.identity caller !head_epoch job_id) in
      match status, !requested with
      | Ok value, Some header when status_name value = "funding" ->
        if !execution_count <> 0 || !calls <> [] then fail "unfunded work dispatched";
        let decoded = match field "header" value with
          | Some (`String body) -> Protocol.decode_header (Base64.decode_exn body) |> get
          | _ -> fail "funding header missing" in
        if decoded <> header then fail "funding header differs";
        Lwt.return_unit
      | _ -> let* () = Lwt_unix.sleep 0.01 in wait () in
    let* () = Lwt_unix.with_timeout 120. wait in
    let* () = Service.shutdown coordinator.service in
    let restored = make_node coordinator.identity in
    nodes := restored :: List.tl !nodes;
    let* status = Rpc.status restored.service
      (read_params restored.identity caller !head_epoch job_id) in
    begin match status with
    | Ok value when status_name value = "funding" -> ()
    | _ -> fail "funding wait lost after service restart"
    end;
    let* submitted = Rpc.submit restored.service params in
    begin match submitted with
    | Ok value when field "job_id" value = Some (`String job_id) -> ()
    | _ -> fail "funding resubmit changed job"
    end;
    if !execution_count <> 0 || !calls <> [] then fail "funding restart dispatched work";
    check_wait := (fun () ->
      let+ status = Rpc.status restored.service
        (read_params restored.identity caller !head_epoch job_id) in
      match status with
      | Ok value -> status_name value = "funding"
      | Error _ -> false);
    funding_open := true;
    Lwt.return restored
  end in
  let* coordinator = if cut_at = Keep then Lwt.return coordinator else begin
    let* () = Lwt.pick [reached;
      (let* () = Lwt_unix.sleep 120. in Lwt.fail_with "compute restart point timeout")] in
    let before = !execution_count in
    Printf.eprintf "event = service_check phase = restart_cut\n%!";
    let* () = Lwt_unix.with_timeout 120. (fun () -> Service.shutdown coordinator.service) in
    Printf.eprintf "event = service_check phase = restart_closed\n%!";
    let* () = if cut_at = Cold then
      Lwt_list.iter_s (fun node -> Service.shutdown node.service) (List.tl !nodes)
      else Lwt.return_unit in
    let db = history data_dir coordinator.identity in
    let saved = Octra_core.Compute_history.work db ~caller:caller.address
      ~id:(Octra_node_runtime.Text.hex_to_string job_id) in
    if fetch_restart then begin
      let rec consume () =
        match Octra_core.Compute_history.claim_fetch db ~caller:caller.address ~session:session_id
            ~id:(Octra_node_runtime.Text.hex_to_string job_id) ~limit:Protocol.fetch_rounds with
        | Ok (Some round) when round = Protocol.fetch_rounds - 2 -> ()
        | Ok (Some round) when round < Protocol.fetch_rounds - 2 -> consume ()
        | _ -> fail "fetch setup exhausted" in
      consume ();
      reads := [];
      answers := []
    end;
    Octra_core.Compute_history.close db;
    begin match cut_at, saved with
    | At_plan, Ok (Some (Octra_core.Compute_history.Planned _)) -> ()
    | (At_call | Lost | Cold), Ok (Some (Octra_core.Compute_history.Issued _)) -> ()
    | At_vote, Ok (Some (Octra_core.Compute_history.Issued body))
        when List.exists (fun result -> result.Protocol.vote.Certificate.node_id
          <> coordinator.identity.address) (Record.decode body).results -> ()
    | _ -> fail "compute restart phase not durable"
    end;
    let phase = match saved with Ok (Some phase) -> phase | _ -> assert false in
    let body = match phase with
      | Octra_core.Compute_history.Planned body | Funding body | Issued body -> body in
    let record = Record.decode body in
    let check_bad name body =
      let root = Filename.concat (Filename.concat data_dir coordinator.identity.address) name in
      Unix.mkdir root 0o700;
      let db = history root coordinator.identity in
      let id = Octra_node_runtime.Text.hex_to_string job_id in
      if Octra_core.Compute_history.ensure db ~caller:caller.address ~session:session_id <> Ok true
         || Octra_core.Compute_history.plan_job db ~caller:caller.address ~session:session_id
              ~id ~body <> Ok true then fail "invalid work setup";
      begin match phase with
      | Octra_core.Compute_history.Planned _ -> ()
      | Funding _ -> fail "unexpected funding wait in execution test"
      | Issued _ ->
        if Octra_core.Compute_history.advance_job db ~caller:caller.address ~session:session_id
            ~id ~before:(Planned body) ~after:(Issued body) <> Ok true then
          fail "invalid issued setup"
      end;
      Octra_core.Compute_history.close db;
      let counter = ref 0 in
      let traffic = ref 0 in
      let node = make_node ~root ~counter ~traffic coordinator.identity in
      let* status = Rpc.status node.service (read_params node.identity caller !head_epoch job_id) in
      begin match status with
      | Ok value when status_name value = "refused" -> ()
      | _ -> fail ("pending job accepted " ^ name)
      end;
      let* () = Lwt.pause () in
      if !traffic <> 0 || !counter <> 0 then
        fail "invalid work caused execution";
      Service.shutdown node.service in
    let* () = check_bad "work_signature" (Record.encode {record with
      call = {record.call with signature = String.make 64 '\000'}}) in
    let* () = check_bad "work_wire" (body ^ "x") in
    let* () = if cut_at = At_plan then
      check_bad "work_set" (Record.encode {record with validators = Some validator_set})
      else Lwt.return_unit in
    let* () = if cut_at <> At_plan then Lwt.return_unit else begin
      let check_plan ?(fuel_limit = None) name valid_until =
        let unsigned = {record.call with valid_until;
          request = {record.call.request with unit_price = None; fee_cap = None;
            memory_bytes = None; guest_bytes = None; fuel_limit}} in
        let call = {unsigned with caller_signature =
          sign caller (Protocol.intent_sign_bytes unsigned)} in
        let call = {call with signature =
          sign coordinator.identity (Protocol.call_sign_bytes call)} in
        let id = intent_id call in
        let root = Filename.concat (Filename.concat data_dir coordinator.identity.address) name in
        Unix.mkdir root 0o700;
        let db = history root coordinator.identity in
        if Octra_core.Compute_history.ensure db ~caller:caller.address ~session:session_id <> Ok true
           || Octra_core.Compute_history.plan_job db ~caller:caller.address ~session:session_id
                ~id ~body:(Record.encode {record with call}) <> Ok true then
          fail "historical plan setup";
        Octra_core.Compute_history.close db;
        let counter = ref 0 in
        let traffic = ref 0 in
        let node = make_node ~root ~counter ~traffic coordinator.identity in
        Lwt.finalize (fun () ->
          let* status = Rpc.status node.service (read_params node.identity caller !head_epoch
            (Octra_node_runtime.Text.raw_to_hex id)) in
          begin match status with
          | Ok value when status_name value = "refused" -> ()
          | _ -> fail "historical plan resumed execution"
          end;
          if !counter <> 0 || !traffic <> 0 then fail "historical plan caused work";
          Lwt.return_unit) (fun () -> Service.shutdown node.service) in
      let* () = check_plan "old_plan" None in
      let* () = check_plan "timed_plan" record.call.valid_until in
      check_plan ~fuel_limit:record.call.request.fuel_limit "fuel_plan" record.call.valid_until
    end in
    let* () = match record.results with
      | [] -> Lwt.return_unit
      | result :: rest -> check_bad "work_output" (Record.encode {record with
          results = {result with output_json = "{}"} :: rest}) in
    let* () = if cut_at <> At_vote then Lwt.return_unit else begin
      let rec quorum attempts =
        let results = List.filter_map (fun payload -> match Protocol.decode payload with
          | Protocol.Result result -> Some result | _ -> None) !held_results
          |> List.sort_uniq (fun left right -> String.compare
            left.Protocol.vote.Certificate.node_id right.Protocol.vote.Certificate.node_id) in
        if List.length results >= 4 then Lwt.return results
        else if attempts = 0 then Lwt.fail_with "saved quorum timeout"
        else let* () = Lwt_unix.sleep 0.01 in quorum (attempts - 1) in
      let* results = quorum 500 in
      let root = Filename.concat (Filename.concat data_dir coordinator.identity.address) "work_quorum" in
      Unix.mkdir root 0o700;
      let db = history root coordinator.identity in
      let id = Octra_node_runtime.Text.hex_to_string job_id in
      let body = Record.encode {record with results} in
      if Octra_core.Compute_history.ensure db ~caller:caller.address ~session:session_id <> Ok true
         || Octra_core.Compute_history.plan_job db ~caller:caller.address ~session:session_id
              ~id ~body <> Ok true
         || Octra_core.Compute_history.advance_job db ~caller:caller.address ~session:session_id
              ~id ~before:(Planned body) ~after:(Issued body) <> Ok true then
        fail "saved quorum setup";
      Octra_core.Compute_history.close db;
      let counter = ref 0 in
      let traffic = ref 0 in
      let node = make_node ~root ~counter ~traffic coordinator.identity in
      let* _ = await_finished node caller head_epoch job_id 200 in
      if !counter <> 0 || !traffic <> 0 then fail "saved quorum repeated work";
      Service.shutdown node.service
    end in
    if rotate then begin
      let other = identity () in
      active_set := Octra_consensus.C_types.make_validator_set [
        {address = other.address; pubkey = other.public_key}]
    end;
    let restored = make_node coordinator.identity in
    nodes := restored :: (if cut_at = Cold then List.map make_node (List.tl identities)
      else List.tl !nodes);
    cut := Keep;
    let* resumed = Rpc.status restored.service
      (read_params restored.identity caller !head_epoch job_id) in
    Printf.eprintf "event = service_check phase = restart_read\n%!";
    begin match resumed with
    | Ok value when status_name value = "waiting" || status_name value = "finished" -> ()
    | _ -> fail "compute pending status not restored"
    end;
    let* restored = if not fetch_restart then Lwt.return restored else begin
      let targets () = List.map (fun (fetch, _) -> fetch.Protocol.target) !answers
        |> List.sort_uniq String.compare in
      let rec waiting attempts =
        if List.length (targets ()) >= validator_count - 1 then Lwt.return_unit
        else if attempts = 0 then fail "last saved fetch not sent"
        else let* () = Lwt_unix.sleep 0.01 in waiting (attempts - 1) in
      let* () = waiting 300 in
      if !reads = [] || List.exists (fun (fetch : Protocol.fetch) ->
          fetch.round <> Protocol.fetch_rounds - 1) !reads then
        fail "fetch round reset after restart";
      let sent = List.map (fun fetch -> Protocol.encode (Protocol.Fetch fetch)) !reads
        |> List.sort_uniq String.compare in
      let rec reboot count current =
        if count = 0 then Lwt.return current else
          let () = Printf.eprintf "event = service_check phase = fetch_close count = %d\n%!" count in
          let* () = Lwt_unix.with_timeout 120. (fun () -> Service.shutdown current.service) in
          Printf.eprintf "event = service_check phase = fetch_open count = %d\n%!" count;
          let next = make_node current.identity in
          nodes := next :: List.tl !nodes;
          let* status = Rpc.status next.service
            (read_params next.identity caller !head_epoch job_id) in
          begin match status with
          | Ok value when status_name value = "waiting" -> ()
          | _ -> fail "fetch exhaustion prevented result recovery"
          end;
          let* () = Lwt_unix.sleep 1.2 in
          if List.sort_uniq String.compare
              (List.map (fun fetch -> Protocol.encode (Protocol.Fetch fetch)) !reads) <> sent then
            fail "restart renewed fetch budget";
          reboot (count - 1) next in
      let* next = reboot 2 restored in
      Printf.eprintf "event = service_check phase = fetch_answers\n%!";
      let* () = Lwt_list.iter_s (fun (fetch, result) ->
        deliver_now "" "" (Protocol.encode (Protocol.Answer (fetch, result)))) (List.rev !answers) in
      Lwt.return next
    end in
    let* () = if cut_at = Lost || cut_at = Cold then Lwt.return_unit
      else Lwt_list.iter_s (deliver_now "" "") (List.rev !held_results) in
    if cut_at <> At_plan && !execution_count > max before validator_count then
      fail "compute restart repeated execution";
    Lwt.return restored
  end in
  let* response =
    await_finished coordinator caller head_epoch job_id 1000 in
  let chosen = match field "selection" response with
    | Some value -> (match field "offer_id" value with
      | Some (`String id) -> Octra_node_runtime.Text.hex_to_string id
      | _ -> fail "selected offer absent")
    | _ -> fail "selection absent" in
  let check_release () =
    if Hashtbl.fold (fun (_, offer) _ found -> found || offer = chosen) held_commitments false then
      fail "selected commitment not released" in
  let* () = if not inject_duplicate_offer then Lwt.return_unit else
    match Hashtbl.fold (fun (sender, offer) wire found ->
      if offer = chosen then Some (sender, wire) else found) released_commitments None with
    | None -> fail "released commitment absent"
    | Some (sender, wire) -> deliver sender "" wire in
  check_release ();
  if !late_bad then fail "changed funding head released wait";
  if funding_wait && (not !late_seen || !late_check) then fail "funding head race not checked";
  if funding_wait && (not !slow_read || !peak_reads <> 1) then
    fail "funding read duplicated after timeout";
  let rec finished attempts =
    if !execution_count >= validator_count then Lwt.return_unit
    else if attempts = 0 then fail "compute restored execution count"
    else let* () = Lwt_unix.sleep 0.01 in finished (attempts - 1) in
  let* () = if cut_at = Keep then Lwt.return_unit else finished 300 in
  if cut_at <> Keep && !execution_count <> validator_count then fail "compute repeated work";
  if (cut_at = Lost || cut_at = Cold)
      && (!reads = [] || !answers = [] || Hashtbl.length dropped < 3) then
    fail "saved reply recovery not exercised";
  let* () = if not rotate then Lwt.return_unit else begin
    let before = !execution_count in
    let* submit = Rpc.submit coordinator.service
      (build_request !active_set coordinator.identity caller !head_epoch
        (raw_hash "removed-read-coordinator") 1L) in
    if Result.is_ok submit then fail "read authority started fresh work";
    let traffic = ref 0 in
    let counter = ref 0 in
    let observer = make_node ~detached:true ~traffic ~counter (identity ()) in
    let fetch, result = List.hd !answers in
    let* () = Lwt.finalize (fun () ->
      let* () = Service.on_message observer.service ~source:coordinator.identity.address
        (Protocol.encode (Protocol.Fetch fetch)) in
      let* () = Service.on_message observer.service ~source:coordinator.identity.address
        (Protocol.encode (Protocol.Answer (fetch, result))) in
      if !traffic <> 0 || !counter <> 0 then fail "unknown historical set accepted";
      Lwt.return_unit) (fun () -> Service.shutdown observer.service) in
    let peer = List.find (fun node -> node.identity.address = fetch.target) !nodes in
    let context = Record.encode_read fetch.call validator_set in
    if Record.decode_read fetch.call context <> validator_set then
      fail "read context differs";
    begin match Record.encode_read fetch.call !active_set with
    | _ -> fail "read context accepted wrong set"
    | exception Invalid_argument _ -> ()
    end;
    let refused call raw =
      match Record.decode_read call raw with
      | _ -> fail "altered read context accepted"
      | exception (Invalid_argument _ | Failure _) -> () in
    refused {fetch.call with signature = String.make 64 '\000'} context;
    refused {fetch.call with coordinator = (identity ()).address} context;
    refused fetch.call (context ^ "x");
    let sent = List.length !answers in
    let changed = {fetch.call with coordinator = (identity ()).address} in
    let forged = {fetch with call = changed} in
    let forged = {forged with signature = sign coordinator.identity (Protocol.fetch_sign_bytes forged)} in
    let* () = Service.on_message peer.service ~source:coordinator.identity.address
      (Protocol.encode (Protocol.Fetch forged)) in
    let* () = Lwt_unix.sleep 0.05 in
    if List.length !answers <> sent then fail "saved read accepted different call";
    if !execution_count <> before then fail "historical read executed work";
    active_set := validator_set;
    Lwt.return_unit
  end in
  let first_call = List.find (fun (call : Protocol.call) -> call.request.sequence = 1L) !calls in
  let* () = if not funding_wait then Lwt.return_unit else begin
    let root = Filename.concat data_dir "unfunded_peer" in
    Unix.mkdir root 0o700;
    let counter = ref 0 in
    let peer = make_node ~root ~detached:true ~counter ~funding:no_funding
      coordinator.identity in
    Lwt.finalize (fun () ->
      let messages = List.rev !relayed |> List.filter (fun wire ->
        match Protocol.decode wire with
        | Protocol.Offer _ | Capability _ | Commitment _ | Reveal _ -> true
        | _ -> false) in
      let* () = Lwt_list.iter_s (Service.on_message peer.service
        ~source:coordinator.identity.address) messages in
      let* () = Service.on_message peer.service ~source:coordinator.identity.address
        (Protocol.encode (Protocol.Call first_call)) in
      let* () = Lwt_unix.sleep 0.2 in
      if !counter <> 0 then fail "provider ran without committed funding";
      Lwt.return_unit) (fun () -> Service.shutdown peer.service)
  end in
  if first_call.request.memory_bytes <> Some 67_108_864 then
    fail "first call memory differs";
  begin match field "request" response with
  | Some value when field "memory_bytes" value = Some (`Int 67_108_864) -> ()
  | _ -> fail "saved memory term differs"
  end;
  begin match field "request" response, field "certificate" response, field "quote" response with
  | Some request, Some certificate, Some quote ->
    let number json key = match field key json with
      | Some (`String value) -> Z.of_string value
      | Some (`Int value) -> Z.of_int value
      | _ -> fail "quote field missing" in
    let price = number request "unit_price" in
    let maximum = Z.mul price (number request "fuel_limit") in
    let used = Z.mul price (number certificate "steps") in
    if number quote "used" <> used || number quote "unused" <> Z.sub maximum used
       || Z.gt maximum (number request "fee_cap") then fail "saved quote differs"
  | _ -> fail "saved quote absent"
  end;
  if first_call.valid_until <> Some (Int64.add 100L span)
     || first_call.request.expires_epoch > Int64.add 100L span then
    fail "call exceeded signed expiry";
  if span = 12L && List.exists (fun wire -> match Protocol.decode wire with
      | Protocol.Offer offer -> offer.expires_epoch > 112L
      | _ -> false) !relayed then
    fail "offer exceeded signed expiry";
  test_fetch_wire coordinator.identity first_call;
  let before_calls = List.length !calls in
  let unsigned = {first_call with request = {first_call.request with unit_price = None; fee_cap = None}} in
  let old = {unsigned with caller_signature = sign caller (Protocol.intent_sign_bytes unsigned)} in
  let old = {old with signature = sign coordinator.identity (Protocol.call_sign_bytes old)} in
  let* () = deliver_now "" "" (Protocol.encode (Protocol.Call old)) in
  if List.length !calls <> before_calls then fail "unpriced call relayed";
  let changed = {first_call with request = {first_call.request with unit_price = Some 1L}} in
  let changed = {changed with signature = sign coordinator.identity (Protocol.call_sign_bytes changed)} in
  let* () = deliver_now "" "" (Protocol.encode (Protocol.Call changed)) in
  if List.length !calls <> before_calls then fail "altered fee price relayed";
  let changed = {first_call with request = {first_call.request with fee_cap = Some 4_000_000_001L}} in
  let changed = {changed with signature = sign coordinator.identity (Protocol.call_sign_bytes changed)} in
  let* () = deliver_now "" "" (Protocol.encode (Protocol.Call changed)) in
  if List.length !calls <> before_calls then fail "altered fee cap relayed";
  let unsigned = {first_call with valid_until = None;
    request = {first_call.request with unit_price = None; fee_cap = None;
      memory_bytes = None; guest_bytes = None; fuel_limit = None}} in
  let old = {unsigned with caller_signature = sign caller (Protocol.intent_sign_bytes unsigned)} in
  let old = {old with signature = sign coordinator.identity (Protocol.call_sign_bytes old)} in
  let* () = deliver_now "" "" (Protocol.encode (Protocol.Call old)) in
  if List.length !calls <> before_calls then fail "untimed call relayed";
  let unsigned = {first_call with request = {first_call.request with unit_price = None; fee_cap = None;
    memory_bytes = None; guest_bytes = None; fuel_limit = None}} in
  let old = {unsigned with caller_signature = sign caller (Protocol.intent_sign_bytes unsigned)} in
  let old = {old with signature = sign coordinator.identity (Protocol.call_sign_bytes old)} in
  let* () = deliver_now "" "" (Protocol.encode (Protocol.Call old)) in
  if List.length !calls <> before_calls then fail "unmetered call relayed";
  let changed = {first_call with request = {first_call.request with fuel_limit = Some 1}} in
  let changed = {changed with signature = sign coordinator.identity (Protocol.call_sign_bytes changed)} in
  let* () = deliver_now "" "" (Protocol.encode (Protocol.Call changed)) in
  if List.length !calls <> before_calls then fail "unsigned fuel change relayed";
  let changed = {first_call with request = {first_call.request with guest_bytes = Some 131_072}} in
  let changed = {changed with signature = sign coordinator.identity (Protocol.call_sign_bytes changed)} in
  let* () = deliver_now "" "" (Protocol.encode (Protocol.Call changed)) in
  if List.length !calls <> before_calls then fail "unsigned guest change relayed";
  let changed = {first_call with request = {first_call.request with memory_bytes = Some 65_536}} in
  let changed = {changed with signature = sign coordinator.identity (Protocol.call_sign_bytes changed)} in
  let* () = deliver_now "" "" (Protocol.encode (Protocol.Call changed)) in
  if List.length !calls <> before_calls then fail "unsigned memory change relayed";
  let unsigned = {first_call with request = {first_call.request with unit_price = None; fee_cap = None; memory_bytes = None}} in
  let old = {unsigned with caller_signature = sign caller (Protocol.intent_sign_bytes unsigned)} in
  let old = {old with signature = sign coordinator.identity (Protocol.call_sign_bytes old)} in
  let* () = deliver_now "" "" (Protocol.encode (Protocol.Call old)) in
  if List.length !calls <> before_calls then fail "call without memory relayed";
  let unsigned = {first_call with request = {first_call.request with unit_price = None; fee_cap = None;
    memory_bytes = None; guest_bytes = None}} in
  let old = {unsigned with caller_signature = sign caller (Protocol.intent_sign_bytes unsigned)} in
  let old = {old with signature = sign coordinator.identity (Protocol.call_sign_bytes old)} in
  let* () = deliver_now "" "" (Protocol.encode (Protocol.Call old)) in
  if List.length !calls <> before_calls then fail "unlimited guest call relayed";
  let extended = {first_call with valid_until = Option.map Int64.succ first_call.valid_until} in
  let extended = {extended with signature = sign coordinator.identity (Protocol.call_sign_bytes extended)} in
  let* () = deliver_now "" "" (Protocol.encode (Protocol.Call extended)) in
  if List.length !calls <> before_calls then fail "unsigned expiry extension relayed";
  let* () = if cut_at <> Lost && cut_at <> Cold then Lwt.return_unit else begin
    let traffic = ref 0 in
    let counter = ref 0 in
    let observer = make_node ~detached:true ~traffic ~counter (identity ()) in
    let fetch, result = List.hd !answers in
    let send name expected message =
      let before = !traffic in
      let* () = Service.on_message observer.service ~source:coordinator.identity.address
        (Protocol.encode message) in
      if !traffic <> before + (if expected then 1 else 0) then fail name;
      if !counter <> 0 then fail "fetch executed work";
      Lwt.return_unit in
    let fresh call round =
      let value = Protocol.{call; target = fetch.target; epoch_id = !head_epoch;
        expires_epoch = Int64.add !head_epoch 4L; round; signature = String.make 64 '\000'} in
      {value with signature = sign coordinator.identity (Protocol.fetch_sign_bytes value)} in
    Lwt.finalize (fun () ->
      let* () = send "unsigned fetch relayed" false
        (Protocol.Fetch {fetch with signature = String.make 64 '\000'}) in
      let* () = send "retargeted fetch relayed" false
        (Protocol.Fetch {fetch with target = coordinator.identity.address}) in
      let changed = {fetch with call = {fetch.call with caller_signature = String.make 64 '\000'}} in
      let changed = {changed with signature = sign coordinator.identity (Protocol.fetch_sign_bytes changed)} in
      let* () = send "unsigned caller fetch relayed" false (Protocol.Fetch changed) in
      let* () = send "altered answer relayed" false
        (Protocol.Answer (fetch, {result with output_json = String.make (String.length result.output_json) 'x'})) in
      let* () = send "unsigned answer relayed" false
        (Protocol.Answer (fetch, {result with vote = {result.vote with signature = String.make 64 '\000'}})) in
      let provider = List.find (fun item -> item.address = result.vote.node_id) identities in
      let over = {result.vote with steps =
        Int64.succ (Int64.of_int (Option.get fetch.call.request.fuel_limit))} in
      let over = {over with signature = sign provider (Certificate.vote_sign_bytes over)} in
      let* () = send "excessive answer usage relayed" false
        (Protocol.Answer (fetch, {result with vote = over})) in
      let other_target = {fetch with target = (List.find (fun item -> item.address <> fetch.target) identities).address} in
      let other_target = {other_target with signature = sign coordinator.identity (Protocol.fetch_sign_bytes other_target)} in
      let* () = send "answer provider differs" false (Protocol.Answer (other_target, result)) in
      head_available := false;
      let* () = send "fetch without head" false (Protocol.Fetch fetch) in
      head_available := true;
      roots_available := false;
      let* () = send "fetch without root" false (Protocol.Fetch fetch) in
      roots_available := true;
      let old = Protocol.{fetch with epoch_id = first_call.request.epoch_id;
        expires_epoch = first_call.request.epoch_id} in
      let old = {old with signature = sign coordinator.identity (Protocol.fetch_sign_bytes old)} in
      let* () = send "expired fetch relayed" false (Protocol.Fetch old) in
      let changed_set = !active_set in
      let other = identity () in
      active_set := Octra_consensus.C_types.make_validator_set [
        {address = other.address; pubkey = other.public_key}];
      let* () = send "fetch changed validators" false (Protocol.Fetch fetch) in
      active_set := changed_set;
      head_epoch := Int64.succ !head_epoch;
      let make index =
        let unsigned = {first_call with request = {first_call.request with sequence = Int64.of_int (index + 100)}} in
        let signed = {unsigned with caller_signature = sign caller (Protocol.intent_sign_bytes unsigned)} in
        let signed = {signed with signature = sign coordinator.identity (Protocol.call_sign_bytes signed)} in
        fresh signed 0 in
      let* () = Lwt_list.iter_s (fun index -> send "fetch allowance" true (Protocol.Fetch (make index)))
        (List.init 32 Fun.id) in
      let* () = send "fetch duplicate relayed" false (Protocol.Fetch (make 0)) in
      let* () = send "fetch limit ignored" false (Protocol.Fetch (make 32)) in
      let provider = List.find (fun item -> item.address = result.vote.node_id) identities in
      let answer index =
        let fetch = make (index + 100) in
        let vote = {result.vote with request_id = Certificate.request_id fetch.call.request} in
        let vote = {vote with signature = sign provider (Certificate.vote_sign_bytes vote)} in
        Protocol.Answer (fetch, {result with vote}) in
      let* () = Lwt_list.iter_s (fun index -> send "answer allowance" true (answer index))
        (List.init 32 Fun.id) in
      let* () = send "answer duplicate relayed" false (answer 0) in
      let* () = send "answer limit ignored" false (answer 32) in
      head_epoch := Int64.succ !head_epoch;
      let* () = send "fetch allowance not renewed" true (Protocol.Fetch (make 32)) in
      let* () = send "answer allowance not renewed" true (answer 32) in
      let* cancelled = Rpc.cancel observer.service (cancellation_params caller session_id) in
      if Result.is_ok cancelled then fail "relay opened unknown local cancellation";
      let* () = send "unknown timed cancellation relayed" false
        (Protocol.Cancel (signed_cancel ~until:(Option.get first_call.valid_until) caller session_id)) in
      let* () = send "unknown cancellation preserves fetch" true (Protocol.Fetch (make 33)) in
      send "unknown cancellation preserves answer" true (answer 33))
      (fun () -> head_available := true; roots_available := true; Service.shutdown observer.service)
  end in
  let* unsigned = Rpc.status coordinator.service (`Assoc [
    "caller", `String caller.address; "job_id", `String job_id]) in
  if Result.is_ok unsigned then fail "unsigned status exposed result";
  let read = read_request coordinator.identity caller !head_epoch job_id in
  let* direct = Service.status coordinator.service
    {read with caller_signature = String.make 64 '\000'} in
  if Result.is_ok direct then fail "unsigned actor read accepted";
  let* absent = Service.status coordinator.service
    {read with job_id = raw_hash "missing job"; caller_signature = String.make 64 '\000'} in
  if direct <> absent then fail "invalid read disclosed job existence";
  let read_json = read_params coordinator.identity caller !head_epoch job_id in
  let* repeated = Rpc.status coordinator.service (`List [read_json]) in
  begin match repeated with
  | Ok value when value = response -> ()
  | _ -> fail "signed read array differs"
  end;
  head_available := false;
  let* no_head = Rpc.status coordinator.service read_json in
  head_available := true;
  if Result.is_ok no_head then fail "read accepted without head";
  let fields = match read_json with `Assoc fields -> fields | _ -> assert false in
  let* () = Lwt_list.iter_s (fun params ->
    let+ reply = Rpc.status coordinator.service params in
    if Result.is_ok reply then fail "invalid read parameters accepted") [
      `Assoc (("caller", `String caller.address) :: fields);
      `Assoc (("extra", `Null) :: fields);
      `Assoc (List.map (fun (key, value) -> key,
        if key = "caller_signature" then `String (String.make 4096 'A') else value) fields);
      `Assoc (List.map (fun (key, value) -> key,
        if key = "job_id" then `String (hex_hash "wrong job") else value) fields)] in
  begin
    match field "output_json" response with
    | Some (`String value) when value = output -> ()
    | _ -> fail "deterministic output"
  end;
  begin
    match field "certificate" response with
    | Some (`Assoc fields) ->
      if List.assoc_opt "steps" fields <> Some (`String "20000") then
        fail "native usage missing from certificate";
      begin
        match List.assoc_opt "signer_count" fields with
        | Some (`Int count)
          when count >= validator_count - ((validator_count - 1) / 3) -> ()
        | _ -> fail "certificate signer floor"
      end
    | _ -> fail "certificate"
  end;
  begin
    match field "selection" response with
    | Some (`Assoc fields) ->
      begin
        match List.assoc_opt "members" fields with
        | Some (`List members)
          when List.length members = min 5 validator_count -> ()
        | _ -> fail "selection size"
      end
    | _ -> fail "selection"
  end;
  if !execution_count < validator_count - ((validator_count - 1) / 3) then
    fail "independent executions";
  check_release ();
  let offers_after_first = Hashtbl.length observed_offers in
  if inject_duplicate_offer && offers_after_first <> 2 then
    fail "duplicate offer composition was not exercised";
  let executions_after_first = !execution_count in
  let* coordinator =
    if not (restart || cancel_restart) then Lwt.return coordinator
    else begin
      let* () = if not cancel_restart then Lwt.return_unit else
        let invalid = Protocol.Cancel {(signed_cancel caller session_id) with
          caller_signature = String.make 64 '\000'} in
        let* () = deliver_now "" "" (Protocol.encode invalid) in
        let* () = deliver_now "" "" (Protocol.encode
          (Protocol.Cancel (signed_cancel ~chain_id:"other-chain" caller session_id))) in
        let* still_open = Rpc.submit coordinator.service params in
        if Result.is_error still_open then fail "invalid cancellation poisoned session";
        let* cancelled = Rpc.cancel coordinator.service (cancellation_params caller session_id) in
        if Result.is_error cancelled then fail "restart cancellation failed";
        let before = List.length !relayed in
        let* () = deliver_now "" "" (Protocol.encode
          (Protocol.Cancel (signed_cancel caller session_id))) in
        if List.length !relayed <> before then fail "duplicate cancel relayed";
        Lwt.return_unit in
      let messages = List.rev !relayed in
      let* () = Lwt_list.iter_s (fun node -> Service.shutdown node.service) !nodes in
      let db = history data_dir coordinator.identity in
      let id = Octra_node_runtime.Text.hex_to_string job_id in
      let body = match Octra_core.Compute_history.load_job db ~caller:caller.address ~id with
        | Ok (Some body) -> body
        | _ -> fail "completed job record missing" in
      Octra_core.Compute_history.close db;
      let dir = Filename.concat (Filename.concat data_dir coordinator.identity.address) "compute_history" in
      let env = Lmdb.Env.create Lmdb.Rw ~max_maps:4 ~map_size:Octra_core.Compute_history.work_limit
        ~flags:Lmdb.Env.Flags.no_tls dir in
      Fun.protect ~finally:(fun () -> Lmdb.Env.close env) (fun () ->
        let jobs = Lmdb.Map.create Lmdb.Map.Nodup ~key:Lmdb.Conv.string
          ~value:Lmdb.Conv.string ~name:"jobs" env in
        Fun.protect ~finally:(fun () -> Octra_core.Lmdb_handle.close jobs) (fun () ->
          let prefix = Bytes.create 8 in
          Bytes.set_int64_be prefix 0 (Int64.of_int (String.length caller.address));
          let key = Bytes.to_string prefix ^ caller.address ^ id in
          match Lmdb.Txn.go Lmdb.Ro env (fun txn ->
            let row = try Lmdb.Map.get jobs ~txn ("\003" ^ key)
              with Not_found -> fail "coordinator completion lacks reserved storage" in
            if String.length row < 42 || row.[32] <> '\003' then
              fail "coordinator completion has wrong storage";
            if (try ignore (Lmdb.Map.get jobs ~txn ("\002" ^ key)); true with Not_found -> false) then
              fail "coordinator completion moved storage") with
          | Some () -> () | None -> fail "coordinator storage read aborted"));
      let record = Record.decode body in
      let empty = {record with results = []} in
      let members = List.map (fun node -> node.address) identities in
      let max_results = List.map (fun node ->
        let vote = Certificate.{request_id = request_id record.call.request;
          node_id = node; output_hash = String.make 32 '\000'; trace_root = String.make 32 '\000';
          output_bytes = record.call.request.max_output_bytes; steps = 0L; operations = 0L;
          signature = String.make 64 '\000'} in
        Protocol.{vote; output_json = String.make record.call.request.max_output_bytes 'x'}) members in
      let expected = String.length (Record.encode {record with results = max_results}) in
      if Record.job_space empty members <> expected then fail "coordinator allocation differs";
      List.iter (fun members ->
        match Record.job_space empty members with
        | _ -> fail "coordinator allocation accepted invalid members"
        | exception Invalid_argument _ -> ()) [ []; [List.hd members; List.hd members] ];
      let* () = test_cancel_selection data_dir coordinator.identity caller identities record in
      let* () = test_funding_status data_dir coordinator.identity caller record in
      let* () = test_reveal_send data_dir coordinator.identity identities record
        ~queued:false ~fault:false ~cancelled:false ~held:false in
      let* () = test_reveal_send data_dir coordinator.identity identities record
        ~queued:true ~fault:false ~cancelled:false ~held:false in
      let* () = test_reveal_send data_dir coordinator.identity identities record
        ~queued:false ~fault:true ~cancelled:false ~held:false in
      let* () = test_reveal_send data_dir coordinator.identity identities record
        ~queued:false ~fault:false ~cancelled:true ~held:false in
      let* () = test_reveal_send data_dir coordinator.identity identities record
        ~queued:false ~fault:false ~cancelled:false ~held:true in
      let* () = test_reveal_send ~slow:true data_dir coordinator.identity identities record
        ~queued:false ~fault:false ~cancelled:false ~held:false in
      let* () = test_reveal_send ~slow:true ~busy:true data_dir coordinator.identity identities record
        ~queued:false ~fault:false ~cancelled:false ~held:false in
      let* () = test_reveal_send ~drop:true data_dir coordinator.identity identities record
        ~queued:false ~fault:false ~cancelled:false ~held:true in
      let* () = if validator_count < 2 then Lwt.return_unit else
        let reveals = List.sort (fun left right -> String.compare
          (Protocol.encode (Protocol.Reveal right)) (Protocol.encode (Protocol.Reveal left))) record.reveals in
        let origin = List.find (fun (who : identity) ->
          who.address = (List.hd reveals).Selection.node_id) identities in
        test_reveal_send ~reserve:true data_dir origin identities {record with reveals}
          ~queued:false ~fault:false ~cancelled:false ~held:false in
      let* () = test_funding_cancel data_dir coordinator.identity caller record in
      let* () = test_cancel_keys data_dir coordinator.identity caller identities record in
      let* () = test_cancel_keys ~many:true data_dir coordinator.identity caller identities record in
      let* () = if validator_count < 2 then Lwt.return_unit else
        test_cancel_keys ~retain:true data_dir coordinator.identity caller identities record in
      if record.validators <> Some validator_set || Record.encode record <> body then
        fail "completed job validator snapshot differs";
      let old = Record.encode {record with validators = None} in
      if (Record.decode old).validators <> None || Record.encode (Record.decode old) <> old then
        fail "historical job encoding differs";
      let weighted = match Octra_consensus.C_types.make_weighted_validator_set
        (List.mapi (fun index value -> value, Z.of_int (index + 1)) validators) with
        | Ok value -> value | Error reason -> fail reason in
      let weighted_call = {record.call with request = {record.call.request with
        validator_set_root = Octra_consensus.C_config.validator_set_hash weighted}} in
      let weighted_record = {record with call = weighted_call; validators = Some weighted} in
      if Record.decode (Record.encode weighted_record) <> weighted_record then
        fail "weighted job encoding differs";
      let votes = List.map (fun result -> result.Protocol.vote) record.results in
      let extra = match record.results with
        | result :: _ -> Protocol.{result with vote = {
            result.vote with node_id = (identity ()).address}}
        | [] -> fail "completed job has no votes" in
      if Record.select votes (extra :: List.rev record.results) <> record.results then
        fail "completed job changed by late vote";
      begin match Record.select (extra.vote :: votes) record.results with
      | _ -> fail "completed job accepted absent vote"
      | exception Invalid_argument _ -> ()
      end;
      let check_bad name body =
        let root = Filename.concat (Filename.concat data_dir coordinator.identity.address) name in
        Unix.mkdir root 0o700;
        let db = history root coordinator.identity in
        if Octra_core.Compute_history.ensure db ~caller:caller.address ~session:session_id <> Ok true
           || Octra_core.Compute_history.save_job db ~caller:caller.address
                ~session:session_id ~id ~body <> Ok true then fail "invalid record setup";
        Octra_core.Compute_history.close db;
        let node = make_node ~root coordinator.identity in
        let* unauthenticated = Service.status node.service
          {read with caller_signature = String.make 64 '\000'} in
        if unauthenticated <> direct then fail "invalid read disclosed stored corruption";
        let* status = Rpc.status node.service (read_params node.identity caller !head_epoch job_id) in
        begin match status with
        | Ok value when status_name value = "refused" -> ()
        | _ -> fail ("completed job accepted " ^ name)
        end;
        Service.shutdown node.service in
      let results = match record.results with
        | result :: rest -> Protocol.{result with vote = {
            result.vote with signature = String.make 64 '\000'}} :: rest
        | [] -> fail "completed job has no votes" in
      let* () = check_bad "bad_signature" (Record.encode {record with results}) in
      let results = match record.results with
        | result :: rest -> Protocol.{result with output_json = "{}"} :: rest
        | [] -> fail "completed job has no output" in
      let* () = check_bad "bad_output" (Record.encode {record with results}) in
      let results = List.map (fun (result : Protocol.result) ->
        let signer = List.find (fun item -> item.address = result.vote.node_id) identities in
        let vote = {result.vote with steps =
          Int64.succ (Int64.of_int (Option.get record.call.request.fuel_limit))} in
        {result with vote = {vote with signature = sign signer (Certificate.vote_sign_bytes vote)}})
        record.results in
      let* () = check_bad "bad_fuel" (Record.encode {record with results}) in
      let* () = check_bad "bad_wire" (body ^ "x") in
      let other = identity () in
      let changed = Octra_consensus.C_types.make_validator_set [
        {address = other.address; pubkey = other.public_key}] in
      let suffix = Octra_net.Oce1.encode (fun buffer ->
        Octra_net.Oce1.put_string buffer "validators";
        Octra_net.Oce1.put_string buffer
          (Octra_consensus.C_codec.encode_validator_set changed)) in
      let* () = check_bad "bad_set" (old ^ suffix) in
      let check_old_set name expected =
        let root = Filename.concat (Filename.concat data_dir coordinator.identity.address) name in
        Unix.mkdir root 0o700;
        let db = history root coordinator.identity in
        if Octra_core.Compute_history.ensure db ~caller:caller.address ~session:session_id <> Ok true
           || Octra_core.Compute_history.save_job db ~caller:caller.address
                ~session:session_id ~id ~body:old <> Ok true then fail "historical set setup";
        Octra_core.Compute_history.close db;
        let counter = ref 0 in
        let node = make_node ~root ~detached:true ~counter coordinator.identity in
        let* status = Rpc.status node.service (read_params node.identity caller !head_epoch job_id) in
        begin match status with
        | Ok value when if expected then value = response else status_name value = "refused" -> ()
        | _ -> fail "historical set verification differs"
        end;
        if !counter <> 0 then fail "historical set read executed work";
        Service.shutdown node.service in
      let* () = check_old_set "set_current" true in
      active_set := changed;
      let* () = check_old_set "set_absent" false in
      active_set := validator_set;
      let check_old ?(fuel_limit = None) name phase valid_until =
        let unsigned = {record.call with valid_until;
          request = {record.call.request with unit_price = None; fee_cap = None;
            memory_bytes = None; guest_bytes = None; fuel_limit}} in
        let call = {unsigned with caller_signature =
          sign caller (Protocol.intent_sign_bytes unsigned)} in
        let call = {call with signature =
          sign coordinator.identity (Protocol.call_sign_bytes call)} in
        let id = intent_id call in
        let request_id = Certificate.request_id call.request in
        let results = List.map (fun (result : Protocol.result) ->
          let signer = List.find (fun item -> item.address = result.vote.node_id) identities in
          let vote = {result.vote with request_id} in
          {result with vote = {vote with signature = sign signer (Certificate.vote_sign_bytes vote)}})
          record.results in
        let body = Record.encode {record with call; results} in
        let root = Filename.concat (Filename.concat data_dir coordinator.identity.address) name in
        Unix.mkdir root 0o700;
        let db = history root coordinator.identity in
        if Octra_core.Compute_history.ensure db ~caller:caller.address ~session:session_id <> Ok true then
          fail "historical session setup";
        let saved = if phase = `Done then
          Octra_core.Compute_history.save_job db ~caller:caller.address ~session:session_id ~id ~body
        else Octra_core.Compute_history.plan_job db ~caller:caller.address ~session:session_id ~id ~body in
        if saved <> Ok true then fail "historical job setup";
        if phase = `Issued && Octra_core.Compute_history.advance_job db ~caller:caller.address
            ~session:session_id ~id ~before:(Planned body) ~after:(Issued body) <> Ok true then
          fail "historical issued setup";
        Octra_core.Compute_history.close db;
        let counter = ref 0 in
        let traffic = ref 0 in
        let node = make_node ~root ~counter ~traffic coordinator.identity in
        Lwt.finalize (fun () ->
          let id = Octra_node_runtime.Text.raw_to_hex id in
          let* value = await_finished node caller head_epoch id 200 in
          List.iter (fun key ->
            if field key value <> field key response then fail "historical output changed")
            ["status"; "output"; "output_json"; "selection"];
          let expected = match field "request" response with
            | Some (`Assoc fields) -> `Assoc (List.remove_assoc "unit_price" (List.remove_assoc "fee_cap"
                (List.remove_assoc "memory_bytes" (List.remove_assoc "guest_bytes"
                (if fuel_limit = None then List.remove_assoc "fuel_limit" fields else fields)))))
            | _ -> fail "historical request missing" in
          if field "request" value <> Some expected then fail "historical request changed";
          begin match field "certificate" value with
          | Some certificate when field "request_id" certificate =
              Some (`String (Octra_node_runtime.Text.raw_to_hex request_id)) -> ()
          | _ -> fail "historical certificate changed"
          end;
          if !counter <> 0 || !traffic <> 0 then fail "historical read executed work";
          Lwt.return_unit) (fun () -> Service.shutdown node.service) in
      let* () = check_old "old_issued" `Issued None in
      let* () = check_old "old_done" `Done None in
      let* () = check_old "timed_issued" `Issued record.call.valid_until in
      let* () = check_old "timed_done" `Done record.call.valid_until in
      let* () = check_old ~fuel_limit:record.call.request.fuel_limit
        "fuel_issued" `Issued record.call.valid_until in
      let* () = check_old ~fuel_limit:record.call.request.fuel_limit
        "fuel_done" `Done record.call.valid_until in
      reorder := false;
      nodes := List.map (fun identity -> make_node identity) identities;
      let restored = List.hd !nodes in
      let* refused = Service.status restored.service
        {read with caller_signature = String.make 64 '\000'} in
      if Result.is_ok refused then fail "unsigned restored read accepted";
      let* archived = Rpc.status restored.service
        (read_params restored.identity caller !head_epoch job_id) in
      begin match archived with
      | Ok value when value = response -> ()
      | _ -> fail "completed job lost on restart"
      end;
      let* repeated = Rpc.submit restored.service params in
      begin match repeated with
      | Ok value when field "job_id" value = Some (`String job_id) -> ()
      | _ when cancel_restart -> ()
      | _ -> fail "completed job resubmission differs"
      end;
      if !execution_count <> executions_after_first then fail "completed job reexecuted";
      let* () = Lwt_list.iter_s (fun payload ->
        match Protocol.decode payload with
        | Protocol.Call _ | Protocol.Result _ | Protocol.Cancel _ | Protocol.Cancel_round _
        | Protocol.Cancel_ack _ ->
          Lwt.return_unit
        | _ -> deliver_now "" "" payload) messages in
      let call = List.find (fun (call : Protocol.call) ->
        call.request.session_id = session_id && call.request.sequence = 1L) !calls in
      let expected = List.filter (fun payload -> match Protocol.decode payload with
        | Protocol.Result result ->
          result.vote.request_id = Certificate.request_id call.request
        | _ -> false) messages |> List.sort_uniq String.compare in
      let copies payload = List.fold_left (fun count wire ->
        count + if wire = payload then 1 else 0) 0 !relayed in
      let counts = List.map (fun payload -> payload, copies payload) expected in
      let before = List.length !relayed in
      let payload = Protocol.encode (Protocol.Call call) in
      let calls_before = copies payload in
      let* () = deliver_now "" "" payload in
      if not cancel_restart && List.length !relayed <= before then
        fail "restart replay not verified";
      if cancel_restart && validator_count = 1 && copies payload <> calls_before then
        fail "closed restart call relayed";
      let* () = if cancel_restart then Lwt.return_unit else begin
        if List.length counts <> validator_count then fail "provider result coverage";
        let rec returned attempts =
          if List.for_all (fun (payload, count) -> copies payload > count) counts then
            Lwt.return_unit
          else if attempts = 0 then fail "restart lost provider result"
          else let* () = Lwt_unix.sleep 0.005 in returned (attempts - 1) in
        returned 200
      end in
      let* () = Lwt_unix.sleep 0.02 in
      if !execution_count <> executions_after_first then fail "restart repeated execution";
      Lwt.return (List.hd !nodes)
    end in
  if cancel_restart then begin
    let* () = Lwt_list.iter_s (fun node ->
      let second = build_request validator_set node.identity caller !head_epoch session_id 2L in
      let* refused = Rpc.submit node.service second in
      if Result.is_ok refused then fail "restart revived cancelled session";
      Lwt.return_unit) !nodes in
    let result = List.find (fun payload -> match Protocol.decode payload with
      | Protocol.Result _ -> true
      | _ -> false) !relayed in
    let before = List.length !relayed in
    let* () = deliver_now "" "" result in
    if List.length !relayed <> before then fail "cancelled result relayed";
    let call = List.find (fun (call : Protocol.call) ->
      call.request.session_id = session_id && call.request.sequence = 1L) !calls in
    let call = {call with request = {call.request with sequence = 2L}} in
    let call = {call with caller_signature = sign caller (Protocol.intent_sign_bytes call)} in
    let call = {call with signature = sign coordinator.identity (Protocol.call_sign_bytes call)} in
    let* () = deliver_now "" "" (Protocol.encode (Protocol.Call call)) in
    let* () = Lwt_unix.sleep 0.02 in
    if !execution_count <> executions_after_first then fail "cancelled provider executed";
    Lwt_list.iter_s (fun node -> Service.shutdown node.service) !nodes
  end else
  let* () = if restart then head_progress else Lwt.return_unit in
  let second_params =
    build_request
      ~memory_bytes:33_554_432
      validator_set
      coordinator.identity
      caller
      !head_epoch
      session_id
      2L in
  let* second_submit = Rpc.submit coordinator.service second_params in
  let second_job_id =
    match second_submit with
    | Error error -> fail error.Octra_core.Rpc.message
    | Ok json ->
      begin
        match field "job_id" json with
        | Some (`String value) -> value
        | _ -> fail "second job id"
      end in
  let second_progress =
    if restart || funding_wait || cut_at <> Keep then
      advance_funds (Int64.succ !head_epoch) (Int64.add !head_epoch 12L)
    else head_progress in
  let* second_response =
    await_finished coordinator caller head_epoch second_job_id 1000 in
  Printf.eprintf "event = service_check phase = second_reply\n%!";
  begin match field "request" second_response with
  | Some value when field "memory_bytes" value = Some (`Int 33_554_432) -> ()
  | _ -> fail "reused committee memory differs"
  end;
  begin
    match field "output_json" second_response with
    | Some (`String value) when value = output -> ()
    | _ -> fail "reused deterministic output"
  end;
  if not restart && not funding_wait && cut_at = Keep
      && Hashtbl.length observed_offers <> offers_after_first then
    fail "committee lease created another offer";
  if
    !execution_count
    < executions_after_first + validator_count - ((validator_count - 1) / 3)
  then
    fail "committee lease independent executions";
  let saved_call = List.find (fun (call : Protocol.call) ->
    call.request.session_id = session_id && call.request.sequence = 2L) !calls in
  let* () = second_progress in
  Printf.eprintf "event = service_check phase = second_head\n%!";
  let* () =
    if validator_count <> 1 || restart || cut_at <> Keep || span <> 64L then Lwt.return_unit
    else match !calls with
    | call :: [_] ->
      let observer = make_node ~detached:true (identity ()) in
      Lwt.finalize
        (fun () ->
          let before = List.length !relayed in
          let* () = Lwt_list.iter_s
            (Service.on_message observer.service ~source:coordinator.identity.address)
            (List.rev !relayed) in
          let added = List.length !relayed - before in
          let present = List.filteri (fun index _ -> index < added) !relayed in
          test_request_expiry ~fund ~present
            ~next_window:(fun () -> clock := Int64.add !clock 60_001L)
            observer.service caller coordinator.identity call
            head_epoch relayed execution_count)
        (fun () -> Service.shutdown observer.service)
    | _ -> fail "initial request count" in
  let cancelled_session = raw_hash "cancelled-session" in
  let cancel_until = Int64.add !head_epoch 64L in
  let cancel_request =
    build_request
      validator_set
      coordinator.identity
      caller
      !head_epoch
      cancelled_session
      1L in
  if cancel_on_offer then
    stop_request := Some (cancellation_params ~until:cancel_until caller cancelled_session);
  let* cancel_submit = Rpc.submit coordinator.service cancel_request in
  Printf.eprintf "event = service_check phase = cancel_submit\n%!";
  let cancel_id =
    match cancel_submit with
    | Error error -> fail error.Octra_core.Rpc.message
    | Ok json ->
      match field "job_id" json with
      | Some (`String value) -> value
      | _ -> fail "cancel job id" in
  let* cancel_result =
    if cancel_on_offer then Lwt.pick [
      stopped;
      (let* () = Lwt_unix.sleep 120. in fail "cancel offer trigger");
    ]
    else Rpc.cancel coordinator.service
      (cancellation_params ~until:cancel_until caller cancelled_session) in
  begin
    match cancel_result with
    | Error error -> fail error.Octra_core.Rpc.message
    | Ok json ->
      begin
        match field "status" json, field "cancelled_jobs" json with
        | Some (`String "cancelled"), Some (`Int 1) -> ()
        | _ -> fail "cancel result"
      end
  end;
  Printf.eprintf "event = service_check phase = cancel_reply\n%!";
  let* active_jobs = Service.active_jobs coordinator.service in
  if active_jobs <> 0 then fail "cancelled caller lease";
  let* () =
    if not cancel_on_offer then Lwt.return_unit
    else
      let* status = Rpc.status coordinator.service
        (read_params coordinator.identity caller !head_epoch cancel_id) in
      begin match status with
      | Ok json when status_name json = "refused" -> Lwt.return_unit
      | _ -> fail "cancelled actor wait remains live"
      end in
  let* repeated_submit = Rpc.submit coordinator.service cancel_request in
  begin
    match repeated_submit with
    | Error _ -> ()
    | Ok _ -> fail "cancelled session accepted"
  end;
  let* () =
    if not cancel_on_offer then Lwt.return_unit
    else
      let signed_call session_id =
        let unsigned = Protocol.{saved_call with request = {
          saved_call.request with session_id; sequence = 1L;
        }} in
        let authorized = {unsigned with
          caller_signature = sign caller (Protocol.intent_sign_bytes unsigned)} in
        Protocol.Call {authorized with signature =
          sign coordinator.identity (Protocol.call_sign_bytes authorized)}
        |> Protocol.encode in
      head_epoch := 114L;
      let owners = caller :: List.init 7 (fun _ -> identity ()) in
      let* () = Lwt_list.iter_s (fun index ->
        let session = raw_hash ("cancel-entry:" ^ string_of_int index) in
        let owner = List.nth owners ((index + 1) / 32) in
        cancel_work coordinator.disk owner session;
        let* result = Rpc.cancel coordinator.service (cancellation_params owner session) in
        match result with
        | Ok _ -> Lwt.return_unit
        | Error _ -> fail "cancellation acceptance") (List.init 255 Fun.id) in
      let extra = identity () in
      cancel_work coordinator.disk extra (raw_hash "cancel-overflow");
      let* overflow = Rpc.cancel coordinator.service
        (cancellation_params extra (raw_hash "cancel-overflow")) in
      let before = List.length !calls in
      let* () = Service.on_message coordinator.service ~source:caller.address
        (signed_call cancelled_session) in
      if List.length !calls <> before then fail "cancelled call admitted";
      if Result.is_ok overflow then fail "live cancellation limit";
      let* unscoped = Rpc.cancel coordinator.service
        (cancellation_params caller cancelled_session) in
      if Result.is_ok unscoped then fail "repeat cancellation lost signed deadline";
      let* repeated = Rpc.cancel coordinator.service
        (cancellation_params ~until:cancel_until caller cancelled_session) in
      if Result.is_error repeated then fail "repeat cancellation at capacity";
      let executions = !execution_count in
      funding_open := false;
      let* () = Service.on_message coordinator.service ~source:caller.address
        (signed_call (raw_hash "after-cancel")) in
      if List.length !calls <> before then fail "new session reused another deposit";
      if !execution_count <> executions then fail "unfunded session executed";
      let wire = signed_call (raw_hash "after-cancel") in
      let call = match Protocol.decode wire with Protocol.Call call -> call | _ -> assert false in
      let* () = fund (Int64.succ !head_epoch) [call] in
      funding_open := true;
      let* () = Service.on_message coordinator.service ~source:caller.address wire in
      if List.length !calls <> before + 1 then fail "funded session refused";
      let rec started () =
        if !execution_count >= executions + 1 then Lwt.return_unit
        else let* () = Lwt_condition.wait executed in started () in
      let* () = Lwt.pick [started ();
        (let* () = Lwt_unix.sleep 120. in fail "funded session execution timeout")] in
      let* () = advance_head ~first:115L ~last:128L head_epoch in
      if List.length !calls <> before + 1 then fail "call after cancellation";
      if !execution_count <> executions + 1 then fail "execution after cancellation";
      head_epoch := 178L;
      cancel_work coordinator.disk caller (raw_hash "cancel-at-expiry");
      let* full = Rpc.cancel coordinator.service
        (cancellation_params caller (raw_hash "cancel-at-expiry")) in
      if Result.is_ok full then fail "live cancellation removed";
      head_epoch := 179L;
      cancel_work coordinator.disk caller (raw_hash "cancel-after-expiry");
      let* available = Rpc.cancel coordinator.service
        (cancellation_params caller (raw_hash "cancel-after-expiry")) in
      if Result.is_error available then fail "expired cancellation capacity";
      Lwt.return_unit in
  let* () =
    if validator_count < 2 then Lwt.return_unit
    else test_remote_cancel ~advance:advance_funds ~calls !nodes validator_set caller head_epoch
      session_id execution_count in
  let* () = if not restart then Lwt.return_unit else begin
    head_epoch := Int64.add !head_epoch 100L;
    let* expired = Service.status coordinator.service read in
    if Result.is_ok expired then fail "expired actor read exposed result";
    let future = read_request coordinator.identity caller (Int64.succ !head_epoch) job_id in
    let* early = Service.status coordinator.service future in
    if Result.is_ok early then fail "future actor read exposed result";
    let params = read_params coordinator.identity caller !head_epoch job_id in
    let* archived = Rpc.status coordinator.service params in
    begin match archived with
    | Ok value when value = response -> ()
    | _ -> fail "expired completed job unavailable"
    end;
    roots_available := false;
    let* refused = Rpc.status coordinator.service params in
    begin match refused with
    | Ok value when status_name value = "refused" -> ()
    | _ -> fail "completed job ignored missing root"
    end;
    roots_available := true;
    let other = identity () in
    active_set := Octra_consensus.C_types.make_validator_set [
      {address = other.address; pubkey = other.public_key}];
    let count = !execution_count in
    let* restored = Rpc.status coordinator.service params in
    begin match restored with
    | Ok value when value = response -> ()
    | _ -> fail "completed job lost original validators"
    end;
    let* submit = Rpc.submit coordinator.service
      (build_request !active_set coordinator.identity caller !head_epoch
        (raw_hash "set-change") 1L) in
    if Result.is_ok submit then fail "removed coordinator started new work";
    if !execution_count <> count then fail "validator snapshot repeated execution";
    active_set := validator_set;
    let* absent = Rpc.status coordinator.service
      (read_params coordinator.identity (identity ()) !head_epoch job_id) in
    if Result.is_ok absent then fail "completed job leaked to another caller";
    Lwt.return_unit
  end in
  let* () = Lwt_list.iter_s (fun node -> Service.shutdown node.service) !nodes in
  Store.close fund_store

let () =
  Test_workspace.with_dir "resource_service" (fun data_dir ->
    match Array.to_list Sys.argv with
    | [_; "--restart"] -> Lwt_main.run (run ~restart:true data_dir 1)
    | [_; "--short"] -> Lwt_main.run (Lwt_unix.with_timeout 60.
        (fun () -> run ~span:12L data_dir 1))
    | [_; "--funding"] -> Lwt_main.run (run ~funding_wait:true data_dir 1)
    | [_; "--funding-cancel"] -> Lwt_main.run (Lwt_unix.with_timeout 180.
        (fun () -> run ~restart:true data_dir 1))
    | [_; "--cancel-offer"] -> Lwt_main.run (run ~cancel_on_offer:true data_dir 1)
    | [_; "--cancel-slots"] -> Lwt_main.run (test_cancel_slots data_dir)
    | [_; "--cancel-hops"] -> Lwt_main.run (test_cancel_hops data_dir)
    | [_; "--relay"] -> Lwt_main.run (Lwt_unix.with_timeout 180. (fun () ->
        let open Lwt.Syntax in
        let* () = test_cancel_slots data_dir in
        let* () = test_cancel_pace data_dir in
        run ~restart:true data_dir 1))
    | [_] ->
      test_cancel_wire ();
      test_read_scope ();
      test_read_vector ();
      Lwt_main.run (run ~span:12L data_dir 1);
      Lwt_main.run (run ~cut:At_plan data_dir 1);
      Lwt_main.run (run ~cut:At_call data_dir 5);
      Lwt_main.run (run ~cut:At_vote data_dir 5);
      Lwt_main.run (run ~cut:Lost data_dir 5);
      Lwt_main.run (run ~cut:Cold data_dir 5);
      Lwt_main.run (run ~cut:Cold ~rotate:true data_dir 5);
      Lwt_main.run (run ~cut:Lost ~fetch_restart:true data_dir 5);
      Lwt_main.run (test_cancel_slots data_dir);
      Lwt_main.run (test_cancel_delivery data_dir);
      Lwt_main.run (test_cancel_hops data_dir);
      Lwt_main.run (test_cancel_receipts data_dir);
      Lwt_main.run (test_cancel_saved data_dir);
      Lwt_main.run (test_cancel_scope data_dir);
      Lwt_main.run (test_cancel_priority data_dir);
      Lwt_main.run (test_offer_expiry data_dir);
      Lwt_main.run (run ~inject_duplicate_offer:true data_dir 5);
      Lwt_main.run (run data_dir 1);
      Lwt_main.run (run ~restart:true data_dir 1);
      Lwt_main.run (run ~restart:true data_dir 5);
      Lwt_main.run (run ~cancel_restart:true data_dir 1);
      Lwt_main.run (run ~cancel_restart:true data_dir 5);
      Lwt_main.run (run ~cancel_on_offer:true data_dir 1)
    | [_; "--cancel-restart"] -> Lwt_main.run (run ~cancel_restart:true data_dir 1)
    | [_; "--cancel-net"] -> Lwt_main.run (run ~cancel_restart:true data_dir 5)
    | [_; "--cancel-keys"] -> Lwt_main.run (run ~cancel_restart:true data_dir 5)
    | [_; "--cancel-priority"] -> Lwt_main.run (test_cancel_priority data_dir)
    | [_; "--cancel-send"] ->
      test_cancel_wire ();
      Lwt_main.run (test_cancel_slots data_dir);
      Lwt_main.run (test_cancel_delivery data_dir);
      Lwt_main.run (test_cancel_hops data_dir);
      Lwt_main.run (test_cancel_receipts data_dir);
      Lwt_main.run (test_cancel_saved data_dir);
      Lwt_main.run (test_cancel_scope data_dir);
      Lwt_main.run (test_cancel_priority data_dir);
      Lwt_main.run (run ~cancel_restart:true data_dir 1);
      Lwt_main.run (run ~cancel_restart:true data_dir 5)
    | [_; "--cancel-traffic"] ->
      test_cancel_wire ();
      Lwt_main.run (Lwt_unix.with_timeout 180. (fun () ->
        let open Lwt.Syntax in
        let* () = test_cancel_slots data_dir in
        let* () = test_cancel_pace data_dir in
        let* () = test_cancel_hops data_dir in
        test_cancel_receipts data_dir))
    | [_; "--result-restart"] -> Lwt_main.run (run ~restart:true data_dir 5)
    | [_; "--offers"] -> Lwt_main.run (run ~inject_duplicate_offer:true data_dir 5)
    | [_; "--stopped"] ->
      Lwt_main.run (Lwt_unix.with_timeout 180. (fun () ->
        run ~output:"{\"compute_stop\":7}" ~restart:true data_dir 5));
      Lwt_main.run (Lwt_unix.with_timeout 180. (fun () ->
        run ~output:"{\"compute_stop\":\"fuel\"}" ~cut:Cold data_dir 5))
    | [_; "--set"] ->
      Lwt_main.run (run ~cut:At_plan data_dir 1);
      Lwt_main.run (run ~restart:true data_dir 1);
      Lwt_main.run (run ~restart:true data_dir 5)
    | [_; "--work-restart"] ->
      Lwt_main.run (run ~cut:At_plan data_dir 1);
      Lwt_main.run (run ~cut:At_call data_dir 5);
      Lwt_main.run (run ~cut:At_vote data_dir 5)
    | [_; "--fetch"] ->
      Lwt_main.run (run ~cut:Lost data_dir 5);
      Lwt_main.run (run ~cut:Cold data_dir 5);
      Lwt_main.run (run ~cut:Lost ~fetch_restart:true data_dir 5)
    | [_; "--read-set"] -> Lwt_main.run (run ~cut:Cold ~rotate:true data_dir 5)
    | _ -> invalid_arg "resource_service arguments");
  print_endline "status = pass test = resource_service"