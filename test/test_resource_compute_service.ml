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

type identity = {
  address : string;
  private_key : Mirage_crypto_ec.Ed25519.priv;
  public_key : string;
}

type node = {
  identity : identity;
  service : Service.t;
}

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

let state_root epoch =
  raw_hash ("state:" ^ Int64.to_string epoch)

let engine execution_count =
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
    load_model = (fun request ->
      Lwt.return
        (Ok Engine.{
           snapshots = [request.Provider_rpc.model_epoch, request.model_state_root];
           circle_id = request.circle_id;
           graph_root = request.graph_root;
           model_root = request.model_root;
           program_root = request.program_root;
           executor_root = request.executor_root;
           cache_key = hex_hash request.circle_id;
           entries = 291;
           bytes = 409_000_000L;
         }));
    drop_model = (fun _ -> Lwt.return (Ok ()));
    self_test = (fun () ->
      Lwt.return
        (Ok Provider_rpc.{
           executor_root;
           evidence_root;
           profile = "q24-v1";
         }));
    execute = (fun ~model:_ ~program:_ ~storage:_ _ ->
      incr execution_count;
      Lwt.return
        (Ok Engine.{
           output_json = "{\"tokens\":[198]}";
           trace_root = hex_hash "trace";
           steps = 437_546_558L;
           operations = 1_200L;
           effort_used = 20_000_000;
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

let build_request validator_set coordinator caller head_epoch session_id sequence =
  let params_json = "[\"15496,11,995\",32]" in
  let certificate_request = Certificate.{
    chain_id = "octra-resource-compute-service-test";
    epoch_id = head_epoch;
    expires_epoch = Int64.add head_epoch 64L;
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
    method_name = "complete";
    params_json;
    caller_public_key = caller.public_key;
    caller_signature = String.make 64 '\000';
    coordinator = coordinator.address;
    signature = String.make 64 '\000';
  } in
  let caller_signature = sign caller (Protocol.intent_sign_bytes unsigned_call) in
  `Assoc [
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

let cancellation_params caller session_id =
  let caller_signature =
    Service.cancellation_sign_bytes
      ~chain_id:"octra-resource-compute-service-test"
      ~caller:caller.address
      ~caller_public_key:caller.public_key
      ~session_id
    |> sign caller in
  `Assoc [
    "caller", `String caller.address;
    "caller_public_key", `String (Base64.encode_exn caller.public_key);
    "caller_signature", `String (Base64.encode_exn caller_signature);
    "session_id", `String (Octra_node_runtime.Text.raw_to_hex session_id);
  ]

let advance_head ?(first = 101L) ?(last = 112L) head =
  let open Lwt.Syntax in
  let rec loop epoch =
    if epoch > last then Lwt.return_unit
    else
      let* () = Lwt_unix.sleep 0.1 in
      head := epoch;
      loop (Int64.succ epoch) in
  loop first

let rec await_finished service caller job_id attempts =
  if attempts <= 0 then fail "committee result timeout";
  let open Lwt.Syntax in
  let* response =
    Rpc.status
      service
      (`Assoc [
         "caller", `String caller;
         "job_id", `String job_id;
       ]) in
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
    await_finished service caller job_id (attempts - 1)

let test_cancel_slots () =
  Mirage_crypto_rng_unix.use_default ();
  let open Lwt.Syntax in
  let signer = identity () in
  let caller = identity () in
  let peers = List.init 8 (fun _ -> identity ()) in
  let validator_set = Octra_consensus.C_types.make_validator_set
    [Octra_consensus.C_types.{address = signer.address; pubkey = signer.public_key}] in
  let epoch = ref 100L in
  let service = Service.create ~provider:None ~deps:Service.{
    chain_id = "octra-resource-compute-service-test";
    node_id = signer.address;
    sign = sign signer;
    validator_set = (fun () -> validator_set);
    head = (fun () -> Some {epoch_id = !epoch; state_root = state_root !epoch});
    state_root_at = (fun height -> Some (state_root height));
    self_test = (fun () -> Lwt.return (Error "unused"));
    nonce = (fun () -> raw_hash "nonce");
    broadcast = (fun _ -> fst (Lwt.task ()));
    relay = (fun ~source:_ _ -> Lwt.return_unit);
    sleep = Lwt_unix.sleep;
  } in
  let cancel name expected owner session =
    let* result = Rpc.cancel service (cancellation_params owner session) in
    if Result.is_ok result <> expected then fail name;
    Lwt.return_unit in
  let entry index = raw_hash ("quota:" ^ string_of_int index) in
  let request owner session = build_request validator_set signer owner !epoch session 1L in
  Lwt.finalize (fun () ->
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
    let* () = Lwt_list.iter_s (fun index ->
      cancel "expired caller slots released" true caller (entry index)) (List.init 31 Fun.id) in
    let pending = request caller (entry 31) in
    let* submitted = Rpc.submit service pending in
    if Result.is_error submitted then fail "last caller slot reserved";
    let* duplicate = Rpc.submit service pending in
    if duplicate <> submitted then fail "reserved job not idempotent";
    let* active = Service.active_jobs service in
    if active <> 1 then fail "reserved job not active";
    let* () = cancel "caller reservation consumed" false caller (entry 32) in
    let* () = cancel "reserved caller cancellation" true caller (entry 31) in
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
    cancel "global quota released after expiry" true caller (entry 1004))
    (fun () -> Service.shutdown service)

let test_offer_expiry () =
  Mirage_crypto_rng_unix.use_default ();
  let open Lwt.Syntax in
  let signer = identity () in
  let validator_set = Octra_consensus.C_types.make_validator_set
    [Octra_consensus.C_types.{address = signer.address; pubkey = signer.public_key}] in
  let head = ref (Some 100L) in
  let relayed = ref [] in
  let service = Service.create ~provider:None ~deps:Service.{
    chain_id = "octra-resource-compute-service-test";
    node_id = signer.address;
    sign = sign signer;
    validator_set = (fun () -> validator_set);
    head = (fun () -> Option.map
      (fun epoch_id -> {epoch_id; state_root = state_root epoch_id}) !head);
    state_root_at = (fun epoch -> Some (state_root epoch));
    self_test = (fun () -> Lwt.return (Error "unused"));
    nonce = (fun () -> raw_hash "nonce");
    broadcast = (fun _ -> Lwt.return_unit);
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
      send "offer admission" true (Protocol.Offer (offer 100L index)))
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
    let* () = Lwt_list.iter_s (fun index ->
      send "reused offer capacity" true (Protocol.Offer (offer 117L (index + 68))))
      (List.init 62 Fun.id) in
    let* () = send "reused offer limit" false (Protocol.Offer (offer 117L 130)) in
    head := Some 134L;
    send "second offer expiry" true (Protocol.Offer (offer 134L 131)))
    (fun () -> Service.shutdown service)

let test_request_expiry service caller signer call head relayed executions =
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
  let vote (call : Protocol.call) =
    let output_json = "{}" in
    let unsigned = Certificate.{
      request_id = Certificate.request_id call.request;
      node_id = signer.address;
      output_hash = Protocol.output_hash output_json;
      trace_root = raw_hash "expiry trace";
      output_bytes = String.length output_json;
      steps = 1L;
      operations = 1L;
      signature = String.make 64 '\000';
    } in
    Protocol.Result {vote = {unsigned with
      signature = sign signer (Certificate.vote_sign_bytes unsigned)}; output_json} in
  head := 112L;
  let* () = Lwt_list.iter_s (fun index ->
    send "request admission" true (Protocol.Call (call_at index 112L)))
    (List.init 125 Fun.id) in
  let live = call_at 125 113L in
  let* () = send "last request slot" true (Protocol.Call live) in
  let* () = send "live request limit" false (Protocol.Call (call_at 126 114L)) in
  head := 113L;
  let next = call_at 127 114L in
  let* () = send "expired requests release capacity" true (Protocol.Call next) in
  let* () = send "live request preserved" true (vote live) in
  let* () = send "expired result refused" false (vote (call_at 0 112L)) in
  let* () = send "duplicate call refused" false (Protocol.Call next) in
  let* () = send "duplicate result refused" false (vote live) in
  let* () = send "new request result" true (vote next) in
  if !executions <> before then fail "request expiry reset sequence guard";
  Lwt.return_unit

let test_remote_expiry nodes validator_set caller head session executions =
  let open Lwt.Syntax in
  let remote = List.nth nodes 1 in
  let epoch = Int64.add !head 65L in
  head := epoch;
  let before = !executions in
  let params = build_request validator_set remote.identity caller epoch session 2L in
  let* submitted = Rpc.submit remote.service params in
  let job_id = match submitted with
    | Error error -> fail error.Octra_core.Rpc.message
    | Ok json ->
      match field "job_id" json with
      | Some (`String value) -> value
      | _ -> fail "remote job id" in
  let progress = advance_head ~first:(Int64.succ epoch)
    ~last:(Int64.add epoch 12L) head in
  let* _ = await_finished remote.service caller.address job_id 1000 in
  let* () = progress in
  if !executions <> before + List.length nodes then
    fail "expired remote cancellation refused work";
  Lwt.return_unit

let run ?(inject_duplicate_offer = false) ?(cancel_on_offer = false) validator_count =
  Mirage_crypto_rng_unix.use_default ();
  let open Lwt.Syntax in
  let identities = List.init validator_count (fun _ -> identity ()) in
  let validators =
    List.map
      (fun (value : identity) ->
        Octra_consensus.C_types.{
          address = value.address;
          pubkey = value.public_key;
        })
      identities in
  let validator_set = Octra_consensus.C_types.make_validator_set validators in
  let head_epoch = ref 100L in
  let nodes = ref [] in
  let execution_count = ref 0 in
  let held_commitments = Hashtbl.create 8 in
  let observed_offers = Hashtbl.create 8 in
  let duplicate_injected = ref false in
  let relayed = ref [] in
  let calls = ref [] in
  let stop_request = ref None in
  let stopped, stop_reply = Lwt.wait () in
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
    | Protocol.Commitment commitment when commitment.Selection.node_id = sender ->
      Hashtbl.replace held_commitments (sender, commitment.offer_id) payload;
      Lwt.return_unit
    | Protocol.Reveal reveal when reveal.Selection.node_id = sender ->
      let* () = deliver_now sender excluded payload in
      begin
        let key = sender, reveal.offer_id in
        match Hashtbl.find_opt held_commitments key with
        | None -> fail "held commitment missing"
        | Some commitment ->
          Hashtbl.remove held_commitments key;
          deliver_now sender excluded commitment
      end
    | Protocol.Call call ->
      calls := call :: !calls;
      deliver_now sender excluded payload
    | _ -> deliver_now sender excluded payload in
  let make_node identity =
    let provider = Service.{ limits; engine = engine execution_count } in
    let service =
      Service.create
        ~deps:Service.{
          chain_id = "octra-resource-compute-service-test";
          node_id = identity.address;
          sign = sign identity;
          validator_set = (fun () -> validator_set);
          head = (fun () ->
            Some Service.{
              epoch_id = !head_epoch;
              state_root = state_root !head_epoch;
            });
          state_root_at = (fun epoch ->
            if epoch >= 0L && epoch <= !head_epoch then Some (state_root epoch)
            else None);
          self_test = (fun () ->
            Lwt.return
              (Ok Provider_rpc.{
                 executor_root;
                 evidence_root;
                 profile = "q24-v1";
               }));
          nonce = (fun () -> Mirage_crypto_rng.generate 32);
          broadcast = deliver identity.address "";
          relay = (fun ~source payload -> deliver identity.address source payload);
          sleep = Lwt_unix.sleep;
        }
        ~provider:(Some provider) in
    { identity; service } in
  nodes := List.map make_node identities;
  let coordinator = List.hd !nodes in
  let caller = identity () in
  let session_id = raw_hash "session" in
  let params =
    build_request
      validator_set
      coordinator.identity
      caller
      !head_epoch
      session_id
      1L in
  let open Lwt.Syntax in
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
  let head_progress = advance_head head_epoch in
  Lwt.async (fun () -> head_progress);
  let* response =
    await_finished coordinator.service caller.address job_id 1000 in
  begin
    match field "output_json" response with
    | Some (`String "{\"tokens\":[198]}") -> ()
    | _ -> fail "deterministic output"
  end;
  begin
    match field "certificate" response with
    | Some (`Assoc fields) ->
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
  if Hashtbl.length held_commitments <> 0 then fail "commitment release";
  let offers_after_first = Hashtbl.length observed_offers in
  if inject_duplicate_offer && offers_after_first <> 2 then
    fail "duplicate offer composition was not exercised";
  let executions_after_first = !execution_count in
  let second_params =
    build_request
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
  let* second_response =
    await_finished coordinator.service caller.address second_job_id 1000 in
  begin
    match field "output_json" second_response with
    | Some (`String "{\"tokens\":[198]}") -> ()
    | _ -> fail "reused deterministic output"
  end;
  if Hashtbl.length observed_offers <> offers_after_first then
    fail "committee lease created another offer";
  if
    !execution_count
    < executions_after_first + validator_count - ((validator_count - 1) / 3)
  then
    fail "committee lease independent executions";
  let saved_call = List.find (fun (call : Protocol.call) ->
    call.request.session_id = session_id && call.request.sequence = 2L) !calls in
  let* () = head_progress in
  let* () =
    if validator_count <> 1 then Lwt.return_unit
    else match !calls with
    | call :: [_] ->
      test_request_expiry coordinator.service caller coordinator.identity call
        head_epoch relayed execution_count
    | _ -> fail "initial request count" in
  let cancelled_session = raw_hash "cancelled-session" in
  let cancel_request =
    build_request
      validator_set
      coordinator.identity
      caller
      !head_epoch
      cancelled_session
      1L in
  if cancel_on_offer then
    stop_request := Some (cancellation_params caller cancelled_session);
  let* cancel_submit = Rpc.submit coordinator.service cancel_request in
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
      (let* () = Lwt_unix.sleep 3.0 in fail "cancel offer trigger");
    ]
    else Rpc.cancel coordinator.service (cancellation_params caller cancelled_session) in
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
  let* active_jobs = Service.active_jobs coordinator.service in
  if active_jobs <> 0 then fail "cancelled caller lease";
  let* () =
    if not cancel_on_offer then Lwt.return_unit
    else
      let* status = Rpc.status coordinator.service (`Assoc [
        "caller", `String caller.address;
        "job_id", `String cancel_id;
      ]) in
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
        let* result = Rpc.cancel coordinator.service (cancellation_params owner session) in
        match result with
        | Ok _ -> Lwt.return_unit
        | Error _ -> fail "cancellation admission") (List.init 255 Fun.id) in
      let* overflow = Rpc.cancel coordinator.service
        (cancellation_params (identity ()) (raw_hash "cancel-overflow")) in
      let before = List.length !calls in
      let* () = Service.on_message coordinator.service ~source:caller.address
        (signed_call cancelled_session) in
      if List.length !calls <> before then fail "cancelled call admitted";
      if Result.is_ok overflow then fail "live cancellation limit";
      let* repeated = Rpc.cancel coordinator.service
        (cancellation_params caller cancelled_session) in
      if Result.is_error repeated then fail "repeat cancellation at capacity";
      let executions = !execution_count in
      let* () = Service.on_message coordinator.service ~source:caller.address
        (signed_call (raw_hash "after-cancel")) in
      if List.length !calls <> before + 1 then fail "new session refused";
      let* () = Lwt.pause () in
      if !execution_count <> executions + 1 then fail "new session execution";
      let* () = advance_head ~first:115L ~last:128L head_epoch in
      if List.length !calls <> before + 1 then fail "call after cancellation";
      if !execution_count <> executions + 1 then fail "execution after cancellation";
      head_epoch := 178L;
      let* full = Rpc.cancel coordinator.service
        (cancellation_params caller (raw_hash "cancel-at-expiry")) in
      if Result.is_ok full then fail "live cancellation removed";
      head_epoch := 179L;
      let* available = Rpc.cancel coordinator.service
        (cancellation_params caller (raw_hash "cancel-after-expiry")) in
      if Result.is_error available then fail "expired cancellation capacity";
      Lwt.return_unit in
  let* () =
    if validator_count < 2 then Lwt.return_unit
    else test_remote_expiry !nodes validator_set caller head_epoch
      cancelled_session execution_count in
  let* () = Lwt_list.iter_s (fun node -> Service.shutdown node.service) !nodes in
  Lwt.return_unit

let () =
  Lwt_main.run (test_cancel_slots ());
  Lwt_main.run (test_offer_expiry ());
  Lwt_main.run (run ~inject_duplicate_offer:true 5);
  Lwt_main.run (run 1);
  Lwt_main.run (run ~cancel_on_offer:true 1);
  print_endline "status = pass test = resource_service"