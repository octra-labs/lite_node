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

let advance_head head =
  let open Lwt.Syntax in
  let rec loop epoch =
    if epoch > 112L then Lwt.return_unit
    else
      let* () = Lwt_unix.sleep 0.1 in
      head := epoch;
      loop (Int64.succ epoch) in
  loop 101L

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

let run ?(inject_duplicate_offer = false) validator_count =
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
  let deliver_now sender excluded payload =
    !nodes
    |> List.filter (fun node ->
      node.identity.address <> sender
      && node.identity.address <> excluded)
    |> Lwt_list.iter_s (fun node ->
      Service.on_message node.service ~source:sender payload) in
  let deliver sender excluded payload =
    match Protocol.decode payload with
    | Protocol.Offer offer ->
      Hashtbl.replace observed_offers (Protocol.offer_id offer) ();
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
  Lwt.async (fun () -> advance_head head_epoch);
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
  let cancelled_session = raw_hash "cancelled-session" in
  let cancel_request =
    build_request
      validator_set
      coordinator.identity
      caller
      !head_epoch
      cancelled_session
      1L in
  let* cancel_submit = Rpc.submit coordinator.service cancel_request in
  begin
    match cancel_submit with
    | Error error -> fail error.Octra_core.Rpc.message
    | Ok _ -> ()
  end;
  let* cancel_result =
    Rpc.cancel coordinator.service (cancellation_params caller cancelled_session) in
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
  let* repeated_submit = Rpc.submit coordinator.service cancel_request in
  begin
    match repeated_submit with
    | Error _ -> ()
    | Ok _ -> fail "cancelled session accepted"
  end;
  let* () = Lwt_list.iter_s (fun node -> Service.shutdown node.service) !nodes in
  Lwt.return_unit

let () =
  Lwt_main.run (run ~inject_duplicate_offer:true 5);
  Lwt_main.run (run 1);
  print_endline "test_resource_compute_service: ok"