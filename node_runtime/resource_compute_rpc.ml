(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Certificate = Octra_consensus.Resource_compute_certificate
module Rpc = Octra_core.Rpc
module Selection = Octra_consensus.Resource_compute_selection
module Service = Resource_compute_service

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error _ as error -> error

let invalid message =
  Error (Rpc.invalid_params message)

let exact_fields expected fields =
  let names = List.map fst fields |> List.sort String.compare in
  let expected = List.sort String.compare expected in
  names = expected

let object_fields expected = function
  | `Assoc fields when exact_fields expected fields -> Ok fields
  | `List [`Assoc fields] when exact_fields expected fields -> Ok fields
  | _ -> invalid "resource compute parameters differ"

let field fields name =
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> invalid ("resource compute parameter missing: " ^ name)

let string_field fields name =
  let* value = field fields name in
  match value with
  | `String value -> Ok value
  | _ -> invalid ("resource compute string required: " ^ name)

let int_field fields name =
  let* value = field fields name in
  match value with
  | `Int value -> Ok value
  | _ -> invalid ("resource compute integer required: " ^ name)

let int64_field fields name =
  let* value = field fields name in
  match value with
  | `String value ->
    begin
      match Int64.of_string_opt value with
      | Some value -> Ok value
      | None -> invalid ("resource compute int64 required: " ^ name)
    end
  | `Int value -> Ok (Int64.of_int value)
  | _ -> invalid ("resource compute int64 required: " ^ name)

let raw_hash_field fields name =
  let* value = string_field fields name in
  if Text.is_hex_len 64 value then Ok (Text.hex_to_string value)
  else invalid ("resource compute hash refused: " ^ name)

let address_field fields name =
  let* value = string_field fields name in
  if Octra_core.Crypto.Address.is_valid_address value then Ok value
  else invalid ("resource compute address refused: " ^ name)

let base64_field fields name bytes =
  let* value = string_field fields name in
  match Base64.decode value with
  | Ok raw when String.length raw = bytes -> Ok raw
  | _ -> invalid ("resource compute base64 refused: " ^ name)

let submit_fields = [
  "caller";
  "caller_public_key";
  "caller_signature";
  "circle_id";
  "executor_root";
  "graph_root";
  "max_output_bytes";
  "method_name";
  "model_epoch";
  "model_root";
  "model_state_root";
  "params_json";
  "program_root";
  "sequence";
  "session_id";
]

let request_of_params params =
  let* fields = object_fields submit_fields params in
  let* model_epoch = int64_field fields "model_epoch" in
  let* model_state_root = raw_hash_field fields "model_state_root" in
  let* circle_id = address_field fields "circle_id" in
  let* graph_root = raw_hash_field fields "graph_root" in
  let* model_root = raw_hash_field fields "model_root" in
  let* program_root = raw_hash_field fields "program_root" in
  let* executor_root = raw_hash_field fields "executor_root" in
  let* caller = address_field fields "caller" in
  let* caller_public_key = base64_field fields "caller_public_key" 32 in
  let* caller_signature = base64_field fields "caller_signature" 64 in
  let* session_id = raw_hash_field fields "session_id" in
  let* sequence = int64_field fields "sequence" in
  let* method_name = string_field fields "method_name" in
  let* params_json = string_field fields "params_json" in
  let* max_output_bytes = int_field fields "max_output_bytes" in
  Ok Service.{
    model_epoch;
    model_state_root;
    circle_id;
    graph_root;
    model_root;
    program_root;
    executor_root;
    caller;
    caller_public_key;
    caller_signature;
    session_id;
    sequence;
    method_name;
    params_json;
    max_output_bytes;
  }

let hex value =
  `String (Text.raw_to_hex value)

let int64 value =
  `String (Int64.to_string value)

let signer_json (signer : Certificate.signer) =
  `Assoc [
    "node_id", `String signer.node_id;
    "vote_id", hex signer.vote_id;
    "weight", `String (Z.to_string signer.weight);
  ]

let vote_json (vote : Certificate.vote) =
  `Assoc [
    "node_id", `String vote.node_id;
    "operations", int64 vote.operations;
    "output_bytes", `Int vote.output_bytes;
    "output_hash", hex vote.output_hash;
    "request_id", hex vote.request_id;
    "signature", `String (Base64.encode_exn vote.signature);
    "steps", int64 vote.steps;
    "trace_root", hex vote.trace_root;
  ]

let certificate_json (certificate : Certificate.certificate) =
  `Assoc [
    "operations", int64 certificate.operations;
    "output_bytes", `Int certificate.output_bytes;
    "output_hash", hex certificate.output_hash;
    "request_id", hex certificate.request_id;
    "root", hex certificate.root;
    "signed_weight", `String (Z.to_string certificate.signed_weight);
    "signer_count", `Int (List.length certificate.signers);
    "signers", `List (List.map signer_json certificate.signers);
    "steps", int64 certificate.steps;
    "trace_root", hex certificate.trace_root;
    "votes", `List (List.map vote_json certificate.votes);
  ]

let request_json (request : Certificate.request) =
  `Assoc [
    "caller", `String request.caller;
    "chain_id", `String request.chain_id;
    "circle_id", `String request.circle_id;
    "epoch_id", int64 request.epoch_id;
    "executor_root", hex request.executor_root;
    "expires_epoch", int64 request.expires_epoch;
    "graph_root", hex request.graph_root;
    "input_bytes", `Int request.input_bytes;
    "input_hash", hex request.input_hash;
    "max_output_bytes", `Int request.max_output_bytes;
    "model_epoch", int64 request.model_epoch;
    "model_root", hex request.model_root;
    "model_state_root", hex request.model_state_root;
    "program_root", hex request.program_root;
    "selection_root", hex request.selection_root;
    "sequence", int64 request.sequence;
    "session_id", hex request.session_id;
    "state_root", hex request.state_root;
    "validator_set_root", hex request.validator_set_root;
  ]

let member_json (member : Selection.member) =
  `Assoc [
    "evidence_root", hex member.evidence_root;
    "executor_root", hex member.executor_root;
    "graph_root", hex member.graph_root;
    "model_root", hex member.model_root;
    "node_id", `String member.node_id;
    "program_root", hex member.program_root;
    "score", hex member.score;
  ]

let selection_json (selection : Selection.selection) =
  `Assoc [
    "challenge", hex selection.challenge;
    "members", `List (List.map member_json selection.members);
    "offer_id", hex selection.offer_id;
    "root", hex selection.root;
    "target_epoch", int64 selection.target_epoch;
  ]

let output_value output_json =
  match Yojson.Safe.from_string output_json with
  | value -> value
  | exception _ -> `String output_json

let response_json (response : Service.response) =
  `Assoc [
    "certificate", certificate_json response.certificate;
    "output", output_value response.output_json;
    "output_json", `String response.output_json;
    "request", request_json response.request;
    "selection", selection_json response.selection;
    "status", `String "finished";
  ]

let submit service params =
  match request_of_params params with
  | Error error -> Lwt.return (Error error)
  | Ok request ->
    let open Lwt.Syntax in
    let* result = Service.submit service request in
    begin
      match result with
      | Ok job_id ->
        Lwt.return
          (Ok
             (`Assoc [
                "job_id", `String job_id;
                "status", `String "waiting";
              ]))
      | Error message ->
        Lwt.return
          (Error
             (Rpc.err
                (-32040)
                "resource compute request refused"
                (Some (`String message))))
    end

let status service params =
  match object_fields ["caller"; "job_id"] params with
  | Error error -> Lwt.return (Error error)
  | Ok fields ->
    begin
      match address_field fields "caller", string_field fields "job_id" with
      | Error error, _ | _, Error error -> Lwt.return (Error error)
      | Ok caller, Ok job_id ->
        let open Lwt.Syntax in
        let* job_status = Service.status service ~caller ~job_id in
        begin
          match job_status with
          | None -> Lwt.return (Error (Rpc.not_found "resource compute job"))
          | Some Service.Waiting ->
            Lwt.return
              (Ok
                 (`Assoc [
                    "job_id", `String job_id;
                    "status", `String "waiting";
                  ]))
          | Some (Service.Refused message) ->
            Lwt.return
              (Ok
                 (`Assoc [
                    "job_id", `String job_id;
                    "reason", `String message;
                    "status", `String "refused";
                  ]))
          | Some (Service.Finished response) ->
            Lwt.return (Ok (response_json response))
        end
    end

let cancel service params =
  match
    object_fields
      ["caller"; "caller_public_key"; "caller_signature"; "session_id"]
      params
  with
  | Error error -> Lwt.return (Error error)
  | Ok fields ->
    begin
      match
        address_field fields "caller",
        base64_field fields "caller_public_key" 32,
        base64_field fields "caller_signature" 64,
        raw_hash_field fields "session_id"
      with
      | Error error, _, _, _
      | _, Error error, _, _
      | _, _, Error error, _
      | _, _, _, Error error -> Lwt.return (Error error)
      | Ok caller, Ok caller_public_key, Ok caller_signature, Ok session_id ->
        let open Lwt.Syntax in
        let* result =
          Service.cancel
            service
            Service.{
              caller;
              caller_public_key;
              caller_signature;
              session_id;
            } in
        begin
          match result with
          | Error message ->
            Lwt.return
              (Error
                 (Rpc.err
                    (-32041)
                    "resource compute cancellation refused"
                    (Some (`String message))))
          | Ok cancelled ->
            Lwt.return
              (Ok
                 (`Assoc [
                    "cancelled_jobs", `Int cancelled;
                    "status", `String "cancelled";
                  ]))
        end
    end