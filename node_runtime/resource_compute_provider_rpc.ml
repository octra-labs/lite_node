(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type prepare = {
  model_epoch : int64;
  model_state_root : string;
  circle_id : string;
  graph_root : string;
  model_root : string;
  program_root : string;
  executor_root : string;
}

type execute = {
  request_id : string;
  session_id : string;
  epoch_id : int64;
  state_root : string;
  model_epoch : int64;
  model_state_root : string;
  circle_id : string;
  graph_root : string;
  model_root : string;
  program_root : string;
  executor_root : string;
  caller : string;
  method_name : string;
  params_json : string;
  program_b64 : string;
  program_runtime : string;
  storage : (string * string) list;
  max_output_bytes : int;
}

type self_test = {
  executor_root : string;
  evidence_root : string;
  profile : string;
}

type prepared = {
  cache_key : string;
  graph_root : string;
  model_root : string;
  program_root : string;
  executor_root : string;
  entries : int;
  bytes : int64;
}

type executed = {
  output_json : string;
  output_hash : string;
  trace_root : string;
  steps : int64;
  operations : int64;
  effort_used : int;
}

let max_http_body_bytes = 4 * 1024 * 1024
let max_program_bytes = 1024 * 1024
let max_params_bytes = 1024 * 1024
let max_storage_entries = 2048
let max_storage_bytes = 2 * 1024 * 1024
let max_storage_key_bytes = 256
let max_storage_value_bytes = 1024 * 1024
let max_output_bytes = 1024 * 1024

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error _ as error -> error

let exact_fields names fields =
  let actual = List.map fst fields |> List.sort String.compare in
  let expected = List.sort String.compare names in
  List.length actual = List.length expected && actual = expected

let assoc names = function
  | `Assoc fields when exact_fields names fields -> Ok fields
  | _ -> Error "resource compute provider fields differ"

let field fields name =
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> Error ("resource compute provider field missing: " ^ name)

let string_field fields name =
  let* value = field fields name in
  match value with
  | `String value -> Ok value
  | _ -> Error ("resource compute provider string required: " ^ name)

let int_field fields name =
  let* value = field fields name in
  match value with
  | `Int value -> Ok value
  | _ -> Error ("resource compute provider integer required: " ^ name)

let int64_field fields name =
  let* value = field fields name in
  match value with
  | `String value ->
    begin
      match Int64.of_string_opt value with
      | Some value -> Ok value
      | None -> Error ("resource compute provider int64 required: " ^ name)
    end
  | _ -> Error ("resource compute provider int64 required: " ^ name)

let lower_hex_64 value =
  String.length value = 64
  && String.for_all
       (function
        | '0' .. '9' | 'a' .. 'f' -> true
        | _ -> false)
       value

let bounded_ascii maximum value =
  let length = String.length value in
  length > 0
  && length <= maximum
  && String.for_all (fun char -> Char.code char < 128) value

let address value =
  Octra_core.Crypto.Address.is_valid_address value

let require_hash name value =
  if lower_hex_64 value then Ok value
  else Error ("resource compute provider hash refused: " ^ name)

let require_address name value =
  if address value then Ok value
  else Error ("resource compute provider address refused: " ^ name)

let json_int64 value =
  `String (Int64.to_string value)

let prepare_to_json (value : prepare) =
  `Assoc [
    "circle_id", `String value.circle_id;
    "executor_root", `String value.executor_root;
    "graph_root", `String value.graph_root;
    "model_epoch", json_int64 value.model_epoch;
    "model_root", `String value.model_root;
    "model_state_root", `String value.model_state_root;
    "program_root", `String value.program_root;
  ]

let prepare_of_json json =
  let* fields =
    assoc
      [
        "circle_id";
        "executor_root";
        "graph_root";
        "model_epoch";
        "model_root";
        "model_state_root";
        "program_root";
      ]
      json in
  let* model_epoch = int64_field fields "model_epoch" in
  let* model_state_root = string_field fields "model_state_root" in
  let* circle_id = string_field fields "circle_id" in
  let* graph_root = string_field fields "graph_root" in
  let* model_root = string_field fields "model_root" in
  let* program_root = string_field fields "program_root" in
  let* executor_root = string_field fields "executor_root" in
  let* model_state_root = require_hash "model_state_root" model_state_root in
  let* circle_id = require_address "circle_id" circle_id in
  let* graph_root = require_hash "graph_root" graph_root in
  let* model_root = require_hash "model_root" model_root in
  let* program_root = require_hash "program_root" program_root in
  let* executor_root = require_hash "executor_root" executor_root in
  if model_epoch < 0L then Error "resource compute provider model epoch refused"
  else
    Ok {
      model_epoch;
      model_state_root;
      circle_id;
      graph_root;
      model_root;
      program_root;
      executor_root;
    }

let storage_to_json storage =
  `List
    (List.map
       (fun (key, value) ->
         `Assoc [
           "key", `String key;
           "value_b64", `String (Base64.encode_exn value);
         ])
       storage)

let storage_entry_of_json = function
  | `Assoc fields when exact_fields ["key"; "value_b64"] fields ->
    let* key = string_field fields "key" in
    let* value_b64 = string_field fields "value_b64" in
    if not (bounded_ascii max_storage_key_bytes key) then
      Error "resource compute provider storage key refused"
    else
      begin
        match Base64.decode value_b64 with
        | Error _ -> Error "resource compute provider storage base64 refused"
        | Ok value when String.length value <= max_storage_value_bytes -> Ok (key, value)
        | Ok _ -> Error "resource compute provider storage value exceeds limit"
      end
  | _ -> Error "resource compute provider storage entry differs"

let storage_of_json fields =
  let* value = field fields "storage" in
  match value with
  | `List values when List.length values <= max_storage_entries ->
    let rec loop bytes keys entries = function
      | [] -> Ok (List.rev entries)
      | json :: rest ->
        let* key, value = storage_entry_of_json json in
        let next_bytes = bytes + String.length key + String.length value in
        if next_bytes > max_storage_bytes then
          Error "resource compute provider storage exceeds limit"
        else if List.mem key keys then
          Error "resource compute provider storage key duplicated"
        else
          loop next_bytes (key :: keys) ((key, value) :: entries) rest in
    loop 0 [] [] values
  | `List _ -> Error "resource compute provider storage entries exceed limit"
  | _ -> Error "resource compute provider storage list required"

let execute_to_json (value : execute) =
  `Assoc [
    "caller", `String value.caller;
    "circle_id", `String value.circle_id;
    "epoch_id", json_int64 value.epoch_id;
    "executor_root", `String value.executor_root;
    "graph_root", `String value.graph_root;
    "max_output_bytes", `Int value.max_output_bytes;
    "method_name", `String value.method_name;
    "model_epoch", json_int64 value.model_epoch;
    "model_root", `String value.model_root;
    "model_state_root", `String value.model_state_root;
    "params_json", `String value.params_json;
    "program_b64", `String value.program_b64;
    "program_root", `String value.program_root;
    "program_runtime", `String value.program_runtime;
    "request_id", `String value.request_id;
    "session_id", `String value.session_id;
    "state_root", `String value.state_root;
    "storage", storage_to_json value.storage;
  ]

let params_valid raw =
  String.length raw <= max_params_bytes
  && match Yojson.Safe.from_string raw with
     | `List _ -> true
     | _ -> false
     | exception _ -> false

let program_valid raw =
  match Base64.decode raw with
  | Ok value -> String.length value > 0 && String.length value <= max_program_bytes
  | Error _ -> false

let execute_of_json json =
  let names = [
    "caller";
    "circle_id";
    "epoch_id";
    "executor_root";
    "graph_root";
    "max_output_bytes";
    "method_name";
    "model_epoch";
    "model_root";
    "model_state_root";
    "params_json";
    "program_b64";
    "program_root";
    "program_runtime";
    "request_id";
    "session_id";
    "state_root";
    "storage";
  ] in
  let* fields = assoc names json in
  let* request_id = string_field fields "request_id" in
  let* session_id = string_field fields "session_id" in
  let* epoch_id = int64_field fields "epoch_id" in
  let* state_root = string_field fields "state_root" in
  let* model_epoch = int64_field fields "model_epoch" in
  let* model_state_root = string_field fields "model_state_root" in
  let* circle_id = string_field fields "circle_id" in
  let* graph_root = string_field fields "graph_root" in
  let* model_root = string_field fields "model_root" in
  let* program_root = string_field fields "program_root" in
  let* executor_root = string_field fields "executor_root" in
  let* caller = string_field fields "caller" in
  let* method_name = string_field fields "method_name" in
  let* params_json = string_field fields "params_json" in
  let* program_b64 = string_field fields "program_b64" in
  let* program_runtime = string_field fields "program_runtime" in
  let* storage = storage_of_json fields in
  let* max_output_bytes_value = int_field fields "max_output_bytes" in
  let* request_id = require_hash "request_id" request_id in
  let* session_id = require_hash "session_id" session_id in
  let* state_root = require_hash "state_root" state_root in
  let* model_state_root = require_hash "model_state_root" model_state_root in
  let* circle_id = require_address "circle_id" circle_id in
  let* graph_root = require_hash "graph_root" graph_root in
  let* model_root = require_hash "model_root" model_root in
  let* program_root = require_hash "program_root" program_root in
  let* executor_root = require_hash "executor_root" executor_root in
  let* caller = require_address "caller" caller in
  if epoch_id < 0L || model_epoch < 0L then
    Error "resource compute provider epoch refused"
  else if not (bounded_ascii 64 method_name) then
    Error "resource compute provider method refused"
  else if not (params_valid params_json) then
    Error "resource compute provider params refused"
  else if not (program_valid program_b64) then
    Error "resource compute provider program refused"
  else if program_runtime <> "wasm_v1" then
    Error "resource compute provider runtime refused"
  else if max_output_bytes_value <= 0 || max_output_bytes_value > max_output_bytes then
    Error "resource compute provider output limit refused"
  else
    Ok {
      request_id;
      session_id;
      epoch_id;
      state_root;
      model_epoch;
      model_state_root;
      circle_id;
      graph_root;
      model_root;
      program_root;
      executor_root;
      caller;
      method_name;
      params_json;
      program_b64;
      program_runtime;
      storage;
      max_output_bytes = max_output_bytes_value;
    }

let self_test_to_json (value : self_test) =
  `Assoc [
    "evidence_root", `String value.evidence_root;
    "executor_root", `String value.executor_root;
    "profile", `String value.profile;
  ]

let self_test_of_json json =
  let* fields = assoc ["evidence_root"; "executor_root"; "profile"] json in
  let* evidence_root = string_field fields "evidence_root" in
  let* executor_root = string_field fields "executor_root" in
  let* profile = string_field fields "profile" in
  let* evidence_root = require_hash "evidence_root" evidence_root in
  let* executor_root = require_hash "executor_root" executor_root in
  if not (bounded_ascii 32 profile) then Error "resource compute provider profile refused"
  else Ok { executor_root; evidence_root; profile }

let prepared_to_json (value : prepared) =
  `Assoc [
    "bytes", json_int64 value.bytes;
    "cache_key", `String value.cache_key;
    "entries", `Int value.entries;
    "executor_root", `String value.executor_root;
    "graph_root", `String value.graph_root;
    "model_root", `String value.model_root;
    "program_root", `String value.program_root;
  ]

let prepared_of_json json =
  let* fields =
    assoc
      [
        "bytes";
        "cache_key";
        "entries";
        "executor_root";
        "graph_root";
        "model_root";
        "program_root";
      ]
      json in
  let* bytes = int64_field fields "bytes" in
  let* cache_key = string_field fields "cache_key" in
  let* entries = int_field fields "entries" in
  let* executor_root = string_field fields "executor_root" in
  let* graph_root = string_field fields "graph_root" in
  let* model_root = string_field fields "model_root" in
  let* program_root = string_field fields "program_root" in
  let* cache_key = require_hash "cache_key" cache_key in
  let* executor_root = require_hash "executor_root" executor_root in
  let* graph_root = require_hash "graph_root" graph_root in
  let* model_root = require_hash "model_root" model_root in
  let* program_root = require_hash "program_root" program_root in
  if entries <= 0 || bytes <= 0L then Error "resource compute provider cache dimensions refused"
  else
    Ok {
      cache_key;
      graph_root;
      model_root;
      program_root;
      executor_root;
      entries;
      bytes;
    }

let executed_to_json (value : executed) =
  `Assoc [
    "effort_used", `Int value.effort_used;
    "operations", json_int64 value.operations;
    "output_hash", `String value.output_hash;
    "output_json", `String value.output_json;
    "steps", json_int64 value.steps;
    "trace_root", `String value.trace_root;
  ]

let executed_of_json json =
  let* fields =
    assoc
      [
        "effort_used";
        "operations";
        "output_hash";
        "output_json";
        "steps";
        "trace_root";
      ]
      json in
  let* effort_used = int_field fields "effort_used" in
  let* operations = int64_field fields "operations" in
  let* output_hash = string_field fields "output_hash" in
  let* output_json = string_field fields "output_json" in
  let* steps = int64_field fields "steps" in
  let* trace_root = string_field fields "trace_root" in
  let* output_hash = require_hash "output_hash" output_hash in
  let* trace_root = require_hash "trace_root" trace_root in
  if effort_used < 0 || steps < 0L || operations < 0L then
    Error "resource compute provider counters refused"
  else if String.length output_json > max_output_bytes then
    Error "resource compute provider output exceeds limit"
  else
    begin
      match Yojson.Safe.from_string output_json with
      | exception _ -> Error "resource compute provider output JSON refused"
      | _ ->
        Ok {
          output_json;
          output_hash;
          trace_root;
          steps;
          operations;
          effort_used;
        }
    end