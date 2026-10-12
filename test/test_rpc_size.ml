(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Rpc = Octra_core.Rpc
module Limit = Octra_node_runtime.Rpc_response_limit

let require ok reason = if not ok then failwith reason

let refused response id =
  let expected = Rpc.response_json
    (Rpc.Error_ (Rpc.err (-32013) "RPC response exceeds byte limit" None, id)) in
  require (response = expected) "RPC size refusal differs"

let check_edges apply limit make =
  let base = make (`String "") in
  let overhead = Limit.response_bytes (Rpc.response_json base) in
  List.iter (fun delta ->
    let response = make (`String (String.make (limit - overhead + delta) 'x')) in
    if delta <= 0 then
      require (apply response = Rpc.response_json response) "RPC response changed"
    else refused (apply response) (`Int 7)) [-1; 0; 1]

let check_ids () =
  let large = `String (String.make Limit.max_single_bytes 'x') in
  List.iter (fun raw ->
    let id = `String raw in
    let actual = Limit.single (Rpc.Result (large, id)) in
    let bytes = String.length (Yojson.Safe.to_string id) in
    refused actual (if bytes <= 256 then id else `Null))
    [String.make 42 '\000'; String.make 42 '\000' ^ "xy";
      String.make 43 '\000'; String.make 254 'x'; String.make 255 'x'];
  let id = `String (String.make 256 'x') in
  require (Limit.single (Rpc.Result (`Null, id)) = Rpc.response_json (Rpc.Result (`Null, id)))
    "successful RPC id shortened"

let check_memory () =
  let large = `String (String.make (32 * 1_024 * 1_024) '\000') in
  let response = Rpc.Result (large, large) in
  let before = Gc.allocated_bytes () in
  let actual = Limit.single response in
  let allocated = Gc.allocated_bytes () -. before in
  refused actual `Null;
  require (allocated < 1_048_576.) "RPC size check allocated encoded JSON"

let check_errors () =
  let text = `String (String.make 200_000 'x') in
  let failure action =
    try ignore (action ()); None with Yojson.Json_error reason -> Some reason in
  List.iter (fun value -> List.iter (fun json ->
    List.iter (fun response ->
      let expected = failure (fun () -> Yojson.Safe.to_string (Rpc.response_json response)) in
      require (Option.is_some expected) "invalid RPC float accepted";
      require (failure (fun () -> Limit.batch_item ~count:100 response) = expected)
        "RPC size check changed codec error")
      [Rpc.Result (`Null, json); Rpc.Result (json, `Int 7);
        Rpc.Error_ (Rpc.err (-32000) "failure" (Some json), `Int 7)])
    [`List [text; value]; `List [value; text]])
    [`Float nan; `Float infinity; `Float neg_infinity]

let () =
  let make value = Rpc.Result (value, `Int 7) in
  let error value = Rpc.Error_ (Rpc.err (-32000) "failure" (Some value), `Int 7) in
  List.iter (fun make ->
    check_edges Limit.single Limit.max_single_bytes make;
    List.iter (fun count ->
      check_edges (Limit.batch_item ~count)
        (min Limit.max_single_bytes (Limit.max_batch_bytes / max 1 count)) make)
      [-1; 0; 1; 2; 3; 7]) [make; error];
  refused (Limit.batch_item ~count:max_int (make `Null)) (`Int 7);
  check_ids ();
  check_memory ();
  check_errors ();
  Printf.printf "status = pass test = rpc_size\n"