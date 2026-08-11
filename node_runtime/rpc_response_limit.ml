(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Rpc = Octra_core.Rpc

let max_single_bytes = 8 * 1_024 * 1_024
let max_batch_bytes = 16 * 1_024 * 1_024
let max_id_bytes = 256

let response_bytes value =
  String.length (Yojson.Safe.to_string value)

let response_id = function
  | Rpc.Result (_, id)
  | Rpc.Error_ (_, id) -> id

let bounded_id id =
  if response_bytes id <= max_id_bytes then id else `Null

let refused response =
  Rpc.response_json
    (Rpc.Error_
       (Rpc.err (-32013) "RPC response exceeds byte limit" None,
        bounded_id (response_id response)))

let within limit response =
  let value = Rpc.response_json response in
  if response_bytes value <= limit then value else refused response

let single response =
  within max_single_bytes response

let batch_item ~count response =
  let count = max 1 count in
  let item_limit = min max_single_bytes (max_batch_bytes / count) in
  within item_limit response