(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type rpc_error = {
  code : int;
  message : string;
  data : Yojson.Safe.t option;
}

type request = {
  jsonrpc : string;
  method_ : string;
  params : Yojson.Safe.t;
  id : Yojson.Safe.t;
}

type response =
  | Result of Yojson.Safe.t * Yojson.Safe.t
  | Error_ of rpc_error * Yojson.Safe.t

let parse_error = { code = -32700; message = "parse error"; data = None }
let invalid_request = { code = -32600; message = "invalid request"; data = None }
let method_not_found m = { code = -32601; message = Printf.sprintf "method not found: %s" m; data = None }
let invalid_params msg = { code = -32602; message = "invalid params"; data = Some (`String msg) }

let err code message data = { code; message; data }

let sender_not_found = err 100 "sender not found" None
let invalid_signature = err 101 "invalid signature" None
let invalid_nonce = err 102 "invalid nonce" None
let nonce_too_far = err 103 "nonce too far ahead" None
let insufficient_balance = err 104 "insufficient balance" None
let malformed_tx msg = err 105 "malformed transaction" (Some (`String msg))
let duplicate_tx = err 106 "duplicate transaction" None
let staging_full = err 107 "staging full" None
let self_transfer = err 108 "self transfer" None
let invalid_address msg = err 109 "invalid address" (Some (`String msg))
let service_unavailable = err 110 "service unavailable" None
let supply_violation = err 111 "supply violation" None
let not_found msg = err 112 "not found" (Some (`String msg))
let fee_too_low msg = err 113 "fee too low" (Some (`String msg))
let tx_too_large msg = err 114 "transaction too large" (Some (`String msg))
let self_only_op msg = err 115 "invalid operation target" (Some (`String msg))

let response_json = function
  | Result (result, id) ->
      `Assoc [ "jsonrpc", `String "2.0"; "result", result; "id", id ]
  | Error_ (e, id) ->
      let err_obj = [ "code", `Int e.code; "message", `String e.message ] @
        (match e.data with Some d -> ["data", d] | None -> []) in
      `Assoc [ "jsonrpc", `String "2.0"; "error", `Assoc err_obj; "id", id ]

let max_body_size = 100_000_000
let max_method_len = 64
let max_param_depth = 8
let max_string_param_len = 100_000_000
let max_batch_size = 100

let valid_method_char = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true
  | _ -> false

let validate_method m =
  String.length m > 0
  && String.length m <= max_method_len
  && String.for_all valid_method_char m

let rec check_depth depth json =
  if depth > max_param_depth then false
  else match json with
    | `Assoc pairs -> List.for_all (fun (_, v) -> check_depth (depth + 1) v) pairs
    | `List items -> List.for_all (check_depth (depth + 1)) items
    | `String s -> String.length s <= max_string_param_len
    | _ -> true

let validate_params params =
  check_depth 0 params

let parse_single (json : Yojson.Safe.t) : (request, rpc_error) result =
  match json with
  | `Assoc fields ->
      let get k = List.assoc_opt k fields in
      let jsonrpc = match get "jsonrpc" with Some (`String v) -> v | _ -> "" in
      let method_ = match get "method" with Some (`String v) -> v | _ -> "" in
      let params = match get "params" with Some p -> p | None -> `List [] in
      let id = match get "id" with Some v -> v | None -> `Null in
      if jsonrpc <> "2.0" then Error invalid_request
      else if not (validate_method method_) then Error (invalid_params "invalid method name")
      else if not (validate_params params) then Error (invalid_params "params too deep or too large")
      else Ok { jsonrpc; method_; params; id }
  | _ -> Error invalid_request

let parse_body body =
  if String.length body > max_body_size then
    Error parse_error
  else
    match (try Some (Yojson.Safe.from_string body) with _ -> None) with
    | None -> Error parse_error
    | Some (`List batch) ->
        if List.length batch > max_batch_size then
          Error (invalid_params (Printf.sprintf "batch size %d exceeds limit %d" (List.length batch) max_batch_size))
        else
          Ok (`Batch (List.map (fun j -> parse_single j) batch))
    | Some json ->
        (match parse_single json with
         | Ok req -> Ok (`Single req)
         | Error e -> Error e)

let param_string params idx =
  match params with
  | `List l -> (match List.nth_opt l idx with Some (`String s) -> Some s | _ -> None)
  | `Assoc _ -> None
  | _ -> None

let param_int params idx =
  match params with
  | `List l -> (match List.nth_opt l idx with Some (`Int i) -> Some i | _ -> None)
  | _ -> None

let param_json params idx =
  match params with
  | `List l -> List.nth_opt l idx
  | _ -> None

let require_string params idx name =
  match param_string params idx with
  | Some s -> Ok s
  | None -> Error (invalid_params (Printf.sprintf "missing required param: %s (index %d)" name idx))

let require_int params idx name =
  match param_int params idx with
  | Some i -> Ok i
  | None -> Error (invalid_params (Printf.sprintf "missing required param: %s (index %d)" name idx))

let sanitize_address addr =
  if String.length addr <> 47 then None
  else if not (String.starts_with ~prefix:"oct" addr) then None
  else if not (String.for_all (function
    | '1'..'9' | 'A'..'H' | 'J'..'N' | 'P'..'Z' | 'a'..'k' | 'm'..'z' -> true
    | _ -> false) (String.sub addr 3 44)) then None
  else Some addr

let sanitize_hash h =
  if String.length h <> 64 then None
  else if not (String.for_all (function '0'..'9' | 'a'..'f' -> true | _ -> false) h) then None
  else Some h

let require_address params idx name =
  match require_string params idx name with
  | Error e -> Error e
  | Ok raw ->
      match sanitize_address raw with
      | Some a -> Ok a
      | None -> Error (invalid_address (Printf.sprintf "invalid address format for %s" name))

let require_hash params idx name =
  match require_string params idx name with
  | Error e -> Error e
  | Ok raw ->
      match sanitize_hash raw with
      | Some h -> Ok h
      | None -> Error (invalid_params (Printf.sprintf "invalid hash format for %s" name))