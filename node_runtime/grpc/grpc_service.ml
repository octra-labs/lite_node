(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Rpc = Octra_core.Rpc

type call =
  Rpc_http.meta ->
  Rpc.request ->
  Rpc.response Lwt.t

type reply = {
  body : string option;
  status : Grpc_status.t;
  rpc_method : string option;
}

type decoded =
  | Health of string
  | Rpc of string * Yojson.Safe.t

let health_path = "/grpc.health.v1.Health/Check"
let status_path = "/octra.node.v1.Node/Status"
let account_path = "/octra.node.v1.Node/Account"
let transaction_path = "/octra.node.v1.Node/Transaction"
let epoch_path = "/octra.node.v1.Node/Epoch"

let paths = [
  health_path;
  status_path;
  account_path;
  transaction_path;
  epoch_path;
]

let map result make =
  Result.map make result

let decode path payload =
  match path with
  | path when path = health_path ->
    map (Grpc_proto.decode_health_service payload) (fun service -> Health service)
  | path when path = status_path ->
    map (Grpc_proto.decode_empty payload) (fun () -> Rpc ("node_status", `List []))
  | path when path = account_path ->
    map
      (Grpc_proto.decode_address payload)
      (fun address -> Rpc ("octra_account", `List [`String address]))
  | path when path = transaction_path ->
    map
      (Grpc_proto.decode_hash payload)
      (fun hash -> Rpc ("octra_transaction", `List [`String hash]))
  | path when path = epoch_path ->
    map
      (Grpc_proto.decode_epoch payload)
      (fun epoch -> Rpc ("epoch_get", `List [`Int epoch]))
  | _ -> Error "gRPC method is not implemented"

let reply ?body ?rpc_method status =
  { body; status; rpc_method }

let health service =
  if service = ""
     || service = "grpc.health.v1.Health"
     || service = "octra.node.v1.Node"
  then
    reply ~body:(Grpc_proto.encode_serving ()) Grpc_status.ok
  else
    reply (Grpc_status.make Grpc_status.Not_found "service is not registered")

let rpc_request method_ params =
  Rpc.{ jsonrpc = "2.0"; method_; params; id = `Null }

let invoke ~call ~meta ~path payload =
  match decode path payload with
  | Error message ->
    let code =
      if List.mem path paths then Grpc_status.Invalid_argument
      else Grpc_status.Unimplemented
    in
    Lwt.return (reply (Grpc_status.make code message))
  | Ok (Health service) ->
    Lwt.return (health service)
  | Ok (Rpc (method_, params)) ->
    let open Lwt.Syntax in
    let* response = call meta (rpc_request method_ params) in
    begin
      match response with
      | Rpc.Result (result, _) ->
        let json = Yojson.Safe.to_string result in
        Lwt.return
          (reply
             ~body:(Grpc_proto.encode_json json)
             ~rpc_method:method_
             Grpc_status.ok)
      | Rpc.Error_ (error, _) ->
        Lwt.return
          (reply
             ~rpc_method:method_
             (Grpc_status.of_rpc error))
    end