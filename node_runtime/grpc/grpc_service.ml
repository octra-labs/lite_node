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
let epochs_path = "/octra.node.v1.Node/Epochs"
let submit_path = "/octra.node.v1.Node/Submit"

let paths = [
  health_path;
  status_path;
  account_path;
  transaction_path;
  epoch_path;
  epochs_path;
  submit_path;
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
  | path when path = epochs_path ->
    map (Grpc_proto.decode_page payload)
      (fun request -> Rpc ("octra_epochPage", Epoch_page.request_json request))
  | path when path = submit_path ->
    map (Grpc_proto.decode_submit payload)
      (fun transaction -> Rpc ("octra_submit", `List [transaction]))
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

let invoke ?submit ~call ~meta ~path payload =
  let selected = if path = submit_path then submit else Some call in
  match selected with
  | None ->
    Lwt.return (reply (Grpc_status.make Grpc_status.Unimplemented "submission is disabled"))
  | Some call ->
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
    let* response =
      match Rpc.parse_single (`Assoc [
        "jsonrpc", `String "2.0";
        "method", `String method_;
        "params", params;
        "id", `Null;
      ]) with
      | Ok request -> call meta request
      | Error error -> Lwt.return (Rpc.Error_ (error, `Null))
    in
    begin
      match response with
      | Rpc.Result (result, _) ->
        let body =
          if path = epochs_path then
            Result.map Grpc_proto.encode_page (Epoch_page.of_json result)
          else Ok (Grpc_proto.encode_json (Yojson.Safe.to_string result))
        in
        Lwt.return (match body with
          | Ok body -> reply ~body ~rpc_method:method_ Grpc_status.ok
          | Error message -> reply ~rpc_method:method_
              (Grpc_status.make Grpc_status.Internal message))
      | Rpc.Error_ (error, _) ->
        Lwt.return
          (reply
             ~rpc_method:method_
             (Grpc_status.of_rpc error))
    end