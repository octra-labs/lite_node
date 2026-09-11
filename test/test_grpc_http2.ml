(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Body = H2.Body
module Client = H2_lwt_unix.Client
module Config = Octra_node_runtime.Grpc_config
module Frame = Octra_node_runtime.Grpc_frame
module Grpc = Octra_node_runtime.Grpc_http2
module Rpc = Octra_core.Rpc

type response = {
  status : H2.Status.t;
  body : string;
  trailers : H2.Headers.t option;
}

let fail reason =
  failwith ("test_grpc_http2: " ^ reason)

let config = Config.{
  host = "127.0.0.1";
  port = 8081;
  max_request_bytes = 64;
  max_response_bytes = 1024;
  max_streams = 4;
  default_deadline_s = 1.0;
  max_deadline_s = 2.0;
}

let call _meta request =
  let open Lwt.Syntax in
  let* () =
    if request.Rpc.method_ = "node_status" then Lwt_unix.sleep 0.01
    else Lwt.return_unit
  in
  Lwt.return
    (Rpc.Result
       (`Assoc [
          "method", `String request.method_;
          "params", request.params;
        ],
        request.id))

let read_body body =
  let result, resolve = Lwt.wait () in
  let buffer = Buffer.create 128 in
  let rec next () =
    Body.Reader.schedule_read
      body
      ~on_eof:(fun () -> Lwt.wakeup_later resolve (Buffer.contents buffer))
      ~on_read:(fun data ~off ~len ->
        Buffer.add_string buffer (Bigstringaf.substring data ~off ~len);
        next ())
  in
  next ();
  result

let client_error = function
  | `Invalid_response_body_length _ -> "response length"
  | `Exn exn -> Printexc.to_string exn
  | `Malformed_response reason -> "malformed response: " ^ reason
  | `Protocol_error (code, reason) ->
    Printf.sprintf "protocol error: %s %s" (H2.Error_code.to_string code) reason

let close socket =
  Lwt.catch
    (fun () -> Lwt_unix.close socket)
    (fun _ -> Lwt.return_unit)

let request
    ?(content_type = "application/grpc+proto")
    ?(close_request = true)
    ?(expect_trailers = true)
    ?(server_config = config)
    ~path
    ~deadline
    ~te
    wire =
  let open Lwt.Syntax in
  let server_socket, client_socket =
    Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let server =
    Grpc.accept server_config ~call (Unix.ADDR_UNIX "grpc") server_socket
  in
  let error, fail_error = Lwt.wait () in
  let error_handler value =
    if Lwt.is_sleeping error then
      Lwt.wakeup_later fail_error (client_error value)
  in
  let* client = Client.create_connection ~error_handler client_socket in
  let body_result, set_body = Lwt.wait () in
  let trailers_result, set_trailers = Lwt.wait () in
  let response_handler response body =
    Lwt.async (fun () ->
      let* value = read_body body in
      Lwt.wakeup_later set_body (response.H2.Response.status, value);
      Lwt.return_unit)
  in
  let trailers_handler trailers =
    Lwt.wakeup_later set_trailers trailers
  in
  let headers = [
    "grpc-timeout", deadline;
    "content-type", content_type;
  ] in
  let headers =
    match te with
    | None -> headers
    | Some value -> headers @ ["te", value]
  in
  let headers = H2.Headers.of_list headers in
  let h2_request = H2.Request.create ~headers ~scheme:"http" `POST path in
  let writer =
    Client.request
      client
      h2_request
      ~trailers_handler
      ~error_handler
      ~response_handler
  in
  Body.Writer.write_string writer wire;
  if close_request then Body.Writer.close writer;
  let completed =
    let* status, body = body_result in
    if expect_trailers then begin
      let* trailers = trailers_result in
      Lwt.return { status; body; trailers = Some trailers }
    end else
      Lwt.return { status; body; trailers = None }
  in
  let failed =
    let* reason = error in
    Lwt.fail_with reason
  in
  Lwt.finalize
    (fun () -> Lwt.pick [completed; failed])
    (fun () ->
      let* () = Client.shutdown client in
      Lwt.cancel server;
      let* () = close client_socket in
      close server_socket)

let grpc_status response =
  Option.bind response.trailers (fun trailers ->
    H2.Headers.get trailers "grpc-status")

let test_success () =
  let open Lwt.Syntax in
  let* response =
    request
      ~path:"/octra.node.v1.Node/Status"
      ~deadline:"1S"
      ~te:(Some "trailers")
      (Frame.encode "")
  in
  if response.status <> `OK then fail "HTTP status differs";
  if grpc_status response <> Some "0" then fail "gRPC status differs";
  match Frame.decode ~max_message:1024 response.body with
  | Error _ -> fail "response frame is invalid"
  | Ok payload ->
    let decoder = Pbrt.Decoder.of_bytes payload in
    begin
      match Pbrt.Decoder.key decoder with
      | Some (1, Pbrt.Bytes) ->
        let json = Pbrt.Decoder.bytes decoder |> Bytes.to_string in
        let expected =
          `Assoc ["method", `String "node_status"; "params", `List []]
        in
        if Yojson.Safe.from_string json <> expected then
          fail "response payload differs";
        Lwt.return_unit
      | _ -> fail "response protobuf differs"
    end

let test_deadline () =
  let open Lwt.Syntax in
  let* response =
    request
      ~path:"/octra.node.v1.Node/Status"
      ~deadline:"1m"
      ~te:(Some "trailers")
      (Frame.encode "")
  in
  if grpc_status response <> Some "4" then fail "deadline status differs";
  if response.body <> "" then fail "deadline response has a body";
  Lwt.return_unit

let test_deadline_header () =
  let open Lwt.Syntax in
  let* response =
    request
      ~path:"/octra.node.v1.Node/Status"
      ~deadline:"invalid"
      ~te:(Some "trailers")
      (Frame.encode "")
  in
  if grpc_status response <> Some "3" then fail "deadline header status differs";
  if response.body <> "" then fail "deadline header response has a body";
  Lwt.return_unit

let test_deadline_body () =
  let open Lwt.Syntax in
  let* response =
    request
      ~close_request:false
      ~path:"/octra.node.v1.Node/Status"
      ~deadline:"1m"
      ~te:(Some "trailers")
      (Frame.encode "")
  in
  if grpc_status response <> Some "4" then fail "body deadline status differs";
  if response.body <> "" then fail "body deadline response has a body";
  Lwt.return_unit

let test_request_limit () =
  let open Lwt.Syntax in
  let* response =
    request
      ~path:"/octra.node.v1.Node/Status"
      ~deadline:"1S"
      ~te:(Some "trailers")
      (String.make 70 '\000')
  in
  if grpc_status response <> Some "8" then fail "request limit status differs";
  Lwt.return_unit

let test_response_limit () =
  let open Lwt.Syntax in
  let server_config = Config.{ config with max_response_bytes = 1 } in
  let* response =
    request
      ~server_config
      ~path:"/octra.node.v1.Node/Status"
      ~deadline:"1S"
      ~te:(Some "trailers")
      (Frame.encode "")
  in
  if grpc_status response <> Some "8" then fail "response limit status differs";
  if response.body <> "" then fail "response limit has a body";
  Lwt.return_unit

let test_te () =
  let open Lwt.Syntax in
  let* response =
    request
      ~path:"/octra.node.v1.Node/Status"
      ~deadline:"1S"
      ~te:None
      (Frame.encode "")
  in
  if grpc_status response <> Some "3" then fail "te status differs";
  Lwt.return_unit

let test_content_type () =
  let open Lwt.Syntax in
  let* response =
    request
      ~content_type:"application/grpc+json"
      ~expect_trailers:false
      ~path:"/octra.node.v1.Node/Status"
      ~deadline:"1S"
      ~te:(Some "trailers")
      (Frame.encode "")
  in
  if response.status <> `Unsupported_media_type then
    fail "content type status differs";
  Lwt.return_unit

let main () =
  let open Lwt.Syntax in
  let* () = test_success () in
  let* () = test_deadline () in
  let* () = test_deadline_header () in
  let* () = test_deadline_body () in
  let* () = test_request_limit () in
  let* () = test_response_limit () in
  let* () = test_te () in
  test_content_type ()

let () =
  let limit =
    let open Lwt.Syntax in
    let* () = Lwt_unix.sleep 5.0 in
    Lwt.fail_with "test_grpc_http2: time limit exceeded"
  in
  Lwt_main.run (Lwt.pick [main (); limit]);
  print_endline "test_grpc_http2: ok"