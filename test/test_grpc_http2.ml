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
  submit_bytes = None;
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
    ?(rpc = call)
    ?submit
    ?accept
    ?(content_type = "application/grpc+proto")
    ?(close_request = true)
    ?(expect_trailers = true)
    ?(server_config = config)
    ?client_config
    ~path
    ~deadline
    ~te
    wire =
  let open Lwt.Syntax in
  let server_socket, client_socket =
    Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let serve =
    match accept with
    | Some serve -> serve
    | None -> Grpc.accept ?submit server_config ~call:rpc
  in
  let server = serve (Unix.ADDR_UNIX "grpc") server_socket in
  let error, fail_error = Lwt.wait () in
  let error_handler value =
    if Lwt.is_sleeping error then
      Lwt.wakeup_later fail_error (client_error value)
  in
  let* client = Client.create_connection ?config:client_config ~error_handler client_socket in
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
  if close_request then Body.Writer.flush writer (fun _ -> Body.Writer.close writer);
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

let test_deadline_cancel () =
  let open Lwt.Syntax in
  let work, _ = Lwt.task () in
  let started = ref false in
  let rpc _ _ = started := true; work in
  let* response =
    request ~rpc
      ~path:"/octra.node.v1.Node/Status"
      ~deadline:"50m"
      ~te:(Some "trailers")
      (Frame.encode "")
  in
  if not !started then fail "deadline skipped backend";
  if grpc_status response <> Some "4" then fail "deadline status differs";
  begin
    match Lwt.state work with
    | Lwt.Fail Lwt.Canceled -> Lwt.return_unit
    | _ -> fail "deadline left backend running"
  end

let test_disconnect ~stop () =
  let open Lwt.Syntax in
  let work, _ = Lwt.task () in
  let entered, enter = Lwt.wait () in
  let rpc _ _ = Lwt.wakeup_later enter (); work in
  let server_socket, client_socket =
    Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let server = Grpc.accept config ~call:rpc (Unix.ADDR_UNIX "grpc") server_socket in
  Lwt.finalize
    (fun () ->
      let* client = Client.create_connection ~error_handler:ignore client_socket in
      let headers = H2.Headers.of_list [
        "content-type", "application/grpc+proto";
        "te", "trailers";
      ] in
      let query = H2.Request.create ~headers ~scheme:"http" `POST
        "/octra.node.v1.Node/Status"
      in
      let writer = Client.request client query
        ~error_handler:ignore
        ~response_handler:(fun _ _ -> ())
      in
      Body.Writer.write_string writer (Frame.encode "");
      Body.Writer.close writer;
      let* () = entered in
      let* () =
        if stop then begin
          Lwt.cancel server;
          Lwt.catch
            (fun () -> server)
            (function Lwt.Canceled -> Lwt.return_unit | exn -> Lwt.fail exn)
        end else begin
          let* () = close client_socket in
          server
        end
      in
      if Lwt_unix.state server_socket <> Lwt_unix.Closed then
        fail "connection kept socket open";
      match Lwt.state work with
      | Lwt.Fail Lwt.Canceled -> Lwt.return_unit
      | _ -> fail "closed connection left backend running")
    (fun () ->
      Lwt.cancel work;
      Lwt.cancel server;
      let* () = close client_socket in
      close server_socket)

let epoch_reply client headers =
  let open Lwt.Syntax in
  let result, reply = Lwt.wait () in
  let status, set_status = Lwt.wait () in
  let response_handler _ body =
    Lwt.async (fun () -> let* data = read_body body in Lwt.wakeup reply data; Lwt.return_unit)
  in
  let query = H2.Request.create ~headers ~scheme:"http" `POST "/octra.node.v1.Node/Epoch" in
  let writer = Client.request client query
    ~error_handler:(fun _ -> if Lwt.is_sleeping result then Lwt.wakeup_exn reply (Failure "next stream failed"))
    ~trailers_handler:(fun fields -> Lwt.wakeup set_status (H2.Headers.get fields "grpc-status"))
    ~response_handler
  in
  Body.Writer.write_string writer (Frame.encode "\008\001");
  Body.Writer.close writer;
  let* data = result in
  let* code = status in
  if code <> Some "0" || data = "" then fail "next stream did not complete";
  Lwt.return_unit

let test_stream_reset ~body () =
  let open Lwt.Syntax in
  let work, _ = Lwt.task () in
  let entered, enter = Lwt.wait () in
  let canceled, cancel = Lwt.wait () in
  let started = ref false in
  Lwt.on_cancel work (fun () -> Lwt.wakeup cancel ());
  let rpc meta request =
    if request.Rpc.method_ = "node_status" then begin
      started := true;
      Lwt.wakeup_later enter ();
      work
    end else call meta request
  in
  let server_socket, client_socket =
    Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let server_config = Config.{ config with max_streams = 1 } in
  let server = Grpc.accept server_config ~call:rpc (Unix.ADDR_UNIX "grpc") server_socket in
  Lwt.finalize
    (fun () ->
      let* client = Client.create_connection ~error_handler:ignore client_socket in
      let headers = H2.Headers.of_list [
        "content-type", "application/grpc+proto"; "te", "trailers";
      ] in
      let query path = H2.Request.create ~headers ~scheme:"http" `POST path in
      let writer = Client.request client (query "/octra.node.v1.Node/Status")
        ~error_handler:ignore
        ~response_handler:(fun _ _ -> if body then Lwt.wakeup_later enter ())
      in
      Body.Writer.write_string writer (Frame.encode "");
      if not body then Body.Writer.close writer;
      let* () = entered in
      let reset = "\000\000\004\003\000\000\000\000\001\000\000\000\008" in
      let* size = Lwt_unix.write_string client_socket reset 0 (String.length reset) in
      if size <> String.length reset then fail "reset frame write differs";
      let* () =
        if body then Lwt.return_unit
        else
          let timeout =
            let* () = Lwt_unix.sleep 0.5 in
            Lwt.fail_with "stream reset left backend running"
          in
          Lwt.pick [canceled; timeout]
      in
      if not (Lwt.is_sleeping server) then fail "stream reset closed connection";
      let* () = epoch_reply client headers in
      if body && !started then fail "incomplete canceled body reached backend";
      Lwt.return_unit)
    (fun () ->
      Lwt.cancel work;
      Lwt.cancel server;
      let* () = close client_socket in
      close server_socket)

let test_parallel_reset () =
  let open Lwt.Syntax in
  let work, _ = Lwt.task () in
  let other, release = Lwt.task () in
  let entered, enter = Lwt.wait () in
  let ready, mark_ready = Lwt.wait () in
  let canceled, cancel = Lwt.wait () in
  Lwt.on_cancel work (fun () -> Lwt.wakeup cancel ());
  let rpc _ request =
    if request.Rpc.method_ = "node_status" then begin
      Lwt.wakeup_later enter ();
      work
    end else begin
      Lwt.wakeup_later mark_ready ();
      let* () = other in
      Lwt.return (Rpc.Result (`String "ok", request.id))
    end
  in
  let server_socket, client_socket =
    Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let server = Grpc.accept config ~call:rpc (Unix.ADDR_UNIX "grpc") server_socket in
  Lwt.finalize
    (fun () ->
      let* client = Client.create_connection ~error_handler:ignore client_socket in
      let headers = H2.Headers.of_list [
        "content-type", "application/grpc+proto"; "te", "trailers";
      ] in
      let query = H2.Request.create ~headers ~scheme:"http" `POST "/octra.node.v1.Node/Status" in
      let writer = Client.request client query
        ~error_handler:ignore ~response_handler:(fun _ _ -> ())
      in
      Body.Writer.write_string writer (Frame.encode "");
      Body.Writer.close writer;
      let* () = entered in
      let second = epoch_reply client headers in
      let* () = ready in
      let reset = "\000\000\004\003\000\000\000\000\001\000\000\000\008" in
      let* size = Lwt_unix.write_string client_socket reset 0 (String.length reset) in
      if size <> String.length reset then fail "reset frame write differs";
      let timeout = let* () = Lwt_unix.sleep 0.5 in Lwt.fail_with "parallel reset was not applied" in
      let* () = Lwt.pick [canceled; timeout] in
      if not (Lwt.is_sleeping other) then fail "reset affected another request";
      Lwt.wakeup release ();
      second)
    (fun () ->
      Lwt.cancel work;
      Lwt.cancel other;
      Lwt.cancel server;
      let* () = close client_socket in
      close server_socket)

let test_response_expire () =
  let open Lwt.Syntax in
  let rpc _ request =
    let value = if request.Rpc.method_ = "node_status" then String.make 130_000 'v' else "ok" in
    Lwt.return (Rpc.Result (`String value, request.id))
  in
  let server_socket, client_socket =
    Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let server_config = Config.{ config with
    max_response_bytes = 150_000; max_streams = 1;
    default_deadline_s = 0.05; max_deadline_s = 0.1 }
  in
  let server = Grpc.accept server_config ~call:rpc (Unix.ADDR_UNIX "grpc") server_socket in
  Lwt.finalize
    (fun () ->
      let config = H2.Config.{ default with initial_window_size = 1024l } in
      let* client = Client.create_connection ~config ~error_handler:ignore client_socket in
      let headers = H2.Headers.of_list [
        "content-type", "application/grpc+proto"; "te", "trailers";
      ] in
      let expired, expire = Lwt.wait () in
      let query = H2.Request.create ~headers ~scheme:"http" `POST "/octra.node.v1.Node/Status" in
      let writer = Client.request client query
        ~error_handler:(function
          | `Protocol_error (H2.Error_code.InternalError, _) ->
            if Lwt.is_sleeping expired then Lwt.wakeup expire ()
          | _ -> if Lwt.is_sleeping expired then Lwt.wakeup_exn expire (Failure "response error differs"))
        ~response_handler:(fun _ _ -> ())
      in
      Body.Writer.write_string writer (Frame.encode "");
      Body.Writer.close writer;
      let timeout =
        let* () = Lwt_unix.sleep 0.5 in
        Lwt.fail_with "response kept request capacity"
      in
      let* () = Lwt.pick [expired; timeout] in
      if not (Lwt.is_sleeping server) then fail "response expiry closed connection";
      epoch_reply client headers)
    (fun () ->
      Lwt.cancel server;
      let* () = close client_socket in
      close server_socket)

let test_preface_close () =
  let open Lwt.Syntax in
  let server_socket, client_socket =
    Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let calls = ref 0 in
  let rpc meta request = incr calls; call meta request in
  let server = Grpc.accept config ~call:rpc (Unix.ADDR_UNIX "grpc") server_socket in
  Lwt.finalize
    (fun () ->
      let* _ = Lwt_unix.write_string client_socket "PRI *" 0 5 in
      let* () = close client_socket in
      let* () = server in
      if !calls <> 0 then fail "partial preface reached backend";
      Lwt.return_unit)
    (fun () ->
      Lwt.cancel server;
      let* () = close client_socket in
      close server_socket)

let test_response_flow () =
  let open Lwt.Syntax in
  let value = `String (String.make 130_000 'v') in
  let rpc _ request = Lwt.return (Rpc.Result (value, request.Rpc.id)) in
  let server_config = Config.{ config with max_response_bytes = 150_000 } in
  let client_config = H2.Config.{ default with initial_window_size = 1024l } in
  let* response = request ~rpc ~server_config ~client_config
    ~path:"/octra.node.v1.Node/Status"
    ~deadline:"1S"
    ~te:(Some "trailers")
    (Frame.encode "")
  in
  if grpc_status response <> Some "0" then fail "flow status differs";
  match Frame.decode ~max_message:150_000 response.body with
  | Error _ -> fail "flow frame differs"
  | Ok payload ->
    let decoder = Pbrt.Decoder.of_bytes payload in
    match Pbrt.Decoder.key decoder with
    | Some (1, Pbrt.Bytes) ->
      let json = Pbrt.Decoder.bytes decoder |> Bytes.to_string in
      if Yojson.Safe.from_string json <> value then fail "flow payload differs";
      Lwt.return_unit
    | _ -> fail "flow protobuf differs"

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

let submit_path = Octra_node_runtime.Grpc_service.submit_path

let submit_wire json =
  Frame.encode (Octra_node_runtime.Grpc_proto.encode_json json)

let test_submit_size () =
  let open Lwt.Syntax in
  let calls = ref 0 in
  let submit _ request =
    incr calls;
    if request.Rpc.method_ <> "octra_submit" then fail "submission method differs";
    Lwt.return (Rpc.Result (`Assoc ["tx_hash", `String "accepted"], request.id))
  in
  let server_config = { config with submit_bytes = Some 5_000_005 } in
  let json = "{\"payload\":\"" ^ String.make 4_142_248 'a' ^ "\"}" in
  let* reply = request ~submit ~server_config ~path:submit_path
    ~deadline:"2S" ~te:(Some "trailers") (submit_wire json) in
  if grpc_status reply <> Some "0" || !calls <> 1 then
    fail (Printf.sprintf "large submission status = %s calls = %d trailers = %s"
      (Option.value ~default:"none" (grpc_status reply)) !calls
      (Option.fold ~none:"none" ~some:H2.Headers.to_string reply.trailers));
  let* disabled = request ~submit ~path:submit_path
    ~deadline:"1S" ~te:(Some "trailers") (submit_wire "{}") in
  if grpc_status disabled <> Some "12" || !calls <> 1 then fail "disabled submission reached backend";
  let* absent = request ~server_config ~path:submit_path
    ~deadline:"1S" ~te:(Some "trailers") (submit_wire "{}") in
  if grpc_status absent <> Some "12" then fail "missing submission capability admitted";
  let server_config = { config with submit_bytes = Some 8 } in
  let* large = request ~submit ~server_config ~path:submit_path
    ~deadline:"1S" ~te:(Some "trailers") (submit_wire "{\"nonce\":7}") in
  if grpc_status large <> Some "8" || !calls <> 1 then fail "submission cap differs";
  let* invalid = request ~submit ~server_config ~path:submit_path
    ~deadline:"1S" ~te:(Some "trailers") (submit_wire "[]") in
  if grpc_status invalid <> Some "3" || !calls <> 1 then fail "array submission admitted";
  Lwt.return_unit

let test_submit_slots () =
  let open Lwt.Syntax in
  let entered, enter = Lwt.wait () in
  let held, release = Lwt.task () in
  let calls = ref 0 in
  let submit _ request =
    incr calls;
    let* () = if !calls = 1 then begin Lwt.wakeup_later enter (); held end
      else Lwt.return_unit in
    Lwt.return (Rpc.Result (`Null, request.Rpc.id))
  in
  let server_config = { config with submit_bytes = Some 1024 } in
  let accept = Grpc.accept ~submit server_config ~call in
  let send () = request ~accept ~path:submit_path ~deadline:"1S"
    ~te:(Some "trailers") (submit_wire "{}") in
  let first = send () in
  let* () = entered in
  let* busy = send () in
  if grpc_status busy <> Some "8" || !calls <> 1 then fail "submission slot not shared";
  let* read = request ~accept ~path:"/octra.node.v1.Node/Status"
    ~deadline:"1S" ~te:(Some "trailers") (Frame.encode "") in
  if grpc_status read <> Some "0" then fail "submission blocked reads";
  Lwt.wakeup_later release ();
  let* first = first in
  if grpc_status first <> Some "0" then fail "first submission failed";
  let* last = send () in
  if grpc_status last <> Some "0" || !calls <> 2 then fail "submission slot not released";
  Lwt.return_unit

let test_submit_expiry () =
  let open Lwt.Syntax in
  let calls = ref 0 in
  let submit _ request = incr calls; Lwt.return (Rpc.Result (`Null, request.Rpc.id)) in
  let server_config = { config with submit_bytes = Some 1024 } in
  let accept = Grpc.accept ~submit server_config ~call in
  let first = request ~accept ~close_request:false ~path:submit_path
    ~deadline:"50m" ~te:(Some "trailers") "" in
  let* () = Lwt_unix.sleep 0.01 in
  let* busy = request ~accept ~path:submit_path ~deadline:"1S"
    ~te:(Some "trailers") (submit_wire "{}") in
  if grpc_status busy <> Some "8" || !calls <> 0 then fail "body read lost submission slot";
  let* expired = first in
  if grpc_status expired <> Some "4" || !calls <> 0 then fail "expired body was submitted";
  let* next = request ~accept ~path:submit_path ~deadline:"1S"
    ~te:(Some "trailers") (submit_wire "{}") in
  if grpc_status next <> Some "0" || !calls <> 1 then fail "expired submission kept slot";
  Lwt.return_unit

let test_submit_result () =
  let open Lwt.Syntax in
  let accepted = ref 0 in
  let work, _ = Lwt.task () in
  let submit _ request =
    incr accepted;
    if !accepted = 1 then work else Lwt.return (Rpc.Result (`Null, request.Rpc.id))
  in
  let server_config = { config with submit_bytes = Some 1024 } in
  let accept = Grpc.accept ~submit server_config ~call in
  let* reply = request ~accept ~path:submit_path ~deadline:"10m"
    ~te:(Some "trailers") (submit_wire "{}") in
  if grpc_status reply <> Some "4" || !accepted <> 1 then fail "deadline result differs";
  if Lwt.state work <> Lwt.Fail Lwt.Canceled then fail "expired response work not canceled";
  let* next = request ~accept ~path:submit_path ~deadline:"1S"
    ~te:(Some "trailers") (submit_wire "{}") in
  if grpc_status next <> Some "0" || !accepted <> 2 then fail "result slot not released";
  Lwt.return_unit

let test_submit_depth () =
  let open Lwt.Syntax in
  let calls = ref 0 in
  let submit _ request = incr calls; Lwt.return (Rpc.Result (`Null, request.Rpc.id)) in
  let server_config = { config with submit_bytes = Some 1024 } in
  let accept = Grpc.accept ~submit server_config ~call in
  let send depth =
    let json = "{\"payload\":" ^ String.make depth '[' ^ "0"
      ^ String.make depth ']' ^ "}"
    in
    request ~accept ~path:submit_path ~deadline:"1S" ~te:(Some "trailers")
      (submit_wire json)
  in
  let* rejected = send (Rpc.max_param_depth - 1) in
  if grpc_status rejected <> Some "3" || !calls <> 0 || rejected.body <> "" then
    fail "deep submission reached backend";
  let code = Option.bind rejected.trailers (fun headers -> H2.Headers.get headers "octra-rpc-code") in
  if code <> Some "-32602" then fail "depth RPC code differs";
  let detail = Option.bind rejected.trailers (fun headers -> H2.Headers.get headers "octra-rpc-error-bin") in
  let error = Rpc.invalid_params "params too deep or too large" in
  let expected = Octra_node_runtime.Grpc_status.(trailers (of_rpc error)) in
  if detail <> List.assoc_opt "octra-rpc-error-bin" expected then
    fail "depth RPC detail differs";
  let* accepted = send (Rpc.max_param_depth - 2) in
  if grpc_status accepted <> Some "0" || !calls <> 1 then
    fail "depth refusal kept submission slot";
  Lwt.return_unit

let listen () =
  let open Lwt.Syntax in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)) in
  Lwt_unix.listen socket 8;
  match Lwt_unix.getsockname socket with
  | Unix.ADDR_INET (_, port) -> Lwt.return (socket, port)
  | _ -> fail "listener address differs"

let test_accept_delay () =
  if Grpc.accept_codes = [] then fail "native accept codes missing";
  let retry = [
    Unix.EINTR; Unix.EAGAIN; Unix.EWOULDBLOCK; Unix.ECONNABORTED;
    Unix.ENETDOWN; Unix.ENETUNREACH; Unix.EHOSTDOWN; Unix.EHOSTUNREACH;
    Unix.ENOPROTOOPT; Unix.EOPNOTSUPP; Unix.EMFILE;
    Unix.ENFILE; Unix.ENOBUFS; Unix.ENOMEM;
  ] @ List.map (fun code -> Unix.EUNKNOWNERR code) Grpc.accept_codes in
  List.iter (fun reason ->
    let error = Unix.Unix_error (reason, "accept", "") in
    if Grpc.accept_delay 0 error <> Some 0.01
      || Grpc.accept_delay 7 error <> Some 1.0
      || Grpc.accept_delay max_int error <> Some 1.0
      || Grpc.accept_delay min_int error <> Some 0.01 then
      fail "accept delay differs") retry;
  List.iter (fun error ->
    if Grpc.accept_delay 0 error <> None then fail "fatal accept retried")
    [Lwt.Canceled; Failure "accept";
     Unix.Unix_error (Unix.EUNKNOWNERR (-1), "accept", "");
     Unix.Unix_error (Unix.EUNKNOWNERR max_int, "accept", "");
     Unix.Unix_error (Unix.EBADF, "accept", "");
     Unix.Unix_error (Unix.EINVAL, "accept", "");
     Unix.Unix_error (Unix.ENOTSOCK, "accept", "")]

let test_accept_resume () =
  let open Lwt.Syntax in
  let* socket, port = listen () in
  let client = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let errors = ref 20 in
  let delays = ref [] in
  let take socket =
    if !errors = 0 then Lwt_unix.accept socket
    else begin
      decr errors;
      let reason = if !errors mod 2 = 0 then Unix.EMFILE
        else Unix.EUNKNOWNERR (List.hd Grpc.accept_codes) in
      Lwt.fail (Unix.Unix_error (reason, "accept", ""))
    end
  in
  let sleep delay = delays := delay :: !delays; Lwt.pause () in
  Lwt.finalize
    (fun () ->
      let run () =
        let* peer, _ = Grpc.take_client ~take ~sleep socket in
        let* () = close peer in
        let expected = List.init 20 (fun i ->
          min 1.0 (0.01 *. Float.of_int (1 lsl min i 7))) in
        if List.rev !delays <> expected then fail "accept retry sequence differs";
        Lwt.return_unit
      in
      let accepted = run () in
      let* () = Lwt_unix.connect client (Unix.ADDR_INET (Unix.inet_addr_loopback, port)) in
      let* () = accepted in
      errors := 20;
      delays := [];
      let second = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Lwt.finalize
        (fun () ->
          let accepted = run () in
          let* () = Lwt_unix.connect second (Unix.ADDR_INET (Unix.inet_addr_loopback, port)) in
          accepted)
        (fun () -> close second))
    (fun () -> let* () = close client in close socket)

let test_accept_cancel () =
  let open Lwt.Syntax in
  let* socket, _ = listen () in
  let paused, _ = Lwt.task () in
  let calls = ref 0 in
  let take _ =
    incr calls;
    Lwt.fail (Unix.Unix_error (Unix.ENOBUFS, "accept", ""))
  in
  let sleep _ = paused in
  Lwt.finalize
    (fun () ->
      let task = Grpc.take_client ~take ~sleep socket in
      Lwt.cancel task;
      if Lwt.state paused <> Lwt.Fail Lwt.Canceled || !calls <> 1 then
        fail "accept retry ignored cancellation";
      let* () = close socket in
      Lwt.catch
        (fun () ->
          let* _ = Grpc.take_client ~sleep:(fun _ -> fail "closed accept retried") socket in
          fail "closed listener admitted")
        (function
          | Unix.Unix_error (Unix.EBADF, _, _) -> Lwt.return_unit
          | error -> Lwt.fail error))
    (fun () -> close socket)

let test_listen_busy () =
  let open Lwt.Syntax in
  let* occupied, port = listen () in
  let* socket, http_port = listen () in
  let stop, finish = Lwt.task () in
  let callback _ _ _ =
    Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"alive" ()
  in
  let http = Cohttp_lwt_unix.Server.create ~stop ~mode:(`TCP (`Socket socket))
    (Cohttp_lwt_unix.Server.make ~callback ())
  in
  let config = { config with port } in
  let server = Grpc.serve config ~call ~http in
  Lwt.finalize
    (fun () ->
      let* () = Lwt.catch
        (fun () -> let* () = Grpc.start config ~call in fail "occupied port admitted")
        (function
          | Unix.Unix_error (Unix.EADDRINUSE, _, _) -> Lwt.return_unit
          | exn -> Lwt.fail exn)
      in
      let probe () =
        let uri = Uri.of_string (Printf.sprintf "http://127.0.0.1:%d/" http_port) in
        let* response, body = Cohttp_lwt_unix.Client.get uri in
        let* body = Cohttp_lwt.Body.to_string body in
        if Cohttp.Response.status response <> `OK || body <> "alive" then
          fail "gRPC bind failure affected HTTP response";
        if not (Lwt.is_sleeping http && Lwt.is_sleeping server) then
          fail "gRPC bind failure ended HTTP";
        Lwt.return_unit
      in
      let* () = probe () in
      let* () = probe () in
      Lwt.wakeup finish ();
      server)
    (fun () ->
      Lwt.cancel server;
      let* () = close socket in
      close occupied)

let test_listen_stop mode =
  let open Lwt.Syntax in
  let* socket, port = listen () in
  let* () = close socket in
  let http, finish = Lwt.task () in
  let work, _ = Lwt.task () in
  let entered, enter = Lwt.wait () in
  let stopped, stop = Lwt.wait () in
  Lwt.on_cancel work (fun () -> Lwt.wakeup stop ());
  let rpc meta request =
    if request.Rpc.method_ <> "node_status" then call meta request
    else begin Lwt.wakeup enter (); work end
  in
  let listener = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let server = Grpc.serve ~socket:listener { config with port } ~call:rpc ~http in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let address = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  Lwt.finalize
    (fun () ->
      let rec connect attempts =
        Lwt.catch
          (fun () -> Lwt_unix.connect socket address)
          (function
            | Unix.Unix_error (Unix.ECONNREFUSED, _, _) when attempts > 0 ->
              let* () = Lwt_unix.sleep 0.01 in
              connect (attempts - 1)
            | exn -> Lwt.fail exn)
      in
      let* () = connect 20 in
      let* client = Client.create_connection ~error_handler:ignore socket in
      let headers = H2.Headers.of_list [
        "content-type", "application/grpc+proto"; "te", "trailers";
      ] in
      let* () = epoch_reply client headers in
      let query = H2.Request.create ~headers ~scheme:"http" `POST
        "/octra.node.v1.Node/Status" in
      let writer = Client.request client query ~error_handler:ignore
        ~response_handler:(fun _ _ -> fail "stopped request replied") in
      Body.Writer.write_string writer (Frame.encode "");
      Body.Writer.close writer;
      let* () = entered in
      let* () = match mode with
        | `Close -> Lwt.wakeup finish (); Lwt.return_unit
        | `Fail -> Lwt.wakeup_exn finish (Failure "http stopped"); Lwt.return_unit
        | `Cancel -> Lwt.cancel server; Lwt.return_unit
        | `Accept ->
          let* () = close listener in
          let* () = stopped in
          if not (Lwt.is_sleeping http && Lwt.is_sleeping server) then
            fail "accept failure ended HTTP";
          Lwt.wakeup finish ();
          Lwt.return_unit
      in
      let* () = Lwt.catch
        (fun () ->
          let* () = server in
          if mode <> `Close && mode <> `Accept then fail "listener failure was hidden";
          Lwt.return_unit)
        (function
          | Failure message when mode = `Fail && message = "http stopped" -> Lwt.return_unit
          | Lwt.Canceled when mode = `Cancel -> Lwt.return_unit
          | exn -> Lwt.fail exn)
      in
      if Lwt.is_sleeping http then fail "listener cancellation left HTTP running";
      if Lwt.state work <> Lwt.Fail Lwt.Canceled then
        fail "listener stopped with active request";
      if Lwt_unix.state listener <> Lwt_unix.Closed then
        fail "listener socket remained open";
      let* () = Client.shutdown client in
      let* () = close socket in
      let probe = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Lwt.finalize
        (fun () ->
          Lwt_unix.setsockopt probe Unix.SO_REUSEADDR true;
          Lwt_unix.bind probe address)
        (fun () -> close probe))
    (fun () ->
      Lwt.cancel work;
      Lwt.cancel server;
      let* () = close socket in
      close listener)

let main () =
  let open Lwt.Syntax in
  test_accept_delay ();
  let* () = test_accept_resume () in
  let* () = test_accept_cancel () in
  let* () = test_success () in
  let* () = test_deadline () in
  let* () = test_deadline_cancel () in
  let* () = test_disconnect ~stop:false () in
  let* () = test_disconnect ~stop:true () in
  let* () = test_stream_reset ~body:false () in
  let* () = test_stream_reset ~body:true () in
  let* () = test_parallel_reset () in
  let* () = test_response_expire () in
  let* () = test_preface_close () in
  let* () = test_response_flow () in
  let* () = test_deadline_header () in
  let* () = test_deadline_body () in
  let* () = test_request_limit () in
  let* () = test_response_limit () in
  let* () = test_te () in
  let* () = test_submit_size () in
  let* () = test_submit_slots () in
  let* () = test_submit_expiry () in
  let* () = test_submit_result () in
  let* () = test_submit_depth () in
  let* () = test_listen_busy () in
  let* () = test_listen_stop `Close in
  let* () = test_listen_stop `Fail in
  let* () = test_listen_stop `Cancel in
  let* () = test_listen_stop `Accept in
  test_content_type ()

let () =
  let limit =
    let open Lwt.Syntax in
    let* () = Lwt_unix.sleep 5.0 in
    Lwt.fail_with "test_grpc_http2: time limit exceeded"
  in
  let run = match Sys.argv with
    | [| _ |] -> main ()
    | [| _; "flow" |] -> test_response_flow ()
    | [| _; "expire" |] -> test_response_expire ()
    | _ -> Lwt.fail_with "test_grpc_http2: unknown case"
  in
  Lwt_main.run (Lwt.pick [run; limit]);
  print_endline "test_grpc_http2: ok"