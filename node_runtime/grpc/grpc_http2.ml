(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Body = H2.Body
module Headers = H2.Headers
module Reqd = H2.Reqd
module Request = H2.Request
module Response = H2.Response

let peer = function
  | Unix.ADDR_INET (address, port) ->
    Printf.sprintf "%s:%d" (Unix.string_of_inet_addr address) port
  | Unix.ADDR_UNIX _ -> "local"

let content_type headers =
  match Headers.get headers "content-type" with
  | None -> false
  | Some raw ->
    let media =
      String.split_on_char ';' raw
      |> List.hd
      |> String.trim
      |> String.lowercase_ascii
    in
    media = "application/grpc"
    || media = "application/grpc+proto"

let trailers_header headers =
  match Headers.get headers "te" with
  | None -> false
  | Some raw ->
    String.split_on_char ',' raw
    |> List.exists (fun value ->
      String.lowercase_ascii (String.trim value) = "trailers")

let identity_encoding headers =
  match Headers.get headers "grpc-encoding" with
  | None -> true
  | Some raw -> String.lowercase_ascii (String.trim raw) = "identity"

let plain reqd status =
  Body.Reader.close (Reqd.request_body reqd);
  Reqd.respond_with_string reqd (Response.create status) ""

let body_length_ok request limit =
  match Request.body_length request with
  | `Fixed length -> Int64.compare length (Int64.of_int limit) <= 0
  | `Unknown -> true
  | `Error _ -> false

let read_body ~limit body =
  let promise, resolve = Lwt.task () in
  let open_read = ref true in
  let buffer = Buffer.create (min 4096 limit) in
  let finish value =
    if !open_read then begin
      open_read := false;
      Lwt.wakeup_later resolve value
    end
  in
  let rec next () =
    Body.Reader.schedule_read
      body
      ~on_eof:(fun () -> finish (Ok (Buffer.contents buffer)))
      ~on_read:(fun data ~off ~len ->
        if len > limit - Buffer.length buffer then begin
          Body.Reader.close body;
          finish (Error Grpc_status.Resource_exhausted)
        end else begin
          Buffer.add_string buffer (Bigstringaf.substring data ~off ~len);
          next ()
        end)
  in
  Lwt.on_cancel promise (fun () ->
    open_read := false;
    Body.Reader.close body);
  next ();
  promise

let frame_status = function
  | Grpc_frame.Compressed ->
    Grpc_status.make Grpc_status.Unimplemented "message compression is not supported"
  | Grpc_frame.Too_large ->
    Grpc_status.make Grpc_status.Resource_exhausted "request message exceeds limit"
  | Grpc_frame.Extra ->
    Grpc_status.make Grpc_status.Invalid_argument "unary request has extra data"
  | Grpc_frame.Incomplete ->
    Grpc_status.make Grpc_status.Invalid_argument "request frame is incomplete"

let response_headers =
  Headers.of_list [
    "content-type", "application/grpc+proto";
    "grpc-accept-encoding", "identity";
  ]

type send_result =
  | Sent
  | Stream_closed

let response_writer reqd =
  Reqd.respond_with_streaming
    ~flush_headers_immediately:true
    reqd
    (Response.create ~headers:response_headers `OK)

let send reqd writer body status =
  try
    Reqd.schedule_trailers reqd (Headers.of_list (Grpc_status.trailers status));
    Option.iter
      (fun value -> Body.Writer.write_string writer (Grpc_frame.encode value))
      body;
    Body.Writer.close writer;
    Sent
  with
  | Failure message
    when message = "h2.Reqd.schedule_trailers: stream already closed" ->
    Stream_closed

let limited_reply config value =
  match value.Grpc_service.body with
  | Some body when String.length body > config.Grpc_config.max_response_bytes ->
    Grpc_service.{
      body = None;
      status = Grpc_status.make
        Grpc_status.Resource_exhausted
        "response message exceeds limit";
      rpc_method = value.rpc_method;
    }
  | _ -> value

let meta address body_bytes =
  Rpc_http.{
    rpc_peer = peer address;
    rpc_user_agent = "grpc";
    rpc_body_bytes = body_bytes;
  }

let run config call address request input =
  let open Lwt.Syntax in
  let* wire = read_body ~limit:(config.Grpc_config.max_request_bytes + 5) input in
  match wire with
  | Error code ->
    Lwt.return
      (Grpc_service.reply
         (Grpc_status.make code "request body exceeds limit"))
  | Ok wire ->
    begin
      match Grpc_frame.decode ~max_message:config.max_request_bytes wire with
      | Error error -> Lwt.return (Grpc_service.reply (frame_status error))
      | Ok payload ->
        Grpc_service.invoke
          ~call
          ~meta:(meta address (String.length wire))
          ~path:request.Request.target
          payload
    end

let run_limited config call address request input =
  match
    Grpc_deadline.seconds
      ~default:config.Grpc_config.default_deadline_s
      ~limit:config.max_deadline_s
      (Headers.get request.Request.headers "grpc-timeout")
  with
  | Error message ->
    Body.Reader.close input;
    Lwt.return
      (Grpc_service.reply
         (Grpc_status.make Grpc_status.Invalid_argument message))
  | Ok deadline ->
    let work =
      Lwt.catch
        (fun () -> run config call address request input)
        (function
          | Lwt.Canceled as canceled -> Lwt.fail canceled
          | _ ->
            Lwt.return
              (Grpc_service.reply
                 (Grpc_status.make Grpc_status.Internal "request failed")))
    in
    let elapsed =
      let open Lwt.Syntax in
      let* () = Lwt_unix.sleep deadline in
      Lwt.return
        (Grpc_service.reply
           (Grpc_status.make Grpc_status.Deadline_exceeded "deadline exceeded"))
    in
    Lwt.pick [work; elapsed]

let handle config call address reqd =
  let request = Reqd.request reqd in
  if request.Request.meth <> `POST then
    plain reqd `Method_not_allowed
  else if not (content_type request.headers) then
    plain reqd `Unsupported_media_type
  else if not (body_length_ok request (config.Grpc_config.max_request_bytes + 5)) then begin
    Body.Reader.close (Reqd.request_body reqd);
    let writer = response_writer reqd in
    ignore
      (send
         reqd
         writer
         None
         (Grpc_status.make Grpc_status.Resource_exhausted "request body exceeds limit"))
  end else if not (trailers_header request.headers) then begin
    Body.Reader.close (Reqd.request_body reqd);
    let writer = response_writer reqd in
    ignore
      (send
         reqd
         writer
         None
         (Grpc_status.make Grpc_status.Invalid_argument "te header must include trailers"))
  end else if not (identity_encoding request.headers) then begin
    Body.Reader.close (Reqd.request_body reqd);
    let writer = response_writer reqd in
    ignore
      (send
         reqd
         writer
         None
         (Grpc_status.make Grpc_status.Unimplemented "message encoding is not supported"))
  end else begin
    let writer = response_writer reqd in
    Lwt.async (fun () ->
      let started = Unix.gettimeofday () in
      let open Lwt.Syntax in
      let* reply = run_limited config call address request (Reqd.request_body reqd) in
      let reply = limited_reply config reply in
      let elapsed_ms = (Unix.gettimeofday () -. started) *. 1000.0 in
      Log.trace
        "grpc"
        "event = call path = %s rpc = %s status = %d elapsed_ms = %.0f peer = %s"
        request.target
        (Option.value ~default:"none" reply.Grpc_service.rpc_method)
        (Grpc_status.number reply.status.code)
        elapsed_ms
        (peer address);
      begin
        match send reqd writer reply.body reply.status with
        | Sent -> ()
        | Stream_closed ->
          Log.trace
            "grpc"
            "event = response status = dropped reason = stream_closed path = %s peer = %s"
            request.target
            (peer address)
      end;
      Lwt.return_unit)
  end

let error_text = function
  | `Bad_request -> "bad request"
  | `Internal_server_error -> "internal server error"
  | `Exn _ -> "connection error"

let connection config call =
  let h2_config = H2.Config.{
    default with
    request_body_buffer_size = 4096;
    response_body_buffer_size = 4096;
    enable_server_push = false;
    max_concurrent_streams = Int32.of_int config.Grpc_config.max_streams;
    initial_window_size = 65_535l;
  } in
  let request_handler address reqd =
    handle config call address reqd
  in
  let error_handler address ?request error start =
    let path =
      match request with
      | None -> "none"
      | Some value -> value.H2.Request.target
    in
    Log.warn
      "grpc"
      "event = connection status = closed reason = %s path = %s peer = %s"
      (error_text error)
      path
      (peer address);
    let writer = start Headers.empty in
    Body.Writer.close writer
  in
  H2_lwt_unix.Server.create_connection_handler
    ~config:h2_config
    ~request_handler
    ~error_handler

let pipe_signal = lazy (Sys.set_signal Sys.sigpipe Sys.Signal_ignore)

let accept config ~call =
  Lazy.force pipe_signal;
  connection config call

let start config ~call =
  let address =
    Unix.ADDR_INET
      (Unix.inet_addr_of_string config.Grpc_config.host, config.port)
  in
  let open Lwt.Syntax in
  let* server =
    Lwt_io.establish_server_with_client_socket
      ~backlog:128
      address
      (accept config ~call)
  in
  Log.info
    "grpc"
    "event = listen host = %s port = %d streams = %d request_bytes = %d response_bytes = %d"
    config.host
    config.port
    config.max_streams
    config.max_request_bytes
    config.max_response_bytes;
  let forever, _ = Lwt.wait () in
  Lwt.finalize
    (fun () -> forever)
    (fun () -> Lwt_io.shutdown_server server)