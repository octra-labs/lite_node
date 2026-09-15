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
  Reqd.respond_with_string reqd (Response.create status) "";
  Lwt.return_unit

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
    begin match body with
    | None -> Body.Writer.close writer
    | Some value ->
      Body.Writer.write_string writer (Grpc_frame.encode value);
      Body.Writer.flush writer (function
        | `Written when Reqd.response reqd <> None -> Body.Writer.close writer
        | `Written | `Closed -> ())
    end;
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

let request_limit config request =
  if request.Request.target = Grpc_service.submit_path then
    Option.value ~default:0 config.Grpc_config.submit_bytes
  else config.Grpc_config.max_request_bytes

let run config call submit address request input =
  let open Lwt.Syntax in
  let limit = request_limit config request in
  let* wire = read_body ~limit:(limit + 5) input in
  match wire with
  | Error code ->
    Lwt.return
      (Grpc_service.reply
         (Grpc_status.make code "request body exceeds limit"))
  | Ok wire ->
    begin
      match Grpc_frame.decode ~max_message:limit wire with
      | Error error -> Lwt.return (Grpc_service.reply (frame_status error))
      | Ok payload ->
        Grpc_service.invoke
          ?submit
          ~call
          ~meta:(meta address (String.length wire))
          ~path:request.Request.target
          payload
    end

let run_limited config call submit address request input =
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
        (fun () -> run config call submit address request input)
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

let handle config call submit lock address reqd =
  let request = Reqd.request reqd in
  if request.Request.meth <> `POST then
    plain reqd `Method_not_allowed
  else if not (content_type request.headers) then
    plain reqd `Unsupported_media_type
  else if request.target = Grpc_service.submit_path && Option.is_none submit then begin
    Body.Reader.close (Reqd.request_body reqd);
    let writer = response_writer reqd in
    ignore (send reqd writer None
      (Grpc_status.make Grpc_status.Unimplemented "submission is disabled"));
    Lwt.return_unit
  end else if not (body_length_ok request (request_limit config request + 5)) then begin
    Body.Reader.close (Reqd.request_body reqd);
    let writer = response_writer reqd in
    ignore
      (send
         reqd
         writer
         None
         (Grpc_status.make Grpc_status.Resource_exhausted "request body exceeds limit"));
    Lwt.return_unit
  end else if not (trailers_header request.headers) then begin
    Body.Reader.close (Reqd.request_body reqd);
    let writer = response_writer reqd in
    ignore
      (send
         reqd
         writer
         None
         (Grpc_status.make Grpc_status.Invalid_argument "te header must include trailers"));
    Lwt.return_unit
  end else if not (identity_encoding request.headers) then begin
    Body.Reader.close (Reqd.request_body reqd);
    let writer = response_writer reqd in
    ignore
      (send
         reqd
         writer
         None
         (Grpc_status.make Grpc_status.Unimplemented "message encoding is not supported"));
    Lwt.return_unit
  end else begin
    let writer = response_writer reqd in
    let started = Mtime_clock.elapsed_ns () in
    let open Lwt.Syntax in
    let work () = run_limited config call submit address request (Reqd.request_body reqd) in
    let* reply =
      if request.target <> Grpc_service.submit_path then work ()
      else if Lwt_mutex.is_locked lock then begin
        Body.Reader.close (Reqd.request_body reqd);
        Lwt.return (Grpc_service.reply
          (Grpc_status.make Grpc_status.Resource_exhausted "submission is busy"))
      end else Lwt_mutex.with_lock lock work
    in
    let reply = limited_reply config reply in
    let elapsed_ms =
      Int64.to_float (Int64.sub (Mtime_clock.elapsed_ns ()) started) /. 1_000_000.
    in
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
    Lwt.return_unit
  end

let error_text = function
  | `Bad_request -> "bad request"
  | `Internal_server_error -> "internal server error"
  | `Exn _ -> "connection error"

type pending = {
  reqd : Reqd.t;
  ended : unit Lwt.t;
  finish : unit Lwt.u;
  job : unit Lwt.t;
}

let connection config call submit lock address socket =
  let h2_config = H2.Config.{
    default with
    request_body_buffer_size = 4096;
    response_body_buffer_size = 4096;
    enable_server_push = false;
    max_concurrent_streams = Int32.of_int config.Grpc_config.max_streams;
    initial_window_size = 65_535l;
  } in
  let jobs = ref [] in
  let advance () =
    List.iter (fun pending ->
      if Reqd.response pending.reqd = None && Lwt.is_sleeping pending.ended then
        Lwt.wakeup pending.finish ()) !jobs
  in
  let request_handler reqd =
    advance ();
    if List.length !jobs >= config.max_streams then begin
      Body.Reader.close (Reqd.request_body reqd);
      let writer = response_writer reqd in
      ignore (send reqd writer None
        (Grpc_status.make Grpc_status.Resource_exhausted "connection request limit"))
    end else begin
      let ended, finish = Lwt.task () in
      let job = Lwt.catch
        (fun () ->
          let open Lwt.Syntax in
          let run =
            let* () = handle config call submit lock address reqd in
            let timeout =
              let* () = Lwt_unix.sleep config.max_deadline_s in
              Reqd.report_exn reqd (Failure "response delivery timeout");
              Lwt.return_unit
            in
            Lwt.pick [ended; timeout]
          in
          Lwt.pick [run; ended])
        (function
          | Lwt.Canceled -> Lwt.return_unit
          | exn -> Reqd.report_exn reqd exn; Lwt.return_unit)
      in
      jobs := { reqd; ended; finish; job } :: !jobs;
      let remove _ = jobs := List.filter (fun current -> current.job != job) !jobs in
      Lwt.on_any job remove remove;
      advance ();
      Lwt.async (fun () -> job)
    end
  in
  let error_handler ?request error start =
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
  Lwt.finalize
    (fun () -> Grpc_io.serve ~config:h2_config ~request_handler ~error_handler ~advance socket)
    (fun () ->
      let pending = !jobs in
      jobs := [];
      List.iter (fun pending -> Lwt.cancel pending.job) pending;
      Lwt.return_unit)

let pipe_signal = lazy (Sys.set_signal Sys.sigpipe Sys.Signal_ignore)

let accept ?submit config ~call =
  Lazy.force pipe_signal;
  let submit = if Option.is_some config.Grpc_config.submit_bytes then submit else None in
  let lock = Lwt_mutex.create () in
  connection config call submit lock

external socket_codes : unit -> int list = "octra_accept_codes"

let accept_codes = socket_codes ()

let accept_delay attempt error =
  let delay = min 1.0 (0.01 *. Float.of_int (1 lsl min 7 (max 0 attempt))) in
  match error with
  | Unix.Unix_error (Unix.EUNKNOWNERR code, _, _)
      when List.mem code accept_codes -> Some delay
  | Unix.Unix_error ((Unix.EINTR | Unix.EAGAIN | Unix.EWOULDBLOCK
      | Unix.ECONNABORTED | Unix.ENETDOWN | Unix.ENETUNREACH
      | Unix.EHOSTDOWN | Unix.EHOSTUNREACH
      | Unix.ENOPROTOOPT | Unix.EOPNOTSUPP | Unix.EMFILE | Unix.ENFILE
      | Unix.ENOBUFS | Unix.ENOMEM), _, _) ->
    Some delay
  | _ -> None

let take_client ?(take = fun socket -> Lwt_unix.accept ~cloexec:true socket)
    ?(sleep = Lwt_unix.sleep) socket =
  let open Lwt.Syntax in
  let rec next attempt =
    let* result = Lwt.catch
      (fun () -> Lwt.map Result.ok (take socket))
      (fun error -> Lwt.return_error error)
    in
    match result with
    | Ok client ->
      if attempt > 0 then Log.info "grpc" "event = accept status = resumed";
      Lwt.return client
    | Error error ->
      match accept_delay attempt error with
      | None -> Lwt.fail error
      | Some delay ->
        if attempt = 0 then
          Log.warn "grpc" "event = accept status = waiting reason = %s"
            (Printexc.to_string error);
        let* () = sleep delay in
        next (min 7 (attempt + 1))
  in
  next 0

let start ?submit ?socket config ~call =
  let address =
    Unix.ADDR_INET
      (Unix.inet_addr_of_string config.Grpc_config.host, config.port)
  in
  let open Lwt.Syntax in
  let socket = match socket with
    | Some value -> value
    | None -> Lwt_unix.socket (Unix.domain_of_sockaddr address) Unix.SOCK_STREAM 0
  in
  let close socket =
    if Lwt_unix.state socket = Lwt_unix.Closed then Lwt.return_unit
    else Lwt_unix.close socket
  in
  let clients = ref [] in
  let handle = accept ?submit config ~call in
  let rec listen () =
    let* client, address = take_client socket in
    let* () =
        if List.length !clients >= 128 then close client
        else begin
          let job = Lwt.catch
            (fun () -> Lwt.finalize
              (fun () ->
                Lwt_unix.set_close_on_exec client;
                handle address client)
              (fun () -> close client))
            (function
              | Lwt.Canceled -> Lwt.return_unit
              | error ->
                Log.warn "grpc" "event = connection status = closed reason = %s"
                  (Printexc.to_string error);
                Lwt.return_unit)
          in
          clients := job :: !clients;
          let remove _ = clients := List.filter (fun item -> item != job) !clients in
          Lwt.on_any job remove remove;
          Lwt.return_unit
        end
    in
    let* () = Lwt.pause () in
    listen ()
  in
  Lwt.finalize
    (fun () ->
      Lwt_unix.set_close_on_exec socket;
      Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
      let* () = Lwt_unix.bind socket address in
      Lwt_unix.listen socket 128;
      Log.info
        "grpc"
        "event = listen host = %s port = %d streams = %d request_bytes = %d response_bytes = %d submit_bytes = %d"
        config.host
        config.port
        config.max_streams
        config.max_request_bytes
        config.max_response_bytes
        (if Option.is_some submit then Option.value ~default:0 config.submit_bytes else 0);
      listen ())
    (fun () ->
      let pending = !clients in
      clients := [];
      List.iter Lwt.cancel pending;
      Lwt.finalize (fun () -> Lwt.join pending) (fun () -> close socket))

let serve ?submit ?socket config ~call ~http =
  let grpc =
    Lwt.catch
      (fun () -> start ?submit ?socket config ~call)
      (function
        | Lwt.Canceled as exn -> Lwt.fail exn
        | exn ->
          Log.error "grpc"
            "event = listen status = unavailable host = %s port = %d reason = %s"
            config.Grpc_config.host config.port (Printexc.to_string exn);
          Lwt.return_unit)
  in
  Lwt.finalize
    (fun () -> http)
    (fun () ->
      Lwt.cancel grpc;
      Lwt.catch
        (fun () -> grpc)
        (function Lwt.Canceled -> Lwt.return_unit | exn -> Lwt.fail exn))