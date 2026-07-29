(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Header = Cohttp.Header
module Request = Cohttp.Request
module Server = Cohttp_lwt_unix.Server

open Lwt.Infix

let max_body_bytes = Octra_net.P2p_tx_gossip.max_tx_json + 65_536

type meta = {
  rpc_peer : string;
  rpc_user_agent : string;
  rpc_body_bytes : int;
}

let header_trim req name =
  match Header.get (Request.headers req) name with
  | Some s -> String.trim s
  | None -> ""

let meta_of_request ~body_bytes req =
  {
    rpc_peer = Rpc_view.peer_of_headers
      ~cf_connecting_ip:(header_trim req "cf-connecting-ip")
      ~x_real_ip:(header_trim req "x-real-ip")
      ~x_forwarded_for:(header_trim req "x-forwarded-for");
    rpc_user_agent = Rpc_view.user_agent (header_trim req "user-agent");
    rpc_body_bytes = body_bytes;
  }

let respond_json data =
  Server.respond_string
    ~status:`OK
    ~headers:(Header.of_list [
      "Content-Type", "application/json";
      "Access-Control-Allow-Origin", "*"
    ])
    ~body:(Yojson.Safe.to_string data)
    ()

let respond_error status message =
  Server.respond_string
    ~status
    ~headers:(Header.of_list [
      "Content-Type", "application/json";
      "Access-Control-Allow-Origin", "*"
    ])
    ~body:(Yojson.Safe.to_string (`Assoc ["error", `String message]))
    ()

let content_length_too_large req =
  match Header.get (Request.headers req) "content-length" with
  | None -> false
  | Some value ->
    match Int64.of_string_opt (String.trim value) with
    | None -> false
    | Some n -> Int64.compare n (Int64.of_int max_body_bytes) > 0

let body_to_string_limited body =
  let stream = Cohttp_lwt.Body.to_stream body in
  let buf = Buffer.create 4096 in
  let rec loop total =
    Lwt_stream.get stream >>= function
    | None -> Lwt.return (Ok (Buffer.contents buf))
    | Some chunk ->
      let n = String.length chunk in
      if n > max_body_bytes - total then
        Lwt.return (Error "request body too large")
      else begin
        Buffer.add_string buf chunk;
        loop (total + n)
      end
  in
  loop 0

let handle_rpc_post ~process req body =
  if content_length_too_large req then
    respond_error `Request_entity_too_large "request body too large"
  else
    body_to_string_limited body >>= function
    | Error message ->
      respond_error `Request_entity_too_large message
    | Ok body_str ->
      let meta = meta_of_request ~body_bytes:(String.length body_str) req in
      process meta body_str >>= fun result ->
      respond_json result

let route_rpc_or_fallback ~rpc_handler ~fallback_handler _conn req body =
  match Request.meth req, Uri.path (Request.uri req) with
  | `POST, "/rpc" ->
    rpc_handler req body
  | _ ->
    fallback_handler req body

let with_connection_close ~timeout_s callback conn req body =
  let timeout =
    Lwt_unix.sleep timeout_s >>= fun () ->
    Server.respond_string
      ~status:`Gateway_timeout
      ~body:"{\"error\":\"request timeout\"}"
      ()
  in
  Lwt.pick [callback conn req body; timeout] >>= fun (resp, body) ->
  let headers = Cohttp.Response.headers resp in
  let headers = Header.add headers "Connection" "close" in
  let resp = { resp with Cohttp.Response.headers } in
  Lwt.return (resp, body)

let create_server ~port ~callback =
  let conn_closed _ = () in
  let callback = with_connection_close ~timeout_s:30.0 callback in
  let mode = `TCP (`Port port) in
  let server = Server.make ~callback ~conn_closed () in
  Server.create ~mode server