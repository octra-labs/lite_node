(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


module Header = Cohttp.Header
module Request = Cohttp.Request
module Server = Cohttp_lwt_unix.Server

open Lwt.Infix

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

let handle_rpc_post ~process req body =
  Cohttp_lwt.Body.to_string body >>= fun body_str ->
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