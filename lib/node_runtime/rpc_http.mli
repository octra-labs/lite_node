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


type meta = {
  rpc_peer : string;
  rpc_user_agent : string;
  rpc_body_bytes : int;
}

val meta_of_request :
  body_bytes:int ->
  Cohttp.Request.t ->
  meta

val respond_json :
  Yojson.Safe.t ->
  (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

val handle_rpc_post :
  process:(meta -> string -> Yojson.Safe.t Lwt.t) ->
  Cohttp.Request.t ->
  Cohttp_lwt.Body.t ->
  (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

val route_rpc_or_fallback :
  rpc_handler:(Cohttp.Request.t -> Cohttp_lwt.Body.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t) ->
  fallback_handler:(Cohttp.Request.t -> Cohttp_lwt.Body.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t) ->
  'conn ->
  Cohttp.Request.t ->
  Cohttp_lwt.Body.t ->
  (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

val with_connection_close :
  timeout_s:float ->
  ('conn -> Cohttp.Request.t -> Cohttp_lwt.Body.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t) ->
  'conn ->
  Cohttp.Request.t ->
  Cohttp_lwt.Body.t ->
  (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

val create_server :
  port:int ->
  callback:(Cohttp_lwt_unix.Server.conn ->
            Cohttp.Request.t ->
            Cohttp_lwt.Body.t ->
            (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t) ->
  unit Lwt.t