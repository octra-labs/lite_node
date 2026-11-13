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

let control_path data_dir = Filename.concat (Filename.concat data_dir "control") "sync.sock"

let control_active = ref false

let control_address path =
  let path = Filename.concat (Unix.realpath (Filename.dirname path)) (Filename.basename path) in
  let rec relative left right = match left, right with
    | a :: left, b :: right when a = b -> relative left right
    | left, right -> String.concat "/" (List.map (fun _ -> "..") left @ right) in
  let short = relative (String.split_on_char '/' (Unix.realpath (Sys.getcwd ())))
    (String.split_on_char '/' path) in
  let address = if String.length short < String.length path then short else path in
  if String.length address > 100 then failwith "sync control socket path is too long";
  Unix.ADDR_UNIX address

let control_owned kind mode path =
  let stat = Unix.lstat path in
  if stat.Unix.st_kind <> kind || stat.st_uid <> Unix.geteuid ()
     || stat.st_perm land mode <> 0 then
    failwith "sync control path ownership differs";
  stat

let control_lock data_dir =
  if !control_active then failwith "sync control listener is already active";
  let data_dir = Unix.realpath data_dir in
  ignore (control_owned Unix.S_DIR 0o022 data_dir);
  let directory = Filename.concat data_dir "control" in
  (try Unix.mkdir directory 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  ignore (control_owned Unix.S_DIR 0o077 directory);
  let path = Filename.concat directory "sync.lock" in
  (try ignore (control_owned Unix.S_REG 0o077 path)
   with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
  let fd = Unix.openfile path [Unix.O_RDWR; Unix.O_CREAT; Unix.O_CLOEXEC] 0o600 in
  try
    let stat = control_owned Unix.S_REG 0o077 path in
    let opened = Unix.fstat fd in
    if opened.st_dev <> stat.st_dev || opened.st_ino <> stat.st_ino || opened.st_nlink <> 1 then
      failwith "sync control lock identity differs";
    Unix.lockf fd Unix.F_TLOCK 0;
    control_active := true;
    fd
  with error -> Unix.close fd; raise error

let control_reply ~query body =
  let module Rpc = Octra_core.Rpc in
  let error value id = Lwt.return (Rpc.response_json (Rpc.Error_ (value, id))) in
  match Rpc.parse_body body with
  | Error value -> error value `Null
  | Ok (`Batch _) -> error Rpc.invalid_request `Null
  | Ok (`Single request) when request.method_ <> "octra_syncTarget" ->
    error (Rpc.method_not_found request.method_) request.id
  | Ok (`Single request) when request.params <> `List [] ->
    error (Rpc.invalid_params "octra_syncTarget takes no parameters") request.id
  | Ok (`Single request) ->
    let open Lwt.Syntax in
    let+ result = query () in
    Rpc.response_json (match result with
      | Ok value -> Rpc.Result (value, request.id)
      | Error value -> Rpc.Error_ (value, request.id))

let control_read fd =
  let open Lwt.Syntax in
  let bytes = Bytes.create 4096 in
  let buffer = Buffer.create 128 in
  let rec read () =
    let* count = Lwt_unix.read fd bytes 0 (Bytes.length bytes) in
    if count = 0 || Buffer.length buffer + count > 4096 then
      Lwt.fail_with "sync control request exceeds frame"
    else
      let chunk = Bytes.sub_string bytes 0 count in
      match String.index_opt chunk '\n' with
      | Some index when index = count - 1 ->
        Buffer.add_substring buffer chunk 0 index;
        Lwt.return (Buffer.contents buffer)
      | Some _ -> Lwt.fail_with "sync control accepts one frame"
      | None -> Buffer.add_string buffer chunk; read () in
  read ()

let control_write fd json =
  let bytes = Bytes.of_string (Yojson.Safe.to_string json ^ "\n") in
  let rec write offset =
    if offset = Bytes.length bytes then Lwt.return_unit
    else
      let open Lwt.Syntax in
      let* count = Lwt_unix.write fd bytes offset (Bytes.length bytes - offset) in
      if count = 0 then Lwt.fail End_of_file else write (offset + count) in
  if Bytes.length bytes > 4096 then Lwt.fail_with "sync control response exceeds frame"
  else write 0

let control_close fd = Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit)

let run_sync_control ~unavailable ~data_dir ~query serve =
  let open Lwt.Syntax in
  let data_dir = Unix.realpath data_dir in
  let lock = control_lock data_dir in
  let path = control_path data_dir in
  let socket = try Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0
    with error -> control_active := false; Unix.close lock; raise error in
  Lwt_unix.set_close_on_exec socket;
  let identity = ref None in
  let clients = Hashtbl.create 16 in
  let client fd =
    Lwt.finalize
      (fun () -> Lwt.catch (fun () -> Lwt_unix.with_timeout 2.8 (fun () ->
        let* body = control_read fd in
        let* response = control_reply ~query body in
        control_write fd response)) (fun _ -> Lwt.return_unit))
      (fun () -> Hashtbl.remove clients fd; control_close fd) in
  let rec accept () =
    let* fd, _ = Lwt_unix.accept socket in
    Lwt_unix.set_close_on_exec fd;
    if Hashtbl.length clients >= 16 then
      let* () = control_close fd in accept ()
    else begin
      let task = let* () = Lwt.pause () in client fd in
      Hashtbl.add clients fd task;
      Lwt.async (fun () -> task);
      accept ()
    end in
  Lwt.finalize
    (fun () ->
      (try ignore (control_owned Unix.S_SOCK 0o077 path); Unix.unlink path
       with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
      let* () = Lwt_unix.bind socket (control_address path) in
      Unix.chmod path 0o600;
      identity := Some (control_owned Unix.S_SOCK 0o077 path);
      Lwt_unix.listen socket 16;
      let public = serve () in
      let control = Lwt.catch accept (function
        | Lwt.Canceled -> Lwt.fail Lwt.Canceled
        | error -> unavailable (Printexc.to_string error); Lwt.protected public) in
      Lwt.pick [public; control])
    (fun () ->
      let sockets = Hashtbl.fold (fun fd _ sockets -> fd :: sockets) clients [] in
      let tasks = Hashtbl.fold (fun _ task tasks -> task :: tasks) clients [] in
      List.iter Lwt.cancel tasks;
      let* () = control_close socket in
      let* () = Lwt_list.iter_s control_close sockets in
      let* () = Lwt.join (List.map (fun task -> Lwt.catch
        (fun () -> task) (fun _ -> Lwt.return_unit)) tasks) in
      Fun.protect ~finally:(fun () -> control_active := false; Unix.close lock) (fun () ->
        match !identity with
        | None -> ()
        | Some prior ->
          (try let current = Unix.lstat path in
            if current.st_kind = Unix.S_SOCK && current.st_dev = prior.st_dev
               && current.st_ino = prior.st_ino then Unix.unlink path
           with Unix.Unix_error (Unix.ENOENT, _, _) -> ()));
      Lwt.return_unit)

let with_sync_control
    ?(unavailable = fun reason -> Log.warn "rpc"
        "event = sync_control status = unavailable reason = %s" reason)
    ~data_dir ~query serve =
  let started = ref false in
  let run () = started := true; serve () in
  Lwt.catch (fun () -> run_sync_control ~unavailable ~data_dir ~query run)
    (function
      | Lwt.Canceled -> Lwt.fail Lwt.Canceled
      | error when not !started -> unavailable (Printexc.to_string error); serve ()
      | error -> Lwt.fail error)