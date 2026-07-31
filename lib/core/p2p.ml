(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Infix

let read_timeout_seconds = 5.0

let send peer node =
  match String.split_on_char ':' peer with
  | [ip; port] ->
    let sockaddr = Lwt_unix.ADDR_INET (Unix.inet_addr_of_string ip, int_of_string port) in
    let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.output sock in
    let data = Node.to_yojson node |> Yojson.Safe.to_string in
    Lwt.catch
      (fun () ->
         Lwt_unix.connect sock sockaddr >>= fun () ->
         Lwt_io.write_line oc data >>= fun () ->
         Lwt_unix.close sock)
      (fun _ -> Lwt_unix.close sock)
  | _ -> Lwt.return_unit

let handle_connection ~read ~close ~callback =
  let run () =
    Lwt.catch
      (fun () ->
         read () >>= function
         | None -> Lwt.return_unit
         | Some json ->
           (match
              try Node.of_yojson (Yojson.Safe.from_string json)
              with _ -> Error "invalid node"
            with
            | Ok node -> callback node
            | Error _ -> Lwt.return_unit))
      (fun _ -> Lwt.return_unit)
  in
  Lwt.finalize run (fun () -> Lwt.catch close (fun _ -> Lwt.return_unit))

let serve_client fd callback =
  let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd in
  handle_connection
    ~read:(fun () ->
      Lwt_unix.with_timeout read_timeout_seconds (fun () ->
        Lwt_io.read_line_opt ic))
    ~close:(fun () -> Lwt_io.close ic)
    ~callback

let accept_connection sock =
  Lwt.catch
    (fun () -> Lwt_unix.accept sock >|= fun value -> Some value)
    (function
      | Unix.Unix_error ((Unix.EINTR | Unix.ECONNABORTED), _, _) ->
        Lwt.return_none
      | error -> Lwt.fail error)

let listen ~port ~callback =
  let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.bind sock (Lwt_unix.ADDR_INET (Unix.inet_addr_any, port)) >>= fun () ->
  Lwt_unix.listen sock 10;
  let rec loop () =
    accept_connection sock >>= function
    | None -> loop ()
    | Some (fd, _) ->
      Lwt.async (fun () -> serve_client fd callback);
      loop ()
  in
  loop ()