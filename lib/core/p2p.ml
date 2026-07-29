(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Infix

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

let listen ~port ~callback =
  let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.bind sock (Lwt_unix.ADDR_INET (Unix.inet_addr_any, port)) >>= fun () ->
  Lwt_unix.listen sock 10;
  let rec loop () =
    Lwt_unix.accept sock >>= fun (fd, _) ->
    let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd in
    Lwt_io.read_line_opt ic >>= function
    | Some json ->
      (try match Yojson.Safe.from_string json |> Node.of_yojson with
           | Ok n -> callback n | _ -> Lwt.return_unit
       with _ -> Lwt.return_unit)
      >>= fun () -> Lwt_unix.close fd >>= loop
    | None -> Lwt_unix.close fd >>= loop
  in
  loop ()