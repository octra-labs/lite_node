(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module H = H2.Server_connection
module B = Gluten.Buffer

let read socket buffer =
  let result, resolve = Lwt.task () in
  B.put buffer
    ~f:(fun bytes ~off ~len commit ->
      let task = Lwt_bytes.read socket bytes off len in
      Lwt.on_cancel result (fun () -> Lwt.cancel task);
      Lwt.on_any task
        (fun count -> if Lwt.is_sleeping result then commit count)
        (fun exn ->
          if Lwt.is_sleeping result then Lwt.wakeup_later_exn resolve exn))
    (fun count ->
      if Lwt.is_sleeping result then Lwt.wakeup_later resolve count);
  result

let rec receive socket buffer connection advance =
  let open Lwt.Syntax in
  match H.next_read_operation connection with
  | `Close -> Lwt.return_unit
  | `Read ->
    let* count = read socket buffer in
    if count = 0 then begin
      ignore (B.get buffer ~f:(H.read_eof connection));
      advance ();
      Lwt.return_unit
    end else begin
      ignore (B.get buffer ~f:(H.read connection));
      advance ();
      receive socket buffer connection advance
    end

let rec transmit socket connection advance =
  let open Lwt.Syntax in
  match H.next_write_operation connection with
  | `Close _ -> Lwt.return_unit
  | `Write vectors ->
    let* result = Faraday_lwt_unix.writev_of_fd socket vectors in
    H.report_write_result connection result;
    advance ();
    begin
      match result with
      | `Closed -> Lwt.return_unit
      | `Ok _ -> transmit socket connection advance
    end
  | `Yield ->
    let ready, wake = Lwt.task () in
    H.yield_writer connection (fun () ->
      if Lwt.is_sleeping ready then Lwt.wakeup_later wake ());
    let* () = ready in
    transmit socket connection advance

let serve ~config ~request_handler ~error_handler ~advance socket =
  let connection = H.create ~config ~error_handler request_handler in
  let buffer = B.create config.H2.Config.read_buffer_size in
  let receive = receive socket buffer connection advance in
  let transmit = transmit socket connection advance in
  Lwt.finalize
    (fun () ->
      Lwt.catch
        (fun () -> Lwt.pick [receive; transmit])
        (function
          | Lwt.Canceled as exn -> Lwt.fail exn
          | exn -> H.report_exn connection exn; Lwt.return_unit))
    (fun () ->
      Lwt.cancel receive;
      Lwt.cancel transmit;
      H.shutdown connection;
      match Lwt_unix.state socket with
      | Lwt_unix.Closed -> Lwt.return_unit
      | _ -> Lwt_unix.close socket)