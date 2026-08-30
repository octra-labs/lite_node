(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type peer_id = string

type direction =
  | Inbound
  | Outbound

type loop =
  | Again
  | Done

type t = {
  fd : Lwt_unix.file_descr;
  peer_id : peer_id;
  addr : string;
  direction : direction;
  mutable connected : bool;
  write_queue : P2p_frame.frame Lwt_mvar.t;
  mutable msg_count_in : int;
  mutable msg_count_out : int;
  mutable last_seen : float;
  frame_budget : P2p_frame_budget.t;
}

let read_idle_timeout_s = 60.0

let monotonic_seconds () =
  Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1_000_000_000.0

let create ~peer_class fd ~peer_id ~addr ~direction = {
  fd;
  peer_id;
  addr;
  direction;
  connected = true;
  write_queue = Lwt_mvar.create_empty ();
  msg_count_in = 0;
  msg_count_out = 0;
  last_seen = Unix.gettimeofday ();
  frame_budget = P2p_frame_budget.create ~now:(monotonic_seconds ()) ~peer_class;
}

let set_peer_class t peer_class =
  P2p_frame_budget.set_peer_class t.frame_budget peer_class

let peer_class t =
  P2p_frame_budget.peer_class t.frame_budget

let is_connected t = t.connected

let log_conn addr fmt =
  Printf.ksprintf
    (fun msg -> Octra_log.info "p2p" "conn = %s %s" addr msg)
    fmt

let trace_conn addr fmt =
  Printf.ksprintf
    (fun msg -> Octra_log.trace "p2p" "conn = %s %s" addr msg)
    fmt

let err_conn addr fmt =
  Printf.ksprintf
    (fun msg -> Octra_log.warn "p2p" "conn = %s %s" addr msg)
    fmt

let expected_disconnect = function
  | End_of_file
  | Unix.Unix_error (Unix.EBADF, _, _)
  | Unix.Unix_error (Unix.ECONNRESET, _, _)
  | Unix.Unix_error (Unix.EPIPE, _, _)
  | Unix.Unix_error (Unix.ENOTCONN, _, _) -> true
  | Failure reason when String.equal reason "connection_closed" -> true
  | _ -> false

let close t =
  if t.connected then begin
    t.connected <- false;
    (try Lwt_unix.shutdown t.fd Lwt_unix.SHUTDOWN_ALL with _ -> ());
    Lwt.catch
      (fun () -> Lwt_unix.close t.fd)
      (fun _ -> Lwt.return_unit)
  end else
    Lwt.return_unit

let send t (frame : P2p_frame.frame) =
  if t.connected then
    Lwt_mvar.put t.write_queue frame
  else
    Lwt.return_unit

let rec repeat step =
  let open Lwt.Syntax in
  let* state = step () in
  match state with
  | Done -> Lwt.return_unit
  | Again ->
    let* () = Lwt.pause () in
    repeat step

let write_loop t =
  let open Lwt.Syntax in
  let step () =
    if not t.connected then Lwt.return Done
    else
      let* frame = Lwt_mvar.take t.write_queue in
      if not t.connected then Lwt.return Done
      else
        Lwt.catch
          (fun () ->
            let* () = P2p_frame.write_frame t.fd frame in
            t.msg_count_out <- t.msg_count_out + 1;
            Lwt.return Again)
          (fun exn ->
            let error = Printexc.to_string exn in
            if expected_disconnect exn then
              trace_conn t.addr "event = disconnected reason = %s" error
            else
              err_conn t.addr "event = write_loop_error error = %s" error;
            let* () = close t in
            Lwt.return Done)
  in
  repeat step

let read_loop t ~on_message =
  let open Lwt.Syntax in
  let step () =
    if not t.connected then Lwt.return Done
    else
      Lwt.catch
        (fun () ->
          let* frame = P2p_frame.read_frame ~timeout_s:read_idle_timeout_s t.fd in
          let budget_now = monotonic_seconds () in
          match P2p_frame_budget.admit t.frame_budget ~now:budget_now
            ~msg_type:frame.msg_type ~bytes:(String.length frame.payload) with
          | P2p_frame_budget.Drop rejection ->
            err_conn t.addr
              "event = frame_budget_rejected reason = %s action = %s lane = %s peer_class = %s frames = %d bytes = %d frame_bytes = %d max_frames = %d max_bytes = %d"
              rejection.reason
              (P2p_frame_budget.rejection_action_name rejection.action)
              (P2p_frame_budget.lane_name rejection.lane)
              (P2p_frame_budget.peer_class_name rejection.peer_class)
              rejection.frames
              rejection.bytes
              rejection.frame_bytes
              rejection.max_frames
              rejection.max_bytes;
            (match rejection.action with
             | P2p_frame_budget.Close ->
               let* () = close t in
               Lwt.return Done
             | P2p_frame_budget.Discard -> Lwt.return Again)
          | P2p_frame_budget.Accept ->
            t.msg_count_in <- t.msg_count_in + 1;
            t.last_seen <- Unix.gettimeofday ();
            let* () = on_message t frame in
            Lwt.return Again)
        (fun exn ->
          let error = Printexc.to_string exn in
          if expected_disconnect exn then
            trace_conn t.addr "event = disconnected reason = %s" error
          else
            err_conn t.addr "event = read_loop_error error = %s" error;
          let* () = close t in
          Lwt.return Done)
  in
  repeat step

let start t ~on_message =
  log_conn t.addr "event = start_loops";
  Lwt.async (fun () -> write_loop t);
  read_loop t ~on_message