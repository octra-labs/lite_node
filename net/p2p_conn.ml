(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type peer_id = string

type direction =
  | Inbound
  | Outbound

type loop =
  | Again
  | Done

type tx_write = {
  frame : P2p_frame.frame;
  written : unit Lwt.t;
  wake : unit Lwt.u;
}

type t = {
  fd : Lwt_unix.file_descr;
  peer_id : peer_id;
  addr : string;
  direction : direction;
  mutable connected : bool;
  write_queue : P2p_frame.frame Lwt_mvar.t;
  mutable service_write : (P2p_frame.frame * unit Lwt.u) option;
  mutable service_pending : bool;
  write_changed : unit Lwt_condition.t;
  mutable pending_writes : int;
  mutable writing : bool;
  mutable tx_writes : tx_write list;
  mutable tx_bytes : int;
  mutable tx_batches : int;
  tx_gate : Lwt_mutex.t;
  closed : unit Lwt.t;
  wake_close : unit Lwt.u;
  mutable msg_count_in : int;
  mutable msg_count_out : int;
  mutable last_seen : float;
  frame_budget : P2p_frame_budget.t;
}

let read_idle_timeout_s = 60.0

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

let monotonic_seconds () =
  Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1_000_000_000.0

let create ~peer_class fd ~peer_id ~addr ~direction =
  let closed, wake_close = Lwt.wait () in
  {
    fd;
    peer_id;
    addr;
    direction;
    connected = true;
    write_queue = Lwt_mvar.create_empty ();
    service_write = None;
    service_pending = false;
    write_changed = Lwt_condition.create ();
    pending_writes = 0;
    writing = false;
    tx_writes = [];
    tx_bytes = 0;
    tx_batches = 0;
    tx_gate = Lwt_mutex.create ();
    closed;
    wake_close;
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

let write_idle t = t.connected && t.pending_writes = 0 && not t.writing
  && t.service_write = None && Lwt_mvar.is_empty t.write_queue

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
    Lwt.wakeup_later t.wake_close ();
    t.tx_writes <- [];
    t.tx_bytes <- 0;
    (try Lwt_unix.shutdown t.fd Lwt_unix.SHUTDOWN_ALL with _ -> ());
    Lwt.finalize (fun () -> Lwt.catch
      (fun () -> Lwt_unix.close t.fd)
      (fun _ -> Lwt.return_unit)) (fun () ->
        let rec drain () = match Lwt_mvar.take_available t.write_queue with
          | None -> () | Some _ -> drain () in
        drain ();
        t.pending_writes <- 0;
        Lwt_condition.broadcast t.write_changed ();
        Lwt.return_unit)
  end else
    Lwt.return_unit

let until_closed t wait =
  Lwt.pick [
    Lwt.map Option.some wait;
    Lwt.map (fun () -> None) (Lwt.protected t.closed);
  ]

let enqueue t (frame : P2p_frame.frame) =
  if t.connected then begin
    t.pending_writes <- t.pending_writes + 1;
    let transferred = ref false in
    Lwt.finalize (fun () ->
      until_closed t (Lwt.map (fun () -> transferred := true)
        (Lwt_mvar.put t.write_queue frame)) |> Lwt.map Option.is_some)
      (fun () ->
        if not !transferred then begin
          t.pending_writes <- max 0 (t.pending_writes - 1);
          Lwt_condition.broadcast t.write_changed ()
        end;
        Lwt.return_unit)
  end
  else
    Lwt.return_false

let send t (frame : P2p_frame.frame) =
  Lwt.map (fun _ -> ()) (enqueue t frame)

let await_service ?(wait = fun () -> Lwt_unix.sleep 10.) t =
  let open Lwt.Syntax in
  if t.service_pending || t.service_write <> None then Lwt.return_false
  else if write_idle t then Lwt.return_true
  else begin
    t.service_pending <- true;
    let rec available () =
      if not t.connected then Lwt.return_false
      else if write_idle t then Lwt.return_true
      else let* _ = until_closed t (Lwt_condition.wait t.write_changed) in available () in
    Lwt.finalize (fun () -> Lwt.pick [available (); (let* () = wait () in Lwt.return_false)])
      (fun () -> t.service_pending <- false; Lwt.return_unit)
  end

let send_service ?wait t frame =
  let open Lwt.Syntax in
  let* available = await_service ?wait t in
  if not available then Lwt.return_unit
  else
    let frame = {frame with P2p_frame.payload = frame.P2p_frame.payload} in
    let written, wake = Lwt.wait () in
    t.service_write <- Some (frame, wake);
    Lwt.finalize (fun () ->
      let* queued = enqueue t frame in
      if queued then Lwt.map (fun _ -> ()) (until_closed t written)
      else Lwt.return_unit)
      (fun () ->
        t.service_write <- None;
        Lwt_condition.broadcast t.write_changed ();
        Lwt.return_unit)

let tx_limit = 8 * 1024 * 1024

let release_tx t frame =
  let retained, released = List.partition (fun item -> item.frame != frame) t.tx_writes in
  t.tx_writes <- retained;
  List.iter (fun item ->
    t.tx_bytes <- t.tx_bytes - String.length item.frame.payload;
    if Lwt.is_sleeping item.written then Lwt.wakeup_later item.wake ()) released

let send_tx ?(wait = fun () -> Lwt_unix.sleep 10.) t frame =
  let open Lwt.Syntax in
  let size = String.length frame.P2p_frame.payload in
  if not t.connected || List.length t.tx_writes >= 64 || size > tx_limit - t.tx_bytes
  then Lwt.return_unit else
  let frame = {frame with P2p_frame.payload = frame.P2p_frame.payload} in
  let written, wake = Lwt.wait () in
  t.tx_writes <- {frame; written; wake} :: t.tx_writes;
  t.tx_bytes <- t.tx_bytes + size;
  let transferred = ref false in
  Lwt.finalize (fun () ->
    let* queued = Lwt.pick [
      (let* queued = enqueue t frame in transferred := queued; Lwt.return queued);
      (let* () = wait () in Lwt.return_false)] in
    if queued then Lwt.map (fun _ -> ()) (until_closed t (Lwt.protected written))
    else begin
      err_conn t.addr "event = tx_queue_deadline action = skip";
      Lwt.return_unit
    end)
    (fun () -> if not !transferred then release_tx t frame; Lwt.return_unit)

let send_batch ?(wait = fun () -> Lwt_unix.sleep 10.) t ~msg_type builders =
  let open Lwt.Syntax in
  if not t.connected || t.tx_batches >= 4 || List.length builders > 64 then Lwt.return_unit
  else begin
    t.tx_batches <- t.tx_batches + 1;
    let rec drain () =
      if not t.connected || t.tx_writes = [] then Lwt.return_unit else
      let* _ = until_closed t
        (Lwt.join (List.map (fun item -> Lwt.protected item.written) t.tx_writes)) in
      drain () in
    Lwt.finalize (fun () -> Lwt_mutex.with_lock t.tx_gate (fun () ->
      Lwt.catch (fun () ->
      Lwt_list.iter_s (fun build ->
        let* () = if t.tx_writes = [] then Lwt.return_unit else
          Lwt.pick [
            drain ();
            (let* () = wait () in Lwt.fail Lwt_unix.Timeout)] in
        if not t.connected then Lwt.return_unit else
        match (try build () with Invalid_argument reason ->
          err_conn t.addr "event = tx_build_refused reason = %s action = skip" reason;
          None) with
        | None -> Lwt.return_unit
        | Some payload -> send_tx ~wait t P2p_frame.{msg_type; payload}) builders)
      (function
        | Lwt_unix.Timeout ->
          err_conn t.addr "event = tx_batch_deadline action = skip";
          Lwt.return_unit
        | error -> Lwt.fail error)))
      (fun () -> t.tx_batches <- t.tx_batches - 1; Lwt.return_unit)
  end

let rec repeat step =
  let open Lwt.Syntax in
  let* state = step () in
  match state with
  | Done -> Lwt.return_unit
  | Again ->
    let* () = Lwt.pause () in
    repeat step

let write_loop ?idle_s t =
  let open Lwt.Syntax in
  let step () =
    if not t.connected then Lwt.return Done
    else
      let* item = until_closed t (Lwt_mvar.take t.write_queue) in
      match item with
      | None -> Lwt.return Done
      | Some _ when not t.connected -> Lwt.return Done
      | Some frame ->
        t.writing <- true;
        Lwt.finalize (fun () -> Lwt.catch
          (fun () ->
            let* () = P2p_frame.write_frame ?idle_s t.fd frame in
            t.msg_count_out <- t.msg_count_out + 1;
            (match t.service_write with
             | Some (sent, wake) when sent == frame -> Lwt.wakeup_later wake ()
             | _ -> ());
            Lwt.return Again)
          (fun exn ->
            let error = Printexc.to_string exn in
            (match exn with
             | Lwt_unix.Timeout ->
               err_conn t.addr "event = write_idle seconds = %.3f action = close"
                 (Option.value idle_s ~default:P2p_frame.default_write_idle_s)
             | _ when expected_disconnect exn ->
               trace_conn t.addr "event = disconnected reason = %s" error
             | _ ->
               err_conn t.addr "event = write_loop_error error = %s" error);
            let* () = close t in
            Lwt.return Done)) (fun () ->
              t.writing <- false;
              t.pending_writes <- max 0 (t.pending_writes - 1);
              release_tx t frame;
              Lwt_condition.broadcast t.write_changed ();
              Lwt.return_unit)
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