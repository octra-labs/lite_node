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


type peer_id = string

type direction =
  | Inbound
  | Outbound

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
}

let create fd ~peer_id ~addr ~direction = {
  fd;
  peer_id;
  addr;
  direction;
  connected = true;
  write_queue = Lwt_mvar.create_empty ();
  msg_count_in = 0;
  msg_count_out = 0;
  last_seen = Unix.gettimeofday ();
}

let is_connected t = t.connected

let close t =
  if t.connected then begin
    t.connected <- false;
    (try Lwt_unix.shutdown t.fd Lwt_unix.SHUTDOWN_ALL with _ -> ());
    Lwt_unix.close t.fd
  end else
    Lwt.return_unit

let send t (frame : P2p_frame.frame) =
  if t.connected then
    Lwt_mvar.put t.write_queue frame
  else
    Lwt.return_unit

let write_loop t =
  let open Lwt.Syntax in
  let rec loop () =
    if not t.connected then Lwt.return_unit
    else
      let* frame = Lwt_mvar.take t.write_queue in
      if not t.connected then Lwt.return_unit
      else
        Lwt.catch
          (fun () ->
            let* () = P2p_frame.write_frame t.fd frame in
            t.msg_count_out <- t.msg_count_out + 1;
            loop ())
          (fun _exn ->
            let* () = close t in
            Lwt.return_unit)
  in
  loop ()

let read_loop t ~on_message =
  let open Lwt.Syntax in
  let rec loop () =
    if not t.connected then Lwt.return_unit
    else
      Lwt.catch
        (fun () ->
          let* frame = P2p_frame.read_frame t.fd in
          t.msg_count_in <- t.msg_count_in + 1;
          t.last_seen <- Unix.gettimeofday ();
          let* () = on_message t frame in
          loop ())
        (fun exn ->
          Octra_log.stderr "P2P CONN [%s]: read_loop error: %s\n%!" t.addr (Printexc.to_string exn);
          let* () = close t in
          Lwt.return_unit)
  in
  loop ()

let start t ~on_message =
  Octra_log.stdout "P2P CONN [%s]: starting read+write loops\n%!" t.addr;
  Lwt.async (fun () -> write_loop t);
  read_loop t ~on_message