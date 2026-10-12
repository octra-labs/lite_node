(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Frame = Octra_net.P2p_frame
module Guard = Octra_net.P2p_peer_guard
module Handshake = Octra_net.P2p_handshake
module Swarm = Octra_net.P2p_swarm

let () = Mirage_crypto_rng_unix.use_default ()

let require condition message =
  if not condition then failwith message

let contains text part =
  let text_len = String.length text in
  let part_len = String.length part in
  let rec loop index =
    if part_len = 0 then true
    else if index + part_len > text_len then false
    else if String.sub text index part_len = part then true
    else loop (index + 1)
  in
  loop 0

let header msg_type payload_len =
  let frame_len = payload_len + 1 in
  let raw = Bytes.create 5 in
  Bytes.set raw 0 (Char.chr ((frame_len lsr 24) land 0xff));
  Bytes.set raw 1 (Char.chr ((frame_len lsr 16) land 0xff));
  Bytes.set raw 2 (Char.chr ((frame_len lsr 8) land 0xff));
  Bytes.set raw 3 (Char.chr (frame_len land 0xff));
  Bytes.set raw 4 (Char.chr msg_type);
  Bytes.unsafe_to_string raw

let close fd =
  Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit)

let pair run =
  let left, right =
    Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  Lwt.finalize (fun () -> run left right) (fun () ->
    let open Lwt.Syntax in
    let* () = close left in
    close right)

let preauth_refusal () =
  let payload_len = Frame.handshake_payload_max + 1 in
  let raw = header Frame.msg_bundle_response payload_len in
  pair (fun writer reader ->
    let open Lwt.Syntax in
    let* () = Frame.write_all writer raw in
    Lwt.catch
      (fun () ->
        let* _ = Frame.read_handshake_frame ~timeout_s:0.1 reader in
        Lwt.return_false)
      (function
        | Failure reason ->
          Lwt.return
            (String.equal reason
               "frame_payload_limit size = 65537 max = 65536")
        | _ -> Lwt.return_false))

let identity port =
  let private_key, public_key = Mirage_crypto_ec.Ed25519.generate () in
  let pubkey = Mirage_crypto_ec.Ed25519.pub_to_octets public_key in
  let sign value = Mirage_crypto_ec.Ed25519.sign ~key:private_key value in
  let hello =
    Handshake.make_hello
      ~chain_id:"preauth-test"
      ~node_addr:"127.0.0.1"
      ~pubkey_raw:pubkey
      ~consensus_config_hash:(String.make 32 '\x11')
      ~listen_port:port
      ~best_epoch:0L
      ~best_root:(String.make 32 '\x22')
      ~sign_fn:sign
  in
  hello, sign

let refused = function
  | Handshake.Error reason ->
    contains reason "frame_payload_limit size = 65537 max = 65536"
  | Handshake.Ok _ -> false

let oversized () =
  header Frame.msg_bundle_response (Frame.handshake_payload_max + 1)

let accept_refusal () =
  let hello, sign = identity 9101 in
  pair (fun writer reader ->
    let open Lwt.Syntax in
    let* () = Frame.write_all writer (oversized ()) in
    let* result =
      Handshake.accept_handshake reader
        ~my_hello:hello
        ~allowed_pubkeys:[]
        ~sign_fn:sign
    in
    Lwt.return (refused result))

let dial_refusal () =
  let hello, sign = identity 9102 in
  pair (fun client peer ->
    let open Lwt.Syntax in
    let reject =
      let* frame = Frame.read_frame peer in
      if frame.Frame.msg_type <> Frame.msg_hello then
        Lwt.fail (Failure "dial HELLO is absent")
      else
        Frame.write_all peer (oversized ())
    in
    let* result, () =
      Lwt.both
        (Handshake.dial_handshake client
           ~my_hello:hello
           ~allowed_pubkeys:[]
           ~sign_fn:sign)
        reject
    in
    Lwt.return (refused result))

let finish_refusal () =
  let peer_hello, _ = identity 9103 in
  let local_hello, sign = identity 9104 in
  pair (fun client server ->
    let open Lwt.Syntax in
    let reject =
      let* () =
        Frame.write_frame client
          {
            Frame.msg_type = Frame.msg_hello;
            payload = Handshake.encode_hello peer_hello;
          }
      in
      let* frame = Frame.read_frame client in
      if frame.Frame.msg_type <> Frame.msg_hello_ack then
        Lwt.fail (Failure "HELLO_ACK is absent")
      else
        Frame.write_all client (oversized ())
    in
    let* result, () =
      Lwt.both
        (Handshake.accept_handshake server
           ~my_hello:local_hello
           ~allowed_pubkeys:[]
           ~sign_fn:sign)
        reject
    in
    Lwt.return (refused result))

let handshake_roundtrip () =
  let dial_hello, dial_sign = identity 9105 in
  let accept_hello, accept_sign = identity 9106 in
  pair (fun dial_fd accept_fd ->
    let open Lwt.Syntax in
    let* dial_result, accept_result =
      Lwt.both
        (Handshake.dial_handshake dial_fd
           ~my_hello:dial_hello
           ~allowed_pubkeys:[]
           ~sign_fn:dial_sign)
        (Handshake.accept_handshake accept_fd
           ~my_hello:accept_hello
           ~allowed_pubkeys:[]
           ~sign_fn:accept_sign)
    in
    Lwt.return
      (match dial_result, accept_result with
       | Handshake.Ok dial_peer, Handshake.Ok accept_peer ->
         String.equal dial_peer.Handshake.node_id accept_hello.node_id
         && String.equal accept_peer.Handshake.node_id dial_hello.node_id
       | _ -> false))

let roundtrip read msg_type payload =
  let raw = Frame.encode_frame { Frame.msg_type; payload } in
  pair (fun writer reader ->
    let open Lwt.Syntax in
    let* _, frame =
      Lwt.both
        (Frame.write_all writer raw)
        (read reader)
    in
    Lwt.return
      (frame.Frame.msg_type = msg_type
       && String.equal frame.payload payload))

let timeout_ban () =
  let guard = Guard.create () in
  let key = "127.0.0.1" in
  for index = 0 to 2 do
    ignore
      (Guard.report_bad guard
         ~now:(float_of_int index)
         ~key
         ~reason:"timeout_handshake")
  done;
  Guard.is_banned guard ~now:3.0 ~key

let close_waiters () =
  let module Conn = Octra_net.P2p_conn in
  pair (fun fd _ ->
    let open Lwt.Syntax in
    let conn = Conn.create ~peer_class:Octra_net.P2p_frame_budget.Observer
      fd ~peer_id:"waiting-peer" ~addr:"local" ~direction:Conn.Inbound in
    let frame = Frame.{ msg_type = msg_ping; payload = "queued" } in
    let* () = Conn.send conn frame in
    let cancelled = Conn.send conn frame in
    require (Lwt.is_sleeping cancelled) "first sender did not wait";
    Lwt.cancel cancelled;
    require (Conn.is_connected conn) "sender cancellation closed the peer";
    let waiting = Conn.send conn frame in
    require (Lwt.is_sleeping waiting) "sender did not wait for the queue";
    Lwt.finalize
      (fun () ->
        let* () = Conn.close conn in
        let* () = Lwt.pause () in
        Lwt.return (match Lwt.state waiting with
          | Lwt.Return () -> true
          | Lwt.Sleep | Lwt.Fail _ -> false))
      (fun () ->
        Lwt.cancel waiting;
        Conn.close conn))

let close_idle () =
  let module Conn = Octra_net.P2p_conn in
  pair (fun fd _ ->
    let open Lwt.Syntax in
    let conn = Conn.create ~peer_class:Octra_net.P2p_frame_budget.Observer
      fd ~peer_id:"idle-peer" ~addr:"local" ~direction:Conn.Inbound in
    let writing = Conn.write_loop conn in
    require (Lwt.is_sleeping writing) "writer did not wait for a frame";
    Lwt.finalize
      (fun () ->
        let* () = Conn.close conn in
        let* () = Lwt_unix.with_timeout 1.0 (fun () -> Lwt.protected writing) in
        Lwt.return (not (Conn.is_connected conn)))
      (fun () ->
        Lwt.cancel writing;
        Conn.close conn))

let write_idle () =
  let module Conn = Octra_net.P2p_conn in
  pair (fun fd peer ->
    let open Lwt.Syntax in
    Lwt_unix.setsockopt_int fd Unix.SO_SNDBUF 4096;
    Lwt_unix.setsockopt_int peer Unix.SO_RCVBUF 4096;
    let conn = Conn.create ~peer_class:Octra_net.P2p_frame_budget.Validator
      fd ~peer_id:"paused-peer" ~addr:"local" ~direction:Conn.Inbound in
    let writing = Conn.write_loop ~idle_s:0.05 conn in
    Lwt.finalize
      (fun () ->
        let frame = Frame.{ msg_type = msg_bundle_response;
          payload = String.make (2 * 1024 * 1024) 'p' } in
        let* () = Conn.send conn frame in
        let* () = Lwt_unix.with_timeout 2.0 (fun () -> Lwt.protected writing) in
        require (not (Conn.is_connected conn)) "idle write kept the peer open";
        require (conn.msg_count_out = 0) "partial frame counted as sent";
        let* () = Conn.send conn frame in
        require (Lwt_mvar.is_empty conn.write_queue) "closed peer queued a frame";
        Lwt.catch
          (fun () ->
            let* _ = Frame.read_frame ~timeout_s:1.0 peer in
            Lwt.return_false)
          (function
            | Failure reason -> Lwt.return (reason = "connection_closed")
            | _ -> Lwt.return_false))
      (fun () ->
        Lwt.cancel writing;
        Conn.close conn))

let write_progress () =
  pair (fun writer reader ->
    let open Lwt.Syntax in
    Lwt_unix.setsockopt_int writer Unix.SO_SNDBUF 4096;
    Lwt_unix.setsockopt_int reader Unix.SO_RCVBUF 4096;
    let payload = String.make (256 * 1024) 'r' in
    let received = Bytes.create (String.length payload) in
    let rec read off =
      if off = Bytes.length received then Lwt.return_unit
      else
        let* count = Lwt_unix.read reader received off
          (min 4096 (Bytes.length received - off)) in
        require (count > 0) "progressing writer closed early";
        let* () = Lwt_unix.sleep 0.02 in
        read (off + count)
    in
    let started = Mtime_clock.elapsed_ns () in
    let writing =
      let* () = Frame.write_all ~idle_s:0.3 writer payload in
      Lwt.return (Int64.sub (Mtime_clock.elapsed_ns ()) started)
    in
    let* elapsed, () = Lwt.both writing (read 0) in
    Lwt.return (Int64.compare elapsed 300_000_000L > 0
      && Bytes.to_string received = payload))

let write_interval () =
  pair (fun writer _ ->
    Lwt.return (List.for_all (fun idle_s ->
      try
        ignore (Frame.write_all ~idle_s writer "");
        false
      with Invalid_argument reason -> reason = "invalid write idle interval")
      [0.; -1.; Float.nan; Float.infinity]))

let wait_release () =
  let module Conn = Octra_net.P2p_conn in
  pair (fun fd _ ->
    let open Lwt.Syntax in
    let conn = Conn.create ~peer_class:Octra_net.P2p_frame_budget.Validator
      fd ~peer_id:"queue-peer" ~addr:"local" ~direction:Conn.Inbound in
    let proxies = Weak.create 3 in
    let run n =
      let wait, wake = Lwt.task () in
      if n = 0 then Lwt.wakeup wake ();
      let proxy = Lwt.protected conn.closed in
      Weak.set proxies n (Some proxy);
      let selected = Lwt.pick [
        Lwt.map Option.some wait;
        Lwt.map (fun () -> None) proxy;
      ] in
      if n = 1 then Lwt.wakeup wake ();
      if n = 2 then Lwt.cancel selected;
      Lwt.catch (fun () -> Lwt.map (fun _ -> ()) selected) (function
        | Lwt.Canceled when n = 2 -> Lwt.return_unit
        | error -> Lwt.fail error)
    in
    Lwt.finalize
      (fun () ->
        let* () = Lwt_list.iter_s run [0; 1; 2] in
        Gc.full_major ();
        Gc.full_major ();
        Lwt.return (List.for_all (fun n -> not (Weak.check proxies n)) [0; 1; 2]
          && Conn.is_connected conn
          && Lwt.state conn.closed = Lwt.Sleep))
      (fun () -> Conn.close conn))

let swarm port =
  let port = if port <> 0 then port else
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Fun.protect ~finally:(fun () -> Unix.close socket) (fun () ->
      Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      match Unix.getsockname socket with
      | Unix.ADDR_INET (_, port) -> port
      | _ -> failwith "listener address differs") in
  let hello, sign_fn = identity port in
  let state = Swarm.create {
    listen_port = port;
    chain_id = hello.chain_id;
    node_id = hello.node_id;
    node_addr = "127.0.0.1";
    pubkey_raw = hello.pubkey;
    consensus_config_hash = hello.consensus_config_hash;
    binary_hash = String.make 32 '\000';
    require_binary_hash = false;
    upgrade_plan = None;
    profile_plan = [];
    allowed_pubkeys = [];
    bootstrap_peers = [];
    max_peers = 8;
    sign_fn;
    best_epoch_fn = (fun () -> 0L);
    best_root_fn = (fun () -> hello.best_root);
  } in
  state.running <- true;
  state

let accept_delays () =
  let retry = [Unix.EINTR; Unix.EAGAIN; Unix.EWOULDBLOCK;
    Unix.ECONNABORTED; Unix.ENETDOWN; Unix.ENETUNREACH; Unix.EHOSTDOWN;
    Unix.EHOSTUNREACH; Unix.ENOPROTOOPT; Unix.EOPNOTSUPP; Unix.EMFILE;
    Unix.ENFILE; Unix.ENOBUFS; Unix.ENOMEM]
    @ List.map (fun code -> Unix.EUNKNOWNERR code) Octra_net.P2p_conn.accept_codes in
  List.iter (fun reason ->
    let error = Unix.Unix_error (reason, "accept", "") in
    require (Octra_net.P2p_conn.accept_delay min_int error = Some 0.01
      && Octra_net.P2p_conn.accept_delay 0 error = Some 0.01
      && Octra_net.P2p_conn.accept_delay 7 error = Some 1.
      && Octra_net.P2p_conn.accept_delay max_int error = Some 1.)
      "accept delay differs") retry;
  List.iter (fun error ->
    require (Octra_net.P2p_conn.accept_delay 0 error = None) "fatal accept retried")
    [Lwt.Canceled; Failure "accept";
     Unix.Unix_error (Unix.EBADF, "accept", "");
     Unix.Unix_error (Unix.EINVAL, "accept", "");
     Unix.Unix_error (Unix.ENOTSOCK, "accept", "");
     Unix.Unix_error (Unix.EUNKNOWNERR (-1), "accept", "")]

let accept_resume () =
  let open Lwt.Syntax in
  let state = swarm 0 in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let first, first_fd = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let second, second_fd = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let pending, _ = Lwt.task () in
  let calls = ref 0 in
  let delays = ref [] in
  let take _ =
    incr calls;
    match !calls with
    | 1 | 5 -> raise (Unix.Unix_error (Unix.EMFILE, "accept", ""))
    | 2 | 3 | 6 -> Lwt.fail (Unix.Unix_error (Unix.ENOBUFS, "accept", ""))
    | 4 -> Lwt.return (first_fd, Unix.ADDR_UNIX "first")
    | 7 -> Lwt.return (second_fd, Unix.ADDR_UNIX "second")
    | _ -> pending in
  let sleep delay = delays := delay :: !delays; Lwt.pause () in
  let received, deliver = Lwt.task () in
  Swarm.set_handler state (fun _ frame ->
    if frame.Frame.msg_type = Frame.msg_tx_gossip
      && frame.payload = "recovered" && Lwt.is_sleeping received then
      Lwt.wakeup_later deliver ();
    Lwt.return_unit);
  let listening = Swarm.listen ~socket ~take ~sleep state in
  let connect client port =
    let hello, sign = identity port in
    let* result = Handshake.dial_handshake client ~my_hello:hello
      ~allowed_pubkeys:[] ~sign_fn:sign in
    require (match result with Handshake.Ok _ -> true | _ -> false)
      "recovered listener rejected handshake";
    Lwt.return_unit in
  Lwt.finalize
    (fun () ->
      let* () = connect first 9107 in
      let* () = connect second 9108 in
      let* () = Frame.write_frame second
        { Frame.msg_type = Frame.msg_tx_gossip; payload = "recovered" } in
      let* () = Lwt_unix.with_timeout 2.0 (fun () -> received) in
      require (Swarm.connected_count state = 2) "recovered peers missing";
      require (List.rev !delays = [0.01; 0.02; 0.04; 0.01; 0.02])
        "accept retry did not reset";
      let* () = Swarm.stop state in
      let* () = listening in
      require (Lwt_unix.state socket = Lwt_unix.Closed) "listener socket retained";
      require (Swarm.connected_count state = 0) "stopped peers remain connected";
      Lwt.return_unit)
    (fun () ->
      let* () = Swarm.stop state in
      Lwt_list.iter_s close [first; second; first_fd; second_fd; socket])

let accept_cancel ~halt sleeping =
  let open Lwt.Syntax in
  let state = swarm 0 in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let paused, _ = Lwt.task () in
  let entered, enter = Lwt.task () in
  let take _ =
    if sleeping then Lwt.fail (Unix.Unix_error (Unix.EMFILE, "accept", ""))
    else begin
      Lwt.wakeup_later enter ();
      Lwt.map (fun () -> failwith "cancelled accept resumed") paused
    end in
  let sleep _ = Lwt.wakeup_later enter (); paused in
  let listening = Swarm.listen ~socket ~take ~sleep state in
  Lwt.finalize
    (fun () ->
      let* () = entered in
      let* () = if halt then Swarm.stop state else begin
        Lwt.cancel listening;
        Lwt.catch
          (fun () ->
            let* () = listening in
            Lwt.fail_with "listener cancellation was swallowed")
          (function Lwt.Canceled -> Lwt.return_unit | error -> Lwt.fail error)
      end in
      let* () = if halt then listening else Lwt.return_unit in
      let* () = Swarm.stop state in
      require (Lwt.state paused = Lwt.Fail Lwt.Canceled)
        "listener stop left pending work";
      require (Lwt_unix.state socket = Lwt_unix.Closed)
        "listener stop left socket open";
      Lwt.return_unit)
    (fun () -> Swarm.stop state)

let accept_fatal () =
  let open Lwt.Syntax in
  let state = swarm 0 in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let error = Unix.Unix_error (Unix.EINVAL, "accept", "") in
  let entered, enter = Lwt.task () in
  let take _ = Lwt.wakeup_later enter (); Lwt.fail error in
  let listener = Swarm.listen ~socket ~take state in
  let idle, _ = Lwt.task () in
  let tasks = Swarm.run_tasks listener [Lwt.return_unit; idle] in
  let* () = entered in
  let* () = Lwt.catch
    (fun () ->
      let* () = Lwt_unix.with_timeout 1.0 (fun () -> tasks) in
      Lwt.fail_with "fatal accept was hidden")
    (fun found -> if found = error then Lwt.return_unit else Lwt.fail found) in
  require (Lwt_unix.state socket = Lwt_unix.Closed) "fatal accept retained socket";
  require (Lwt.state idle = Lwt.Fail Lwt.Canceled) "fatal accept retained service";
  Swarm.stop state

let bootstrap_finish () =
  let open Lwt.Syntax in
  let listening, finish = Lwt.task () in
  let idle, _ = Lwt.task () in
  let tasks = Swarm.run_tasks listening [Lwt.return_unit; idle] in
  let* () = Lwt.pause () in
  require (Lwt.is_sleeping tasks) "bootstrap completion stopped listener";
  Lwt.wakeup_later finish ();
  let* () = tasks in
  require (Lwt.state idle = Lwt.Fail Lwt.Canceled) "listener completion retained service";
  Lwt.return_unit

let listen_busy () =
  let open Lwt.Syntax in
  let occupied = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.bind occupied (Unix.ADDR_INET (Unix.inet_addr_any, 0)) in
  Lwt_unix.listen occupied 1;
  let port = match Lwt_unix.getsockname occupied with
    | Unix.ADDR_INET (_, port) -> port
    | _ -> failwith "listener address differs" in
  let state = swarm port in
  Lwt.finalize
    (fun () ->
      let* () = Lwt.catch
        (fun () ->
          let* () = Swarm.listen ~socket state in
          Lwt.fail_with "occupied listener accepted")
        (function
          | Unix.Unix_error (Unix.EADDRINUSE, _, _) -> Lwt.return_unit
          | error -> Lwt.fail error) in
      require (Lwt_unix.state socket = Lwt_unix.Closed) "bind failure retained socket";
      Lwt.return_unit)
    (fun () -> let* () = Swarm.stop state in close occupied)

let late_handshake () =
  let open Lwt.Syntax in
  let state = swarm 0 in
  let client, server = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let waiting, _ = Lwt.task () in
  let handed = ref false in
  let take _ =
    if !handed then waiting else begin
      handed := true;
      Lwt.return (server, Unix.ADDR_UNIX "late")
    end in
  let listening = Swarm.listen ~take state in
  Lwt.finalize
    (fun () ->
      let* () = Lwt.pause () in
      require !handed "late handshake not started";
      let* () = Swarm.stop state in
      let* () = listening in
      require (Lwt_unix.state server = Lwt_unix.Closed)
        "stopped swarm retained handshake socket";
      require (state.pending = []) "stopped swarm retained handshake task";
      state.running <- true;
      require (Swarm.connected_count state = 0) "late handshake registered peer";
      Lwt.return_unit)
    (fun () ->
      let* () = Swarm.stop state in
      let* () = close client in
      close server)

let old_peer () =
  let module Conn = Octra_net.P2p_conn in
  let open Lwt.Syntax in
  let state = swarm 0 in
  let generation = state.generation in
  let hello, _ = identity 9110 in
  pair (fun fd _ ->
    let conn = Conn.create ~peer_class:Octra_net.P2p_frame_budget.Validator
      fd ~peer_id:hello.node_id ~addr:"old" ~direction:Conn.Inbound in
    let* () = Swarm.stop state in
    state.running <- true;
    require (not (Swarm.add_peer state ~generation conn hello))
      "previous generation registered peer";
    let* () = Lwt.pause () in
    require (Swarm.connected_count state = 0 && not (Conn.is_connected conn))
      "previous generation retained peer";
    Lwt.return true)

let bootstrap_stop () =
  let open Lwt.Syntax in
  let base = swarm 0 in
  let remote = swarm 0 in
  let endpoint = Printf.sprintf "127.0.0.1:%d" remote.config.listen_port in
  let state = Swarm.create { base.config with bootstrap_peers = [endpoint] } in
  let rec wait () =
    if Hashtbl.mem state.dialing endpoint && List.length state.pending = 1 then
      Lwt.return_unit
    else let* () = Lwt_unix.sleep 0.001 in wait () in
  let run () =
    let task = Swarm.start state ~on_message:(fun _ _ -> Lwt.return_unit) in
    Lwt.finalize
      (fun () ->
        let* () = wait () in
        let* () = Swarm.stop state in
        let* () = task in
        require (state.pending = [] && Hashtbl.length state.dialing = 0)
          "stopped bootstrap retained work";
        Lwt.return_unit)
      (fun () -> Lwt.cancel task; Swarm.stop state) in
  let* () = run () in
  run ()

let dial_stop () =
  let open Lwt.Syntax in
  let state = swarm 0 in
  let listener = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)) in
  Lwt_unix.listen listener 1;
  let port = match Lwt_unix.getsockname listener with
    | Unix.ADDR_INET (_, port) -> port
    | _ -> failwith "dial address differs" in
  let task = Swarm.dial state "127.0.0.1" port in
  Lwt.finalize
    (fun () ->
      let* server, _ = Lwt_unix.accept listener in
      Lwt.finalize
        (fun () ->
          let* _ = Frame.read_handshake_frame ~timeout_s:1.0 server in
          let error = Failure "listener failed" in
          let* () = Lwt.catch
            (fun () -> Swarm.run_tasks ~stop:(fun () -> Swarm.stop state)
              (Lwt.fail error) [Lwt.map (fun _ -> ()) task])
            (fun caught -> if caught == error then Lwt.return_unit else Lwt.fail caught) in
          require (state.pending = [] &&
            (Lwt.state task = Lwt.Fail Lwt.Canceled || Lwt.state task = Lwt.Return None))
            "stopped dial retained work";
          Lwt.catch
            (fun () ->
              let* _ = Frame.read_handshake_frame ~timeout_s:1.0 server in
              Lwt.fail_with "stopped dial retained socket")
            (function
              | Failure reason when reason = "connection_closed" -> Lwt.return_unit
              | error -> Lwt.fail error))
        (fun () -> close server))
    (fun () ->
      let* () = Swarm.stop state in
      close listener)

let swarm_stop () =
  let open Lwt.Syntax in
  let state = swarm 0 in
  let run () =
    let task = Swarm.start state ~on_message:(fun _ _ -> Lwt.return_unit) in
    let duplicate = try
      ignore (Swarm.start state ~on_message:(fun _ _ -> Lwt.return_unit));
      false
    with Invalid_argument _ -> true in
    require duplicate "duplicate swarm start accepted";
    let client = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Lwt.finalize
      (fun () ->
        let* () = Lwt_unix.connect client
          (Unix.ADDR_INET (Unix.inet_addr_loopback, state.config.listen_port)) in
        let hello, sign_fn = identity 9111 in
        let* result = Handshake.dial_handshake client ~my_hello:hello
          ~allowed_pubkeys:[] ~sign_fn in
        require (match result with Handshake.Ok _ -> true | _ -> false)
          "real listener rejected handshake";
        let* () = Lwt.pause () in
        let* () = Swarm.stop state in
        let* () = Lwt_unix.with_timeout 1.0 (fun () -> task) in
        require (not state.running && Swarm.connected_count state = 0)
          "swarm shutdown incomplete";
        Lwt.return_unit)
      (fun () ->
        Lwt.cancel task;
        let* () = Swarm.stop state in
        close client) in
  let* () = run () in
  run ()

let run_case name run =
  try Lwt_main.run (Lwt_unix.with_timeout 5.0 run)
  with error -> failwith (name ^ ": " ^ Printexc.to_string error)

let () =
  require (Lwt_main.run (preauth_refusal ())) "pre-auth payload accepted";
  require (Lwt_main.run (accept_refusal ())) "accept path payload accepted";
  require (Lwt_main.run (dial_refusal ())) "dial path payload accepted";
  require (Lwt_main.run (finish_refusal ())) "finish path payload accepted";
  require (Lwt_main.run (handshake_roundtrip ())) "handshake roundtrip failed";
  require
    (Lwt_main.run
       (roundtrip
          (Frame.read_handshake_frame ~timeout_s:1.0)
          Frame.msg_hello
          "hello"))
    "HELLO frame refused";
  require
    (Lwt_main.run
       (roundtrip
          (Frame.read_frame ~timeout_s:1.0)
          Frame.msg_bundle_response
          (String.make (Frame.handshake_payload_max + 1) 'x')))
    "post-auth frame refused";
  require
    (Guard.handshake_penalty "handshake timeout"
      = Some "timeout_handshake")
    "handshake timeout is unscored";
  require
    (Guard.handshake_penalty
       "handshake failed: frame_payload_limit size = 65537 max = 65536"
      = Some "invalid_frame_handshake")
    "pre-auth payload refusal is unscored";
  require (timeout_ban ()) "handshake timeout ban is absent";
  require (Lwt_main.run (close_waiters ())) "closed peer retains pending sender";
  require (Lwt_main.run (close_idle ())) "closed peer retains idle writer";
  require (Lwt_main.run (write_idle ())) "idle write did not close cleanly";
  require (Lwt_main.run (Lwt_unix.with_timeout 10.0 write_progress))
    "write progress did not renew the interval";
  require (Lwt_main.run (write_interval ())) "invalid write interval accepted";
  require (Lwt_main.run (wait_release ())) "peer queue retained completed waits";
  accept_delays ();
  run_case "accept_resume" accept_resume;
  run_case "accept_stop" (fun () -> accept_cancel ~halt:true false);
  run_case "backoff_stop" (fun () -> accept_cancel ~halt:true true);
  run_case "accept_cancel" (fun () -> accept_cancel ~halt:false false);
  run_case "backoff_cancel" (fun () -> accept_cancel ~halt:false true);
  run_case "accept_fatal" accept_fatal;
  run_case "bootstrap_finish" bootstrap_finish;
  run_case "listen_busy" listen_busy;
  run_case "late_handshake" late_handshake;
  run_case "swarm_stop" swarm_stop;
  run_case "old_peer" (fun () ->
    let open Lwt.Syntax in
    let* result = old_peer () in
    require result "previous generation accepted";
    Lwt.return_unit);
  run_case "bootstrap_stop" bootstrap_stop;
  run_case "dial_stop" dial_stop;
  Printf.printf "p2p_preauth = pass cases = 30\n%!"