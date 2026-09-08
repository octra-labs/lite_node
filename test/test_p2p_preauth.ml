(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Frame = Octra_net.P2p_frame
module Guard = Octra_net.P2p_peer_guard
module Handshake = Octra_net.P2p_handshake

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
  Printf.printf "p2p_preauth = pass cases = 10\n%!"