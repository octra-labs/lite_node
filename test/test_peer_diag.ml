(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Guard = Octra_net.P2p_peer_guard
module Swarm = Octra_net.P2p_swarm
module Handshake = Octra_net.P2p_handshake
module View = Octra_node_runtime.Rpc_view

let require ok reason = if not ok then failwith reason

let score_json () =
  let guard = Guard.create () in
  ignore (Guard.admit_conn guard ~now:5. ~key:"peer");
  for _ = 1 to 3 do ignore (Guard.report_bad guard ~now:5. ~key:"peer" ~reason:"bad_signature") done;
  Guard.report_hold guard ~now:5. ~key:"peer" ~held_s:9.;
  Guard.report_hold guard ~now:5. ~key:"peer" ~held_s:2.;
  ignore (Guard.note_drop guard ~now:5. ~key:"peer" ~reason:"peer_capacity");
  List.iter (fun (now, held, pause) ->
    let row = List.hd (Guard.snapshot ~now guard) in
    let diag = Octra_net.P2p_peer_diag.make ~connected:[] ~records:[] ~scores:[row] () in
    let _, scores = View.peer_diag (Some diag) ~now in
    let json = List.hd scores in
    let get key = Yojson.Safe.Util.member key json in
    List.iter (fun (key, value) -> require (get key = value) ("peer score field: " ^ key))
      ["held_s", `Float held; "pause_until", `Float 15.; "pause_remaining_sec", `Float pause;
        "last_reason", `String "bad_signature"; "bad_count", `Int 3;
        "drop_reason", `String (if now < 15. then "peer_capacity" else "");
        "drop_count", `Int (if now < 15. then 1 else 0);
        "conn_count", `Int (if now < 15. then 1 else 0);
        "ban_until", `Float 125.; "ban_remaining_sec", `Float (max 0. (125. -. now))])
    [5., 11., 10.; 14., 11., 1.; 15., 0., 0.; 200., 0., 0.]

let refusal_logs () =
  Mirage_crypto_rng_unix.use_default ();
  let secret, public = Mirage_crypto_ec.Ed25519.generate () in
  let pubkey_raw = Mirage_crypto_ec.Ed25519.pub_to_octets public in
  let config : Swarm.config = {
    listen_port = 9100; chain_id = "peer-diag";
    node_id = Handshake.node_id_of_pubkey pubkey_raw; node_addr = "local";
    pubkey_raw; consensus_config_hash = String.make 32 'c';
    binary_hash = String.make 32 'b'; require_binary_hash = false;
    upgrade_plan = None; profile_plan = []; allowed_pubkeys = [];
    bootstrap_peers = ["203.0.113.1:9100"]; max_peers = 16;
    sign_fn = (fun value -> Mirage_crypto_ec.Ed25519.sign ~key:secret value);
    best_epoch_fn = (fun () -> 1L); best_root_fn = (fun () -> String.make 32 'r');
  } in
  let swarm = Swarm.create config in
  swarm.running <- true;
  Test_workspace.with_dir "peer-diag" (fun directory ->
    let file = Filename.concat directory "events" in
    let saved = Unix.dup Unix.stdout in
    let output = Unix.openfile file [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600 in
    let sockets = ref [] in
    Fun.protect ~finally:(fun () ->
      flush stdout; Unix.dup2 saved Unix.stdout; Unix.close saved; Unix.close output;
      Lwt_main.run (Lwt_list.iter_s Swarm.close_socket !sockets)) (fun () ->
      flush stdout; Unix.dup2 output Unix.stdout;
      List.iter (fun host ->
        Guard.report_hold swarm.peer_guard ~now:(Unix.gettimeofday ()) ~key:host ~held_s:9.;
        Guard.report_hold swarm.peer_guard ~now:(Unix.gettimeofday ()) ~key:host ~held_s:2.;
        for _ = 1 to 7 do
          let client, server = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
          sockets := client :: server :: !sockets;
          Swarm.accept swarm server (Unix.ADDR_INET (Unix.inet_addr_of_string host, 9100))
        done) ["203.0.113.1"; "203.0.113.2"];
      flush stdout);
    let input = open_in file in
    let lines = Fun.protect ~finally:(fun () -> close_in input) (fun () ->
      let rec read rows = match input_line input with
        | line -> read (line :: rows) | exception End_of_file -> List.rev rows in
      read []) in
    require (List.length lines = 3) "known refusal log count differs";
    List.iter2 (fun line count ->
      require (Guard.contains line ("event = handshake_refused host = 203.0.113.1 reason = handshake_budget count = " ^ string_of_int count))
        "refusal log fields differ") lines [1; 2; 4];
    require (swarm.accepting = [] && swarm.pending = []) "refused sockets started handshakes")

let () =
  score_json ();
  refusal_logs ();
  print_endline "status = pass test = peer_diag"