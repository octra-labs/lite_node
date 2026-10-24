(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module N = Octra_net
module R = Octra_node_runtime
module V = Octra_vm
module S = Octra_core.Store_irmin

let check value reason = if not value then failwith reason
let run = Lwt_main.run

let header ?(root = String.make 64 'a') count =
  N.Oce1.encode (fun b ->
    N.Oce1.put_u64 b 42L;
    N.Oce1.put_string b root;
    N.Oce1.put_u32 b 1l;
    N.Oce1.put_string b ("oct" ^ String.make 44 '1');
    N.Oce1.put_u32 b count)

let legacy () =
  let payload = header 1l ^ N.Oce1.encode (fun b -> N.Oce1.put_string b "{not JSON") in
  check (R.P2p_epoch_broadcast.observer_event ~observer:false "" =
    Ok R.P2p_epoch_broadcast.Silent) "validator read discarded frame";
  let body = payload ^ String.make (8 * 1024 * 1024 - String.length payload) '[' in
  let before = Gc.allocated_bytes () in
  let value = R.P2p_epoch_broadcast.observer_event ~observer:true body in
  check (Gc.allocated_bytes () -. before < 10_000.) "observer decoded epoch body";
  (match value with
   | Ok (R.P2p_epoch_broadcast.Ignored row) ->
     check (row.epoch = 42 && row.tx_count = 1) "epoch header changed"
   | _ -> failwith "observer parsed discarded JSON");
  List.iter (fun payload ->
    check (Result.is_error (R.P2p_epoch_broadcast.observer_event ~observer:true payload))
      "invalid epoch header accepted") [""; header (-1l); header ~root:(String.make 257 'a') 0l];
  let reports = ref 0 in
  let deps = R.P2p_swarm_dispatch.{observer = true; peer_id = "peer";
    on_consensus = (fun () -> failwith "legacy reached consensus");
    on_resource_compute = (fun () -> failwith "legacy reached compute");
    tx = {guard = N.P2p_tx_gossip_guard.create (); has_tx = (fun _ -> false);
      find_tx = (fun _ -> None); sender_pk = (fun _ -> None); add_tx = (fun _ -> Error "unused");
      send_payload = ignore; send_batch = ignore; broadcast_payload = ignore;
      report_bad_peer = (fun _ -> incr reports); close_peer = (fun () -> ());
      now = (fun () -> 0.); signing = (fun () -> Octra_core.Tx_signature.Historical); max_drift = 1.}} in
  R.P2p_swarm_dispatch.handle_frame deps N.P2p_frame.{msg_type = msg_epoch_broadcast; payload = ""};
  check (!reports = 1) "bad epoch header not reported"

let batch () =
  let calls = ref 0 in
  let names = [|"octra_programAbi"; "octra_contractAbi";
    "circle_program_info"; "octra_circleProgramInfo";
    "circle_program_info_auth"; "octra_circleProgramInfoAuth"|] in
  let routes = List.map (fun name -> name, (fun _ () -> incr calls; Lwt.return_ok `Null))
    ("other" :: Array.to_list names) in
  let body = `List (List.init 100 (fun i -> `Assoc ["jsonrpc", `String "2.0";
    "method", `String (if i = 99 then "other" else names.(i mod Array.length names));
    "params", `List []; "id", `Int i]))
    |> Yojson.Safe.to_string in
  let meta = R.Rpc_http.{rpc_peer = "test"; rpc_user_agent = "test"; rpc_body_bytes = 0} in
  let requests = match Octra_core.Rpc.parse_body body with
    | Ok (`Batch values) -> values | _ -> failwith "batch input refused" in
  let plan = R.Rpc_dispatch.batch_plan requests in
  check (List.filter fst plan |> List.length = 5) "batch plan allowance differs";
  check (!calls = 0) "batch plan performed effects";
  let response = run (R.Rpc_dispatch.process_body meta body () routes) in
  check (!calls = 5) "ABI batch exceeded work allowance";
  check (match response with `List values -> List.length values = 100 | _ -> false)
    "batch lost responses"

let broadcast () =
  let module W = N.P2p_swarm in
  let module C = N.P2p_conn in
  let key = String.make 32 'v' in
  let validator = N.P2p_handshake.node_id_of_pubkey key in
  let swarm = W.create {listen_port = 0; chain_id = "test";
    node_id = "node"; node_addr = "node"; pubkey_raw = key;
    consensus_config_hash = key; binary_hash = key; require_binary_hash = false;
    upgrade_plan = None; profile_plan = []; allowed_pubkeys = [];
    bootstrap_peers = []; max_peers = 4; sign_fn = (fun _ -> String.make 64 's');
    best_epoch_fn = (fun () -> 0L); best_root_fn = (fun () -> key)} in
  check (Result.is_ok (W.set_validator_pubkeys swarm [key])) "validator set refused";
  let connect name =
    let fd = run (Lwt_unix.openfile "/dev/null" [Unix.O_RDONLY] 0) in
    let conn = C.create ~peer_class:(if name = validator then N.P2p_frame_budget.Validator
      else N.P2p_frame_budget.Observer) fd ~peer_id:name ~addr:name ~direction:C.Inbound in
    Hashtbl.add swarm.peers name conn;
    conn in
  let voting = connect validator and observer = connect "observer" in
  Fun.protect ~finally:(fun () -> run (Lwt.join [C.close voting; C.close observer])) (fun () ->
    let frame = N.P2p_frame.{msg_type = msg_epoch_broadcast; payload = header 0l} in
    List.iter (fun send ->
      run (send frame);
      check (Lwt_mvar.is_empty voting.write_queue) "legacy broadcast reached validator";
      check (not (Lwt_mvar.is_empty observer.write_queue)) "legacy broadcast missed observer";
      check (run (Lwt_mvar.take observer.write_queue) = frame) "legacy bytes changed")
      [W.broadcast swarm; W.broadcast_except swarm ~except:"absent"])

let abi () =
  Test_workspace.with_dir "read_work" (fun path ->
    let store = run (S.open_store (Filename.concat path "store")) in
    let data = Octra_core.Store_chaindata.open_chaindata (Filename.concat path "data") in
    Fun.protect ~finally:(fun () -> run (S.close store); Octra_core.Store_chaindata.close data)
      (fun () ->
        run (S.set_meta store "last_epoch" "0");
        let address = "oct" ^ String.make 44 '1' in
        let compile n =
          let result = V.Oct_compile.compile_program
            (Printf.sprintf "program Read { public view fn value%d(): int { return %d } }" n n) in
          check (result.error = None) "ABI source failed";
          Option.get result.program_envelope in
        let install raw = run (S.deploy_contract store ~address
          ~code_hash:Digestif.SHA256.(digest_string raw |> to_hex)
          ~version:"1" ~owner:address ~ctype:"CUSTOM" ~check:"source"
          ~bytecode_b64:(Base64.encode_exn raw)) in
        install (compile 1);
        let read () = V.Contract_rpc.abi_params ~trusted:[] ~point_ops:true
          ~store ~chaindata:data (`List [`String address]) in
        let first_work = read () in
        check (Lwt.is_sleeping first_work) "ABI did not yield to event loop";
        let refused = run (read ()) in
        check (match refused with Error e -> e.Octra_core.Rpc.code = 107 | _ -> false)
          "ABI reader queued concurrent work";
        let first = run first_work in
        check (Result.is_ok first && run (read ()) = first) "warm ABI differs";
        let first_methods = run (V.Contract.methods_async ~trusted:[] ~point_ops:true store address)
          |> Option.get in
        let second_methods = run (V.Contract.methods_async ~trusted:[] ~point_ops:true store address)
          |> Option.get in
        check (first_methods == second_methods) "ABI methods scanned again";
        let worker = run (V.Contract.read_async store address
          (fun _ _ -> Some (Thread.id (Thread.self ())))) |> Option.get in
        check (worker <> Thread.id (Thread.self ())) "ABI decode ran on event loop";
        let snapshot = run (S.capture_read_snapshot store) |> Result.get_ok in
        install (compile 2);
        check (run (read ()) <> first) "ABI reused changed code";
        check (run (V.Contract.methods_async ~snapshot ~trusted:[] ~point_ops:true store address)
          = Some first_methods) "ABI ignored pinned snapshot";
        run (Lwt_mutex.lock V.Contract_rpc.abi_lock);
        check (match run (read ()) with Error e -> e.Octra_core.Rpc.code = 107 | _ -> false)
          "ABI capacity ignored";
        Lwt_mutex.unlock V.Contract_rpc.abi_lock))

let circle () =
  let module C = Octra_core.Circles in
  let module P = Octra_circle_runtime.Circle_program in
  check (run (Octra_core.Exec_resource.joined (fun () -> Thread.id (Thread.self ())) ()) <> Thread.id (Thread.self ()))
    "Circle code check ran on event loop";
  Test_workspace.with_dir "read_circle" (fun path ->
    let store = run (S.open_store (Filename.concat path "store")) in
    Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
      let circle_id = "read_circle" in
      let regs = List.init 255 (fun _ -> 0) in
      let raw = V.Bytecode.encode
        (Array.init 5801 (fun n -> if n = 0 then V.Contract_vm.STOP else V.Contract_vm.EMIT ("e", regs))) in
      let info = C.{circle_id; runtime = Octb; version = 1L; owner = "owner";
        code_hash = sha256_hex raw; stable_root = zero_hash_hex; assets_root = zero_hash_hex;
        privacy_class = Public; browser_mode = Native_sealed; resource_mode = Public_resources;
        policy_hash = None; members_root = None; export_policy = None; limits = default_limits} in
      run (S.deploy_circle store info);
      run (S.save_circle_program_code_b64 store circle_id (Base64.encode_exn raw));
      let read () = R.Circle_read_rpc.program_descriptor ~table_mode:Octra_core.Rule_graph.Prior
        store ~circle_id in
      let work = read () in
      check (Lwt.is_sleeping work) "Circle descriptor blocked event loop";
      check (match run (read ()) with Error e -> e.Octra_core.Rpc.code = 107 | _ -> false)
        "Circle descriptor queued concurrent work";
      let first = run work in
      check (Result.is_ok first && run (read ()) = first) "Circle descriptor changed after caching";
      let methods () = V.Contract.methods_checked ~source:false ~trusted:[] ~point_ops:true raw
        |> Result.get_ok in
      check (methods () == methods ()) "large Circle methods scanned again";
      let loaded () = run (P.load ~table_mode:Octra_core.Rule_graph.Prior ~point_ops:true store circle_id)
        |> Result.get_ok in
      let first = loaded () and second = loaded () in
      check (match first.code, second.code with
        | P.Octb a, P.Octb b -> a.bytecode <> [||] && a.bytecode = b.bytecode && a.bytecode != b.bytecode
        | _ -> false) "Circle load shares writable array";
      let module E = Octra_core.Exec_resource in
      List.iter (fun (error, expected) ->
        let calls = ref 0 in
        let launch _ _ = incr calls; raise error in
        let work = E.run ~hash:"checked" (fun () ->
          P.load ~launch ~table_mode:Octra_core.Rule_graph.Prior ~point_ops:true store circle_id) in
        let outcome = run (Lwt.catch
          (fun () -> Lwt.map (fun _ -> false) work)
          (function E.Exhausted ("checked", resource) -> Lwt.return (resource = expected)
            | error -> Lwt.fail error)) in
        check (outcome && !calls = 1) "Circle worker launch became transaction result")
        [Unix.Unix_error (Unix.EAGAIN, "Thread.create", ""), E.Host;
         Failure "Thread.create", E.Host; Out_of_memory, E.Memory; Stack_overflow, E.Stack]))

let () = legacy (); broadcast (); batch (); abi (); circle (); print_endline "status = pass test = read_work"