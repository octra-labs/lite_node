(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Core = Octra_core
module Rule = Core.Rule_graph
module Epoch = Core.Epoch_exec
module Store = Core.Store_irmin
module Ledger = Core.Ledger
module Circle = Core.Circles
module Tx = Core.Transaction
module Route = Octra_node_runtime.Consensus_vm_transition

let check label ok = if not ok then failwith label

let rec size n =
  let byte = n land 127 in
  if n < 128 then String.make 1 (Char.chr byte)
  else String.make 1 (Char.chr (byte lor 128)) ^ size (n lsr 7)

let text value = size (String.length value) ^ value
let section tag value = String.make 1 (Char.chr tag) ^ text value
let vector items = size (List.length items) ^ String.concat "" items
let body value = text ("\000" ^ value ^ "\011")
let export name kind index = text name ^ String.make 1 (Char.chr kind) ^ size index

let code ?offset ?(functions = 3) elements =
  let manifest = "{\"methods\":[]}" in
  let response = "OCWS1\000\000\000\000\000" in
  let write offset length = "\065" ^ size offset ^ "\065" ^ size length
    ^ "\016\000\026\065\000" in
  Base64.encode_exn (String.concat "" [
    "\000asm\001\000\000\000";
    section 1 (vector ["\096\002\127\127\001\127"; "\096\001\127\001\127"]);
    section 2 (vector [text "octra" ^ text "host_response_write" ^ "\000\000"]);
    section 3 (vector (["\001"; "\000"; "\000"] @ List.init (functions - 3) (fun _ -> "\001")));
    section 4 (vector ["\112\000" ^ size elements]);
    section 5 (vector ["\000\001"]);
    section 7 (vector [export "memory" 2 0; export "octra_alloc" 0 1;
      export "octra_query" 0 2; export "octra_update" 0 2; export "octra_manifest" 0 3]);
    (match offset with None -> "" | Some offset ->
      section 9 (vector ["\000\065" ^ size offset ^ "\011\001\001"]));
    section 10 (vector ([body "\065\128\001";
      body (write (String.length manifest) (String.length response));
      body (write 0 (String.length manifest))] @ List.init (functions - 3) (fun _ -> body "\065\000")));
    section 11 (vector ["\000\065\000\011" ^ text (manifest ^ response)])
  ])

let rec remove path =
  if Sys.file_exists path then
    if Sys.is_directory path then begin
      Sys.readdir path |> Array.iter (fun name -> remove (Filename.concat path name));
      Unix.rmdir path
    end else Sys.remove path

let run bft epoch =
  let path = Printf.sprintf "runtime_data/table_route_%b_%d_%d" bft epoch (Unix.getpid ()) in
  if not (Sys.file_exists "runtime_data") then Unix.mkdir "runtime_data" 0o755;
  remove path;
  let store = Lwt_main.run (Store.open_store path) in
  Fun.protect ~finally:(fun () -> Lwt_main.run (Store.close store); remove path) (fun () ->
    let owner = "octTABLE_ROUTE_OWNER" in
    let ledger = Ledger.create store in
    check "account setup" (Ledger.add_account ledger owner (Z.of_int 1_000) = Ok ());
    let fold height = Epoch.prior_fold height |> Result.map (fun fold ->
      {fold with Epoch.proof_key = if height < 100 then Rule.Prior else Rule.Active}) in
    let backend = Epoch.make_live_backend ~fold store ledger in
    Lwt_main.run (backend.begin_batch Rule.Prior);
    let env = Epoch.{chain_id = "table-route"; epoch_id = epoch;
      proposer_addr = "octPROPOSER"; validator_addrs = []; validator_pubkeys = [];
      prev_state_root = String.make 64 'b'; epoch_ts = 0.;
      ready_state_root_at = None; ready_max_lag = -1} in
    let trust = Octra_vm.Program_trust.of_env (fun _ -> None) |> Result.get_ok in
    let account () = Option.get (Ledger.find_opt ledger owner) in
    let state () = let account = account () in account.balance, account.nonce,
      Lwt_main.run (Store.get_batch_tree_hash store) in
    let tx target op_type message = Tx.{from = owner; to_ = target;
      amount = Z.zero; nonce = (account ()).nonce + 1; ou = Z.of_int 10;
      timestamp = 0.; signature = ""; public_key = None; encrypted_data = None;
      message = Some (Yojson.Safe.to_string message); op_type} in
    let apply tx = Lwt_main.run (if bft then Route.process_tx ~backend ~env
      ~circle_mode:Rule.Prior ~wasm_compute_mode:Rule.Prior ~program_trust:trust
      ~object_cost:false tx
      else Lwt.map (Result.map (fun fee -> Epoch.Confirmed fee))
        (Epoch.process_circle_operation_tx ~backend ~current_epoch:epoch tx)) in
    let accepted label = function
      | Ok (Epoch.Confirmed fee) -> check label (Z.equal fee (Z.of_int 10))
      | Error (_, reason) -> failwith (label ^ ": " ^ reason)
      | _ -> failwith label in
    let refused ?reason label saved result =
      check label (match result with Error ("circle_runtime_invalid", text) ->
        Option.fold ~none:true ~some:(String.equal text) reason | _ -> false);
      check "refusal changed state" (saved = state ()) in
    let payload code = Circle.{runtime = Wasm_v1; privacy_class = Public;
      browser_mode = Native_sealed; resource_mode = Public_resources;
      code_b64 = Some code; policy_hash = None; members_root = None;
      export_policy = None; limits = default_limits} in
    let deploy code =
      let data = payload code in
      let target = Circle.circle_id_of_deploy ~deployer:owner ~nonce:((account ()).nonce + 1) data in
      target, tx target Tx.CircleDeploy (Circle.yojson_of_deploy_payload data) in
    let small, large = code 65_536, code 65_537 in
    let shape_ok, shape_bad = code ~functions:9_999 1, code ~functions:10_000 1 in
    let _, bad_tx = deploy (code ~offset:1 1) in
    let saved = state () in
    let reject () = match apply bad_tx with
      | Error (error_type, reason) ->
        check "element refusal kind" (error_type = "circle_runtime_invalid");
        check "element refusal changed state" (state () = saved);
        Core.Tx_outcome.{position = 0; tx = bad_tx; error_type; reason; charged = false}
      | _ -> failwith "invalid element accepted" in
    let first = reject () in
    let second = reject () in
    if epoch >= 100 then begin
      check "element rejection differs" (Core.Tx_outcome.equal [first] [second]);
      check "element bytes differ"
        (Core.Tx_outcome.encode_rejection first = Core.Tx_outcome.encode_rejection second);
      check "element fields omitted"
        (first.reason = "wasm instantiate failed: element segment does not fit: offset = 1 amount = 1")
    end else check "prior element error changed" (first.reason <> second.reason);
    let large_id, large_tx = deploy large in
    let saved = state () in
    let result = apply large_tx in
    if epoch < 100 then accepted "prior large deploy" result else begin
      refused "active large deploy" saved result;
      check "refused deploy exists" (not (Lwt_main.run (Store.circle_exists store large_id)))
    end;
    let target, small_tx = deploy small in
    accepted "small deploy" (apply small_tx);
    let update code = tx target Tx.CircleProgramUpdate
      (Circle.yojson_of_program_update_payload {Circle.code_b64 = code}) in
    let saved = state () in
    let result = apply (update large) in
    if epoch < 100 then accepted "prior large update" result
    else refused "active large update" saved result;
    let version () = (Option.get (Lwt_main.run (Store.get_circle_info store target))).version in
    let before = version () in
    let nonce = (account ()).nonce in
    accepted "small update" (apply (update small));
    check "update nonce" ((account ()).nonce = nonce + 1);
    check "update version" (version () = Int64.succ before);
    check "update code" (Lwt_main.run (Store.get_circle_program_code_b64 store target) = Some small);
    let shape_id, shape_tx = deploy shape_bad in
    let saved = state () in
    let result = apply shape_tx in
    if epoch < 100 then accepted "prior function count" result
    else refused ~reason:"wasm functions limit exceeded" "active function count" saved result;
    let saved = state () in
    let result = apply (update shape_bad) in
    if epoch < 100 then accepted "prior function update" result
    else refused ~reason:"wasm functions limit exceeded" "active function update" saved result;
    accepted "exact function count" (apply (update shape_ok));
    Lwt_main.run (Store.set_meta store "last_epoch" (string_of_int epoch));
    Lwt_main.run (backend.commit_batch ());
    if epoch < 100 then
      List.iter (fun table_mode ->
        let profile ~epoch:at =
          check "descriptor used wrong epoch" (at = epoch + 1);
          Ok Octra_vm.Contract_rpc.{epoch = at; point_ops = false; math = false;
            object_cost = false; int_work = Octra_vm.Int_work.Prior;
            fhe_work = Rule.Prior; proof_exec = Rule.Prior; proof_key = table_mode;
            wasm_float = Rule.Prior} in
        let reply = Lwt_main.run (Octra_node_runtime.Circle_read_rpc.program_rpc
          ~authenticated:false ~describe:true ~profile store (`List [`String large_id])) in
        check "descriptor ignored table mode"
          (Result.is_ok reply = (table_mode = Rule.Prior));
        let reply = Lwt_main.run (Octra_node_runtime.Circle_read_rpc.program_rpc
          ~authenticated:false ~describe:true ~profile store (`List [`String shape_id])) in
        check "descriptor ignored code mode"
          (Result.is_ok reply = (table_mode = Rule.Prior)))
        [Rule.Prior; Rule.Active; Rule.Prior])

let () =
  List.iter (fun bft -> List.iter (run bft) [100; 99; 101]) [true; false];
  print_endline "status = pass test = table_route"