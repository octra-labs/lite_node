(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Core = Octra_core
module Store = Core.Store_irmin
module Epoch = Core.Epoch_exec
module Node = Octra_node_runtime

let expect label value = if not value then failwith label

let check_workers () =
  List.iter (fun (cause, resource) ->
    List.iter (fun action ->
      let caught = try
        ignore (Lwt_main.run (Core.Exec_resource.run ~hash:"worker" action)); false
        with Core.Exec_resource.Exhausted ("worker", actual) -> actual = resource in
      expect "worker lost resource identity" caught)
      [(fun () -> raise cause); (fun () -> Lwt.fail cause);
       (fun () -> Core.Exec_resource.detach (fun () -> raise cause) ())];
    let result = Lwt_main.run (Octra_vm.Contract_rpc.run_view ~seconds:0.1
      (fun () -> raise cause)) in
    expect "view lost worker resource failure"
      (match result with
       | Error error -> error.Core.Rpc.code = -32005 &&
           error.message = "Program view resources unavailable"
       | Ok _ -> false);
    expect "view did not release failed worker"
      (Lwt_main.run (Octra_vm.Contract_rpc.run_view (fun () -> 7)) = Ok 7))
    [Out_of_memory, Core.Exec_resource.Memory; Stack_overflow, Core.Exec_resource.Stack]

let run arm cipher =
  Test_workspace.with_dir "preview_alloc" (fun dir ->
    let store = Lwt_main.run (Store.open_store (Filename.concat dir "irmin")) in
    Fun.protect ~finally:(fun () -> arm (-1); Lwt_main.run (Store.close store)) (fun () ->
      let owner = "oct11111111111111111111111111111111111111111111" in
      let circle_id = "oct" ^ String.make 44 '2' in
      let compiled = Octra_vm.Oct_compile.compile {|
Program CipherWork {
  state { count: int }
  constructor() { self.count = 0 }
  fn decode(data: string): int {
    self.count = 1
    let ct = fhe_deser(data)
    let encoded = fhe_ser(ct)
    return self.count
  }
}
|} in
      Option.iter (fun error -> failwith ("preview program compile failed: " ^ error)) compiled.error;
      let info = Core.Circles.{circle_id; runtime = Octb; version = 1L; owner;
        code_hash = sha256_hex compiled.bytecode; stable_root = zero_hash_hex;
        assets_root = zero_hash_hex; privacy_class = Public; browser_mode = Native_sealed;
        resource_mode = Public_resources; policy_hash = None; members_root = None;
        export_policy = None; limits = default_limits} in
      Lwt_main.run (Store.deploy_circle store info);
      let policy = Hashtbl.create 1 in
      Hashtbl.add policy Core.Circle_hfhe_policy.require_live_key_policy_key "false";
      ignore (Lwt_main.run (Store.save_circle_stable_storage store circle_id policy));
      Lwt_main.run (Store.save_circle_program_code_b64 store circle_id
        (Base64.encode_exn compiled.bytecode));
      let ledger = Core.Ledger.create store in
      expect "preview funding failed"
        (Core.Ledger.add_account ledger owner (Z.of_int 1_000_000_000) = Ok ());
      Lwt_main.run (Core.Ledger.flush_dirty_lwt ledger);
      Lwt_main.run (Store.set_meta store "total_supply" "1000000000");
      Lwt_main.run (Store.set_meta store "emission_remaining" "0");
      let root = Lwt_main.run (Core.Ledger.hash ledger) in
      let commit = Lwt_main.run (Store.get_commit_hash store) in
      let rules = Core.Rule_graph.create ~chain_id:"preview-test"
        ~root_at:(fun _ -> Core.Rule_graph.Missing) in
      let transaction = Core.Transaction.{from = owner; to_ = circle_id; amount = Z.zero;
        nonce = 1; ou = Z.of_int 1_000_000; timestamp = 0.; signature = ""; public_key = None;
        message = Some (Yojson.Safe.to_string (`List [`String (Base64.encode_exn (Bytes.to_string cipher))]));
        op_type = CircleCall; encrypted_data = Some "decode"} in
      let env = Epoch.{chain_id = "preview-test"; epoch_id = 7; proposer_addr = owner;
        validator_addrs = [owner]; validator_pubkeys = [owner, String.make 32 'a']; prev_state_root = root;
        epoch_ts = 70.; ready_state_root_at = None; ready_max_lag = -1} in
      let runtime = Node.Consensus_circle_preverify.{store; ledger;
        program_trust = Octra_vm.Program_trust.empty; rules;
        env = (fun ~pre_state_root -> {env with prev_state_root = pre_state_root})} in
      let batch = Lwt_main.run (Core.Preverify_worker.run_many
        ~field_policy:Core.Private_ledger.Unique_fields ~strict:false ~ledger
        ~circle_preverify:(Node.Consensus_circle_preverify.run runtime) [transaction]) in
      if batch.skipped <> [] then failwith ("preview receipt unavailable: " ^
        String.concat "; " (List.map (fun item -> item.Core.Preverify_worker.reason) batch.skipped));
      let preverify = Core.Preverify_commit.create
        (Core.Preverify_worker.receipts_for_hashes batch.ready [Core.Transaction.hash transaction]) in
      let backend = Node.Consensus_proposal_preview_shell.node_backend
        ~program_trust:Octra_vm.Program_trust.empty ~rules
        ~legacy_replay:(fun ~epoch:_ ~address:_ ~cipher:_ -> failwith "unexpected legacy replay")
        ~private_result_policy:(fun _ -> Core.Private_result_policy.Recoverable)
        ~max_fhe:1 ~max_stealth:1 store ledger in
      let reward = Node.Consensus_reward_attribution.full_set ~proposer_addr:owner
        ~validator_pubkeys:env.validator_pubkeys in
      let preview () = backend.run ~epoch_id:7 ~proposal_id:"memory-check"
        ~expected_prev_root:(Some root) ~preverify ~parent_commit:None ~reward ~env ~txs:[transaction] in
      let verify () =
        expect "preview changed store commit" (Lwt_main.run (Store.get_commit_hash store) = commit);
        expect "preview changed ledger" (Lwt_main.run (Core.Ledger.hash ledger) = root) in
      let control = Lwt_main.run (preview ()) in
      (match control with
       | Error error -> failwith ("preview control failed: " ^ error)
       | Ok result ->
         if List.length result.Epoch.artifacts.confirmed <> 1 then
           failwith ("preview control rejected: " ^ String.concat "; "
             (List.map (fun row -> row.Epoch.reason) result.artifacts.rejected)));
      verify ();
      let fault = Fun.protect ~finally:(fun () -> arm (-1)) (fun () ->
        arm 0;
        match Lwt_main.run (preview ()) with
        | _ -> false
        | exception Core.Exec_resource.Exhausted (hash, Memory) -> hash = Core.Transaction.hash transaction
        | exception _ -> false) in
      expect "production preview lost transaction allocation failure" fault;
      verify ();
      expect "preview could not retry after allocation failure" (Lwt_main.run (preview ()) = control);
      verify ();
      check_workers ();
      print_endline "event = test name = preview_alloc status = passed"))