(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Contract = Octra_vm.Contract
module ContractVM = Octra_vm.Contract_vm
module Circles = Octra_core.Circles
module Circle_code_admission =
  Octra_node_runtime.Consensus_circle_code_admission
module Epoch_exec = Octra_core.Epoch_exec
module Program_package = Octra_vm.Program_package
module Program_trust = Octra_vm.Program_trust
module Store = Octra_core.Store_irmin
module Transaction = Octra_core.Transaction
module Transition = Octra_node_runtime.Consensus_vm_transition

let fail message =
  failwith ("test_vm_transition: " ^ message)

let expect label condition =
  if not condition then fail label

let rec remove_tree path =
  if Sys.file_exists path then
    if Sys.is_directory path then begin
      Sys.readdir path
      |> Array.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
    end else
      Sys.remove path

let rec make_dir path =
  if not (Sys.file_exists path) then begin
    let parent = Filename.dirname path in
    if not (String.equal parent path) then make_dir parent;
    Unix.mkdir path 0o755
  end

let with_store label run =
  let root =
    Filename.concat
      (Sys.getcwd ())
      (Printf.sprintf
         "runtime_data/consensus_vm_transition/%s_%d"
         label
         (Unix.getpid ()))
  in
  remove_tree root;
  make_dir root;
  let store = Lwt_main.run (Store.open_store (Filename.concat root "irmin")) in
  Fun.protect
    ~finally:(fun () ->
      ignore (Lwt_main.run (Store.close store));
      remove_tree root)
    (fun () -> run store)

let source_v1 = {|
Program ConsensusCounter {
  state {
    counter: int
  }

  constructor() {
    self.counter = 0
  }

  fn inc(): int {
    self.counter = self.counter + 1
    return self.counter
  }

  view fn count(): int {
    return self.counter
  }
}
|}

let source_v2 = {|
Program ConsensusCounter {
  state {
    counter: int
  }

  constructor() {
    self.counter = 0
  }

  fn inc(): int {
    self.counter = self.counter + 2
    return self.counter
  }

  view fn count(): int {
    return self.counter
  }
}
|}

let release_private_key = String.make 32 '\042'

let release_key =
  match Mirage_crypto_ec.Ed25519.priv_of_octets release_private_key with
  | Error _ -> fail "release private key invalid"
  | Ok private_key ->
    {
      Octra_vm.Program_attestation.id = "vm-transition-release";
      public_key =
        Mirage_crypto_ec.Ed25519.pub_to_octets
          (Mirage_crypto_ec.Ed25519.pub_of_priv private_key);
    }

let program_trust =
  match
    Program_trust.of_env (fun name ->
      if String.equal name "OCTRA_PROGRAM_RELEASE_KEYS" then
        Some
          (release_key.id ^ "=" ^ Base64.encode_exn release_key.public_key)
      else
        None)
  with
  | Ok trust -> trust
  | Error error -> fail (Program_trust.error_message error)

let compile source =
  let compiled = Octra_vm.Oct_compile.compile_program source in
  let compiled =
    Octra_vm.Oct_compile.attest_program
      ~key_id:release_key.id
      ~private_key:release_private_key
      compiled
  in
  match compiled.error with
  | Some error -> fail error
  | None ->
    let raw =
      match compiled.program_envelope with
      | Some value -> value
      | None -> fail "program envelope missing"
    in
    raw, Base64.encode_exn raw

let tamper raw =
  let bytes = Bytes.of_string raw in
  let index = Bytes.length bytes - 1 in
  Bytes.set bytes index
    (Char.chr (Char.code (Bytes.get bytes index) lxor 1));
  Bytes.unsafe_to_string bytes

let env =
  {
    Epoch_exec.chain_id = "vm-transition-test";
    epoch_id = 17;
    proposer_addr = "oct_proposer";
    validator_addrs = [];
    validator_pubkeys = [];
    prev_state_root = String.make 64 'b';
    epoch_ts = 170.;
    ready_state_root_at = None;
    ready_max_lag = -1;
  }

let tx ~owner ~target ~nonce ~op_type ~payload ~message =
  {
    Transaction.from = owner;
    to_ = target;
    amount = Z.zero;
    nonce;
    ou = Z.of_int 10;
    timestamp = 0.;
    signature = "";
    public_key = None;
    message;
    op_type;
    encrypted_data = payload;
  }

let process ?(env = env) ?save_receipt_raw backend transaction =
  Lwt_main.run
    (Transition.process_tx
       ?save_receipt_raw
       ~backend
       ~env
       ~circle_mode:Octra_core.Rule_graph.Prior
       ~wasm_compute_mode:Octra_core.Rule_graph.Active
       ~program_trust
       ~object_cost:false
       transaction)

let expect_confirmed label = function
  | Ok (Epoch_exec.Confirmed fee) ->
    expect label (Z.equal fee (Z.of_int 10))
  | Ok (Epoch_exec.Rejected_after_fee rejected) ->
    fail (label ^ ": " ^ rejected.reason)
  | Error (_, reason) ->
    fail (label ^ ": " ^ reason)

let expect_rejected label = function
  | Ok (Epoch_exec.Rejected_after_fee rejected) ->
    expect (label ^ " fee") (Z.equal rejected.fee (Z.of_int 10));
    expect
      (label ^ " type")
      (String.equal rejected.error_type "program_upgrade_rejected")
  | Ok (Epoch_exec.Confirmed _) ->
    fail (label ^ " confirmed")
  | Error (_, reason) ->
    fail (label ^ " did not persist fee: " ^ reason)

let expect_circle_program_rejected label = function
  | Error (tag, _) ->
    expect label (String.equal tag "circle_program_invalid")
  | Ok _ ->
    fail (label ^ " accepted invalid Circle Program")

let view store address owner method_name =
  Contract.execute_view_call
    ~trusted:(Program_trust.keys program_trust)
    store
    address
    method_name
    []
    owner

let view_int store address owner method_name =
  match (view store address owner method_name).return_value with
  | Some (ContractVM.VInt value) -> value
  | _ -> fail (method_name ^ " did not return an integer")

type result = {
  root : string;
  balance : Z.t;
  nonce : int;
  program : string;
  receipts : (string * string) list;
}

let run_sequence label =
  with_store label (fun store ->
    let owner = "oct11111111111111111111111111111111111111111111" in
    let attacker = "oct22222222222222222222222222222222222222222222" in
    let raw, encoded = compile source_v1 in
    let upgraded_raw, upgraded_encoded = compile source_v2 in
    let legacy_upgrade =
      match Octra_vm.Program_envelope.decode upgraded_raw with
      | Ok envelope -> Base64.encode_exn envelope.code
      | Error _ -> fail "compiled program envelope invalid"
    in
    let program = Contract.addr_from_code raw owner 1 in
    let ledger = Octra_core.Ledger.create store in
    begin
      match Octra_core.Ledger.add_account ledger owner (Z.of_int 1_000) with
      | Ok () -> ()
      | Error error -> fail error
    end;
    begin
      match Octra_core.Ledger.add_account ledger attacker (Z.of_int 100) with
      | Ok () -> ()
      | Error error -> fail error
    end;
    Lwt_main.run (Octra_core.Ledger.flush_dirty_lwt ledger);
    let backend = Epoch_exec.make_live_backend store ledger in
    Lwt_main.run (backend.begin_batch Octra_core.Rule_graph.Prior);
    let receipts = ref [] in
    let apply transaction =
      process
        ~save_receipt_raw:(fun ~tx_hash ~json ->
          receipts := (tx_hash, json) :: !receipts)
        backend
        transaction
    in
    expect_confirmed "deploy"
      (apply
         (tx
            ~owner
            ~target:program
            ~nonce:1
            ~op_type:Transaction.ContractDeploy
            ~payload:(Some encoded)
            ~message:(Some "[]")));
    let sink_tx =
      tx
        ~owner
        ~target:program
        ~nonce:2
        ~op_type:Transaction.ProgramExec
        ~payload:(Some "inc")
        ~message:(Some "[]")
    in
    let sink_failed =
      Lwt_main.run
        (Lwt.catch
           (fun () ->
             let open Lwt.Syntax in
             let* _ =
               Octra_core.Tx_savepoint.run
                 ~ledger
                 ~store
                 (fun () ->
                   Transition.process_tx
                     ~save_receipt_raw:(fun ~tx_hash:_ ~json:_ ->
                       failwith "receipt sink failed")
                     ~backend
                     ~env
                     ~circle_mode:Octra_core.Rule_graph.Prior
                     ~wasm_compute_mode:Octra_core.Rule_graph.Active
                     ~program_trust
                     ~object_cost:false
                     sink_tx)
             in
             Lwt.return_false)
           (fun _ -> Lwt.return_true))
    in
    expect "receipt sink failure propagated" sink_failed;
    expect "receipt sink failure rolled back state"
      (Z.equal (view_int store program owner "count") Z.zero);
    let account_after_sink =
      Option.get (Octra_core.Ledger.find_opt ledger owner)
    in
    expect "receipt sink failure rolled back balance"
      (Z.equal account_after_sink.balance (Z.of_int 990));
    expect "receipt sink failure rolled back nonce"
      (account_after_sink.nonce = 1);
    expect_confirmed "program call"
      (apply sink_tx);
    expect "counter committed"
      (Z.equal (view_int store program owner "count") Z.one);
    let old_code_hash =
      match Lwt_main.run (Store.get_contract_info store program) with
      | Some (_, code_hash, _, _) -> code_hash
      | None -> fail "program metadata missing"
    in
    expect_rejected "non-owner program upgrade"
      (apply
         (tx
            ~owner:attacker
            ~target:program
            ~nonce:1
            ~op_type:Transaction.ContractUpgrade
            ~payload:(Some upgraded_encoded)
            ~message:
              (Some
                 (Octra_vm.Program_upgrade.message
                    ~expected_code_hash:old_code_hash))));
    expect_confirmed "program upgrade"
      (apply
         (tx
            ~owner
            ~target:program
            ~nonce:3
            ~op_type:Transaction.ContractUpgrade
            ~payload:(Some upgraded_encoded)
            ~message:
              (Some
                 (Octra_vm.Program_upgrade.message
                    ~expected_code_hash:old_code_hash))));
    expect "upgrade preserves storage"
      (Z.equal (view_int store program owner "count") Z.one);
    let upgraded_code_hash =
      match Lwt_main.run (Store.get_contract_info store program) with
      | Some (_, code_hash, _, stored_owner) ->
        expect "upgrade preserves owner" (String.equal stored_owner owner);
        code_hash
      | None -> fail "upgraded program metadata missing"
    in
    expect "upgrade changes code hash"
      (not (String.equal old_code_hash upgraded_code_hash));
    expect_rejected "stale program upgrade"
      (apply
         (tx
            ~owner
            ~target:program
            ~nonce:4
            ~op_type:Transaction.ContractUpgrade
            ~payload:(Some upgraded_encoded)
            ~message:
              (Some
                 (Octra_vm.Program_upgrade.message
                    ~expected_code_hash:old_code_hash))));
    expect_rejected "legacy program upgrade"
      (apply
         (tx
            ~owner
            ~target:program
            ~nonce:5
            ~op_type:Transaction.ContractUpgrade
            ~payload:(Some legacy_upgrade)
            ~message:
              (Some
                 (Octra_vm.Program_upgrade.message
                    ~expected_code_hash:upgraded_code_hash))));
    expect_confirmed "upgraded program call"
      (apply
         (tx
            ~owner
            ~target:program
            ~nonce:6
            ~op_type:Transaction.ProgramExec
            ~payload:(Some "inc")
            ~message:(Some "[]")));
    expect "upgraded code executes with preserved storage"
      (Z.equal (view_int store program owner "count") (Z.of_int 3));
    begin
      match
        apply
          (tx
             ~owner
             ~target:program
             ~nonce:7
             ~op_type:Transaction.ProgramExec
             ~payload:(Some "missing")
             ~message:(Some "[]"))
      with
      | Ok (Epoch_exec.Rejected_after_fee rejected) ->
        expect "rejected fee" (Z.equal rejected.fee (Z.of_int 10));
        expect "rejected type"
          (String.equal rejected.error_type "program_exec_failed")
      | Ok (Epoch_exec.Confirmed _) -> fail "missing method confirmed"
      | Error (_, reason) -> fail ("missing method did not persist fee: " ^ reason)
    end;
    expect "failed call storage rollback"
      (Z.equal (view_int store program owner "count") (Z.of_int 3));
    Lwt_main.run (Octra_core.Ledger.flush_dirty_lwt ledger);
    let account =
      match Octra_core.Ledger.find_opt ledger owner with
      | Some value -> value
      | None -> fail "owner account missing"
    in
    let root =
      match Lwt_main.run (Store.get_batch_tree_hash store) with
      | Some value -> value
      | None -> fail "batch root missing"
    in
    Store.abort_epoch_batch store;
    {
      root;
      balance = account.balance;
      nonce = account.nonce;
      program;
      receipts = List.rev !receipts;
    })

let test_deterministic_transition () =
  let left = run_sequence "left" in
  let right = run_sequence "right" in
  expect "program address parity" (String.equal left.program right.program);
  expect "state root parity" (String.equal left.root right.root);
  expect "balance parity" (Z.equal left.balance right.balance);
  expect "nonce parity" (left.nonce = right.nonce);
  expect "receipt parity" (left.receipts = right.receipts);
  expect "receipt count" (List.length left.receipts = 4);
  List.iter
    (fun (_, raw) ->
      let json = Yojson.Safe.from_string raw in
      expect "receipt epoch"
        (Yojson.Safe.Util.member "epoch" json = `Int env.epoch_id);
      expect "receipt timestamp"
        (Yojson.Safe.Util.member "ts" json = `Float env.epoch_ts))
    left.receipts;
  expect "fee persisted on execution failure"
    (Z.equal left.balance (Z.of_int 930));
  expect "nonce persisted on execution failure" (left.nonce = 7)

let compile_package ?(compiler = Program_package.Protocol) source =
  match
    Program_package.compile_with ~compiler ~point_ops:true
      ~main:"main.aml"
      ~sources:[Program_package.{ path = "main.aml"; body = source }]
  with
  | Ok package -> package
  | Error error -> fail (Program_package.error_message error)

let run_source_deploy ?submitted ?(refused = false) ?(epoch = 17) label =
  with_store label (fun store ->
    let chain_id = "octra-devnet-9871-cluster" in
    let plan = Option.get
      (Octra_core.Rule_graph.program_source_activation_for_chain chain_id) in
    let rules = Octra_core.Rule_graph.create ~chain_id ~root_at:(fun epoch ->
      match Octra_core.Rule_graph.root_after_floor ~chain_id
        ~floor_epoch:plan.anchor_epoch ~epoch with
      | None -> Octra_core.Rule_graph.Missing
      | Some root -> Octra_core.Rule_graph.Root root) in
    let fold epoch =
      match Octra_core.Rule_graph.program_source rules ~epoch,
            Octra_core.Rule_graph.program_overlap rules ~epoch with
      | Error error, _ | _, Error error -> Error (Octra_core.Rule_graph.fault_message error)
      | Ok program_mode, Ok program_overlap -> Epoch_exec.prior_fold epoch
        |> Result.map (fun ctx -> {ctx with Epoch_exec.program_mode; program_overlap}) in
    let compiler = match fold epoch with
      | Error error -> fail error
      | Ok ctx -> Program_package.compiler_mode ctx.program_mode in
    let process = process ~env:{env with epoch_id = epoch; chain_id} in
    let owner = "oct11111111111111111111111111111111111111111111" in
    let package = compile_package ~compiler:(Option.value submitted ~default:compiler) source_v1 in
    let target = Contract.addr_from_code package.envelope owner 1 in
    let ledger = Octra_core.Ledger.create store in
    begin
      match Octra_core.Ledger.add_account ledger owner (Z.of_int 1_000) with
      | Ok () -> ()
      | Error error -> fail error
    end;
    Lwt_main.run (Octra_core.Ledger.flush_dirty_lwt ledger);
    let backend = Epoch_exec.make_live_backend ~fold store ledger in
    Lwt_main.run (backend.begin_batch Octra_core.Rule_graph.Prior);
    let result = process backend
         (tx
            ~owner
            ~target
            ~nonce:1
            ~op_type:Transaction.ProgramDeploy
            ~payload:(Some (Base64.encode_exn package.package))
            ~message:(Some "[]")) in
    if refused then begin
      begin match result with
      | Ok (Epoch_exec.Rejected_after_fee rejected) ->
        expect "window refusal fee" (Z.equal rejected.fee (Z.of_int 10));
        expect "window refusal type" (rejected.error_type = "program_deploy_rejected");
        expect "window refusal reason"
          (rejected.reason = "Program source and envelope mismatch")
      | _ -> fail "deployment outside compiler window did not refuse"
      end;
      expect "refused window deployed program"
        (Lwt_main.run (Store.load_bytecode store target) = None)
    end else begin
    expect_confirmed "source Program deploy" result;
    expect "source Program constructor"
      (Z.equal (view_int store target owner "count") Z.zero);
    let meta = Lwt_main.run (Store.get_contract_meta store target) in
    begin match Octra_vm.Contract_rpc.verify_compilation
      ~meta ~source:source_v1 ~files_json:None with
    | Error reason -> fail ("source verification failed: " ^ reason)
    | Ok results ->
      expect "stored source has no matching compiler"
        (List.exists (fun (raw, _) -> raw = package.envelope) results)
    end;
    let changed = Bytes.of_string package.package in
    let source_value = String.index package.package '0' in
    Bytes.set changed source_value '1';
    begin
      match
        process backend
          (tx
             ~owner
             ~target
             ~nonce:2
             ~op_type:Transaction.ProgramDeploy
             ~payload:(Some (Base64.encode_exn (Bytes.to_string changed)))
             ~message:(Some "[]"))
      with
      | Ok (Epoch_exec.Rejected_after_fee rejected) ->
        expect "source mismatch fee"
          (Z.equal rejected.fee (Z.of_int 10));
        expect "source mismatch type"
          (String.equal rejected.error_type "program_deploy_rejected")
      | Ok (Epoch_exec.Confirmed _) -> fail "source mismatch confirmed"
      | Error (_, reason) -> fail ("source mismatch fee lost: " ^ reason)
    end;
    end;
    Lwt_main.run (Octra_core.Ledger.flush_dirty_lwt ledger);
    let account = Option.get (Octra_core.Ledger.find_opt ledger owner) in
    let root = Option.get (Lwt_main.run (Store.get_batch_tree_hash store)) in
    Store.abort_epoch_batch store;
    root, account.balance, account.nonce)

let test_source_deploy_parity () =
  let results =
    List.init 5 (fun index ->
      run_source_deploy ("source_deploy_" ^ string_of_int index))
  in
  match results with
  | [] -> fail "source Program parity result missing"
  | expected :: validators ->
    List.iter
      (fun actual -> expect "source Program parity" (actual = expected))
      validators;
    let _, balance, nonce = expected in
    expect "source Program fee parity" (Z.equal balance (Z.of_int 980));
    expect "source Program nonce parity" (nonce = 2)

let test_resource_abort () =
  List.iteri (fun index error ->
    with_store ("resource_" ^ string_of_int index) (fun store ->
      let owner = "oct11111111111111111111111111111111111111111111" in
      let package = compile_package source_v1 in
      let target = Contract.addr_from_code package.envelope owner 1 in
      let ledger = Octra_core.Ledger.create store in
      begin match Octra_core.Ledger.add_account ledger owner (Z.of_int 1_000) with
      | Ok () -> () | Error reason -> fail reason
      end;
      Lwt_main.run (Octra_core.Ledger.flush_dirty_lwt ledger);
      let backend = Epoch_exec.make_live_backend store ledger in
      Lwt_main.run (backend.begin_batch Octra_core.Rule_graph.Prior);
      let root = Lwt_main.run (Store.get_batch_tree_hash store) in
      let backend = {backend with Epoch_exec.fold = (fun _ -> raise error)} in
      let receipts = ref [] in
      let transaction = tx ~owner ~target ~nonce:1 ~op_type:Transaction.ProgramDeploy
        ~payload:(Some (Base64.encode_exn package.package)) ~message:(Some "[]") in
      let actual = try
        ignore (Lwt_main.run (Octra_core.Tx_savepoint.run ~ledger ~store (fun () ->
          Transition.process_tx ~backend ~env
            ~circle_mode:Octra_core.Rule_graph.Prior
            ~wasm_compute_mode:Octra_core.Rule_graph.Active ~program_trust ~object_cost:false
            ~save_receipt_raw:(fun ~tx_hash ~json -> receipts := (tx_hash, json) :: !receipts)
            transaction)));
        None
        with error -> Some error in
      expect "resource failure became transaction result" (actual = Some error);
      let account = Option.get (Octra_core.Ledger.find_opt ledger owner) in
      expect "resource failure charged fee" (Z.equal account.balance (Z.of_int 1_000));
      expect "resource failure advanced nonce" (account.nonce = 0);
      expect "resource failure saved receipt" (!receipts = []);
      expect "resource failure changed state" (root = Lwt_main.run (Store.get_batch_tree_hash store));
      expect "resource failure deployed program" (Lwt_main.run (Store.load_bytecode store target) = None);
      Store.abort_epoch_batch store))
    [Stack_overflow; Out_of_memory;
     Octra_circle_runtime.Circle_exec.Execution_unavailable "test backend unavailable"]

let test_program_switch () =
  let epoch = (Option.get (Octra_core.Rule_graph.program_source_activation_for_chain
    "octra-devnet-9871-cluster")).activation_epoch in
  let old = compile_package ~compiler:Program_package.Protocol source_v1 in
  let next = compile_package ~compiler:Program_package.Source source_v1 in
  expect "window test images must differ" (old.envelope <> next.envelope);
  List.iter (fun (epoch, submitted) ->
    let first = run_source_deploy ~refused:true ~submitted ~epoch
      ("window_refuse_a_" ^ string_of_int epoch) in
    let second = run_source_deploy ~refused:true ~submitted ~epoch
      ("window_refuse_b_" ^ string_of_int epoch) in
    expect "window refusal replay differs" (first = second);
    let _, balance, nonce = first in
    expect "window refused fee differs" (Z.equal balance (Z.of_int 990));
    expect "window refused nonce differs" (nonce = 1))
    [epoch - 1, Program_package.Source; epoch + 64, Program_package.Protocol];
  List.iter (fun epoch ->
    let first = run_source_deploy ~epoch ("program_rule_a_" ^ string_of_int epoch) in
    let second = run_source_deploy ~epoch ("program_rule_b_" ^ string_of_int epoch) in
    expect "activated deployment replay differs" (first = second);
    let _, balance, nonce = first in
    expect "activated deployment fees differ" (Z.equal balance (Z.of_int 980));
    expect "activated deployment nonce differs" (nonce = 2))
    [1_566_999; 1_567_000; 1_571_999; 1_572_000; 1_572_001;
     1_572_063; 1_572_064];
  List.iter (fun epoch ->
    let result = run_source_deploy ~submitted:Program_package.Protocol ~epoch
      ("program_overlap_" ^ string_of_int epoch) in
    let repeated = run_source_deploy ~submitted:Program_package.Protocol ~epoch
      ("program_repeat_" ^ string_of_int epoch) in
    expect "cross-epoch replay differs" (result = repeated);
    let _, balance, nonce = result in
    expect "cross-epoch deployment fee differs" (Z.equal balance (Z.of_int 980));
    expect "cross-epoch deployment nonce differs" (nonce = 2))
    [1_572_000; 1_572_063]

type circle_result = {
  circle_root : string;
  circle_balance : Z.t;
  circle_nonce : int;
  circle_version : int64;
  circle_code_hash : string;
}

let run_circle_program_consensus_admission label =
  with_store label (fun store ->
    let owner = "oct11111111111111111111111111111111111111111111" in
    let raw, encoded = compile source_v1 in
    let tampered = Base64.encode_exn (tamper raw) in
    let ledger = Octra_core.Ledger.create store in
    begin
      match Octra_core.Ledger.add_account ledger owner (Z.of_int 1_000) with
      | Ok () -> ()
      | Error error -> fail error
    end;
    Lwt_main.run (Octra_core.Ledger.flush_dirty_lwt ledger);
    let backend = Epoch_exec.make_live_backend store ledger in
    Lwt_main.run (backend.begin_batch Octra_core.Rule_graph.Prior);
    let deploy_payload code_b64 = {
      Circles.runtime = Circles.Octb;
      privacy_class = Circles.Public;
      browser_mode = Circles.Native_sealed;
      resource_mode = Circles.Public_resources;
      code_b64 = Some code_b64;
      policy_hash = None;
      members_root = None;
      export_policy = None;
      limits = Circles.default_limits;
    } in
    let deploy_tx nonce payload =
      let target =
        Circles.circle_id_of_deploy
          ~deployer:owner
          ~nonce
          payload
      in
      target,
      tx
        ~owner
        ~target
        ~nonce
        ~op_type:Transaction.CircleDeploy
        ~payload:None
        ~message:
          (Some
             (Yojson.Safe.to_string
                (Circles.yojson_of_deploy_payload payload)))
    in
    let invalid_target, invalid_deploy =
      deploy_tx 1 (deploy_payload tampered)
    in
    expect_circle_program_rejected
      "tampered Circle deploy"
      (process backend invalid_deploy);
    expect
      "tampered Circle deploy created state"
      (not (Lwt_main.run (Store.circle_exists store invalid_target)));
    let account_after_reject =
      Option.get (Octra_core.Ledger.find_opt ledger owner)
    in
    expect
      "tampered Circle deploy debited fee"
      (Z.equal account_after_reject.balance (Z.of_int 1_000));
    expect
      "tampered Circle deploy consumed nonce"
      (account_after_reject.nonce = 0);
    let circle_id, valid_deploy =
      deploy_tx 1 (deploy_payload encoded)
    in
    expect_confirmed "signed Circle deploy" (process backend valid_deploy);
    expect
      "signed Circle deploy missing"
      (Lwt_main.run (Store.circle_exists store circle_id));
    let update_message code_b64 =
      Some
        (Yojson.Safe.to_string
           (Circles.yojson_of_program_update_payload
              { Circles.code_b64 }))
    in
    let invalid_update =
      tx
        ~owner
        ~target:circle_id
        ~nonce:2
        ~op_type:Transaction.CircleProgramUpdate
        ~payload:None
        ~message:(update_message tampered)
    in
    expect_circle_program_rejected
      "tampered Circle update"
      (process backend invalid_update);
    let stored_after_reject =
      Lwt_main.run
        (Store.get_circle_program_code_b64 store circle_id)
    in
    expect
      "tampered Circle update changed code"
      (stored_after_reject = Some encoded);
    let account_after_update_reject =
      Option.get (Octra_core.Ledger.find_opt ledger owner)
    in
    expect
      "tampered Circle update debited fee"
      (Z.equal account_after_update_reject.balance (Z.of_int 990));
    expect
      "tampered Circle update consumed nonce"
      (account_after_update_reject.nonce = 1);
    let valid_update =
      tx
        ~owner
        ~target:circle_id
        ~nonce:2
        ~op_type:Transaction.CircleProgramUpdate
        ~payload:None
        ~message:(update_message encoded)
    in
    expect_confirmed "signed Circle update" (process backend valid_update);
    let stored_after_update =
      Lwt_main.run
        (Store.get_circle_program_code_b64 store circle_id)
    in
    expect
      "signed Circle update code mismatch"
      (stored_after_update = Some encoded);
    let account =
      Option.get (Octra_core.Ledger.find_opt ledger owner)
    in
    let info =
      Option.get (Lwt_main.run (Store.get_circle_info store circle_id))
    in
    let root =
      Option.get (Lwt_main.run (Store.get_batch_tree_hash store))
    in
    Store.abort_epoch_batch store;
    {
      circle_root = root;
      circle_balance = account.balance;
      circle_nonce = account.nonce;
      circle_version = info.Circles.version;
      circle_code_hash = info.code_hash;
    })

let test_circle_admission () =
  let results =
    List.init 5 (fun index ->
      run_circle_program_consensus_admission
        ("circle_admission_" ^ string_of_int index))
  in
  match results with
  | [] -> fail "Circle Program parity result missing"
  | expected :: validators ->
    List.iter
      (fun actual ->
        expect
          "Circle Program state root parity"
          (String.equal expected.circle_root actual.circle_root);
        expect
          "Circle Program balance parity"
          (Z.equal expected.circle_balance actual.circle_balance);
        expect
          "Circle Program nonce parity"
          (expected.circle_nonce = actual.circle_nonce);
        expect
          "Circle Program version parity"
          (Int64.equal expected.circle_version actual.circle_version);
        expect
          "Circle Program code hash parity"
          (String.equal
             expected.circle_code_hash
             actual.circle_code_hash))
      validators

let () =
  test_resource_abort ();
  test_deterministic_transition ();
  test_source_deploy_parity ();
  test_program_switch ();
  test_circle_admission ();
  print_endline "status = pass test = vm_transition"