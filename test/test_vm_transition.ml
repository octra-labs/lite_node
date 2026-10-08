(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Contract = Octra_vm.Contract
module ContractVM = Octra_vm.Contract_vm
module Circles = Octra_core.Circles
module Circle_code =
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
  let result = try Ok (run store) with error -> Error error in
  let closed = try
    Lwt_main.run (Store.close store);
    remove_tree root;
    Ok ()
  with error -> Error error in
  match result, closed with
  | Ok value, Ok () -> value
  | Error error, Ok () | Ok _, Error error -> raise error
  | Error error, Error closing ->
    fail (Printf.sprintf "run = %s close = %s"
      (Printexc.to_string error) (Printexc.to_string closing))

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

let compile ?xcalls source =
  let compiled = match xcalls with
    | None -> Octra_vm.Oct_compile.compile_program source
    | Some specs -> Octra_vm.Oct_compile.compile_program_with_xcalls source specs in
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
    expect_rejected "hash_mismatch program upgrade"
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
      ~main:"main.aml" ~meta ~source:source_v1 ~files_json:None with
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
      let expected = match error with
        | Out_of_memory -> Octra_core.Exec_resource.Unavailable Memory
        | Stack_overflow -> Octra_core.Exec_resource.Unavailable Stack
        | failure -> failure
      in
      expect "resource failure became transaction result" (actual = Some expected);
      let account = Option.get (Octra_core.Ledger.find_opt ledger owner) in
      expect "resource failure charged fee" (Z.equal account.balance (Z.of_int 1_000));
      expect "resource failure advanced nonce" (account.nonce = 0);
      expect "resource failure saved receipt" (!receipts = []);
      expect "resource failure changed state" (root = Lwt_main.run (Store.get_batch_tree_hash store));
      expect "resource failure deployed program" (Lwt_main.run (Store.load_bytecode store target) = None);
      Store.abort_epoch_batch store))
    [Stack_overflow; Out_of_memory; Lwt.Canceled; Octra_core.Exec_resource.Unavailable Host;
     Octra_circle_runtime.Circle_exec.Execution_unavailable "test backend unavailable"]

let test_write_abort ?(phase = "") () =
  List.iteri (fun index error ->
    with_store ("receipt_" ^ string_of_int index) (fun store ->
      let owner = "oct11111111111111111111111111111111111111111111" in
      let package = compile_package source_v1 in
      let target = Contract.addr_from_code package.envelope owner 1 in
      let ledger = Octra_core.Ledger.create store in
      begin match Octra_core.Ledger.add_account ledger owner (Z.of_int 1_000) with
      | Ok () -> ()
      | Error reason -> fail reason
      end;
      Lwt_main.run (Octra_core.Ledger.flush_dirty_lwt ledger);
      let backend = Epoch_exec.make_live_backend store ledger in
      Lwt_main.run (backend.begin_batch Octra_core.Rule_graph.Prior);
      let root = Lwt_main.run (Store.get_batch_tree_hash store) in
      let transaction = tx ~owner ~target ~nonce:1 ~op_type:Transaction.ProgramDeploy
        ~payload:(Some (Base64.encode_exn package.package)) ~message:(Some "[]") in
      let attempted = ref false in
      let receipts = ref [] in
      let execute save_receipt_raw =
        Octra_core.Tx_savepoint.run ~ledger ~store (fun () ->
          Transition.process_tx ~backend ~env
            ~circle_mode:Octra_core.Rule_graph.Prior
            ~wasm_compute_mode:Octra_core.Rule_graph.Active ~program_trust ~object_cost:false
            ~save_receipt_raw transaction)
      in
      let previous = List.map (fun name -> name, Sys.getenv_opt name)
        ["OCTRA_CHAOS_FAIL_AT"; "OCTRA_CHAOS_FAIL_KIND"] in
      let actual = Fun.protect
        ~finally:(fun () -> List.iter (fun (name, value) ->
          Unix.putenv name (Option.value ~default:"" value)) previous)
        (fun () ->
          Unix.putenv "OCTRA_CHAOS_FAIL_AT" phase;
          Unix.putenv "OCTRA_CHAOS_FAIL_KIND" (match error with
            | Out_of_memory -> "memory"
            | Stack_overflow -> "stack"
            | Lwt.Canceled -> "cancel"
            | _ -> "host");
          try
            ignore (Lwt_main.run (execute (fun ~tx_hash ~json ->
              attempted := true;
              let account = Option.get (Octra_core.Ledger.find_opt ledger owner) in
              expect "receipt fault preceded execution" (account.nonce = 1);
              if phase = "" then raise error;
              receipts := (tx_hash, json) :: !receipts)));
            None
          with failure -> Some failure) in
      expect "write fault phase" (!attempted = (phase = ""));
      expect "write fault leaked receipt" (!receipts = []);
      let account = Option.get (Octra_core.Ledger.find_opt ledger owner) in
      expect "receipt fault skipped balance rollback" (Z.equal account.balance (Z.of_int 1_000));
      expect "receipt fault skipped nonce rollback" (account.nonce = 0);
      expect "receipt fault skipped state rollback"
        (root = Lwt_main.run (Store.get_batch_tree_hash store));
      expect "receipt fault retained deployment"
        (Lwt_main.run (Store.load_bytecode store target) = None);
      let expected = match error with
        | Out_of_memory -> Octra_core.Exec_resource.Unavailable Memory
        | Stack_overflow -> Octra_core.Exec_resource.Unavailable Stack
        | failure -> failure
      in
      expect "receipt fault changed refusal" (actual = Some expected);
      expect_confirmed "receipt fault repeat"
        (Lwt_main.run (execute (fun ~tx_hash ~json ->
          receipts := (tx_hash, json) :: !receipts)));
      expect "receipt fault repeat count" (List.length !receipts = 1);
      let account = Option.get (Octra_core.Ledger.find_opt ledger owner) in
      expect "receipt fault repeat balance" (Z.equal account.balance (Z.of_int 990));
      expect "receipt fault repeat nonce" (account.nonce = 1);
      expect "receipt fault repeat deployment"
        (Option.is_some (Lwt_main.run (Store.load_bytecode store target)));
      Store.abort_epoch_batch store))
    [Out_of_memory; Stack_overflow; Lwt.Canceled;
     Octra_core.Exec_resource.Unavailable Host]

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

let run_circle_program_check label =
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

let test_circle_check () =
  let results =
    List.init 5 (fun index ->
      run_circle_program_check
        ("circle_check_" ^ string_of_int index))
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

type fhe_route = Direct | Nested | Batch

let fhe_source =
  let steps = List.init 10 (fun index ->
    Printf.sprintf "let c%d = fhe_add(pk, c%d, c%d)" (index + 1) index index) in
  {|
Program FheJournal {
  state { counter: int }
  constructor() { self.counter = 0 }
  fn inc(): int { self.counter = self.counter + 1 return self.counter }
  view fn count(): int { return self.counter }
  fn grow(key: address, data: bytes, destination: address): int {
    self.counter = 7
    require(transfer(destination, 1), "transfer refused")
    let pk = fhe_load_pk(key)
    let c0 = fhe_deser(data)
|} ^ String.concat "\n" steps ^ {|
    return 7
  }
  fn relay(child: address, key: address, data: bytes, destination: address): int {
    self.counter = 9
    require(transfer(destination, 1), "transfer refused")
    return call(child, "grow", [key, data, destination])
  }
}
|}

let test_fhe_journal () =
  let module L = Octra_core.Ledger in
  let module R = Octra_core.Rule_graph in
  let pk, sk = Pvac_ffi.keygen_from_seed (Pvac_ffi.default_params ()) (Bytes.make 32 '\001') in
  let cipher = Pvac_ffi.enc_value_seeded pk sk 1L (Bytes.make 32 '\002') in
  let data = Pvac_ffi.serialize_cipher cipher |> Bytes.to_string |> Base64.encode_exn in
  let raw, encoded = compile ~xcalls:[{
    Octra_vm.Oct_compile.method_name = "grow";
    inputs = [Octra_vm.Program_type_flow.Addr; Bytes; Addr];
    output = Octra_vm.Program_type_flow.Int;
    capabilities = [Octra_vm.Program_type_flow.Storage_write; Transfer; Fhe];
  }] fhe_source in
  let run label route epoch = with_store label (fun store ->
    let owner = "oct11111111111111111111111111111111111111111111" in
    let destination = "oct7xCozDD9JEsbeVpo5C7HXp2BJbKqfmNUHmDDCCTtWcGb" in
    let plan = R.{anchor_epoch = 10; anchor_state_root = "root"; activation_epoch = 20} in
    let fhe_work = R.activation_mode ~root_at:(fun _ -> R.Root "root") (Some plan) ~epoch
      |> Result.get_ok in
    let fold epoch = Epoch_exec.prior_fold epoch
      |> Result.map (fun ctx -> {ctx with Epoch_exec.fhe_work}) in
    let ledger = L.create store in
    List.iter (fun (address, balance) ->
      match L.add_account ledger address (Z.of_int balance) with
      | Ok () -> () | Error reason -> fail reason) [owner, 1000; destination, 100];
    Lwt_main.run (L.set_pvac_pubkey ledger owner (Pvac_ffi.serialize_pubkey pk |> Bytes.to_string));
    Lwt_main.run (L.flush_dirty_lwt ledger);
    let backend = Epoch_exec.make_live_backend ~fold store ledger in
    Lwt_main.run (backend.begin_batch R.Prior);
    let receipts = ref [] in
    let apply = process ~env:{env with epoch_id = epoch}
      ~save_receipt_raw:(fun ~tx_hash ~json -> receipts := (tx_hash, json) :: !receipts) backend in
    let deploy nonce =
      let address = Contract.addr_from_code raw owner nonce in
      expect_confirmed "FHE program deployment"
        (apply (tx ~owner ~target:address ~nonce ~op_type:Transaction.ContractDeploy
          ~payload:(Some encoded) ~message:(Some "[]")));
      begin match L.credit ledger address (Z.of_int 100) with
      | Ok () -> () | Error reason -> fail reason end;
      address in
    let inner = deploy 1 in
    let outer = deploy 2 in
    let args = [`String owner; `String data; `String destination] in
    let transaction = match route with
      | Direct -> tx ~owner ~target:inner ~nonce:3 ~op_type:Transaction.ProgramExec
          ~payload:(Some "grow") ~message:(Some (Yojson.Safe.to_string (`List args)))
      | Nested -> tx ~owner ~target:outer ~nonce:3 ~op_type:Transaction.ProgramExec
          ~payload:(Some "relay") ~message:(Some (Yojson.Safe.to_string (`List (`String inner :: args))))
      | Batch ->
        let call method_name params = `Assoc ["to", `String inner; "method", `String method_name;
          "params", `List params; "amount", `String "0"] in
        tx ~owner ~target:inner ~nonce:3 ~op_type:Transaction.MultiExec ~payload:None
          ~message:(Some (Yojson.Safe.to_string (`List [call "inc" []; call "grow" args]))) in
    begin match apply transaction with
    | Ok (Epoch_exec.Rejected_after_fee rejected) when fhe_work = R.Active ->
      expect "FHE refusal lost fee" (Z.equal rejected.fee (Z.of_int 10))
    | Ok (Epoch_exec.Confirmed _) when fhe_work = R.Prior -> ()
    | Error (_, reason) -> fail ("FHE transition error: " ^ reason)
    | _ -> fail "FHE journal outcome differs from selected rule"
    end;
    let balance address = (Option.get (L.find_opt ledger address)).L.balance in
    expect "FHE failure fee or nonce changed"
      (Z.equal (balance owner) (Z.of_int 970) && (Option.get (L.find_opt ledger owner)).nonce = 3);
    if fhe_work = R.Active then begin
      expect "FHE failure kept transfer" (Z.equal (balance destination) (Z.of_int 100));
      List.iter (fun address ->
        expect "FHE failure kept program debit" (Z.equal (balance address) (Z.of_int 100));
        expect "FHE failure kept storage write" (Z.equal (view_int store address owner "count") Z.zero))
        [inner; outer]
    end else
      expect "historical FHE call did not write" (Z.equal (view_int store inner owner "count") (Z.of_int 7));
    Lwt_main.run (L.flush_dirty_lwt ledger);
    let root = Lwt_main.run (Store.get_batch_tree_hash store) in
    let result = root, List.rev !receipts in
    Store.abort_epoch_batch store;
    result) in
  List.iteri (fun index route ->
    ignore (run ("fhe_prior_" ^ string_of_int index) route 19);
    let first = run ("fhe_active_a_" ^ string_of_int index) route 20 in
    let second = run ("fhe_active_b_" ^ string_of_int index) route 20 in
    expect "FHE failed replay root or receipt mismatch" (first = second)) [Direct; Nested; Batch]

let test_circle_work () =
  let module L = Octra_core.Ledger in
  let module R = Octra_core.Rule_graph in
  let module H = Octra_core.Circle_hfhe_policy in
  let steps = List.init 10 (fun index ->
    Printf.sprintf "let c%d = fhe_add(pk, c%d, c%d)" (index + 1) index index) in
  let source = {|
Program CircleWork {
  state { counter: int }
  fn grow(key: address, data: bytes): int {
    self.counter = 7
    let pk = fhe_load_pk(key)
    let c0 = fhe_deser(data)
|} ^ String.concat "\n" steps ^ {|
    return 7
  }
}
|} in
  let _, encoded = compile source in
  let pk, sk = Pvac_ffi.keygen_from_seed (Pvac_ffi.default_params ()) (Bytes.make 32 '\003') in
  let cipher = Pvac_ffi.enc_value_seeded pk sk 1L (Bytes.make 32 '\004') in
  let data = Pvac_ffi.serialize_cipher cipher |> Bytes.to_string |> Base64.encode_exn in
  let run label fhe_work = with_store label (fun store ->
    let owner = "oct11111111111111111111111111111111111111111111" in
    let ledger = L.create store in
    begin match L.add_account ledger owner (Z.of_int 1000) with
    | Ok () -> () | Error error -> fail error end;
    Lwt_main.run (L.set_pvac_pubkey ledger owner (Pvac_ffi.serialize_pubkey pk |> Bytes.to_string));
    Lwt_main.run (L.flush_dirty_lwt ledger);
    let fold epoch = Epoch_exec.prior_fold epoch
      |> Result.map (fun ctx -> {ctx with Epoch_exec.fhe_work}) in
    let backend = Epoch_exec.make_live_backend ~fold store ledger in
    Lwt_main.run (backend.begin_batch R.Prior);
    let payload = Circles.{runtime = Octb; privacy_class = Public;
      browser_mode = Native_sealed; resource_mode = Public_resources;
      code_b64 = Some encoded; policy_hash = None; members_root = None;
      export_policy = None; limits = default_limits} in
    let target = Circles.circle_id_of_deploy ~deployer:owner ~nonce:1 payload in
    let receipts = ref [] in
    let apply transaction = Lwt_main.run
      (Transition.process_tx ~backend ~env ~circle_mode:R.Active
        ~wasm_compute_mode:R.Active ~program_trust ~object_cost:false
        ~save_receipt_raw:(fun ~tx_hash ~json -> receipts := (tx_hash, json) :: !receipts)
        transaction) in
    expect_confirmed "Circle work deploy"
      (apply (tx ~owner ~target ~nonce:1 ~op_type:Transaction.CircleDeploy ~payload:None
        ~message:(Some (Yojson.Safe.to_string (Circles.yojson_of_deploy_payload payload)))));
    let storage = Lwt_main.run (Store.load_circle_stable_storage store target) |> Result.get_ok in
    Hashtbl.replace storage H.require_live_key_policy_key "false";
    ignore (Lwt_main.run (Store.save_circle_stable_storage store target storage));
    let snapshot () = Lwt_main.run (Store.load_circle_stable_storage store target)
      |> Result.get_ok |> Hashtbl.to_seq |> List.of_seq |> List.sort compare in
    let before = snapshot () in
    let transaction = tx ~owner ~target ~nonce:2 ~op_type:Transaction.CircleCall
      ~payload:(Some "grow")
      ~message:(Some (Yojson.Safe.to_string (`List [`String owner; `String data]))) in
    begin match apply transaction, fhe_work with
    | Ok (Epoch_exec.Confirmed _), R.Prior ->
      expect "Circle prior call did not write" (before <> snapshot ())
    | Ok (Epoch_exec.Rejected_after_fee rejected), R.Active ->
      expect "Circle FHE refusal reason"
        (rejected.reason = "execution reverted");
      expect "Circle FHE failure kept storage" (before = snapshot ())
    | Error (_, reason), _ -> fail ("Circle work transition: " ^ reason)
    | _ -> fail "Circle work rule was not applied"
    end;
    let account = Option.get (L.find_opt ledger owner) in
    expect "Circle work fee or nonce changed"
      (Z.equal account.balance (Z.of_int 980) && account.nonce = 2);
    Lwt_main.run (L.flush_dirty_lwt ledger);
    let result = Lwt_main.run (Store.get_batch_tree_hash store), List.rev !receipts in
    Store.abort_epoch_batch store;
    result) in
  ignore (run "circle_work_prior" R.Prior);
  let first = run "circle_work_active_a" R.Active in
  let second = run "circle_work_active_b" R.Active in
  expect "Circle work replay root or receipt changed" (first = second)

let test_policy_abort () =
  let raw, encoded = compile source_v1 in
  let package = compile_package source_v1 in
  List.iter (fun (label, op_type, raw, encoded) ->
  with_store label (fun store ->
    let module L = Octra_core.Ledger in
    let owner = "oct11111111111111111111111111111111111111111111" in
    let target = Contract.addr_from_code raw owner 1 in
    let ledger = L.create store in
    begin match L.add_account ledger owner (Z.of_int 1_000) with
    | Ok () -> () | Error reason -> fail reason end;
    Lwt_main.run (L.flush_dirty_lwt ledger);
    let backend = Epoch_exec.make_live_backend store ledger in
    Lwt_main.run (backend.begin_batch Octra_core.Rule_graph.Prior);
    let root = Lwt_main.run (Store.get_batch_tree_hash store) in
    let backend = {backend with Epoch_exec.fold = (fun _ -> Error "anchor read failed")} in
    let receipts = ref [] in
    let message = if op_type = Transaction.MultiExec then
      Yojson.Safe.to_string (`List [`Assoc ["to", `String target;
        "method", `String "inc"; "params", `List []; "amount", `String "0"]])
      else "[]" in
    let transaction = tx ~owner ~target ~nonce:1 ~op_type
      ~payload:(Some encoded) ~message:(Some message) in
    let aborted = try
      ignore (Lwt_main.run (Octra_core.Tx_savepoint.run ~ledger ~store (fun () ->
        Transition.process_tx ~backend ~env ~circle_mode:Octra_core.Rule_graph.Prior
          ~wasm_compute_mode:Octra_core.Rule_graph.Active ~program_trust ~object_cost:false
          ~save_receipt_raw:(fun ~tx_hash ~json -> receipts := (tx_hash, json) :: !receipts)
          transaction)));
      false
      with Transition.Policy_unavailable reason -> reason = "anchor read failed" in
    expect "policy read failure became transaction result" aborted;
    let account = Option.get (L.find_opt ledger owner) in
    expect "policy failure charged fee" (Z.equal account.balance (Z.of_int 1_000));
    expect "policy failure consumed nonce" (account.nonce = 0);
    expect "policy failure published receipt" (!receipts = []);
    expect "policy failure changed root" (root = Lwt_main.run (Store.get_batch_tree_hash store));
    expect "policy failure deployed program" (Lwt_main.run (Store.load_bytecode store target) = None);
    Store.abort_epoch_batch store))
    ["policy_legacy", Transaction.ContractDeploy, raw, encoded;
     "policy_source", Transaction.ProgramDeploy, package.envelope, Base64.encode_exn package.package;
     "policy_batch", Transaction.MultiExec, raw, encoded]

let test_spawn_budget () =
  let module VM = Octra_vm.Contract_vm in
  let module R = Octra_core.Rule_graph in
  let module Journal = Octra_vm.Program_journal in
  let raw, _ = compile ~xcalls:[{
    Octra_vm.Oct_compile.method_name = "value";
    inputs = [];
    output = Octra_vm.Program_type_flow.Int;
    capabilities = [Octra_vm.Program_type_flow.View];
  }] {|Program Budget {
    constructor(target: address) { require(call(target, "value", []) == 1, "wrong value") }
  }|} in
  with_store "spawn_budget" (fun store ->
    List.iter (fun (mode, bytes, depth, limit, success) ->
      let observed = ref None in
      let byte_work = if not bytes then None
        else Some (Octra_vm.Byte_work.create (Option.get (Octra_vm.Byte_work.limits
          ~key_bytes:64 ~value_bytes:128 ~copy_bytes:256 ~write_bytes:1024
          ~alloc_bytes:1024 ~unit_bytes:32))) in
      let ctx = {VM.default_ctx with fhe_work = mode; byte_work;
        call_contract = (fun _ _ _ _ scope -> observed := Some scope;
          Ok VM.{return_value = VInt Z.one; effort_used = 1; events = []})} in
      let journal = Journal.create () in
      let result = Contract.deploy_internal ~journal ~trusted:(Program_trust.keys program_trust)
        ~ctx ~depth ~limit
        ~params:[VM.VAddr "oct11111111111111111111111111111111111111111111"]
        store ~deployer:"oct11111111111111111111111111111111111111111111"
        ~bytecode_raw:raw ~nonce:1 in
      expect "constructor budget or depth ignored" (Result.is_ok result = success);
      if success then begin
        let scope = Option.get !observed in
        expect "constructor reset call depth"
          (scope.depth = if mode = R.Prior && not bytes then 1 else depth + 1);
        expect "constructor replaced byte owner" (Option.equal (==) scope.bytes byte_work);
        expect "constructor reset child budget"
          (match scope.limit with
          | None -> mode = R.Prior && not bytes
          | Some value -> (mode = R.Active || bytes) && value >= 0 && value < limit)
      end else begin
        expect "failed constructor staged program"
          (not (Journal.has_deploy journal (Contract.addr_from_code raw
            "oct11111111111111111111111111111111111111111111" 1)));
        expect "failed constructor called child" (!observed = None)
      end)
      [R.Prior, false, 8, 10_000, true; R.Active, false, 7, 10_000, true;
       R.Active, false, 8, 10_000, false; R.Active, false, 0, 1, false;
       R.Prior, true, 7, 10_000, true; R.Prior, true, 8, 10_000, false;
       R.Prior, true, 0, 1, false])

let test_circle_calls () =
  let module R = Octra_core.Rule_graph in
  let module L = Octra_core.Ledger in
  let module T = Octra_vm.Program_type_flow in
  let module D = Octra_core.Circle_deploy in
  let ctx = Octra_circle_runtime.Circle_exec.with_circle_spawn
    {ContractVM.default_ctx with proof_exec = R.Prior; tx_hash = String.make 64 'a'}
    "parent" "caller" (ref []) in
  let scope = ContractVM.{depth = 1; limit = None; memory = None; bytes = None} in
  let pending = ctx.deploy_async "parent" "{}" 0 scope [ContractVM.VString "parent"] in
  expect "prior spawn parsed on the main stack" (Lwt.state pending = Lwt.Sleep);
  expect "prior spawn changed parser result"
    (Lwt_main.run pending = ctx.deploy_contract "parent" "{}" 0 scope [ContractVM.VString "parent"]);
  let saved_env = List.map (fun name -> name, Sys.getenv_opt name)
    ["OCTRA_CHAOS_FAIL_AT"; "OCTRA_CHAOS_FAIL_KIND"] in
  let decoded = Fun.protect
    ~finally:(fun () -> List.iter (fun (name, value) ->
      Unix.putenv name (Option.value ~default:"" value)) saved_env)
    (fun () ->
      Unix.putenv "OCTRA_CHAOS_FAIL_AT" "circle_payload";
      Unix.putenv "OCTRA_CHAOS_FAIL_KIND" "stack";
      try Some (D.decode_spawn_payload_json "{}") with Stack_overflow -> None) in
  expect "prior circle payload changed rejection"
    (decoded = Some (Error ("malformed_transaction", "circle deploy payload is invalid")));
  let raw = String.make 500_000 '[' ^ "0" ^ String.make 500_000 ']' in
  expect "active circle payload reached recursive parser"
    (Result.is_error (D.decode_spawn_payload_json ~resource_errors:true raw));
  List.iter (fun raw ->
    expect "active payload accepted recursive containers"
      (D.decode_spawn_payload_json ~resource_errors:true raw
        = Error ("malformed_transaction", "circle deploy payload is invalid")))
    [String.make 500_000 '(' ^ "0" ^ String.make 500_000 ')';
     String.concat "" (List.init 80_000 (fun _ -> "<\"x\":")) ^ "0" ^ String.make 80_000 '>'];
  let module J = Octra_vm.Program_journal in
  let journal = J.create () in
  let cells = Hashtbl.create 2 in
  Hashtbl.add cells "a" "1";
  Hashtbl.add cells "b" "2";
  ignore (J.circle_storage journal "circle" cells);
  let small = J.snapshot_effort journal in
  Hashtbl.replace cells "a" (String.make 1_000_000 'x');
  expect "journal copy charged string bytes" (Z.equal small (J.snapshot_effort journal));
  Hashtbl.add cells "c" "3";
  expect "journal copy ignored entries" (Z.gt (J.snapshot_effort journal) small);
  let binary = Hashtbl.create 256 in
  expect "json size arithmetic exceeds host range" (Sys.max_string_length <= max_int / 12);
  for byte = 0 to 255 do
    Hashtbl.add binary (String.make 3 (Char.chr byte)) (String.make 17 (Char.chr byte))
  done;
  let extra = Hashtbl.fold (fun key value total ->
    let encoded = Circles.make_stable_entry key (Circles.Inline value)
      |> Circles.yojson_of_stable_entry |> Yojson.Safe.to_string in
    Z.add total (Z.of_int (String.length encoded - String.length key - String.length value)))
    binary Z.zero in
  expect "circle write omitted encoded bytes"
    (Z.equal (J.write_effort binary)
      (Z.add (J.storage_effort binary) (Z.cdiv extra (Z.of_int 16))));
  let source = {|
Program CirclePayments {
  state { counter: int }
  event Paid(to: address, amount: int)

  constructor() { self.counter = 0 }

  fn init() { self.counter = 0 }

  payable fn pay(to: address, amount: int, refuse: bool): int {
    self.counter = self.counter + 1
    require(transfer(to, amount), "payout refused")
    emit Paid(to, amount)
    require(refuse == false, "pay refused")
    return self.counter
  }

  fn relay(next: address, to: address, amount: int, refuse: bool): int {
    self.counter = self.counter + 1
    let paid = call(next, "pay", [to, amount, false])
    require(refuse == false, "relay refused")
    return paid
  }

  fn bounce(next: address, back: address, steps: int): int {
    self.counter = self.counter + 1
    if steps > 0 {
      let visited = call(next, "bounce", [back, next, steps - 1])
    }
    return self.counter
  }

  view fn count(): int { return self.counter }

  fn visit(target: address): int {
    self.counter += 1
    return to_int(call(target, "visit", []))
  }

  fn credit_of(who: address): int {
    return self.counter
  }

  fn vault_read(target: address, who: address): int {
    return to_int(call(target, "credit_of", [who]))
  }

  fn repeat(next: address, more: bool): int {
    let value = call(next, "count", [])
    if more {
      let second = call(next, "count", [])
      let third = call(next, "count", [])
      let fourth = call(next, "count", [])
    }
    return 1
  }

  fn chain(large: address, small: address, more: bool): int {
    let first = call(large, "count", [])
    let second = call(small, "count", [])
    let third = call(small, "count", [])
    if more {
      let fourth = call(small, "count", [])
    }
    return 1
  }
}
|} in
  let raw, encoded = compile ~xcalls:([
    {Octra_vm.Oct_compile.method_name = "pay";
     inputs = [T.Addr; T.Int; T.Bool]; output = T.Int;
     capabilities = [T.Storage_read; T.Storage_write; T.Transfer]};
    {Octra_vm.Oct_compile.method_name = "bounce";
     inputs = [T.Addr; T.Addr; T.Int]; output = T.Int;
     capabilities = [T.Storage_read; T.Storage_write]};
    {Octra_vm.Oct_compile.method_name = "visit";
     inputs = []; output = T.Int;
     capabilities = [T.Storage_read; T.Storage_write]};
    {Octra_vm.Oct_compile.method_name = "credit_of";
     inputs = [T.Addr]; output = T.Int; capabilities = [T.Storage_read]};
  ] @ List.init 8 (fun _ ->
    {Octra_vm.Oct_compile.method_name = "count";
     inputs = []; output = T.Int;
     capabilities = [T.Storage_read]})) source in
  let run epoch = with_store ("circle_calls_" ^ string_of_int epoch) (fun store ->
    let owner = "oct11111111111111111111111111111111111111111111" in
    let receiver = "oct7xCozDD9JEsbeVpo5C7HXp2BJbKqfmNUHmDDCCTtWcGb" in
    let mode = R.proof_exec_at ~chain_id:"octra-devnet-9871-cluster" ~epoch in
    let active = mode = R.Active in
    let selected = ref mode in
    let fold epoch = Epoch_exec.prior_fold epoch
      |> Result.map (fun ctx -> {ctx with Epoch_exec.proof_exec = !selected}) in
    let ledger = L.create store in
    List.iter (fun (address, balance) ->
      match L.add_account ledger address (Z.of_int balance) with
      | Ok () -> ()
      | Error error -> fail error) [owner, 100_000_000; receiver, 100];
    Lwt_main.run (L.flush_dirty_lwt ledger);
    let backend = Epoch_exec.make_live_backend ~fold store ledger in
    Lwt_main.run (backend.begin_batch R.Prior);
    let receipts = ref [] in
    let apply = process ~env:{env with epoch_id = epoch}
      ~save_receipt_raw:(fun ~tx_hash ~json -> receipts := (tx_hash, json) :: !receipts)
      backend in
    let nonce = ref 0 in
    let next () = incr nonce; !nonce in
    let deploy circle =
      let nonce = next () in
      let address, operation, payload, message = if circle then
        let payload = Circles.{runtime = Octb; privacy_class = Public;
          browser_mode = Native_sealed; resource_mode = Public_resources;
          code_b64 = Some encoded; policy_hash = None; members_root = None;
          export_policy = None; limits = default_limits} in
        Circles.circle_id_of_deploy ~deployer:owner ~nonce payload,
        Transaction.CircleDeploy, None,
        Some (Yojson.Safe.to_string (Circles.yojson_of_deploy_payload payload))
      else Contract.addr_from_code raw owner nonce,
        Transaction.ContractDeploy, Some encoded, Some "[]" in
      expect_confirmed "circle calls deploy"
        (apply (tx ~owner ~target:address ~nonce ~op_type:operation ~payload ~message));
      if circle then
        expect_confirmed "circle calls init"
          (apply (tx ~owner ~target:address ~nonce:(next ())
            ~op_type:Transaction.CircleCall ~payload:(Some "init") ~message:(Some "[]")));
      begin match L.credit ledger address (Z.of_int 1000) with
      | Ok () -> ()
      | Error error -> fail error
      end;
      circle, address in
    let program = deploy false in
    let circle = deploy true in
    let second = deploy true in
    if active then begin
      let address = snd second in
      let info = Option.get (Lwt_main.run (Store.get_circle_info store address)) in
      Lwt_main.run (Store.deploy_circle store {info with runtime = Circles.Wasm_v1});
      let effects = Octra_vm.Tx_effects.create ~ledger ~store in
      let transaction = tx ~owner ~target:(snd program) ~nonce:(!nonce + 1)
        ~op_type:Transaction.ProgramExec ~payload:(Some "relay") ~message:None in
      let module Shell = Octra_node_runtime.Consensus_epoch_vm_shell in
      let ctx = Shell.make_live_contract_ctx Shell.{
        value_journal = Octra_vm.Tx_effects.value effects;
        program_journal = Octra_vm.Tx_effects.program effects;
        trusted_program_keys = program_trust; store;
        get_fhe_pubkey = (fun _ -> None);
        proof_mode = R.Active; fhe_work = R.Active; proof_exec = R.Active;
        wasm_float = R.Active; math = true; object_cost = false;
        current_epoch = epoch; epoch_time_ms = 0L; tree_hash = "root";
        node_id = snd program; tx_hash = Transaction.hash transaction;
      } in
      List.iter (fun caller ->
        let result = Lwt_main.run (ctx.ContractVM.call_async caller address "pay"
          [ContractVM.VString receiver; ContractVM.VInt Z.one; ContractVM.VBool false]
          {ContractVM.depth = 1; limit = Some 1_000_000; memory = None; bytes = None}) in
        expect "invalid nested wasm accepted" (Result.is_error result))
        [snd program; snd circle];
      let journal = Octra_vm.Tx_effects.program effects in
      let measure nested entries =
        J.discard journal;
        let cells = Hashtbl.create entries in
        for i = 1 to entries do
          Hashtbl.add cells (string_of_int i) "1"
        done;
        ignore (J.circle_storage journal address cells);
        if nested then
          let result = Lwt_main.run (ctx.ContractVM.call_async owner (snd program) "count" []
            {ContractVM.depth = 1; limit = Some 1_000_000; memory = None; bytes = None})
            |> Result.get_ok in
          result.effort_used
        else
          let result = Lwt_main.run (Contract.execute_call_async
            ~trusted:(Program_trust.keys program_trust) ~journal ~ctx ~limit:1_000_000
            store (snd program) "count" [] owner Z.zero) in
          expect "journal call control failed" result.success;
          result.effort_used in
      List.iter (fun nested ->
        let empty = measure nested 0 in
        List.iter (fun entries ->
          expect (if nested then "outer journal charge differs" else "program journal charge differs")
            (measure nested entries - empty = entries * (if nested then 64 else 32))) [1; 5_000])
        [false; true];
      Octra_vm.Tx_effects.discard effects;
      let rejected = apply (tx ~owner ~target:(snd program) ~nonce:(next ())
        ~op_type:Transaction.ProgramExec ~payload:(Some "relay")
        ~message:(Some (Yojson.Safe.to_string (`List
          [`String address; `String receiver; `Int 1; `Bool false])))) in
      begin match rejected with
      | Ok (Epoch_exec.Rejected_after_fee _) -> ()
      | _ -> fail "nested wasm did not reject"
      end;
      Lwt_main.run (Store.deploy_circle store info)
    end;
    let balance address = (Option.get (L.find_opt ledger address)).L.balance in
    let count (circle, address) =
      let receipt = if circle then Lwt_main.run
        (Octra_circle_runtime.Circle_exec.execute_view_call
          ~trusted:(Program_trust.keys program_trust)
          ~ctx:{ContractVM.default_ctx with proof_exec = mode; current_epoch = epoch}
          store address "count" [] owner)
      else view store address owner "count" in
      match receipt.Contract.return_value with
      | Some (ContractVM.VInt value) -> value
      | Some (ContractVM.VString value) when receipt.success && not active ->
        Z.of_string value
      | value -> fail (Printf.sprintf "circle count unreadable: address = %s success = %b value = %s error = %s"
          address receipt.success (Option.fold ~none:"none" ~some:ContractVM.to_string value)
          (Option.value receipt.error ~default:"none")) in
    let call ?(value = 0) target method_name params success =
      let is_circle, address = target in
      let before = List.map count [program; circle; second] in
      let money = List.map balance [owner; receiver; snd program; snd circle; snd second] in
      let transaction = tx ~owner ~target:address ~nonce:(next ())
        ~op_type:(if is_circle then Transaction.CircleCall else Transaction.ProgramExec)
        ~payload:(Some method_name) ~message:(Some (Yojson.Safe.to_string (`List params))) in
      begin match apply {transaction with amount = Z.of_int value}, success with
      | Ok (Epoch_exec.Confirmed _), true -> ()
      | Ok (Epoch_exec.Rejected_after_fee _), false ->
        expect "circle refusal kept storage"
          (before = List.map count [program; circle; second]);
        let after = List.map balance [owner; receiver; snd program; snd circle; snd second] in
        expect "circle refusal kept money"
          (after = match money with
            | paid :: rest -> Z.sub paid (Z.of_int 10) :: rest
            | [] -> assert false)
      | Ok (Epoch_exec.Rejected_after_fee error), true ->
        fail (Printf.sprintf "circle call epoch = %d method = %s address = %s error = %s"
          epoch method_name address error.reason)
      | _ -> fail "circle call result differs"
      end in
    let pay amount refuse = [`String receiver; `Int amount; `Bool refuse] in
    let wasm_raw =
      let path = Filename.concat (Filename.dirname Sys.executable_name) "vault.wasm" in
      let channel = open_in_bin path in
      Fun.protect ~finally:(fun () -> close_in channel)
        (fun () -> really_input_string channel (in_channel_length channel)) in
    let wasm_payload = Circles.{runtime = Wasm_v1; privacy_class = Public;
      browser_mode = Native_sealed; resource_mode = Public_resources;
      code_b64 = Some (Base64.encode_exn wasm_raw); policy_hash = None; members_root = None;
      export_policy = None; limits = default_limits} in
    let wasm_nonce = next () in
    let wasm = Circles.circle_id_of_deploy ~deployer:owner ~nonce:wasm_nonce wasm_payload in
    expect_confirmed "wasm vault deploy" (apply (tx ~owner ~target:wasm ~nonce:wasm_nonce
      ~op_type:Transaction.CircleDeploy ~payload:None
      ~message:(Some (Yojson.Safe.to_string (Circles.yojson_of_deploy_payload wasm_payload)))));
    let credit () =
      let receipt = Lwt_main.run (Octra_circle_runtime.Circle_exec.execute_view_call
        ~ctx:{ContractVM.default_ctx with proof_exec = mode; current_epoch = epoch}
        store wasm "credit_of" [`String owner] owner) in
      expect "wasm credit unreadable" receipt.success;
      match receipt.return_value with
      | Some (ContractVM.VInt value) -> value
      | _ -> fail "wasm credit type differs" in
    let wasm_call ?(fee = 10) method_name params value =
      let transaction = tx ~owner ~target:wasm ~nonce:(next ())
        ~op_type:Transaction.CircleCall ~payload:(Some method_name)
        ~message:(Some (Yojson.Safe.to_string (`List params))) in
      apply {transaction with amount = Z.of_int value; ou = Z.of_int fee} in
    let funded = balance owner in
    let deposit = wasm_call "deposit" [] 100 in
    if active then begin
      expect_confirmed "wasm deposit" deposit;
      expect "wasm deposit credit differs" (credit () = Z.of_int 100);
      expect "wasm deposit balance differs" (balance wasm = Z.of_int 100);
      expect "wasm deposit created money" (balance owner = Z.sub funded (Z.of_int 110));
      let paid = balance owner in
      expect_confirmed "wasm withdraw" (wasm_call "withdraw" [`Int 30] 0);
      expect "wasm withdraw credit differs" (credit () = Z.of_int 70);
      expect "wasm withdraw balance differs" (balance wasm = Z.of_int 70);
      expect "wasm payout created money" (balance owner = Z.add paid (Z.of_int 20));
      let outsider = tx ~owner:receiver ~target:wasm ~nonce:1
        ~op_type:Transaction.CircleCall ~payload:(Some "withdraw") ~message:(Some "[1]") in
      let funds = balance receiver in
      begin match apply outsider with
      | Ok (Epoch_exec.Rejected_after_fee _) -> ()
      | _ -> fail "wasm foreign withdrawal accepted"
      end;
      expect "wasm foreign withdrawal changed funds"
        (balance receiver = Z.sub funds (Z.of_int 10)
         && balance wasm = Z.of_int 70 && credit () = Z.of_int 70);
      expect_confirmed "wasm nested self read"
        (wasm_call "read" [`String wasm; `String owner] 0);
      expect_confirmed "wasm program read"
        (wasm_call "read" [`String (snd program); `String owner] 0);
      call program "vault_read" [`String wasm; `String owner] true;
      let visits () =
        let storage = Lwt_main.run (Store.load_circle_stable_storage store wasm) |> Result.get_ok in
        Hashtbl.find_opt storage "visits" in
      List.iter (fun target ->
        expect_confirmed "wasm reentry" (wasm_call "probe" [`String (snd target); `Bool false] 0);
        let saved = visits () in
        let count_before = count target in
        begin match wasm_call "probe" [`String (snd target); `Bool true] 0 with
        | Ok (Epoch_exec.Rejected_after_fee _) -> ()
        | _ -> fail "wasm parent refusal accepted"
        end;
        expect "wasm parent refusal retained writes" (saved = visits () && count_before = count target))
        [program; circle];
      expect "wasm reentry lost writes" (visits () = Some "6");
      let root = Lwt_main.run (Store.get_batch_tree_hash store) in
      begin match wasm_call "policy" [] 0 with
      | Ok (Epoch_exec.Rejected_after_fee _) -> ()
      | _ -> fail "wasm transient policy accepted"
      end;
      expect "wasm transient policy reached child" (visits () = Some "6");
      expect "wasm transient policy changed root" (root = Lwt_main.run (Store.get_batch_tree_hash store));
      let saved = balance wasm in
      let paid = balance owner in
      begin match wasm_call "withdraw" [`Int 71] 0 with
      | Ok (Epoch_exec.Rejected_after_fee _) -> ()
      | _ -> fail "wasm overdraft accepted"
      end;
      expect "wasm overdraft changed state" (balance wasm = saved && credit () = Z.of_int 70);
      expect "wasm overdraft changed money" (balance owner = Z.sub paid (Z.of_int 10));
      let calls = [
        `Assoc ["to", `String wasm; "method", `String "withdraw";
          "params", `List [`Int 10]; "amount", `String "0"];
        `Assoc ["to", `String wasm; "method", `String "withdraw";
          "params", `List [`Int 1000]; "amount", `String "0"]] in
      let paid = balance owner in
      begin match apply (tx ~owner ~target:wasm ~nonce:(next ())
        ~op_type:Transaction.MultiExec ~payload:None
        ~message:(Some (Yojson.Safe.to_string (`List calls)))) with
      | Ok (Epoch_exec.Rejected_after_fee _) -> ()
      | _ -> fail "wasm failed batch accepted"
      end;
      expect "wasm failed batch changed state" (balance wasm = saved && credit () = Z.of_int 70);
      expect "wasm failed batch kept payout" (balance owner = Z.sub paid (Z.of_int 10));
      let module Circle = Octra_circle_runtime.Circle_exec in
      let root = Lwt_main.run (Store.get_batch_tree_hash store) in
      let interrupted callback =
        let ctx = {ContractVM.default_ctx with proof_exec = R.Active; current_epoch = epoch;
          call_async = (fun _ _ _ _ _ -> callback ())} in
        Circle.execute_call ~ctx ~limit:1_000_000 ~update_policy:true
          store wasm "probe" [`String (snd program); `Bool false] owner Z.zero in
      List.iter (fun error ->
        let actual = try
          ignore (Lwt_main.run (interrupted (fun () -> Lwt.fail error)));
          None
        with error -> Some error in
        expect "wasm host fault became refusal" (actual = Some error))
        [Circle.Execution_unavailable "test host unavailable"; Lwt.Canceled];
      Lwt_main.run (let open Lwt.Syntax in
        let entered, notify = Lwt.wait () in
        let pending, _ = Lwt.task () in
        let work = interrupted (fun () -> Lwt.wakeup_later notify (); pending) in
        let* () = Lwt.pick [entered; (let* () = Lwt_unix.sleep 5. in
          Lwt.fail_with "wasm callback did not suspend")] in
        Lwt.cancel work;
        Lwt.catch (fun () -> let* _ = work in Lwt.fail_with "wasm cancellation lost")
          (function Lwt.Canceled -> Lwt.return_unit | error -> Lwt.fail error));
      expect "wasm interruption changed root" (root = Lwt_main.run (Store.get_batch_tree_hash store));
      expect "wasm interruption changed credit" (credit () = Z.of_int 70);
      expect_confirmed "wasm after cancellation" (wasm_call "read" [`String wasm; `String owner] 0);
      selected := R.Prior;
      let nonce = next () in
      let old = Circles.circle_id_of_deploy ~deployer:owner ~nonce wasm_payload in
      expect_confirmed "prior wasm deploy" (apply (tx ~owner ~target:old ~nonce
        ~op_type:Transaction.CircleDeploy ~payload:None
        ~message:(Some (Yojson.Safe.to_string (Circles.yojson_of_deploy_payload wasm_payload)))));
      selected := R.Active;
      let enabled () =
        let info = Option.get (Lwt_main.run (Store.get_circle_info store old)) in
        Lwt_main.run (D.calls_enabled store info) in
      expect "old wasm enabled by activation" (not (enabled ()));
      let update = tx ~owner ~target:old ~nonce:(next ())
        ~op_type:Transaction.CircleProgramUpdate ~payload:None
        ~message:(Some (Yojson.Safe.to_string
          (`Assoc ["code_b64", `String (Base64.encode_exn wasm_raw)]))) in
      expect_confirmed "wasm owner update" (apply update);
      expect "wasm owner update did not enable calls" (enabled ());
      let deposit = tx ~owner ~target:old ~nonce:(next ())
        ~op_type:Transaction.CircleCall ~payload:(Some "deposit") ~message:(Some "[]") in
      expect_confirmed "updated wasm deposit" (apply {deposit with amount = Z.of_int 9});
      expect "updated wasm deposit lost value" (balance old = Z.of_int 9);
      let paid = balance owner in
      let funds = balance wasm in
      let received = balance receiver in
      expect_confirmed "wasm nested number" (wasm_call "number" [`Int 32; `String receiver] 0);
      expect "wasm nested number lost writes" (visits () = Some "8");
      expect "wasm nested number lost transfers"
        (balance wasm = Z.sub funds (Z.of_int 2)
         && balance receiver = Z.add received (Z.of_int 2)
         && balance owner = Z.sub paid (Z.of_int 10));
      let saved = visits () in
      let root = Lwt_main.run (Store.get_batch_tree_hash store) in
      let accounts = List.map (fun address ->
        address, Option.get (L.find_opt ledger address)) [owner; receiver; wasm] in
      let fee = Z.of_int 20_000_000 in
      let rejected = try wasm_call ~fee:20_000_000 "number" [`Int 1_800_000; `String receiver] 7 with
        | Circle.Execution_unavailable reason ->
          fail ("wasm response became technical retry: " ^ reason)
        | Octra_core.Exec_resource.Unavailable _ ->
          fail "wasm response became resource retry" in
      begin match rejected with
      | Ok (Epoch_exec.Rejected_after_fee result) ->
        expect "wasm response refusal differs" (result.reason = "circle response exceeds limit");
        expect "wasm response fee differs" (Z.equal result.fee fee)
      | _ -> fail "wasm response did not persist refusal"
      end;
      expect "wasm response retained writes" (visits () = saved && credit () = Z.of_int 70);
      expect "wasm response changed store" (root = Lwt_main.run (Store.get_batch_tree_hash store));
      List.iter (fun (address, before) ->
        let after = Option.get (L.find_opt ledger address) in
        let sender = address = owner in
        expect "wasm response retained money"
          (Z.equal after.L.balance (if sender then Z.sub before.L.balance fee
            else before.balance));
        expect "wasm response nonce differs"
          (after.nonce = if sender then before.nonce + 1 else before.nonce)) accounts;
      expect_confirmed "wasm response next nonce" (wasm_call "read" [`String wasm; `String owner] 0);
      let prefix = "circle reserved key write: " in
      let key = "hfhe_policy:" in
      let key = key ^ String.make (255 - String.length prefix - String.length key) 'a' in
      List.iter (fun scalar ->
        let saved = visits () in
        let root = Lwt_main.run (Store.get_batch_tree_hash store) in
        let accounts = List.map (fun address ->
          address, Option.get (L.find_opt ledger address)) [owner; receiver; wasm] in
        let rejected = try wasm_call "error" [`String (key ^ scalar ^ "z"); `String receiver] 7 with
          | Circle.Execution_unavailable reason ->
            fail ("wasm text became technical retry: " ^ reason)
          | Octra_core.Exec_resource.Unavailable _ -> fail "wasm text became resource retry" in
        begin match rejected with
        | Ok (Epoch_exec.Rejected_after_fee result) ->
          expect "wasm text fee differs" (Z.equal result.fee (Z.of_int 10));
          expect "wasm text outcome differs" (result.error_type = "circle_call_failed"
            && result.reason = prefix ^ key)
        | _ -> fail "wasm text did not persist refusal"
        end;
        expect "wasm text retained writes" (visits () = saved && credit () = Z.of_int 70);
        expect "wasm text changed store" (root = Lwt_main.run (Store.get_batch_tree_hash store));
        List.iter (fun (address, before) ->
          let after = Option.get (L.find_opt ledger address) in
          let sender = address = owner in
          expect "wasm text retained money"
            (Z.equal after.L.balance (if sender then Z.sub before.L.balance (Z.of_int 10)
              else before.balance));
          expect "wasm text nonce differs"
            (after.nonce = if sender then before.nonce + 1 else before.nonce)) accounts;
        expect_confirmed "wasm text next nonce" (wasm_call "read" [`String wasm; `String owner] 0))
        ["\208\182"; "\226\130\172"; "\240\144\128\128"];
      let error = String.make 255 'a' ^ "\208\182" in
      let clipped = Contract.trim_error error in
      expect "wasm text trigger is valid" (Circles.is_valid_utf8 error);
      expect "wasm text trigger was not split" (not (Circles.is_valid_utf8 clipped));
      let root = Lwt_main.run (Store.get_batch_tree_hash store) in
      List.iter (fun (error, expected) ->
        let result = try
          Lwt_main.run (interrupted (fun () -> Lwt.return (Error error)))
        with Circle.Execution_unavailable reason ->
          fail ("wasm text became technical retry: " ^ reason) in
        expect "wasm text refusal lost" (not result.receipt.success);
        expect "wasm text refusal differs" (result.receipt.error = Some expected);
        expect "wasm text changed store" (root = Lwt_main.run (Store.get_batch_tree_hash store)))
        [clipped, String.make 255 'a' ^ "?"; "\255\192\175", "???";
         "short error", "short error"; "valid \208\182", "valid \208\182"];
      let saved = int_of_string (Option.get (visits ())) in
      let funds = balance wasm in
      let received = balance receiver in
      let paid = balance owner in
      begin match wasm_call ~fee:10_000_000 "depth" [`Int 8; `String receiver] 0 with
      | Ok (Epoch_exec.Confirmed fee) ->
        expect "wasm depth fee differs" (Z.equal fee (Z.of_int 10_000_000))
      | _ -> fail "wasm depth eight refused"
      end;
      expect "wasm depth eight lost writes" (visits () = Some (string_of_int (saved + 9)));
      expect "wasm depth eight lost money"
        (balance wasm = Z.sub funds (Z.of_int 9)
         && balance receiver = Z.add received (Z.of_int 9)
         && balance owner = Z.sub paid (Z.of_int 10_000_000));
      List.iter (fun (method_name, steps, fee, reason) ->
        let saved = visits () in
        let root = Lwt_main.run (Store.get_batch_tree_hash store) in
        let accounts = List.map (fun address ->
          address, Option.get (L.find_opt ledger address)) [owner; receiver; wasm] in
        let rejected = try wasm_call ~fee method_name [`Int steps; `String receiver] 7 with
          | Circle.Execution_unavailable reason -> fail ("wasm work technical retry: " ^ reason)
          | Octra_core.Exec_resource.Unavailable _ -> fail "wasm work resource retry" in
        begin match rejected with
        | Ok (Epoch_exec.Rejected_after_fee result) ->
          expect "wasm work fee differs" (result.fee = Z.of_int fee);
          expect ("wasm work refusal differs: " ^ result.reason)
            (result.error_type = "circle_call_failed" && result.reason = reason)
        | _ -> fail "wasm work did not persist refusal"
        end;
        expect "wasm work retained writes" (visits () = saved);
        expect "wasm work changed store" (root = Lwt_main.run (Store.get_batch_tree_hash store));
        List.iter (fun (address, before) ->
          let after = Option.get (L.find_opt ledger address) in
          let sender = address = owner in
          expect "wasm work retained money"
            (after.L.balance = if sender then Z.sub before.L.balance (Z.of_int fee)
              else before.balance);
          expect "wasm work nonce differs"
            (after.nonce = if sender then before.nonce + 1 else before.nonce)) accounts;
        expect_confirmed "wasm work next nonce" (wasm_call "read" [`String wasm; `String owner] 0))
        ["depth", 9, 10_000_000, "circle call depth exceeded";
         "fuel", 100_000, 10, "all fuel consumed by WebAssembly"]
    end else begin
      expect "wasm calls enabled before activation" (match deposit with
        | Ok (Epoch_exec.Rejected_after_fee _) -> true
        | _ -> false);
      expect "historical wasm deposit retained credit" (Z.equal (credit ()) Z.zero);
      expect_confirmed "prior wasm control" (wasm_call "visit" [] 0);
      let saved = Lwt_main.run (Store.load_circle_stable_storage store wasm) |> Result.get_ok in
      let large = Hashtbl.copy saved in
      for index = 1 to 208 do
        Hashtbl.add large ("large" ^ string_of_int index) (String.make 65_536 'x')
      done;
      ignore (Lwt_main.run (Store.save_circle_stable_storage store wasm large));
      Hashtbl.reset Octra_core.Circle_wasm_host.code_seed_cache;
      Octra_core.Circle_wasm_host.clear_storage_payload_cache ();
      let root = Lwt_main.run (Store.get_batch_tree_hash store) in
      let before = Option.get (L.find_opt ledger owner) in
      let funds = L.find_opt ledger wasm in
      let transaction = tx ~owner ~target:wasm ~nonce:(next ())
        ~op_type:Transaction.CircleCall ~payload:(Some "visit") ~message:(Some "[]") in
      let error_type, reason, fee = match apply {transaction with amount = Z.of_int 7} with
        | Ok (Epoch_exec.Rejected_after_fee result) -> result.error_type, result.reason, result.fee
        | _ -> fail "prior wasm size refusal missing" in
      let legacy = try
        Scanf.sscanf reason "input too large: bytes=%d limit=%d%!" (fun bytes limit ->
          expect "prior wasm size limit differs" (bytes > limit && limit = 16_777_216);
          Printf.sprintf "input too large: bytes=%d limit=%d" bytes limit)
        with Scanf.Scan_failure _ | End_of_file -> fail "prior wasm refusal bytes changed" in
      let module Outcome = Octra_core.Tx_outcome in
      let transaction = {transaction with amount = Z.of_int 7} in
      let actual = Outcome.build ~inputs:[transaction]
        [transaction, error_type, reason] |> Result.get_ok in
      let expected = Outcome.{position = 0; tx = transaction;
        error_type = "circle_call_failed"; reason = legacy} in
      expect "prior wasm encoded refusal differs"
        (List.map Outcome.encode_rejection actual = [Outcome.encode_rejection expected]
         && Outcome.equal actual [expected]);
      expect "changed wasm refusal accepted"
        (not (Outcome.equal actual [{expected with reason = legacy ^ " "}]));
      let after = Option.get (L.find_opt ledger owner) in
      expect "prior wasm refusal changed funds"
        (Z.equal fee transaction.ou && L.find_opt ledger wasm = funds
         && Z.equal after.balance (Z.sub before.balance transaction.ou)
         && after.nonce = before.nonce + 1);
      expect "prior wasm refusal retained writes"
        (root = Lwt_main.run (Store.get_batch_tree_hash store));
      ignore (Lwt_main.run (Store.save_circle_stable_storage store wasm saved));
      expect_confirmed "prior wasm size recovery" (wasm_call "visit" [] 0)
    end;
    let before = balance receiver in
    call ~value:17 circle "pay" (pay 17 false) active;
    expect "circle payout amount differs"
      (Z.equal (balance receiver) (Z.add before (Z.of_int (if active then 17 else 0))));
    List.iter (fun (parent, child) ->
      let params refuse = [`String (snd child); `String receiver; `Int 11; `Bool refuse] in
      let before = balance receiver in
      call parent "relay" (params false) active;
      expect "nested payout amount differs"
        (Z.equal (balance receiver) (Z.add before (Z.of_int (if active then 11 else 0))));
      call parent "relay" (params true) false)
      [program, circle; circle, program; circle, second];
    List.iter (fun target ->
      call target "pay" (pay 7 true) false;
      call target "pay" (pay 100_000 false) false;
      call target "pay" (pay (-1) false) false)
      [circle; second];
    if active then begin
      let accounts = [owner; receiver; snd program; snd circle; snd second] in
      let money = List.map balance accounts in
      let counters = List.map count [program; circle; second] in
      let item method_name params = `Assoc ["to", `String (snd program);
        "method", `String method_name; "params", `List params; "amount", `String "0"] in
      let calls = [
        item "relay" [`String (snd circle); `String receiver; `Int 7; `Bool false];
        item "pay" (pay 5 true)] in
      begin match apply (tx ~owner ~target:(snd program) ~nonce:(next ())
        ~op_type:Transaction.MultiExec ~payload:None
        ~message:(Some (Yojson.Safe.to_string (`List calls)))) with
      | Ok (Epoch_exec.Rejected_after_fee _) -> ()
      | _ -> fail "multi payout failure accepted"
      end;
      expect "multi payout retained state" (counters = List.map count [program; circle; second]);
      expect "multi payout retained funds"
        (List.map balance accounts = match money with
          | paid :: rest -> Z.sub paid (Z.of_int 10) :: rest
          | [] -> assert false)
    end;
    if active then begin
      selected := R.Prior;
      let legacy = deploy true in
      selected := R.Active;
      let address = snd legacy in
      let before = balance address in
      let enabled () =
        let info = Option.get (Lwt_main.run (Store.get_circle_info store address)) in
        Lwt_main.run (Octra_core.Circle_deploy.calls_enabled store info) in
      let spawn hash expected =
        let module C = Octra_circle_runtime.Circle_exec in
        let payload = Circles.{runtime = Octb; privacy_class = Public;
          browser_mode = Native_sealed; resource_mode = Public_resources;
          code_b64 = Some encoded; policy_hash = None; members_root = None;
          export_policy = None; limits = default_limits} in
        let payload_json = Yojson.Safe.to_string (Circles.yojson_of_deploy_payload payload) in
        let source = D.Spawn {parent = address; caller = owner; tx_hash = hash;
          spawn_nonce = 0; owner_mode = Circles.Spawn_owner_caller; payload_json} in
        let child = D.circle_id_of_source source payload in
        let result = C.failed_call_result "" in
        let storage = Lwt_main.run (Store.load_circle_stable_storage store address) |> Result.get_ok in
        let result = {result with C.calls = true; caller = owner; tx_hash = hash;
          receipt = {result.receipt with success = true; error = None};
          storage_tbl = storage; baseline_storage_tbl = Hashtbl.copy storage;
          spawns = [Octra_core.Circle_wasm_host.{circle_id = child; spawn_nonce = 0;
            owner_mode = Circles.Spawn_owner_caller; payload_json}]} in
        expect "child commit failed"
          (Lwt_main.run (C.commit_call_result ~proof_mode:R.Active store address result) = Ok ());
        let info = Option.get (Lwt_main.run (Store.get_circle_info store child)) in
        expect "child acquired different call permission"
          (Lwt_main.run (D.calls_enabled store info) = expected) in
      expect "old circle authorized by activation" (not (enabled ()));
      spawn "before" false;
      call program "repeat" [`String address; `Bool false] false;
      call legacy "pay" (pay 5 false) false;
      expect "old circle spent money" (balance address = before);
      let update sender = tx ~owner:sender ~target:address
        ~nonce:(if sender = owner then next () else 1)
        ~op_type:Transaction.CircleProgramUpdate ~payload:None
        ~message:(Some (Yojson.Safe.to_string (`Assoc ["code_b64", `String encoded]))) in
      begin match apply (update receiver) with
      | Ok (Epoch_exec.Confirmed _) -> fail "nonowner enabled circle payouts"
      | _ -> ()
      end;
      expect "failed update authorized code" (not (enabled ()));
      expect_confirmed "owner enabled circle payouts" (apply (update owner));
      expect "owner update did not authorize code" (enabled ());
      spawn "after" true;
      call program "repeat" [`String address; `Bool false] true;
      call legacy "pay" (pay 5 false) true;
      expect "updated circle payout differs" (balance address = Z.sub before (Z.of_int 5));
      Lwt_main.run (Store.write store ["circles"; address; "call_code"] (String.make 64 '0'));
      expect "different code retained permission" (not (enabled ()));
      call legacy "pay" (pay 5 false) false
    end;
    List.iter (fun (parent, child) ->
      let a = count parent in
      let b = count child in
      call parent "bounce" [`String (snd child); `String (snd parent); `Int 2] active;
      if active then begin
        expect "reentry lost parent write" (Z.equal (count parent) (Z.add a (Z.of_int 2)));
        expect "reentry lost child write" (Z.equal (count child) (Z.succ b))
      end;
      call parent "bounce" [`String (snd child); `String (snd parent); `Int 9] false)
      [circle, program; program, circle; circle, second];
    if active then begin
      let address = snd second in
      let saved = Result.get_ok (Lwt_main.run (Store.load_circle_stable_storage store address)) in
      let ctx = {ContractVM.default_ctx with proof_exec = R.Active; current_epoch = epoch} in
      let measured value =
        let cells = Hashtbl.copy saved in
        Hashtbl.replace cells "encoded" value;
        let journal = J.create () in
        ignore (J.circle_storage journal address cells);
        let result = Lwt_main.run (Octra_circle_runtime.Circle_exec.execute_call
          ~journal ~trusted:(Program_trust.keys program_trust) ~ctx ~limit:1_000_000
          store address "count" [] owner Z.zero) in
        expect "serialized work control failed" result.receipt.success;
        result.receipt.effort_used in
      expect "circle execution omitted escaped output"
        (measured (String.make 1024 '\000') - measured (String.make 1024 'x') = 320);
      let large = Hashtbl.copy saved in
      for i = 1 to 32 do
        Hashtbl.add large ("data" ^ string_of_int i) (String.make 32_768 'x')
      done;
      ignore (Lwt_main.run (Store.save_circle_stable_storage store address large));
      let reads = ref 0 in
      let denied = Store.load_circle_stable_storage
        ~charge:(fun _ -> incr reads; false) store address |> Lwt_main.run in
      expect "circle read ignored entry budget"
        (denied = Error "circle storage read effort exceeds limit" && !reads = 1);
      reads := 0;
      let denied = Store.load_circle_stable_storage
        ~charge:(fun _ -> incr reads; !reads = 1) store address |> Lwt_main.run in
      expect "circle read ignored byte budget"
        (denied = Error "circle storage read effort exceeds limit" && !reads = 2);
      let volume = ref Z.zero in
      let loaded = Store.load_circle_stable_storage
        ~charge:(fun cost -> volume := Z.add !volume cost; true) store address
        |> Lwt_main.run |> Result.get_ok in
      expect "circle streamed read differs"
        (Hashtbl.length loaded = Hashtbl.length large &&
         Hashtbl.fold (fun key value ok -> ok && Hashtbl.find_opt loaded key = Some value)
           large true && Z.geq !volume (Z.of_int 65_536));
      let denied = Lwt_main.run (Octra_circle_runtime.Circle_exec.execute_call
        ~trusted:(Program_trust.keys program_trust)
        ~ctx:{ContractVM.default_ctx with proof_exec = R.Active; current_epoch = epoch}
        ~limit:1 store address "count" [] owner Z.zero) in
      expect "circle call skipped read budget"
        (denied.receipt.error = Some "circle storage read effort exceeds limit" &&
         denied.receipt.effort_used = 1);
      call program "repeat" [`String address; `Bool false] true;
      call program "repeat" [`String address; `Bool true] false;
      call program "chain" [`String address; `String (snd program); `Bool false] true;
      call program "chain" [`String address; `String (snd program); `Bool true] true;
      let multi count =
        let item method_name params = `Assoc ["to", `String (snd program);
          "method", `String method_name; "params", `List params; "amount", `String "0"] in
        let calls = item "repeat" [`String address; `Bool false] ::
          List.init count (fun _ -> item "count" []) in
        apply (tx ~owner ~target:(snd program) ~nonce:(next ())
          ~op_type:Transaction.MultiExec ~payload:None
          ~message:(Some (Yojson.Safe.to_string (`List calls)))) in
      expect_confirmed "multi circle control" (multi 1);
      expect_confirmed "multi shallow journal copy" (multi 6);
      ignore (Lwt_main.run (Store.save_circle_stable_storage store address saved))
    end;
    if active then List.iter (fun (phase, kind) ->
      let saved = List.map count [program; circle; second] in
      let accounts = List.map (fun address -> address, Option.get (L.find_opt ledger address))
        [owner; receiver; snd program; snd circle; snd second] in
      let root = Lwt_main.run (Store.get_batch_tree_hash store) in
      let sequence = next () in
      let target, operation, payload, message =
        if phase = "circle_payload" || phase = "circle_prepare" then
          let data = Circles.{runtime = Octb; privacy_class = Public;
            browser_mode = Native_sealed; resource_mode = Public_resources;
            code_b64 = Some encoded; policy_hash = None; members_root = None;
            export_policy = None; limits = default_limits} in
          Circles.circle_id_of_deploy ~deployer:owner ~nonce:sequence data,
          Transaction.CircleDeploy, None, Circles.yojson_of_deploy_payload data
        else if phase = "circle_update" then
          snd circle, Transaction.CircleProgramUpdate, None,
          `Assoc ["code_b64", `String encoded]
        else snd circle, Transaction.CircleCall, Some "relay",
          `List [`String (snd second); `String receiver; `Int 5; `Bool false] in
      let transaction = tx ~owner ~target ~nonce:sequence
        ~op_type:operation ~payload ~message:(Some (Yojson.Safe.to_string message)) in
      let saved_env = List.map (fun name -> name, Sys.getenv_opt name)
        ["OCTRA_CHAOS_FAIL_AT"; "OCTRA_CHAOS_FAIL_KIND"] in
      let written = ref [] in
      let execute () = Octra_core.Tx_savepoint.run ~ledger ~store (fun () ->
        Transition.process_tx ~backend ~env:{env with epoch_id = epoch}
          ~circle_mode:R.Prior ~wasm_compute_mode:R.Active ~program_trust ~object_cost:false
          ~save_receipt_raw:(fun ~tx_hash ~json -> written := (tx_hash, json) :: !written)
          transaction) in
      let refused = Fun.protect
        ~finally:(fun () -> List.iter (fun (name, value) ->
          Unix.putenv name (Option.value ~default:"" value)) saved_env)
        (fun () ->
          Unix.putenv "OCTRA_CHAOS_FAIL_AT" phase;
          Unix.putenv "OCTRA_CHAOS_FAIL_KIND" kind;
          try ignore (Lwt_main.run (execute ())); false with
          | Octra_core.Exec_resource.Unavailable _ -> kind <> "cancel"
          | Lwt.Canceled -> kind = "cancel") in
      expect "circle commit fault did not escape" refused;
      expect "circle commit fault published receipt" (!written = []);
      expect "circle commit fault kept state" (saved = List.map count [program; circle; second]);
      expect "circle commit fault kept store" (root = Lwt_main.run (Store.get_batch_tree_hash store));
      List.iter (fun (address, before) ->
        let after = Option.get (L.find_opt ledger address) in
        expect "circle commit fault kept account"
          (Z.equal before.L.balance after.balance && before.nonce = after.nonce)) accounts;
      expect_confirmed "circle commit repeat" (Lwt_main.run (execute ()));
      expect "circle commit repeat receipt"
        (List.length !written = if operation = Transaction.CircleCall then 1 else 0))
      (List.concat_map (fun phase ->
         List.map (fun kind -> phase, kind) ["host"; "memory"; "stack"; "cancel"])
         ["circle_payload"; "circle_prepare"; "circle_update";
          "vm_commit_after_program"; "vm_commit_after_value"]);
    Lwt_main.run (L.flush_dirty_lwt ledger);
    let result = Lwt_main.run (Store.get_batch_tree_hash store), List.rev !receipts in
    Store.abort_epoch_batch store;
    result) in
  ignore (run 1_662_999);
  expect "circle execution differs across replay" (run 1_663_000 = run 1_663_000)

let test_wasm_pairs ?(keyed = false) () =
  let module R = Octra_core.Rule_graph in
  let module L = Octra_core.Ledger in
  let module H = Octra_core.Circle_hfhe_policy in
  let module F = Pvac_ffi in
  let module B = Octra_core.Crypto.FheBalance in
  let seed = Bytes.make 32 '\012' in
  let pk, one, two = if keyed then
    F.deserialize_pubkey (Hfhe_case.key ()), Hfhe_case.cipher (), Hfhe_case.cipher ~index:2 ()
  else
    let pk, sk = F.keygen_from_seed (F.default_params ()) seed in
    pk, F.enc_values_seeded pk sk [|3L|] seed, F.enc_values_seeded pk sk [|5L; 7L|] seed in
  let reason = if keyed then "hfhe key shape mismatch" else "hfhe slot count mismatch" in
  let path = Filename.concat (Filename.dirname Sys.executable_name) "vault.wasm" in
  let channel = open_in_bin path in
  let code = Fun.protect ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel)) in
  let env = {env with chain_id = "octra-devnet-9871-cluster"} in
  let run replay epoch = with_store ("wasm_pairs_" ^ string_of_int epoch) (fun store ->
    let owner = "oct6wMiXWiH5SKkfpXC9RvGEBTyPPMN9NgQxVWPDwjhmtcZ" in
    let receiver = "oct7xCozDD9JEsbeVpo5C7HXp2BJbKqfmNUHmDDCCTtWcGb" in
    let mode = R.proof_exec_at ~chain_id:env.chain_id ~epoch in
    let active = mode = R.Active in
    let ledger = L.create store in
    List.iter (fun (address, amount) ->
      match L.add_account ledger address (Z.of_int amount) with
      | Ok () -> ()
      | Error reason -> fail reason) [owner, 400_000_000; receiver, 100];
    Lwt_main.run (L.set_pvac_pubkey ledger owner (F.serialize_pubkey pk |> Bytes.to_string));
    Lwt_main.run (L.flush_dirty_lwt ledger);
    let fold epoch = Epoch_exec.prior_fold epoch
      |> Result.map (fun ctx -> {ctx with Epoch_exec.proof_exec = mode}) in
    let backend = Epoch_exec.make_live_backend ~fold store ledger in
    Lwt_main.run (backend.begin_batch R.Prior);
    let receipts = ref [] in
    let apply ?preverify transaction = Lwt_main.run (Octra_core.Tx_savepoint.run ~ledger ~store (fun () ->
      Transition.process_tx ?preverify ~backend ~env:{env with epoch_id = epoch}
        ~circle_mode:R.Prior ~wasm_compute_mode:R.Active ~program_trust ~object_cost:false
        ~save_receipt_raw:(fun ~tx_hash ~json -> receipts := (tx_hash, json) :: !receipts)
        transaction)) in
    let payload = Circles.{runtime = Wasm_v1; privacy_class = Public;
      browser_mode = Native_sealed; resource_mode = Public_resources;
      code_b64 = Some (Base64.encode_exn code); policy_hash = None; members_root = None;
      export_policy = None; limits = default_limits} in
    let target = Circles.circle_id_of_deploy ~deployer:owner ~nonce:1 payload in
    expect_confirmed "pair circle deploy" (apply (tx ~owner ~target ~nonce:1
      ~op_type:Transaction.CircleDeploy ~payload:None
      ~message:(Some (Yojson.Safe.to_string (Circles.yojson_of_deploy_payload payload)))));
    let storage = Lwt_main.run (Store.load_circle_stable_storage store target) |> Result.get_ok in
    Hashtbl.replace storage H.require_live_key_policy_key "false";
    List.iter (fun key -> Hashtbl.replace storage key "any_registered")
      [H.load_pk_mode_key; H.cipher_arithmetic_mode_key;
       H.cipher_serde_mode_key; H.pubkey_serde_mode_key];
    ignore (Lwt_main.run (Store.save_circle_stable_storage store target storage));
    begin match L.credit ledger target (Z.of_int 100) with
    | Ok () -> () | Error reason -> fail reason end;
    let account address = Option.get (L.find_opt ledger address) in
    let cell key = Lwt_main.run (Store.load_circle_stable_storage store target)
      |> Result.get_ok |> fun storage -> Hashtbl.find_opt storage key in
    let call ~nested ~subtract ~valid =
      Lwt_main.run (L.flush_dirty_lwt ledger);
      let saved = cell "visits" in
      let root = Lwt_main.run (Store.get_batch_tree_hash store) in
      let before = List.map (fun address -> address, account address) [owner; receiver; target] in
      let written = List.length !receipts in
      let rhs = if valid then one
        else if keyed && subtract then Hfhe_case.cipher ~width:2 () else two in
      let params = [`String owner; `String (B.encode_cipher one); `String (B.encode_cipher rhs);
        `Bool subtract; `Bool active; `Bool nested; `String receiver] in
      let params = if not keyed then params
        else params @ [`String (Hfhe_case.key () |> Bytes.to_string |> Base64.encode_exn)] in
      let transaction = tx ~owner ~target ~nonce:((account owner).nonce + 1)
        ~op_type:Transaction.CircleCall ~payload:(Some "fhe_pair")
        ~message:(Some (Yojson.Safe.to_string (`List params))) in
      let fee = Z.of_int 20_000_000 in
      let transaction = {transaction with ou = fee} in
      let preverify = if not replay then None else begin
        let module P = Octra_core.Preverify_receipt in
        let module W = Octra_core.Preverify_worker in
        let module C = Octra_core.Preverify_commit in
        let snapshot = Lwt_main.run (L.hash ledger) |> W.state_hash in
        L.begin_journal ledger |> Result.get_ok;
        let saved = Store.save_batch store |> Result.get_ok in
        let result, binding = Fun.protect
          ~finally:(fun () -> Octra_core.Tx_savepoint.restore ledger store saved)
          (fun () -> Lwt_main.run (Transition.capture_circle ~circle_mode:R.Active
            ~wasm_compute_mode:R.Active ~backend ~env:{env with epoch_id = epoch}
            ~program_trust ~object_cost:false transaction)) in
        begin match result with
        | Ok (Epoch_exec.Confirmed actual) when valid ->
          expect "capture pair fee differs" (actual = fee)
        | Ok (Epoch_exec.Rejected_after_fee rejected) when not valid ->
          expect "capture pair refusal differs" (rejected.reason = reason)
        | _ -> fail "pair capture outcome differs"
        end;
        let circle = Octra_node_runtime.Consensus_circle_preverify.circle_state
          snapshot (Option.get binding) in
        let receipt = P.make_circle ~tx_hash:(Transaction.hash transaction)
          ~input_hash:(W.circle_input_hash transaction circle)
          ~output_hash:(W.circle_output_hash transaction circle "ok")
          ~circle ~ok:true ~reason:"" ~cost:(Octra_core.Resource_lanes.cost transaction)
          |> Result.get_ok in
        expect "pair preverify receipt invalid" (C.check_receipt transaction receipt = Ok ());
        expect "pair preverify state differs"
          (Lwt_main.run (C.check_state ledger transaction receipt) = Ok ());
        if not nested then begin
          let method_name = if subtract then "fhe_sub" else "fhe_add" in
          expect "pair refusal entry missing"
            (List.map (fun entry -> entry.Octra_core.Circle_hfhe_transcript.method_name)
              circle.transcript = if keyed then [method_name] else ["fhe_load_pk"; method_name]);
          let transcript = List.map (fun entry ->
            if entry.Octra_core.Circle_hfhe_transcript.method_name <> method_name then entry
            else
              let first = if entry.response_hash.[0] = '0' then "1" else "0" in
              {entry with response_hash = first ^ String.sub entry.response_hash 1 63})
            circle.transcript in
          let changed = {circle with transcript} in
          let altered = {receipt with P.circle = Some changed;
            input_hash = W.circle_input_hash transaction changed;
            output_hash = W.circle_output_hash transaction changed "ok"} in
          expect "altered pair receipt shape invalid" (C.check_receipt transaction altered = Ok ());
          let refused = try ignore (apply ~preverify:(C.create [altered]) transaction); false with
            | Octra_vm.Direct_exec.Receipt_mismatch _ -> true
            | Octra_circle_runtime.Circle_exec.Execution_unavailable reason ->
              reason = "hfhe receipt was not fully consumed" in
          expect "altered pair receipt was applied" refused;
          expect "altered pair receipt was published" (List.length !receipts = written);
          expect "altered pair receipt changed root"
            (root = Lwt_main.run (Store.get_batch_tree_hash store));
          List.iter (fun (address, before) ->
            let after = account address in
            expect "altered pair receipt changed account"
              (after.L.balance = before.L.balance && after.nonce = before.nonce)) before
        end;
        Some (C.create [receipt])
      end in
      let result = try Some (apply ?preverify transaction) with
        | Octra_circle_runtime.Circle_exec.Execution_unavailable reason ->
          expect "active pair became technical retry" (not active && not valid);
          expect "prior pair message differs" (reason = "hfhe backend exception");
          None in
      begin match result with
      | Some (Ok (Epoch_exec.Confirmed actual)) when valid ->
        expect "valid pair fee differs" (actual = fee);
        let expected = if subtract then F.ct_sub pk one one else F.ct_add pk one one in
        let expected = B.encode_cipher expected |> String.length |> string_of_int in
        expect "valid pair size differs" (cell "cipher" = Some expected);
        expect "valid pair did not write" (cell "visits" <> saved)
      | Some (Ok (Epoch_exec.Rejected_after_fee rejected)) when active && not valid ->
        expect ("pair refusal differs: " ^ rejected.reason)
          (rejected.reason = reason && rejected.fee = fee)
      | None when not active && not valid -> ()
      | Some (Ok (Epoch_exec.Rejected_after_fee rejected)) ->
        fail (Printf.sprintf "pair rejected: epoch = %d nested = %b subtract = %b valid = %b reason = %s"
          epoch nested subtract valid rejected.reason)
      | Some (Error (kind, reason)) -> fail ("pair refused: " ^ kind ^ ": " ^ reason)
      | _ -> fail "pair execution outcome differs"
      end;
      if not valid then begin
        expect "pair refusal retained writes" (cell "visits" = saved);
        expect "pair refusal changed root" (root = Lwt_main.run (Store.get_batch_tree_hash store))
      end;
      expect "pair receipt count differs"
        (List.length !receipts = written + if active || valid then 1 else 0);
      List.iter (fun (address, before) ->
        let after = account address in
        let paid = address = owner && (active || valid) in
        let payout = if active && valid then Z.of_int (if nested then 2 else 1) else Z.zero in
        let expected = if paid then Z.sub before.L.balance fee
          else if address = receiver then Z.add before.balance payout
          else if address = target then Z.sub before.balance payout
          else before.balance in
        expect "pair account balance differs" (after.L.balance = expected);
        expect "pair account nonce differs"
          (after.nonce = before.nonce + if paid then 1 else 0)) before in
    List.iter (fun nested ->
      List.iter (fun subtract ->
        call ~nested ~subtract ~valid:false;
        if not active || keyed then call ~nested ~subtract ~valid:true;
        if active then begin
          let transaction = tx ~owner ~target ~nonce:((account owner).nonce + 1)
            ~op_type:Transaction.CircleCall ~payload:(Some "visit") ~message:(Some "[]") in
          expect_confirmed "pair refusal next nonce" (apply transaction)
        end) [false; true])
      (if active then [false; true] else [false]);
    Lwt_main.run (L.flush_dirty_lwt ledger);
    let result = Lwt_main.run (Store.get_batch_tree_hash store), List.rev !receipts in
    Store.abort_epoch_batch store;
    result) in
  ignore (run false 1_662_999);
  expect "pair execution differs across replay" (run false 1_663_000 = run true 1_663_000)

let test_wasm_text () =
  let module W = Octra_circle_runtime.Wasm_call in
  let target = "oct11111111111111111111111111111111111111111111" in
  let check reason expected =
    let ctx = {ContractVM.default_ctx with proof_exec = Octra_core.Rule_graph.Active;
      call_async = (fun _ _ _ _ _ -> Lwt.return (Error reason))} in
    let result = Lwt_main.run (W.dispatch ~ctx ~depth:0 ~address:target ~value:Z.zero
      ~storage:(Hashtbl.create 0) ~events:(ref []) (`Assoc [
        "id", `Int 1; "fuel", `Int 20_000_000; "events", `Int 0;
        "method", `String "program_call"; "storage_pairs", `List [];
        "params", `List (List.map (fun value ->
          `Assoc ["tag", `String "string"; "value", `String value]) [target; "read"])])) in
    let raw = Yojson.Safe.to_string result in
    expect "wasm error json is not utf8" (Circles.is_valid_utf8 raw);
    expect "wasm error bytes differ" (W.field "error" result = `String expected);
    expect "wasm error id differs" (W.field "id" result = `Int 1);
    expect "wasm error returned success" (W.field "response_b64" result = `Null) in
  List.iter (fun scalar ->
    List.iter (fun length ->
      let prefix = String.make length 'a' in
      let reason = prefix ^ scalar ^ "z" in
      let expected = if String.length reason <= 256 then reason
        else if length + String.length scalar <= 256 then prefix ^ scalar
        else prefix in
      check reason expected) [252; 253; 254; 255; 256])
    ["\208\182"; "\226\130\172"; "\240\144\128\128"];
  List.iter (fun (reason, expected) -> check reason expected)
    ["", ""; "short error", "short error"; "\000\001\034\092", "\000\001\034\092";
     String.make 257 'a', String.make 256 'a';
     "\255", "?"; "\128", "?"; "\192\175", "??";
     "\237\160\128", "???"; "\244\144\128\128", "????";
     "\240\159", "??"; "\226\130x", "??x";
     String.make 255 'a' ^ "\208", String.make 255 'a' ^ "?";
     String.make 255 'a' ^ "\255\255", String.make 255 'a' ^ "?"];
  for byte = 128 to 255 do
    check (String.make 1 (Char.chr byte)) "?"
  done;
  for first = 0 to 255 do
    for second = 0 to 255 do
      let text = String.init 2 (function 0 -> Char.chr first | _ -> Char.chr second) in
      let output = W.error_text text in
      expect "wasm text produced invalid utf8" (Circles.is_valid_utf8 output);
      expect "wasm text expanded bytes" (String.length output <= String.length text);
      expect "wasm text changed valid bytes"
        (not (Circles.is_valid_utf8 text) || output = text);
      expect "wasm text is not idempotent" (W.error_text output = output)
    done
  done

let test_wasm_response () =
  let module W = Octra_circle_runtime.Wasm_call in
  let target = "oct11111111111111111111111111111111111111111111" in
  List.iter (fun length ->
    let ctx = {ContractVM.default_ctx with proof_exec = Octra_core.Rule_graph.Active;
      call_async = (fun _ _ _ _ _ -> Lwt.return (Ok ContractVM.{
        return_value = VString (String.make length 'f'); effort_used = 0; events = []}))} in
    let result = Lwt_main.run (W.dispatch ~ctx ~depth:0 ~address:target ~value:Z.zero
      ~storage:(Hashtbl.create 0) ~events:(ref []) (`Assoc [
        "id", `Int 1; "fuel", `Int 20_000_000; "events", `Int 0;
        "method", `String "program_call"; "storage_pairs", `List [];
        "params", `List (List.map (fun value ->
          `Assoc ["tag", `String "string"; "value", `String value]) [target; "read"])])) in
    if length + 10 > 2_097_152 then begin
      expect "wasm response length not refused"
        (W.field "error" result = `String "circle response exceeds limit");
      expect "wasm response returned oversized frame" (W.field "response_b64" result = `Null)
    end else begin
      expect "wasm response valid length refused" (W.field "error" result = `Null);
      match W.field "response_b64" result with
      | `String encoded ->
        expect "wasm response frame length differs"
          (String.length (Base64.decode_exn encoded) = length + 10)
      | _ -> fail "wasm response missing"
    end) [2_097_141; 2_097_142; 2_097_143];
  let storage = Hashtbl.create 2 in
  Hashtbl.add storage "key" "\000\255\127";
  Hashtbl.add storage "\000\255" "value";
  let id = `Intlit "18446744073709551615" in
  let response = W.reply ~id ~bytes:"AA==" ~effort:20_000_000 storage |> Result.get_ok in
  expect "wasm reply changed storage" (W.field "storage_pairs" response
    = `List (Octra_core.Circle_wasm_host.make_storage_pairs_json storage));
  let limit = 64 * 1024 * 1024 in
  Hashtbl.clear storage;
  Hashtbl.add storage "k" (String.make (48 * 1024 * 1024 - 128) 'x');
  let response = W.reply ~id ~bytes:"" ~effort:0 storage |> Result.get_ok in
  let size = String.length (Yojson.Safe.to_string response) in
  List.iter (fun delta ->
    let bytes = String.make (limit - size + delta) 'A' in
    match W.reply ~id ~bytes ~effort:0 storage with
    | Ok payload ->
      expect "wasm reply limit accepted excess" (delta <= 0);
      expect "wasm reply size calculation differs"
        (String.length (Yojson.Safe.to_string payload) = limit + delta)
    | Error reason ->
      expect "wasm reply limit refused valid data"
        (delta > 0 && reason = "circle reply exceeds limit")) [-1; 0; 1];
  Hashtbl.clear storage;
  Gc.full_major ()

let () =
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--keys" then begin
    test_wasm_pairs ~keyed:true ();
    print_endline "status = pass test = wasm_keys";
    exit 0
  end;
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--pairs" then begin
    test_wasm_pairs ();
    print_endline "status = pass test = wasm_pairs"
  end else if Array.length Sys.argv = 2 && Sys.argv.(1) = "--text" then begin
    test_wasm_text ();
    print_endline "status = pass test = wasm_text"
  end else if Array.length Sys.argv = 2 && Sys.argv.(1) = "--response" then begin
    test_wasm_text ();
    test_wasm_response ();
    print_endline "status = pass test = wasm_response"
  end else begin
  test_circle_calls ();
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--circle" then
    print_endline "status = pass test = circle_calls"
  else begin
  test_wasm_pairs ();
  test_wasm_pairs ~keyed:true ();
  test_wasm_text ();
  test_wasm_response ();
  test_write_abort ~phase:"vm_commit_after_program" ();
  test_write_abort ~phase:"vm_commit_after_value" ();
  test_policy_abort ();
  test_circle_work ();
  test_spawn_budget ();
  test_fhe_journal ();
  test_resource_abort ();
  test_write_abort ();
  test_deterministic_transition ();
  test_source_deploy_parity ();
  test_program_switch ();
  test_circle_check ();
  print_endline "status = pass test = vm_transition"
  end
  end