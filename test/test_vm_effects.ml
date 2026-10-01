(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Contract = Octra_vm.Contract
module ContractVM = Octra_vm.Contract_vm
module Call_plan = Octra_vm.Call_plan
module Bytecode = Octra_vm.Bytecode
module Receipt_view = Octra_vm.Receipt_view
module Shell = Octra_node_runtime.Consensus_epoch_vm_shell
module Transaction = Octra_core.Transaction
module Circle_exec = Octra_circle_runtime.Circle_exec

let fail msg =
  failwith ("test_vm_effects: " ^ msg)

let expect label cond =
  if not cond then fail label

let receipt ?(success = true) ?error () =
  Contract.{
    success;
    return_value = Some (ContractVM.VString "ok");
    effort_used = 7;
    events = [];
    error;
    storage_writes = 0;
  }

let deps ~state ~pending ~call ~deploy =
  Shell.{
    get_balance = (fun _ -> Z.of_int !state);
    transfer = (fun ~from_addr:_ ~to_addr:_ ~amount ->
      state := Z.to_int amount;
      true);
    snapshot_value = (fun () -> !state);
    restore_value = (fun snapshot -> state := snapshot);
    snapshot_program = (fun () -> !pending);
    restore_program = (fun snapshot -> pending := snapshot);
    execute_call = call;
    deploy_internal = deploy;
    get_fhe_pubkey = (fun _ -> None);
    object_cost = false;
    current_epoch = 3;
    epoch_time_ms = 30_000L;
    tree_hash = "tree";
    node_id = "node";
    tx_hash = "tx";
    point_ops = false;
    math = false;
    int_work = Octra_vm.Int_work.Prior;
    fhe_work = Octra_core.Rule_graph.Prior;
    wasm_float = Octra_core.Rule_graph.Prior;
  }

let default_deploy ~ctx:_ ~depth:_ ~limit:_ ~params:_ ~deployer:_ ~bytecode_raw:_ ~nonce:_ =
  Ok { ContractVM.spawned_addr = "octSpawn"; effort_used = 1; events = [] }

let default_call ~ctx:_ ~depth:_ ~limit:_ ~target:_ ~method_name:_ ~params:_ ~caller:_ ~amount:_ =
  receipt ()

let test_call_success_keeps_effects () =
  let state = ref 0 in
  let pending = ref 0 in
  let call ~ctx:_ ~depth:_ ~limit:_ ~target:_ ~method_name:_ ~params:_ ~caller:_ ~amount:_ =
    incr state;
    incr pending;
    receipt ()
  in
  let ctx = Shell.make_contract_ctx (deps ~state ~pending ~call ~deploy:default_deploy) in
  let result = ctx.call_contract "caller" "target" "method" [] {depth = 1; limit = None; memory = ctx.fhe_memory} in
  expect "call success" (Result.is_ok result);
  expect "state kept" (!state = 1);
  expect "pending kept" (!pending = 1)

let test_call_restore () =
  let state = ref 0 in
  let pending = ref 0 in
  let call ~ctx:_ ~depth:_ ~limit:_ ~target:_ ~method_name:_ ~params:_ ~caller:_ ~amount:_ =
    incr state;
    incr pending;
    receipt ~success:false ~error:"boom" ()
  in
  let ctx = Shell.make_contract_ctx (deps ~state ~pending ~call ~deploy:default_deploy) in
  let result = ctx.call_contract "caller" "target" "method" [] {depth = 1; limit = None; memory = ctx.fhe_memory} in
  expect "call failure" (Result.is_error result);
  expect "state restored" (!state = 0);
  expect "pending restored" (!pending = 0)

let test_deploy_restore () =
  let state = ref 0 in
  let pending = ref 0 in
  let deploy ~ctx:_ ~depth:_ ~limit:_ ~params:_ ~deployer:_ ~bytecode_raw:_ ~nonce:_ =
    incr state;
    incr pending;
    Error "bad deploy"
  in
  let ctx = Shell.make_contract_ctx (deps ~state ~pending ~call:default_call ~deploy) in
  let result = ctx.deploy_contract "deployer" "bytecode" 1 {depth = 1; limit = None; memory = ctx.fhe_memory} [] in
  expect "deploy failure" (Result.is_error result);
  expect "state restored" (!state = 0);
  expect "pending restored" (!pending = 0)

let test_shared_memory () =
  let module Memory = Octra_vm.Fhe_memory in
  let owner = Memory.create () in
  let spend (ctx : ContractVM.exec_ctx) =
    match ctx.fhe_memory with
    | None -> fail "nested operation lost memory budget"
    | Some budget ->
      expect "nested operation replaced memory owner" (budget == owner);
      expect "nested reservation refused" (Memory.reserve budget Z.one) in
  let state = ref 0 in
  let pending = ref 0 in
  let call ~ctx ~depth:_ ~limit:_ ~target:_ ~method_name:_ ~params:_ ~caller:_ ~amount:_ =
    spend ctx;
    incr state;
    incr pending;
    receipt ~success:false ~error:"refused" () in
  let deploy ~ctx ~depth:_ ~limit:_ ~params:_ ~deployer:_ ~bytecode_raw:_ ~nonce:_ =
    spend ctx;
    incr state;
    incr pending;
    Error "refused" in
  let ctx = Shell.make_contract_ctx
    {(deps ~state ~pending ~call ~deploy) with fhe_work = Octra_core.Rule_graph.Active} in
  let scope = ContractVM.{depth = 1; limit = Some 1000; memory = Some owner} in
  expect "nested call unexpectedly succeeded"
    (Result.is_error (ctx.call_contract "caller" "target" "method" [] scope));
  expect "failed call refunded reservation" (Z.equal (Memory.used owner) Z.one);
  expect "nested constructor unexpectedly succeeded"
    (Result.is_error (ctx.deploy_contract "sender" "code" 1 scope []));
  expect "failed constructor refunded reservation" (Z.equal (Memory.used owner) (Z.of_int 2));
  expect "state rollback changed" (!state = 0 && !pending = 0)

let test_transfer_and_context_fields () =
  let state = ref 0 in
  let pending = ref 0 in
  let ctx =
    Shell.make_contract_ctx
      (deps ~state ~pending ~call:default_call ~deploy:default_deploy)
  in
  expect "balance" (ctx.get_balance "addr" = Z.zero);
  expect "transfer" (ctx.do_transfer "a" "b" (Z.of_int 42));
  expect "transfer effect" (!state = 42);
  expect "epoch" (ctx.current_epoch = 3);
  expect "node id" (ctx.node_id = "node");
  expect "tx hash" (ctx.tx_hash = "tx")

let direct_tx ?(method_name = Some "predict") ?(params = Some "[]") ?(amount = Z.zero) () =
  {
    Transaction.from = "octA";
    to_ = "octContract";
    amount;
    nonce = 1;
    ou = Z.of_int 1_000_000;
    timestamp = 0.;
    public_key = None;
    signature = "";
    encrypted_data = method_name;
    message = params;
    op_type = Transaction.ContractCall;
  }

let test_direct_exec_success () =
  let events = ref [] in
  let push event =
    events := event :: !events
  in
  let tx = direct_tx () in
  let spec =
    Shell.direct_exec_spec_of_tx
      ~domain:Receipt_view.Program_call
      ~reject_domain:Call_plan.Program_exec
      ~balance:Z.zero
      tx
  in
  Lwt_main.run
    (Shell.run_direct_exec
       spec
       ~fee:tx.ou
       ~target:tx.to_
       ~apply_value_effect:(fun _ -> push "apply")
       ~log_failed:(fun _ _ _ _ -> push "failed")
       ~reject_after_fee:(fun _ tag reason ->
         push ("after_fee:" ^ tag ^ ":" ^ reason);
         Lwt.return_unit)
       ~reject:(fun r ->
         push ("reject:" ^ r.Call_plan.error_type);
         Lwt.return_unit)
       ~exec:(fun call ->
         push ("exec:" ^ call.Call_plan.method_name);
         Lwt.return (receipt ()))
       ~receipt:(fun r -> r)
       ~save:(fun call _ -> push ("save:" ^ call.Call_plan.method_name))
       ~ok:(fun meta call _ ->
         push ("ok:" ^ meta.Receipt_view.scope ^ ":" ^ call.Call_plan.method_name);
         Lwt.return_unit));
  expect
    "direct exec success"
    (List.rev !events = [
       "apply";
       "exec:predict";
       "save:predict";
       "ok:contract:predict";
     ])

let test_direct_exec_reject () =
  let events = ref [] in
  let tx = direct_tx ~method_name:None () in
  let spec =
    Shell.direct_exec_spec_of_tx
      ~domain:Receipt_view.Program_call
      ~reject_domain:Call_plan.Program_exec
      ~balance:Z.zero
      tx
  in
  Lwt_main.run
    (Shell.run_direct_exec
       spec
       ~fee:tx.ou
       ~target:tx.to_
       ~apply_value_effect:(fun _ -> events := "apply" :: !events)
       ~log_failed:(fun _ _ _ _ -> events := "failed" :: !events)
       ~reject_after_fee:(fun _ tag reason ->
         events := ("after_fee:" ^ tag ^ ":" ^ reason) :: !events;
         Lwt.return_unit)
       ~reject:(fun r ->
         events :=
           ("reject:" ^ r.Call_plan.error_type ^ ":" ^ r.notify_reason)
           :: !events;
         Lwt.return_unit)
       ~exec:(fun _ -> Lwt.return (receipt ()))
       ~receipt:(fun r -> r)
       ~save:(fun _ _ -> events := "save" :: !events)
       ~ok:(fun _ _ _ -> events := "ok" :: !events; Lwt.return_unit));
  expect
    "direct exec reject"
    (List.rev !events = [
       "reject:malformed_transaction:Invalid contract call format";
     ])

let test_direct_exec_crash () =
  let events = ref [] in
  let tx = direct_tx () in
  let spec =
    Shell.direct_exec_spec_of_tx
      ~domain:Receipt_view.Program_call
      ~reject_domain:Call_plan.Program_exec
      ~balance:Z.zero
      tx
  in
  Lwt_main.run
    (Shell.run_direct_exec
       spec
       ~fee:tx.ou
       ~target:tx.to_
       ~apply_value_effect:(fun _ -> events := "apply" :: !events)
       ~log_failed:(fun _ _ _ _ -> events := "failed" :: !events)
       ~reject_after_fee:(fun _ tag reason ->
         events := ("after_fee:" ^ tag ^ ":" ^ reason) :: !events;
         Lwt.return_unit)
       ~reject:(fun r ->
         events := ("reject:" ^ r.Call_plan.error_type) :: !events;
         Lwt.return_unit)
       ~exec:(fun _ -> Lwt.fail_with "boom")
       ~receipt:(fun r -> r)
       ~save:(fun _ _ -> events := "save" :: !events)
       ~ok:(fun _ _ _ -> events := "ok" :: !events; Lwt.return_unit));
  expect
    "direct exec crash"
    (List.rev !events = [
       "apply";
       "after_fee:program_exec_exception:Failure(\"boom\")";
     ])

let test_circle_abort () =
  let tx = direct_tx () in
  let spec = Shell.direct_exec_spec_of_tx
    ~domain:Receipt_view.Program_call ~reject_domain:Call_plan.Program_exec
    ~balance:Z.zero tx in
  let error = Octra_circle_runtime.Circle_exec.Execution_unavailable "test backend unavailable" in
  let charged = ref false in
  let saved = ref false in
  let raised = try
    Lwt_main.run (Shell.run_direct_exec spec ~fee:tx.ou ~target:tx.to_
      ~apply_value_effect:ignore ~log_failed:(fun _ _ _ _ -> ())
      ~reject_after_fee:(fun _ _ _ -> charged := true; Lwt.return_unit)
      ~reject:(fun _ -> charged := true; Lwt.return_unit)
      ~exec:(fun _ -> Lwt.fail error) ~receipt:(fun value -> value)
      ~save:(fun _ _ -> saved := true)
      ~ok:(fun _ _ _ -> Lwt.return_unit));
    false
  with actual when actual = error -> true in
  expect "circle backend failure preserved" raised;
  expect "circle backend failure has no charge" (not !charged);
  expect "circle backend failure has no receipt" (not !saved)

let run_direct_call_case ?(method_name = Some "predict") events =
  let tx = direct_tx ~method_name () in
  let push event =
    events := event :: !events
  in
  Lwt_main.run
    (Shell.run_direct_call
       {
         with_debited_fee = (fun _ f -> push "debit"; f ());
         make_ctx = (fun _ -> ContractVM.default_ctx);
         balance = (fun _ -> Z.of_int 10);
         apply_value_effect = (fun _ -> push "apply");
         log_failed = (fun _ _ _ _ -> push "failed");
         reject_after_fee = (fun _ tag reason ->
           push ("after_fee:" ^ tag ^ ":" ^ reason);
           Lwt.return_unit);
         reject = (fun r ->
           push ("reject:" ^ r.Call_plan.error_type);
           Lwt.return_unit);
         exec = (fun ~ctx:_ call ->
           push ("exec:" ^ call.Call_plan.method_name);
           Lwt.return (receipt ()));
         receipt_of_result = (fun r -> r);
         save = (fun ~tx_hash:_ call _ ->
           push ("save:" ^ call.Call_plan.method_name));
         ok = (fun meta call _ ->
           push ("ok:" ^ meta.Receipt_view.scope ^ ":" ^ call.Call_plan.method_name);
           Lwt.return_unit);
       }
       ~domain:Receipt_view.Program_call
       ~reject_domain:Call_plan.Program_exec
       tx)

let test_direct_call_success () =
  let events = ref [] in
  run_direct_call_case events;
  expect
    "direct call success"
    (List.rev !events = [
       "debit";
       "apply";
       "exec:predict";
       "save:predict";
       "ok:contract:predict";
     ])

let test_direct_call_reject () =
  let events = ref [] in
  run_direct_call_case ~method_name:None events;
  expect
    "direct call reject"
    (List.rev !events = [
       "debit";
       "reject:malformed_transaction";
     ])

let circle_result ?(success = true) ?error () =
  Circle_exec.{
    receipt = receipt ~success ?error ();
    storage_tbl = Hashtbl.create 1;
    baseline_storage_tbl = Hashtbl.create 1;
    spawns = [];
    assets = [];
    encrypted_assets = [];
    caller = "octA";
    tx_hash = "tx";
    hfhe_binding = {
      circle_id = "octCircle";
      code_hash = String.make 64 '0';
      stable_root = String.make 64 '0';
      public_reads_hash = String.make 64 '0';
      context_hash = String.make 64 '0';
      transcript = [];
    };
  }

let call_runtime events =
  let push event =
    events := event :: !events
  in
  Shell.{
    handle_deploy_reject = (fun r ->
      push ("reject:" ^ r.Call_plan.deploy_error_type);
      Lwt.return_unit);
    with_debited_fee = (fun _ f -> push "debit"; f ());
    make_ctx = (fun _ -> ContractVM.default_ctx);
    balance = (fun _ -> Z.of_int 100);
    apply_value_effect = (fun _ -> push "apply");
    log_failed = (fun _ _ method_name error ->
      push ("failed:" ^ method_name ^ ":" ^ error));
    reject_after_fee = (fun _ tag reason ->
      push ("after_fee:" ^ tag ^ ":" ^ reason);
      Lwt.return_unit);
    reject = (fun r ->
      push ("reject:" ^ r.Call_plan.error_type);
      Lwt.return_unit);
    commit_effects = (fun () -> push "commit");
    confirm = (fun () ->
      push "confirm";
      Lwt.return_unit);
    log_deployed = (fun _ effort -> push ("deployed:" ^ string_of_int effort));
    log_constructor_failed = (fun _ reason -> push ("ctor_failed:" ^ reason));
  }

let run_circle_call_case ?(commit = Ok ()) ?(method_name = Some "inc") events =
  let push event =
    events := event :: !events
  in
  let tx = direct_tx ~method_name () in
  Lwt_main.run
    (Shell.run_circle_call_tx
       (Shell.make_circle_call_deps
          (call_runtime events)
          ~exec:(fun ~ctx:_ call ->
            push ("exec:" ^ call.Call_plan.method_name);
            Lwt.return (circle_result ()))
          ~save:(fun ~tx_hash:_ call _ ->
            push ("save:" ^ call.Call_plan.method_name))
          ~commit:(fun _ ->
            push "circle_commit";
            Lwt.return commit)
          ~log_ok:(fun meta call result ->
            push
              (Printf.sprintf
                 "ok:%s:%s:%d"
                 meta.Receipt_view.scope
                 call.Call_plan.method_name
                 result.Circle_exec.receipt.Contract.effort_used)))
       tx)

let test_circle_call_success () =
  let events = ref [] in
  run_circle_call_case events;
  expect
    "circle call success"
    (List.rev !events = [
       "debit";
       "apply";
       "exec:inc";
       "save:inc";
       "circle_commit";
       "commit";
       "ok:circle:inc:7";
       "confirm";
     ])

let test_circle_call_commit_failure () =
  let events = ref [] in
  run_circle_call_case ~commit:(Error "bad commit") events;
  expect
    "circle call commit failure"
    (List.rev !events = [
       "debit";
       "apply";
       "exec:inc";
       "save:inc";
       "circle_commit";
       "after_fee:circle_call_commit_failed:bad commit";
     ])

let run_program_call_case ?(method_name = Some "predict") events =
  let push event =
    events := event :: !events
  in
  let tx = direct_tx ~method_name () in
  Lwt_main.run
    (Shell.run_program_call_tx
       (Shell.make_program_call_deps
          (call_runtime events)
          ~exec:(fun ~ctx:_ call ->
            push ("exec:" ^ call.Call_plan.method_name);
            Lwt.return (receipt ()))
          ~save:(fun ~tx_hash:_ call _ ->
            push ("save:" ^ call.Call_plan.method_name))
          ~log_ok:(fun meta call result ->
            push
              (Printf.sprintf
                 "ok:%s:%s:%d"
                 meta.Receipt_view.scope
                 call.Call_plan.method_name
                 result.Contract.effort_used)))
       tx)

let test_program_call_success () =
  let events = ref [] in
  run_program_call_case events;
  expect
    "program call success"
    (List.rev !events = [
       "debit";
       "apply";
       "exec:predict";
       "save:predict";
       "commit";
       "ok:contract:predict:7";
       "confirm";
     ])

let test_program_call_reject () =
  let events = ref [] in
  run_program_call_case ~method_name:None events;
  expect
    "program call reject"
    (List.rev !events = [
       "debit";
       "reject:malformed_transaction";
     ])

let run_deploy_case ?balance ?(bytecode = true) ?(success = true) events =
  let deployer = "octA" in
  let nonce = 7 in
  let raw = Bytecode.encode [|ContractVM.STOP|] in
  let target = Contract.addr_from_code raw deployer nonce in
  let bytecode_b64_opt =
    if bytecode then Some (Base64.encode_string raw) else None
  in
  let push event =
    events := event :: !events
  in
  let tx =
    let base = direct_tx ~params:(Some "[]") () in
    {
      base with
      from = deployer;
      to_ = target;
      nonce;
      ou = Z.of_int 9;
      encrypted_data = bytecode_b64_opt;
    }
  in
  Lwt_main.run
    (Shell.run_deploy_tx_runtime
       ~trusted_program_keys:Octra_vm.Program_trust.empty
       ~point_ops:false
       (call_runtime events)
       ~balance
       tx
       ~deploy_and_save:(fun ~params ~bytecode ~bytecode_raw ->
         push
           (Printf.sprintf
              "deploy:%d:%d"
              (List.length params)
              (Array.length bytecode));
         {
           Shell.contract_addr = Contract.addr_from_code bytecode_raw deployer nonce;
           receipt = receipt ~success ?error:(if success then None else Some "ctor") ();
         })
       ~ensure_account:(fun addr -> push ("ensure:" ^ addr)))

let test_contract_deploy_fee_reject () =
  let events = ref [] in
  run_deploy_case events;
  expect "deploy fee reject" (List.rev !events = ["reject:sender_not_found"])

let test_deploy_bytecode () =
  let events = ref [] in
  run_deploy_case ~balance:(Z.of_int 9) ~bytecode:false events;
  expect
    "deploy missing bytecode"
    (List.rev !events = ["debit"; "reject:missing_bytecode"])

let test_contract_deploy_success () =
  let events = ref [] in
  run_deploy_case ~balance:(Z.of_int 9) events;
  match List.rev !events with
  | ["debit"; "deploy:0:1"; ensure; "commit"; "deployed:7"; "confirm"] ->
    expect "deploy ensure" (String.length ensure > 7)
  | _ -> fail "deploy success order"

let test_deploy_constructor () =
  let events = ref [] in
  run_deploy_case ~balance:(Z.of_int 9) ~success:false events;
  match List.rev !events with
  | ["debit"; "deploy:0:1"; ctor; after_fee] ->
    expect "deploy ctor log" (String.length ctor >= 12);
    expect
      "deploy ctor reject"
      (String.sub after_fee 0 29 = "after_fee:constructor_failed:")
  | _ -> fail "deploy constructor failure order"

let multi_target =
  "oct7xCozDD9JEsbeVpo5C7HXp2BJbKqfmNUHmDDCCTtWcGb"

let multi_message ?(method_name = "run") () =
  Yojson.Safe.to_string
    (`List [
       `Assoc [
         "to", `String multi_target;
         "method", `String method_name;
         "params", `List [];
         "amount", `String "0";
       ];
     ])

let multi_deps ?(success = true) ?(throw = false) events =
  let push event =
    events := event :: !events
  in
  Shell.make_multi_exec_deps
    (call_runtime events)
    ~execute_call:(fun ~ctx:_ ~limit:_ ~target:_ ~method_name ~params:_
        ~caller:_ ~amount:_ ->
      push ("exec:" ^ method_name);
      if throw then failwith "boom";
      let error = if success then None else Some "boom" in
      receipt ~success ?error ())
    ~save_receipt_raw:(fun ~tx_hash:_ ~json:_ -> push "save")
    ~log_success:(fun ~calls ~effort ->
      push (Printf.sprintf "ok:%d:%d" calls effort))
    ~log_failed:(fun err -> push ("failed:" ^ err))
    ~reject_malformed:(fun reason ->
      push ("malformed:" ^ reason);
      Lwt.return_unit)
    ~now:(fun () -> 1.)

let run_multi ?success ?throw ?message events =
  let tx =
    let base = direct_tx () in
    { base with message }
  in
  Lwt_main.run
    (Shell.run_multi_exec_tx
       (multi_deps ?success ?throw events)
       ~max_calls:4
       ~epoch:9
       tx)

let test_multi_exec_missing_payload () =
  let events = ref [] in
  run_multi events;
  expect
    "multi missing payload"
    (List.rev !events = ["malformed:multi_exec requires calls payload"])

let test_multi_exec_success () =
  let events = ref [] in
  run_multi ~message:(multi_message ()) events;
  expect
    "multi success"
    (List.rev !events = [
       "debit";
       "apply";
       "exec:run";
       "save";
       "commit";
       "ok:1:7";
       "confirm";
     ])

let test_multi_exec_failed_receipt () =
  let events = ref [] in
  run_multi ~success:false ~message:(multi_message ()) events;
  expect
    "multi failed receipt"
    (List.rev !events = [
       "debit";
       "apply";
       "exec:run";
       "save";
       "failed:call 0 failed: boom";
       "after_fee:multi_exec_failed:call 0 failed: boom";
     ])

let test_multi_exec_exception () =
  let events = ref [] in
  run_multi ~throw:true ~message:(multi_message ()) events;
  expect
    "multi exception"
    (List.rev !events = [
       "debit";
       "apply";
       "exec:run";
       "after_fee:multi_exec_exception:Failure(\"boom\")";
     ])

let test_multi_resource () =
  List.iter (fun error ->
    let events = ref [] in
    let deps = multi_deps events in
    let deps = { deps with Shell.execute_call =
      (fun ~ctx:_ ~limit:_ ~target:_ ~method_name:_ ~params:_ ~caller:_ ~amount:_ ->
        raise error) } in
    let raised = try
      Lwt_main.run (Shell.run_multi_exec deps ~max_calls:4 ~epoch:9
        ~tx_hash:"tx" ~from_addr:"sender" ~message:(Some (multi_message ()))
        ~fee:(Z.of_int 10));
      false
    with actual when actual = error -> true in
    expect "multi resource exception preserved" raised;
    expect "multi resource has no receipt or charge"
      (List.rev !events = ["debit"; "apply"]))
    [Stack_overflow; Out_of_memory]

let vm_tx_deps events =
  let push event =
    events := event :: !events
  in
  Shell.{
    runtime = call_runtime events;
    trusted_program_keys = Octra_vm.Program_trust.empty;
    proof_mode = Octra_core.Rule_graph.Prior;
    math = false;
    deploy_balance = (fun _ -> Some (Z.of_int 9));
    deploy_and_save = (fun _ ~admitted:_ ~params ~bytecode ~bytecode_raw ->
      push
        (Printf.sprintf
           "deploy_dispatch:%d:%d:%d"
           (List.length params)
           (Array.length bytecode)
           (String.length bytecode_raw));
      { contract_addr = "octDeploy"; receipt = receipt () });
    program_prepare = (fun _ -> Lwt.return_error "not expected");
    ensure_account = (fun addr -> push ("ensure:" ^ addr));
    circle_exec = (fun _ ~ctx:_ call ->
      push ("circle_exec:" ^ call.Call_plan.method_name);
      Lwt.return (circle_result ()));
    circle_save = (fun _ ~tx_hash:_ call _ ->
      push ("circle_save:" ^ call.Call_plan.method_name));
    circle_commit = (fun _ _ ->
      push "circle_commit";
      Lwt.return (Ok ()));
    circle_log_ok = (fun _ _ call _ ->
      push ("circle_ok:" ^ call.Call_plan.method_name));
    program_exec = (fun _ ~ctx:_ call ->
      push ("program_exec:" ^ call.Call_plan.method_name);
      Lwt.return (receipt ()));
    program_save = (fun _ ~tx_hash:_ call _ ->
      push ("program_save:" ^ call.Call_plan.method_name));
    program_log_ok = (fun _ _ call _ ->
      push ("program_ok:" ^ call.Call_plan.method_name));
    multi_execute_call = (fun ~ctx:_ ~limit:_ ~target:_ ~method_name
        ~params:_ ~caller:_ ~amount:_ ->
      push ("multi_exec:" ^ method_name);
      receipt ());
    save_receipt_raw = (fun ~tx_hash:_ ~json:_ -> push "multi_save");
    reject_malformed = (fun reason ->
      push ("malformed:" ^ reason);
      Lwt.return_unit);
    max_multi_exec_calls = 4;
    epoch = 9;
    now = (fun () -> 1.);
  }

let test_run_vm_tx_program_exec () =
  let events = ref [] in
  let tx =
    let base = direct_tx () in
    { base with op_type = Transaction.ProgramExec }
  in
  Lwt_main.run (Shell.run_vm_tx (vm_tx_deps events) tx);
  expect
    "vm tx program dispatch"
    (List.rev !events = [
       "debit";
       "apply";
       "program_exec:predict";
       "program_save:predict";
       "commit";
       "program_ok:predict";
       "confirm";
     ])

let test_run_vm_tx_invalid_op () =
  let events = ref [] in
  let tx =
    let base = direct_tx () in
    { base with op_type = Transaction.Standard }
  in
  Lwt_main.run (Shell.run_vm_tx (vm_tx_deps events) tx);
  expect
    "vm tx invalid op"
    (List.rev !events = ["malformed:invalid vm operation"])

let test_max_multi_exec_calls () =
  expect
    "multi default"
    (Shell.max_multi_exec_calls ~env:(fun _ -> None) = 8);
  expect
    "multi invalid"
    (Shell.max_multi_exec_calls ~env:(fun _ -> Some "bad") = 8);
  expect
    "multi clamp"
    (Shell.max_multi_exec_calls ~env:(fun _ -> Some "0") = 8);
  expect
    "multi value"
    (Shell.max_multi_exec_calls ~env:(fun _ -> Some "12") = 12);
  expect
    "multi capped"
    (Shell.max_multi_exec_calls ~env:(fun _ -> Some "65") = 8)

let reject_events () =
  let events = ref [] in
  let push event =
    events := event :: !events
  in
  let reject ?(consume_nonce = false) ?(notify_reason = "")
      ?(persist_state = false) tag reason =
    push
      (Printf.sprintf
         "reject consume = %b persist = %b notify = %s tag = %s reason = %s"
         consume_nonce
         persist_state
         notify_reason
         tag
         reason);
    Lwt.return_unit
  in
  events, push, reject

let test_reject_envelopes () =
  let events, push, reject = reject_events () in
  Lwt_main.run
    (Shell.handle_deploy_reject
       ~reject
       {
         Call_plan.deploy_error_type = "deploy_bad";
         deploy_log_reason = "bad";
         deploy_notify_reason = "Bad deploy";
         deploy_consume_nonce = true;
       });
  Lwt_main.run
    (Shell.handle_direct_exec_reject
       ~discard:(fun () -> push "discard")
       ~reject
       {
         Call_plan.error_type = "call_bad";
         log_reason = "call";
         notify_reason = "Bad call";
         consume_nonce = false;
         discard_effects = true;
       });
  expect
    "reject envelopes"
    (List.rev !events = [
       "reject consume = true persist = false notify = Bad deploy tag = deploy_bad reason = bad";
       "discard";
       "reject consume = false persist = false notify = Bad call tag = call_bad reason = call";
     ])

let test_fee_envelopes () =
  let events, push, reject = reject_events () in
  let tx = direct_tx () in
  Lwt_main.run
    (Shell.with_debited_fee
       ~debit:(fun addr fee nonce ->
         push
           (Printf.sprintf
              "debit addr = %s fee = %s nonce = %d"
              addr
              (Z.to_string fee)
              nonce);
         Ok ())
       ~reject:(fun tag reason -> reject tag reason)
       tx
       (Z.of_int 44)
       (fun () ->
         push "ok";
         Lwt.return_unit));
  Lwt_main.run
    (Shell.reject_after_fee
       ~discard_fee:(fun fee -> push ("discard_fee = " ^ Z.to_string fee))
       ~reject
       (Z.of_int 5)
       "bad_after_fee"
       "bad");
  expect
    "fee envelopes"
    (List.rev !events = [
       "debit addr = octA fee = 44 nonce = 1";
       "ok";
       "discard_fee = 5";
       "reject consume = true persist = true notify =  tag = bad_after_fee reason = bad";
     ])

let test_live_call_runtime_builder () =
  let events = ref [] in
  let push event =
    events := event :: !events
  in
  let reject ?(consume_nonce = false) ?(notify_reason = "")
      ?(persist_state = false) tag reason =
    push
      (Printf.sprintf
         "reject consume = %b persist = %b notify = %s tag = %s reason = %s"
         consume_nonce
         persist_state
         notify_reason
         tag
         reason);
    Lwt.return_unit
  in
  let tx = direct_tx () in
  let runtime =
    Shell.make_live_call_runtime
      Shell.{
        reject;
        debit = (fun addr fee nonce ->
          push
            (Printf.sprintf
               "debit addr = %s fee = %s nonce = %d"
               addr
               (Z.to_string fee)
               nonce);
          Ok ());
        tx;
        make_ctx = (fun _ -> ContractVM.default_ctx);
        balance = (fun _ -> Z.of_int 10);
        apply_value_effect = (fun _ -> push "apply");
        discard_effects = (fun () -> push "discard");
        discard_fee = (fun fee -> push ("discard_fee = " ^ Z.to_string fee));
        commit_effects = (fun () -> push "commit");
        confirm = (fun () ->
          push "confirm";
          Lwt.return_unit);
      }
  in
  Lwt_main.run
    (runtime.with_debited_fee (Z.of_int 3) (fun () ->
       push "inside";
       Lwt.return_unit));
  Lwt_main.run (runtime.reject_after_fee (Z.of_int 4) "bad_fee" "bad");
  Lwt_main.run
    (runtime.reject
       {
         Call_plan.error_type = "call_bad";
         log_reason = "call";
         notify_reason = "Bad call";
         consume_nonce = false;
         discard_effects = true;
       });
  runtime.commit_effects ();
  Lwt_main.run (runtime.confirm ());
  expect
    "live call runtime builder"
    (List.rev !events = [
       "debit addr = octA fee = 3 nonce = 1";
       "inside";
       "discard_fee = 4";
       "reject consume = true persist = true notify =  tag = bad_fee reason = bad";
       "discard";
       "reject consume = false persist = false notify = Bad call tag = call_bad reason = call";
       "commit";
       "confirm";
     ])

let test_save_receipt () =
  let saved = ref None in
  let deps : Shell.receipt_deps = {
    save = (fun ~tx_hash ~contract_addr ~method_name ~success ~effort_used
        ~events_json ~error ~epoch_id ->
      saved :=
        Some
          (tx_hash, contract_addr, method_name, success, effort_used,
           events_json, error, epoch_id));
    epoch = (fun () -> 77);
  } in
  Shell.save_receipt
    deps
    ~program:true
    ~tx_hash:"tx"
    ~contract_addr:"octProgram"
    ~method_name:"run"
    (receipt ~success:false ~error:"bad" ());
  match !saved with
  | Some ("tx", "octProgram", "run", false, 7, _, Some "bad", 77) -> ()
  | _ -> fail "save receipt"

let () =
  test_call_success_keeps_effects ();
  test_call_restore ();
  test_deploy_restore ();
  test_shared_memory ();
  test_transfer_and_context_fields ();
  test_direct_exec_success ();
  test_direct_exec_reject ();
  test_direct_exec_crash ();
  test_circle_abort ();
  test_direct_call_success ();
  test_direct_call_reject ();
  test_circle_call_success ();
  test_circle_call_commit_failure ();
  test_program_call_success ();
  test_program_call_reject ();
  test_contract_deploy_fee_reject ();
  test_deploy_bytecode ();
  test_contract_deploy_success ();
  test_deploy_constructor ();
  test_multi_exec_missing_payload ();
  test_multi_exec_success ();
  test_multi_exec_failed_receipt ();
  test_multi_exec_exception ();
  test_multi_resource ();
  test_run_vm_tx_program_exec ();
  test_run_vm_tx_invalid_op ();
  test_max_multi_exec_calls ();
  test_reject_envelopes ();
  test_fee_envelopes ();
  test_live_call_runtime_builder ();
  test_save_receipt ();
  print_endline "status = pass test = epoch_vm"