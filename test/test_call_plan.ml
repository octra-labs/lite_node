(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C = Octra_vm.Call_plan

let fail msg =
  failwith ("test_call_plan: " ^ msg)

let ok msg cond =
  if not cond then fail msg

let addr = "oct7xCozDD9JEsbeVpo5C7HXp2BJbKqfmNUHmDDCCTtWcGb"

let deployer = "oct7xCozDD9JEsbeVpo5C7HXp2BJbKqfmNUHmDDCCTtWcGb"

let call_json ?(to_addr = addr) ?(method_name = "run") ?(amount = `String "0") ?(params = `List []) () =
  `Assoc [
    "to", `String to_addr;
    "method", `String method_name;
    "params", params;
    "amount", amount;
  ]

let parse ?(max_calls = 8) json =
  C.parse_multi_exec_calls ~max_calls (Yojson.Safe.to_string json)

let test_list_form () =
  match parse (`List [call_json ~amount:(`Int 7) ()]) with
  | Error e -> fail e
  | Ok [call] ->
    ok "target" (call.C.target = addr);
    ok "method" (call.method_name = "run");
    ok "amount" (Z.equal call.amount (Z.of_int 7));
    ok "params empty" (call.params = [])
  | Ok _ -> fail "unexpected call count"

let test_object_form () =
  match parse (`Assoc ["calls", `List [call_json ~params:(`List [`String "x"]) ()]]) with
  | Error e -> fail e
  | Ok [call] -> ok "params" (call.C.params = [`String "x"])
  | Ok _ -> fail "unexpected call count"

let expect_error label json expected =
  match parse json with
  | Ok _ -> fail (label ^ " accepted")
  | Error e -> ok label (String.equal e expected)

let test_rejects () =
  expect_error "empty" (`List []) "multi_exec requires at least one call";
  expect_error "missing calls" (`Assoc []) "multi_exec requires calls list";
  expect_error "bad params" (`List [call_json ~params:(`String "x") ()])
    "multi_exec call 0 params must be list";
  expect_error "bad addr" (`List [call_json ~to_addr:"bad" ()])
    "multi_exec call 0 invalid program address";
  expect_error "negative" (`List [call_json ~amount:(`String "-1") ()])
    "multi_exec call 0 amount must not be negative";
  match C.parse_multi_exec_calls ~max_calls:1 (Yojson.Safe.to_string (`List [call_json (); call_json ()])) with
  | Ok _ -> fail "too many accepted"
  | Error e -> ok "too many" (String.equal e "multi_exec call count exceeds 1")

let test_value_transfer () =
  (match C.value_transfer ~from_addr:"a" ~target:"b" ~amount:Z.zero ~balance:(Z.of_int 10) with
  | C.No_value -> ()
  | _ -> fail "zero should be no value");
  (match C.value_transfer ~from_addr:"a" ~target:"b" ~amount:(Z.of_int (-1)) ~balance:(Z.of_int 10) with
  | C.No_value -> ()
  | _ -> fail "negative should be no value");
  (match C.value_transfer ~from_addr:"a" ~target:"b" ~amount:(Z.of_int 7) ~balance:(Z.of_int 10) with
  | C.Transfer t ->
    ok "transfer from" (t.from_addr = "a");
    ok "transfer target" (t.target = "b");
    ok "transfer amount" (Z.equal t.amount (Z.of_int 7))
  | _ -> fail "expected transfer");
  (match C.value_transfer ~from_addr:"a" ~target:"b" ~amount:(Z.of_int 11) ~balance:(Z.of_int 10) with
  | C.Insufficient -> ()
  | _ -> fail "expected insufficient")

let test_value_effect () =
  (match C.value_effect ~from_addr:"a" ~target:"b" ~amount:Z.zero ~balance:(Z.of_int 10) with
  | C.Value_noop -> ()
  | _ -> fail "zero should be noop");
  (match C.value_effect ~from_addr:"a" ~target:"b" ~amount:(Z.of_int 11) ~balance:(Z.of_int 10) with
  | C.Value_reject -> ()
  | _ -> fail "expected value reject");
  (match C.value_effect ~from_addr:"a" ~target:"b" ~amount:(Z.of_int 7) ~balance:(Z.of_int 10) with
  | C.Value_apply effect ->
    ok "effect journal from" (effect.journal.from_addr = "a");
    ok "effect journal target" (effect.journal.target = "b");
    ok "effect journal amount" (Z.equal effect.journal.amount (Z.of_int 7));
    ok "effect deltas count" (List.length effect.deltas = 2);
    begin
      match effect.deltas with
      | debit :: credit :: [] ->
        ok "effect debit addr" (debit.C.addr = "a");
        ok "effect debit amount" (Z.equal debit.delta (Z.of_int (-7)));
        ok "effect credit addr" (credit.C.addr = "b");
        ok "effect credit amount" (Z.equal credit.delta (Z.of_int 7))
      | _ -> fail "unexpected deltas"
    end
  | _ -> fail "expected value apply")

let test_direct_call () =
  (match C.parse_direct_call ~method_name:(Some "run") ~params_json:(Some "[1,\"x\"]") with
  | Some call ->
    ok "direct method" (String.equal call.C.method_name "run");
    ok "direct params" (call.params = [`Int 1; `String "x"])
  | None -> fail "direct call missing");
  (match C.parse_direct_call ~method_name:(Some "run") ~params_json:(Some "{\"x\":1}") with
  | Some call -> ok "non-list params empty" (call.C.params = [])
  | None -> fail "direct non-list missing");
  (match C.parse_direct_call ~method_name:None ~params_json:(Some "[]") with
  | None -> ()
  | Some _ -> fail "direct missing method accepted");
  let invalid_raised =
    try
      ignore (C.parse_direct_call ~method_name:(Some "run") ~params_json:(Some "{"));
      false
    with _ -> true
  in
  ok "invalid json raises" invalid_raised

let test_direct_exec_plan () =
  (match C.plan_direct_exec
    ~method_name:(Some "run")
    ~params_json:(Some "[1]")
    ~from_addr:"alice"
    ~target:"vault"
    ~amount:Z.zero
    ~balance:(Z.of_int 10)
    ~ou:(Z.of_int 7)
  with
  | C.Direct_exec_ready plan ->
    ok "plan method" (String.equal plan.C.method_name "run");
    ok "plan params" (plan.params = [`Int 1]);
    ok "plan no value" (plan.value_effect = C.Value_noop);
    ok "plan effort floor" (plan.effort_limit = 1_000_000)
  | _ -> fail "direct plan rejected");
  (match C.plan_direct_exec
    ~method_name:(Some "pay")
    ~params_json:(Some "[]")
    ~from_addr:"alice"
    ~target:"vault"
    ~amount:(Z.of_int 7)
    ~balance:(Z.of_int 10)
    ~ou:(Z.of_int 2_000_000)
  with
  | C.Direct_exec_ready plan ->
    ok "plan amount" (Z.equal plan.amount (Z.of_int 7));
    ok "plan effort high" (plan.effort_limit = 2_000_000);
    begin
      match plan.value_effect with
      | C.Value_apply effect ->
        ok "plan transfer from" (effect.journal.from_addr = "alice");
        ok "plan transfer target" (effect.journal.target = "vault");
        ok "plan transfer amount" (Z.equal effect.journal.amount (Z.of_int 7))
      | _ -> fail "missing transfer"
    end
  | _ -> fail "pay plan rejected");
  (match C.plan_direct_exec
    ~method_name:(Some "pay")
    ~params_json:(Some "[]")
    ~from_addr:"alice"
    ~target:"vault"
    ~amount:(Z.of_int 11)
    ~balance:(Z.of_int 10)
    ~ou:(Z.of_int 7)
  with
  | C.Direct_exec_insufficient_value -> ()
  | _ -> fail "insufficient plan accepted");
  (match C.plan_direct_exec
    ~method_name:None
    ~params_json:(Some "[]")
    ~from_addr:"alice"
    ~target:"vault"
    ~amount:Z.zero
    ~balance:(Z.of_int 10)
    ~ou:(Z.of_int 7)
  with
  | C.Direct_exec_invalid_format -> ()
  | _ -> fail "invalid direct format accepted")

let test_direct_exec_reject () =
  let circle_insufficient = C.direct_exec_reject C.Circle_exec C.Direct_exec_insufficient_value in
  let program_invalid = C.direct_exec_reject C.Program_exec C.Direct_exec_invalid_format in
  let circle_invalid = C.direct_exec_reject C.Circle_exec C.Direct_exec_invalid_format in
  ok "circle insufficient type" (circle_insufficient.C.error_type = "insufficient_balance");
  ok "circle insufficient log" (circle_insufficient.log_reason = "insufficient balance for circle value");
  ok "circle insufficient notify" (circle_insufficient.notify_reason = "Insufficient balance for value");
  ok "circle insufficient nonce" circle_insufficient.consume_nonce;
  ok "circle insufficient discard" circle_insufficient.discard_effects;
  ok "program invalid type" (program_invalid.C.error_type = "malformed_transaction");
  ok "program invalid log" (program_invalid.log_reason = "invalid contract call format");
  ok "program invalid notify" (program_invalid.notify_reason = "Invalid contract call format");
  ok "program invalid nonce" program_invalid.consume_nonce;
  ok "program invalid discard" (not program_invalid.discard_effects);
  ok "circle invalid log" (circle_invalid.C.log_reason = "invalid circle call format");
  ok "circle invalid notify" (circle_invalid.notify_reason = "Invalid circle call format");
  ok "circle invalid nonce" (not circle_invalid.consume_nonce);
  let ready_rejected =
    try
      ignore (C.direct_exec_reject C.Program_exec
        (C.Direct_exec_ready {
          method_name = "run";
          params = [];
          amount = Z.zero;
          value_effect = C.Value_noop;
          effort_limit = 1;
        }));
      false
    with Invalid_argument _ -> true
  in
  ok "ready reject fails closed" ready_rejected

let test_multi_exec_step () =
  let call = {
    C.target = "vault";
    method_name = "run";
    params = [];
    amount = Z.of_int 7;
  } in
  (match C.plan_multi_exec_step
    ~from_addr:"alice"
    ~call
    ~effort_used:3
    ~effort_limit:10
    ~balance:(Z.of_int 20)
  with
  | C.Multi_exec_ready step ->
    ok "multi remaining" (step.remaining_effort = 7);
    begin
      match step.value_effect with
      | C.Value_apply effect ->
        ok "multi effect amount" (Z.equal effect.journal.amount (Z.of_int 7))
      | _ -> fail "multi missing value effect"
    end
  | _ -> fail "multi step rejected");
  (match C.plan_multi_exec_step
    ~from_addr:"alice"
    ~call
    ~effort_used:10
    ~effort_limit:10
    ~balance:(Z.of_int 20)
  with
  | C.Multi_exec_effort_exhausted -> ()
  | _ -> fail "multi exhausted accepted");
  (match C.plan_multi_exec_step
    ~from_addr:"alice"
    ~call
    ~effort_used:3
    ~effort_limit:10
    ~balance:(Z.of_int 3)
  with
  | C.Multi_exec_insufficient_value -> ()
  | _ -> fail "multi insufficient accepted");
  let no_value = { call with C.amount = Z.zero } in
  (match C.plan_multi_exec_step
    ~from_addr:"alice"
    ~call:no_value
    ~effort_used:9
    ~effort_limit:10
    ~balance:Z.zero
  with
  | C.Multi_exec_ready step ->
    ok "multi floor remaining" (step.remaining_effort = 1);
    ok "multi no value" (step.value_effect = C.Value_noop)
  | _ -> fail "multi no-value rejected")

let test_deploy_params () =
  ok "deploy list params"
    (C.parse_deploy_params (Some "[1,\"x\"]") = [`Int 1; `String "x"]);
  ok "deploy non-list empty"
    (C.parse_deploy_params (Some "{\"x\":1}") = []);
  ok "deploy missing empty"
    (C.parse_deploy_params None = []);
  ok "deploy invalid empty"
    (C.parse_deploy_params (Some "{") = [])

let test_rpc_json_helpers () =
  ok "params list" (C.params_json (Some (`List [`String "x"])) = [`String "x"]);
  ok "params non-list empty" (C.params_json (Some (`String "x")) = []);
  ok "params missing empty" (C.params_json None = []);
  ok "bool true" (C.bool_json ~default:false (Some (`Bool true)));
  ok "bool default" (C.bool_json ~default:true (Some (`String "x")))

let test_readonly_call () =
  let balance_call =
    C.plan_readonly_call
      ~method_name:"balance_of"
      ~params:(Some (`List [`String addr]))
      ~caller_addr:None
      ~include_storage:None in
  ok "readonly balance method" (String.equal balance_call.C.readonly_method_name "balance_of");
  ok "readonly balance params" (balance_call.readonly_params = [`String addr]);
  ok "readonly default caller"
    (String.equal
       balance_call.readonly_caller_addr
       "oct00000000000000000000000000000000000000000000");
  ok "readonly balance storage default" (not balance_call.readonly_include_storage);
  let default_call =
    C.plan_readonly_call
      ~method_name:"run"
      ~params:None
      ~caller_addr:None
      ~include_storage:None in
  ok "readonly storage opt in" (not default_call.readonly_include_storage);
  let explicit_call =
    C.plan_readonly_call
      ~method_name:"run"
      ~params:(Some (`String "bad"))
      ~caller_addr:(Some addr)
      ~include_storage:(Some (`Bool false)) in
  ok "readonly explicit caller" (String.equal explicit_call.C.readonly_caller_addr addr);
  ok "readonly non-list params" (explicit_call.readonly_params = []);
  ok "readonly explicit storage" (not explicit_call.readonly_include_storage);
  let storage_call =
    C.plan_readonly_call
      ~method_name:"run"
      ~params:None
      ~caller_addr:None
      ~include_storage:(Some (`Bool true)) in
  ok "readonly storage requested" storage_call.readonly_include_storage

let test_effort_limit () =
  ok "effort floor" (C.effort_limit (Z.of_int 7) = 1_000_000);
  ok "effort high" (C.effort_limit (Z.of_int 2_000_000) = 2_000_000);
  ok "effort huge cap" (C.effort_limit (Z.pow (Z.of_int 2) 200) = 1_000_000)

let test_deploy_payload () =
  let raw = Octra_vm.Bytecode.encode [|Octra_vm.Contract_vm.STOP|] in
  let target = Octra_vm.Contract.addr_from_code raw deployer 7 in
  let b64 = Base64.encode_string raw in
  let unsafe_raw =
    Octra_vm.Bytecode.encode
      [|Octra_vm.Contract_vm.ROPE_APPLY (0, 1, 2, 3); Octra_vm.Contract_vm.STOP|]
  in
  let unsafe_target = Octra_vm.Contract.addr_from_code unsafe_raw deployer 7 in
  (match C.parse_deploy_payload ~bytecode_b64:b64 ~deployer ~nonce:7 ~target with
  | C.Deploy_ready plan ->
    ok "deploy raw" (String.equal plan.bytecode_raw raw);
    ok "deploy code length" (Array.length plan.bytecode = 1)
  | _ -> fail "deploy payload rejected");
  (match C.parse_deploy_payload
    ~bytecode_b64:(Base64.encode_string unsafe_raw)
    ~deployer
    ~nonce:7
    ~target:unsafe_target with
  | C.Deploy_invalid_bytecode err ->
    ok "unsafe deploy rejected"
      (String.equal err "consensus unsafe opcode ROPE_APPLY at pc 0")
  | _ -> fail "unsafe deploy accepted");
  (match C.parse_deploy_payload ~bytecode_b64:(Base64.encode_string "bad") ~deployer ~nonce:7 ~target with
  | C.Deploy_invalid_bytecode _ -> ()
  | _ -> fail "invalid bytecode accepted");
  (match C.parse_deploy_payload ~bytecode_b64:b64 ~deployer ~nonce:7 ~target:addr with
  | C.Deploy_address_mismatch -> ()
  | _ -> fail "address mismatch accepted");
  let invalid_b64_raised =
    try
      ignore (C.parse_deploy_payload ~bytecode_b64:"not-base64!" ~deployer ~nonce:7 ~target);
      false
    with _ -> true
  in
  ok "invalid base64 raises" invalid_b64_raised

let test_deploy_reject () =
  let missing = C.deploy_missing_bytecode_reject in
  let invalid = C.deploy_payload_reject (C.Deploy_invalid_bytecode "bad opcode") in
  let mismatch = C.deploy_payload_reject C.Deploy_address_mismatch in
  ok "missing bytecode type" (missing.C.deploy_error_type = "missing_bytecode");
  ok "missing bytecode log" (missing.deploy_log_reason = "no bytecode in transaction");
  ok "missing bytecode notify" (missing.deploy_notify_reason = "Missing bytecode");
  ok "missing bytecode nonce" missing.deploy_consume_nonce;
  ok "invalid bytecode type" (invalid.C.deploy_error_type = "invalid_bytecode");
  ok "invalid bytecode log" (invalid.deploy_log_reason = "bad opcode");
  ok "invalid bytecode notify" (invalid.deploy_notify_reason = "Invalid bytecode: bad opcode");
  ok "invalid bytecode nonce" (not invalid.deploy_consume_nonce);
  ok "mismatch type" (mismatch.C.deploy_error_type = "contract_address_mismatch");
  ok "mismatch log" (mismatch.deploy_log_reason = "expected address does not match");
  ok "mismatch notify" (mismatch.deploy_notify_reason = "Contract address mismatch");
  ok "mismatch nonce" (not mismatch.deploy_consume_nonce);
  let ready_rejected =
    try
      ignore (C.deploy_payload_reject
        (C.Deploy_ready { bytecode_raw = ""; bytecode = [||] }));
      false
    with Invalid_argument _ -> true
  in
  ok "deploy ready reject fails closed" ready_rejected

let test_deploy_fee () =
  (match C.plan_deploy_fee ~balance:None ~fee:(Z.of_int 7) with
  | C.Deploy_fee_rejected reject ->
    ok "deploy fee missing type" (reject.C.deploy_error_type = "sender_not_found");
    ok "deploy fee missing notify" (reject.deploy_notify_reason = "Sender not found");
    ok "deploy fee missing nonce" (not reject.deploy_consume_nonce)
  | C.Deploy_fee_ready -> fail "deploy missing sender accepted");
  (match C.plan_deploy_fee ~balance:(Some (Z.of_int 3)) ~fee:(Z.of_int 7) with
  | C.Deploy_fee_rejected reject ->
    ok "deploy fee insufficient type" (reject.C.deploy_error_type = "insufficient_balance");
    ok "deploy fee insufficient log" (reject.deploy_log_reason = "insufficient balance for deploy fee");
    ok "deploy fee insufficient nonce" (not reject.deploy_consume_nonce)
  | C.Deploy_fee_ready -> fail "deploy insufficient fee accepted");
  (match C.plan_deploy_fee ~balance:(Some (Z.of_int 7)) ~fee:(Z.of_int 7) with
  | C.Deploy_fee_ready -> ()
  | C.Deploy_fee_rejected _ -> fail "deploy exact fee rejected")

let test_deploy_input () =
  let raw = Octra_vm.Bytecode.encode [|Octra_vm.Contract_vm.STOP|] in
  let target = Octra_vm.Contract.addr_from_code raw deployer 7 in
  let b64 = Base64.encode_string raw in
  let unsafe_raw =
    Octra_vm.Bytecode.encode
      [|Octra_vm.Contract_vm.MATMUL_FP (0, 1, 2, 3, 4, 5); Octra_vm.Contract_vm.STOP|]
  in
  let unsafe_target = Octra_vm.Contract.addr_from_code unsafe_raw deployer 7 in
  (match C.plan_deploy_input ~bytecode_b64_opt:(Some b64) ~deployer ~nonce:7 ~target with
  | C.Deploy_input_ready plan ->
    ok "deploy input raw" (String.equal plan.bytecode_raw raw);
    ok "deploy input code" (Array.length plan.bytecode = 1)
  | _ -> fail "deploy input rejected");
  (match C.plan_deploy_input
    ~bytecode_b64_opt:(Some (Base64.encode_string unsafe_raw))
    ~deployer
    ~nonce:7
    ~target:unsafe_target with
  | C.Deploy_input_rejected reject ->
    ok "deploy input unsafe type" (reject.C.deploy_error_type = "invalid_bytecode");
    ok "deploy input unsafe log"
      (String.equal reject.deploy_log_reason
         "consensus unsafe opcode MATMUL_FP at pc 0")
  | _ -> fail "deploy input unsafe accepted");
  (match C.plan_deploy_input ~bytecode_b64_opt:None ~deployer ~nonce:7 ~target with
  | C.Deploy_input_rejected reject ->
    ok "deploy input missing" (reject.C.deploy_error_type = "missing_bytecode")
  | _ -> fail "deploy input missing accepted");
  (match C.plan_deploy_input ~bytecode_b64_opt:(Some (Base64.encode_string "bad")) ~deployer ~nonce:7 ~target with
  | C.Deploy_input_rejected reject ->
    ok "deploy input invalid" (reject.C.deploy_error_type = "invalid_bytecode")
  | _ -> fail "deploy input invalid accepted");
  (match C.plan_deploy_input ~bytecode_b64_opt:(Some b64) ~deployer ~nonce:7 ~target:addr with
  | C.Deploy_input_rejected reject ->
    ok "deploy input mismatch" (reject.C.deploy_error_type = "contract_address_mismatch")
  | _ -> fail "deploy input mismatch accepted");
  (match C.plan_deploy_input ~bytecode_b64_opt:(Some "not-base64!") ~deployer ~nonce:7 ~target with
  | C.Deploy_input_exception e ->
    ok "deploy input exception" (String.length e > 0)
  | _ -> fail "deploy input exception missing")

let () =
  test_list_form ();
  test_object_form ();
  test_rejects ();
  test_value_transfer ();
  test_value_effect ();
  test_direct_call ();
  test_direct_exec_plan ();
  test_direct_exec_reject ();
  test_multi_exec_step ();
  test_deploy_params ();
  test_rpc_json_helpers ();
  test_readonly_call ();
  test_effort_limit ();
  test_deploy_payload ();
  test_deploy_reject ();
  test_deploy_fee ();
  test_deploy_input ();
  print_endline "test_call_plan: ok"