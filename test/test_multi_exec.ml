(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module M = Octra_vm.Multi_exec
module C = Octra_vm.Call_plan
module R = Octra_vm.Receipt_view
module VM = Octra_vm.Contract_vm
module Contract = Octra_vm.Contract
module T = Octra_core.Transaction

let fail msg =
  failwith ("test_multi_exec: " ^ msg)

let ok msg cond =
  if not cond then fail msg

let call ?(target = "oct7xCozDD9JEsbeVpo5C7HXp2BJbKqfmNUHmDDCCTtWcGb") ?(amount = Z.zero) method_name =
  {
    C.target;
    method_name;
    params = [];
    amount;
  }

let receipt ?(success = true) ?(effort = 1) ?error () : Contract.exec_result =
  {
    success;
    return_value = Some (VM.VString "ok");
    effort_used = effort;
    events = [];
    error;
    storage_writes = 0;
  }

let test_success () =
  let steps = ref [] in
  let result =
    M.run
      ~from_addr:"alice"
      ~calls:[call "a"; call ~amount:(Z.of_int 3) "b"]
      ~effort_limit:10
      ~balance:(fun _ -> Z.of_int 10)
      ~exec:(fun (step : M.step) ->
        steps := step.index :: !steps;
        receipt ~effort:(step.index + 1) ())
  in
  ok "success outcome" (result.M.outcome = Ok ());
  ok "success effort" (result.trace.R.effort = 3);
  ok "success steps" (!steps = [1; 0]);
  ok "success calls" (List.length result.trace.calls = 2)

let test_effort_exhausted () =
  let result =
    M.run
      ~from_addr:"alice"
      ~calls:[call "a"; call "b"]
      ~effort_limit:1
      ~balance:(fun _ -> Z.zero)
      ~exec:(fun _ -> receipt ~effort:1 ())
  in
  ok "effort outcome" (result.outcome = Error "effort limit exceeded");
  ok "effort trace" (result.trace.R.effort = 1)

let test_insufficient_value () =
  let result =
    M.run
      ~from_addr:"alice"
      ~calls:[call ~amount:(Z.of_int 11) "pay"]
      ~effort_limit:10
      ~balance:(fun _ -> Z.of_int 10)
      ~exec:(fun _ -> receipt ())
  in
  ok "insufficient outcome" (result.outcome = Error "insufficient balance for program value");
  ok "insufficient trace" (result.trace.R.effort = 0)

let test_failed_receipt () =
  let result =
    M.run
      ~from_addr:"alice"
      ~calls:[call "bad"; call "skip"]
      ~effort_limit:10
      ~balance:(fun _ -> Z.zero)
      ~exec:(fun (step : M.step) ->
        if step.index = 0 then receipt ~success:false ~error:"boom" ()
        else receipt ())
  in
  ok "failed outcome" (result.outcome = Error "call 0 failed: boom");
  ok "failed trace" (List.length result.trace.calls = 1)

let test_cost () =
  let message =
    {|{"calls":[{"to":"a","method":"x","params":[]},{"to":"b","method":"y","params":[]},{"to":"c","method":"z","params":[]}]}|}
  in
  let tx = {
    T.from = "alice";
    to_ = "multi_exec";
    amount = Z.zero;
    nonce = 1;
    ou = Z.zero;
    timestamp = 0.;
    signature = "sig";
    public_key = None;
    message = Some message;
    op_type = T.MultiExec;
    encrypted_data = None;
  } in
  ok "call count cost" (Z.equal (T.ou_cost tx) (Z.of_int 4_000))

let () =
  test_success ();
  test_effort_exhausted ();
  test_insufficient_value ();
  test_failed_receipt ();
  test_cost ();
  print_endline "test_multi_exec: ok"