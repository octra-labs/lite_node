(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module D = Octra_vm.Direct_exec
module C = Octra_vm.Call_plan
module R = Octra_vm.Receipt_view
module VM = Octra_vm.Contract_vm
module Contract = Octra_vm.Contract

let fail msg =
  failwith ("test_direct_exec: " ^ msg)

let ok msg cond =
  if not cond then fail msg

let spec ?(method_name = Some "run") ?(params_json = Some "[]") ?(amount = Z.zero) ?(balance = Z.zero) () =
  {
    D.domain = R.Program_call;
    reject_domain = C.Program_exec;
    method_name;
    params_json;
    from_addr = "alice";
    target = "oct7xCozDD9JEsbeVpo5C7HXp2BJbKqfmNUHmDDCCTtWcGb";
    amount;
    balance;
    ou = Z.of_int 10;
  }

let receipt ?(success = true) ?error () : Contract.exec_result =
  {
    success;
    return_value = Some (VM.VString "ok");
    effort_used = 7;
    events = [];
    error;
    storage_writes = 0;
  }

let test_success () =
  let applied = ref false in
  let saved = ref false in
  let accepted = ref false in
  Lwt_main.run (
    D.run
      (spec ())
      {
        apply = (fun _ -> applied := true);
        exec = (fun _ -> Lwt.return (receipt ()));
        receipt = (fun r -> r);
        save = (fun _ _ -> saved := true);
        ok = (fun meta call _ ->
          accepted := meta.R.failure_type = "program_exec_failed"
            && call.C.method_name = "run";
          Lwt.return_unit);
        fail = (fun _ _ _ -> fail "unexpected fail");
        reject = (fun _ -> fail "unexpected reject");
        crash = (fun _ _ -> fail "unexpected crash");
      });
  ok "success applied" !applied;
  ok "success saved" !saved;
  ok "success accepted" !accepted

let test_failed_receipt () =
  let rejected = ref false in
  Lwt_main.run (
    D.run
      (spec ())
      {
        apply = ignore;
        exec = (fun _ -> Lwt.return (receipt ~success:false ~error:"boom" ()));
        receipt = (fun r -> r);
        save = (fun _ _ -> ());
        ok = (fun _ _ _ -> fail "unexpected ok");
        fail = (fun meta call error ->
          rejected := meta.R.failure_type = "program_exec_failed"
            && call.C.method_name = "run"
            && error = "boom";
          Lwt.return_unit);
        reject = (fun _ -> fail "unexpected reject");
        crash = (fun _ _ -> fail "unexpected crash");
      });
  ok "failed receipt" !rejected

let test_reject () =
  let rejected = ref None in
  Lwt_main.run (
    D.run
      (spec ~method_name:None ())
      {
        apply = ignore;
        exec = (fun _ -> fail "unexpected exec");
        receipt = (fun r -> r);
        save = (fun _ _ -> ());
        ok = (fun _ _ _ -> fail "unexpected ok");
        fail = (fun _ _ _ -> fail "unexpected fail");
        reject = (fun r ->
          rejected := Some r.C.error_type;
          Lwt.return_unit);
        crash = (fun _ _ -> fail "unexpected crash");
      });
  ok "reject" (!rejected = Some "malformed_transaction")

let test_crash () =
  let crashed = ref false in
  Lwt_main.run (
    D.run
      (spec ())
      {
        apply = ignore;
        exec = (fun _ -> failwith "boom");
        receipt = (fun r -> r);
        save = (fun _ _ -> ());
        ok = (fun _ _ _ -> fail "unexpected ok");
        fail = (fun _ _ _ -> fail "unexpected fail");
        reject = (fun _ -> fail "unexpected reject");
        crash = (fun meta error ->
          crashed := meta.R.exception_type = "program_exec_exception"
            && error = Failure "boom";
          Lwt.return_unit);
      });
  ok "crash" !crashed

let test_commit_failure_bubbles () =
  let crashed = ref false in
  let bubbled =
    Lwt_main.run
      (Lwt.catch
         (fun () ->
           let open Lwt.Syntax in
           let* () =
             D.run
               (spec ())
               {
                 apply = ignore;
                 exec = (fun _ -> Lwt.return (receipt ()));
                 receipt = (fun r -> r);
                 save = (fun _ _ -> ());
                 ok = (fun _ _ _ ->
                   Lwt.fail (Octra_vm.Tx_effects.Commit_failed "commit"));
                 fail = (fun _ _ _ -> fail "unexpected fail");
                 reject = (fun _ -> fail "unexpected reject");
                 crash = (fun _ _ ->
                   crashed := true;
                   Lwt.return_unit);
               }
           in
           Lwt.return false)
         (function
           | Octra_vm.Tx_effects.Commit_failed "commit" -> Lwt.return true
           | error -> Lwt.fail error))
  in
  ok "commit failure bubbled" bubbled;
  ok "commit failure bypassed crash" (not !crashed)

let test_resource_failures () =
  List.iter (fun error ->
    List.iter (fun pending ->
      let saved = ref false in
      let charged = ref false in
      let raised = try
        Lwt_main.run (D.run (spec ()) {
          apply = ignore;
          exec = (fun _ -> if pending then Lwt.fail error else raise error);
          receipt = (fun r -> r);
          save = (fun _ _ -> saved := true);
          ok = (fun _ _ _ -> fail "unexpected ok");
          fail = (fun _ _ _ -> fail "unexpected fail");
          reject = (fun _ -> fail "unexpected reject");
          crash = (fun _ _ -> charged := true; Lwt.return_unit);
        });
        false
      with actual when actual = error -> true in
      ok "resource exception preserved" raised;
      ok "resource exception has no receipt" (not !saved);
      ok "resource exception has no charge" (not !charged))
      [false; true]) [Stack_overflow; Out_of_memory]

let () =
  test_resource_failures ();
  test_success ();
  test_failed_receipt ();
  test_reject ();
  test_crash ();
  test_commit_failure_bubbles ();
  print_endline "test_direct_exec: ok"