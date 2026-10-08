(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module W = Octra_vm.Proof_wait
module VM = Octra_vm.Contract_vm

let expect ok = if not ok then failwith "proof reply lifecycle differs"

let () =
  let name = "OCTRA_TEST_PROOF_TIMEOUT" in
  List.iter (fun raw ->
    Unix.putenv name raw;
    expect (Octra_core.Pvac_verify_worker.float_env name 600. 1. 1800. = 600.))
    ["nan"; "infinity"; "-infinity"; "0"; "1801"];
  Unix.putenv name "1";
  expect (Octra_core.Pvac_verify_worker.float_env name 600. 1. 1800. = 1.);
  let module Pool = Octra_core.Compute_pool in
  let pool = Pool.create ~capacity:1 ~required_limit:1 ~speculative_limit:1
    ~required_burst:1 () in
  List.iter (fun (error, resource) ->
    let caught = try
      ignore (Lwt_main.run (VM.proof_host (fun () -> raise error))); false
    with Octra_core.Exec_resource.Unavailable actual -> actual = resource in
    expect caught;
    expect (Lwt_main.run (Lwt.return 7) = 7))
    [Out_of_memory, Octra_core.Exec_resource.Memory;
     Stack_overflow, Octra_core.Exec_resource.Stack];
  List.iter (fun (error, expected) ->
    let work () = Pool.run_threaded pool Pool.Required (fun () -> raise error) () in
    let caught = try ignore (Lwt_main.run (VM.proof_host work)); false with
      | actual when actual = expected -> true in
    expect caught;
    expect ((Pool.stats pool).active = 0)) [
    Sys_error "thread creation failed", Octra_core.Exec_resource.Unavailable Host;
    Failure "worker pipe failed", Octra_core.Exec_resource.Unavailable Host;
    Lwt_unix.Timeout, Octra_core.Exec_resource.Unavailable Host;
    Out_of_memory, Octra_core.Exec_resource.Unavailable Memory;
    Stack_overflow, Octra_core.Exec_resource.Unavailable Stack;
    Lwt.Canceled, Lwt.Canceled;
  ];
  for request = 0 to 9 do
    for generation = 0 to 9 do
      let ticket = W.{request = string_of_int request; generation = string_of_int generation} in
      let state = W.Waiting (ticket, 10L) in
      let closed, effects = W.delta state (W.Reply (ticket, 9L, true)) in
      expect (closed = W.Closed && effects = [W.Deliver true]);
      expect (W.delta closed (W.Reply (ticket, 9L, false)) = (W.Closed, []));
      List.iter (fun control ->
        let closed, effects = W.delta state control in
        expect (effects = [W.Stop (if control = W.Cancel then W.Cancelled else W.Deadline)]);
        expect (W.delta closed (W.Reply (ticket, 9L, true)) = (W.Closed, [])))
        [W.Cancel; W.Expire];
      List.iter (fun now ->
        expect (W.delta state (W.Reply (ticket, now, true))
          = (W.Closed, [W.Stop W.Deadline]))) [10L; 11L; Int64.max_int];
      for other = 0 to 9 do
        if other <> generation then
          expect (W.delta state (W.Reply ({ticket with generation = string_of_int other}, 9L, true))
            = (state, []));
        if other <> request then
          expect (W.delta state (W.Reply ({ticket with request = string_of_int other}, 9L, true))
            = (state, []))
      done
    done
  done;
  Printf.printf "event = proof_wait status = passed\n%!"