(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type step = {
  index : int;
  call : Call_plan.call;
  remaining_effort : int;
  value_effect : Call_plan.value_effect;
}

type result = {
  trace : Receipt_view.multi_exec_trace;
  outcome : (unit, string) Stdlib.result;
}

let run ~from_addr ~calls ~effort_limit ~balance ~exec =
  let rec go index trace = function
    | [] ->
      { trace; outcome = Ok () }
    | call :: tail ->
      match Call_plan.plan_multi_exec_step
        ~from_addr
        ~call
        ~effort_used:trace.Receipt_view.effort
        ~effort_limit
        ~balance:(balance from_addr)
      with
      | Call_plan.Multi_exec_effort_exhausted ->
        { trace; outcome = Error "effort limit exceeded" }
      | Call_plan.Multi_exec_insufficient_value ->
        { trace; outcome = Error "insufficient balance for program value" }
      | Call_plan.Multi_exec_ready planned ->
        let receipt =
          exec {
            index;
            call;
            remaining_effort = planned.remaining_effort;
            value_effect = planned.value_effect;
          } in
        let trace =
          Receipt_view.add_multi_exec_trace
            trace
            ~index
            ~program:call.target
            ~method_name:call.method_name
            ~amount:call.amount
            ~success:receipt.Contract.success
            ~effort:receipt.Contract.effort_used
            ~error:receipt.Contract.error
            ~return_value:receipt.Contract.return_value
            ~events:receipt.Contract.events in
        match Receipt_view.multi_exec_step_error
          ~index
          ~success:receipt.Contract.success
          ~events:receipt.Contract.events
          ~error:receipt.Contract.error
        with
        | None -> go (index + 1) trace tail
        | Some err -> { trace; outcome = Error err }
  in
  go 0 Receipt_view.empty_multi_exec_trace calls