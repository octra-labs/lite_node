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

val run :
  from_addr:string ->
  calls:Call_plan.call list ->
  effort_limit:int ->
  balance:(string -> Z.t) ->
  exec:(step -> Contract.exec_result) ->
  result