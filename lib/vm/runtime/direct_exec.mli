(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type spec = {
  domain : Receipt_view.direct_call_domain;
  reject_domain : Call_plan.direct_exec_domain;
  method_name : string option;
  params_json : string option;
  from_addr : string;
  target : string;
  amount : Z.t;
  balance : Z.t;
  ou : Z.t;
}

type 'a io = {
  apply : Call_plan.value_effect -> unit;
  exec : Call_plan.direct_exec -> 'a Lwt.t;
  receipt : 'a -> Contract.exec_result;
  save : Call_plan.direct_exec -> 'a -> unit;
  ok : Receipt_view.direct_call_meta -> Call_plan.direct_exec -> 'a -> unit Lwt.t;
  fail : Receipt_view.direct_call_meta -> Call_plan.direct_exec -> string -> unit Lwt.t;
  reject : Call_plan.direct_exec_reject -> unit Lwt.t;
  crash : Receipt_view.direct_call_meta -> string -> unit Lwt.t;
}

val plan :
  spec ->
  Call_plan.direct_exec_plan

val run :
  spec ->
  'a io ->
  unit Lwt.t