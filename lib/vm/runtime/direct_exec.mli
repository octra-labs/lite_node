(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


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