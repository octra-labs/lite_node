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