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

let plan spec =
  Call_plan.plan_direct_exec
    ~method_name:spec.method_name
    ~params_json:spec.params_json
    ~from_addr:spec.from_addr
    ~target:spec.target
    ~amount:spec.amount
    ~balance:spec.balance
    ~ou:spec.ou

let run spec io =
  Lwt.catch
    (fun () ->
      match plan spec with
      | Call_plan.Direct_exec_ready call ->
        io.apply call.value_effect;
        let open Lwt.Syntax in
        let* item = io.exec call in
        let receipt = io.receipt item in
        io.save call item;
        begin
          match Receipt_view.direct_call_decision spec.domain receipt with
          | Receipt_view.Direct_call_ok meta ->
            io.ok meta call item
          | Receipt_view.Direct_call_failed { meta; error } ->
            io.fail meta call error
        end
      | (Call_plan.Direct_exec_insufficient_value
        | Call_plan.Direct_exec_invalid_format) as rejected ->
        io.reject (Call_plan.direct_exec_reject spec.reject_domain rejected))
    (fun exn ->
      match exn with
      | Tx_effects.Commit_failed _ -> Lwt.fail exn
      | _ ->
        io.crash
          (Receipt_view.direct_call_meta spec.domain)
          (Printexc.to_string exn))