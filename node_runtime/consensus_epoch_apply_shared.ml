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


module Transaction = Octra_core.Transaction
module Epoch_exec = Octra_core.Epoch_exec

type process = Transaction.t -> (unit, string * string) result Lwt.t

type deps = {
  process : process;
  confirm : Transaction.t -> unit;
  reject : Transaction.t -> error_type:string -> reason:string -> unit;
}

let is_shared_bft_tx (tx : Transaction.t) =
  match tx.Transaction.op_type with
  | Transaction.Standard
  | Transaction.ValidatorSetUpdate
  | Transaction.ValidatorReady -> true
  | _ -> false

let all_shared_bft txs =
  List.for_all is_shared_bft_tx txs

let canonical_order txs =
  List.sort
    (fun (a : Transaction.t) (b : Transaction.t) ->
       let c = String.compare a.Transaction.from b.Transaction.from in
       if c <> 0 then c else compare a.Transaction.nonce b.Transaction.nonce)
    txs

let process_standard ~backend ~env tx =
  let open Lwt.Syntax in
  let* result = Epoch_exec.process_standard_tx ~backend ~env tx in
  match result with
  | Ok _ -> Lwt.return (Ok ())
  | Error e -> Lwt.return (Error e)

let run deps txs =
  Lwt_list.iter_s
    (fun tx ->
       let open Lwt.Syntax in
       let* result = deps.process tx in
       match result with
       | Ok () ->
         deps.confirm tx;
         Lwt.return_unit
       | Error (error_type, reason) ->
         deps.reject tx ~error_type ~reason;
         Lwt.return_unit)
    (canonical_order txs)