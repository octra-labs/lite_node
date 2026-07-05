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

val is_shared_bft_tx : Transaction.t -> bool

val all_shared_bft : Transaction.t list -> bool

val canonical_order : Transaction.t list -> Transaction.t list

val process_standard :
  backend:Epoch_exec.backend ->
  env:Epoch_exec.env ->
  Transaction.t ->
  (unit, string * string) result Lwt.t

val run : deps -> Transaction.t list -> unit Lwt.t