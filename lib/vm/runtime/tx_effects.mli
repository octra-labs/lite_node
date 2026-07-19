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


type t

exception Commit_failed of string

val create :
  ledger:Octra_core.Ledger.t ->
  store:Octra_core.Store_irmin.t ->
  t

val value : t -> Value_journal.t
val program : t -> Program_journal.t
val balance : t -> string -> Z.t
val debit : t -> string -> Z.t -> int -> (unit, string) result
val apply : t -> Call_plan.value_effect -> bool
val ensure_account : t -> string -> (unit, string) result
val commit : t -> (unit, string) result
val commit_exn : t -> unit
val discard : t -> unit