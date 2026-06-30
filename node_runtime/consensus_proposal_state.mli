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

val create : unit -> t
val set : t -> Octra_core.Transaction.t list -> string list -> unit
val reset_empty_received : t -> unit
val mark_unsynced : t -> unit
val txs : t -> Octra_core.Transaction.t list
val tx_hashes : t -> string list
val received : t -> bool