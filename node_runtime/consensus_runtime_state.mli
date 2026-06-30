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
val state_attested : t -> bool
val quarantine_active : t -> bool
val quarantine_active_ref : t -> bool ref
val quarantine_reason : t -> string
val quarantine_since_epoch : t -> int
val prev_root_streak : t -> int
val set_prev_root_streak : t -> int -> unit
val state_root_streak : t -> int
val set_state_root_streak : t -> int -> unit
val ahead_streak : t -> int
val incr_ahead_streak : t -> unit
val clear_state_attested : t -> unit
val set_state_attested : t -> head:int -> root:string -> unit
val attested_head : t -> int -> bool
val enter_quarantine : t -> epoch:int -> reason:string -> bool
val clear_quarantine : t -> unit