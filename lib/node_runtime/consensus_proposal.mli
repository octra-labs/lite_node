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

type limits = {
  max_txs : int;
  max_bytes : int;
  max_ou : Z.t;
}

type totals = {
  count : int;
  bytes : int;
  ou : Z.t;
}

type capped = {
  txs : Transaction.t list;
  skipped : int;
  totals : totals;
}

val limits : max_txs:int -> max_bytes:int -> max_ou:Z.t -> limits

val wire_size : Transaction.t -> int

val totals : Transaction.t list -> totals

val within_limits : limits:limits -> Transaction.t list -> bool

val cap : limits:limits -> Transaction.t list -> capped