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


type verdict =
  | Accept
  | Invalid_address
  | Timestamp_drift of float
  | Invalid_signature

val to_valid : Octra_core.Transaction.t -> bool

val addr_valid : Octra_core.Transaction.t -> bool

val sig_valid : Octra_core.Transaction.t -> string option -> bool

val admit :
  now:float ->
  max_drift:float ->
  sender_pk:string option ->
  Octra_core.Transaction.t ->
  verdict