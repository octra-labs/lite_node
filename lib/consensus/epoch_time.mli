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


type t = int64

val of_seconds : float -> (t, string) result
val to_z : t -> Z.t
val check :
  now:float ->
  previous:t option ->
  candidate:float ->
  (t, string) result