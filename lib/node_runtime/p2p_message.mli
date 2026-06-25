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


type domain =
  | Legacy_epoch
  | Tx_gossip
  | Consensus
  | Unknown of int

val legacy_epoch_broadcast : int

val consensus_types : int list

val is_consensus : int -> bool

val classify : int -> domain