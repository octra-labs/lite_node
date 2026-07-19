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


type effect =
  | Memory_read
  | Memory_write
  | Storage_read
  | Storage_write
  | Call
  | Deploy
  | Transfer
  | Emit
  | Fhe
  | Journal

type t

val scan : Contract_vm.instr array -> t
val names : t -> string list