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


type t = {
  code : string;
  cert : string;
}

type error

val is_program : string -> bool
val encode : code:string -> cert:string -> (string, error) result
val decode : string -> (t, error) result
val error_message : error -> string