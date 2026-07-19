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


type key = {
  id : string;
  public_key : string;
}

type error

val error_message : error -> string
val attach : key_id:string -> private_key:string -> string -> (string, error) result
val verify : trusted:key list -> string -> (unit, error) result