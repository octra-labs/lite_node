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


val addr_short : string -> string

val prefix_or_unknown : int -> string -> string

val hash_short : string -> string

val peer_short : string -> string

val addr14 : string -> string

val root_short : string -> string

val msg_hex : int -> string

val frame_len : Octra_net.P2p_frame.frame -> int

val is_hex_string : string -> bool

val is_hex_len : int -> string -> bool

val hex_to_string : string -> string

val decode_message_if_hex : string -> string

val raw_to_hex : string -> string

val hex_to_raw32_lossy : string -> string