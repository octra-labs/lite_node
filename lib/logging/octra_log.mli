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


type level =
  | FATAL
  | ERROR
  | WARN
  | INFO
  | TRACE

val _set_level : level -> unit

val level_of_string : string -> level option

val init_from_env : unit -> unit

val stdout : ('a, unit, string, unit) format4 -> 'a

val stderr : ('a, unit, string, unit) format4 -> 'a

val info : string -> ('a, unit, string, unit) format4 -> 'a

val warn : string -> ('a, unit, string, unit) format4 -> 'a

val error : string -> ('a, unit, string, unit) format4 -> 'a

val fatal : string -> ('a, unit, string, unit) format4 -> 'a

val trace : string -> ('a, unit, string, unit) format4 -> 'a