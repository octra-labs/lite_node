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


type error

val error_message : error -> string
val parse : Program_type_flow.kind list -> Yojson.Safe.t list -> (Contract_vm.v list, error) result
val validate : Program_type_flow.kind list -> Contract_vm.v list -> (unit, error) result