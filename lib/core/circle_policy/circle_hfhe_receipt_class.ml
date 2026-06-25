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


type t =
  | Detached
  | Transport_bound
  | Relay_witnessed

let string_of_t = function
  | Detached -> "detached"
  | Transport_bound -> "transport_bound"
  | Relay_witnessed -> "relay_witnessed"

let of_string = function
  | "detached" -> Ok Detached
  | "transport_bound" -> Ok Transport_bound
  | "relay_witnessed" -> Ok Relay_witnessed
  | other -> Error ("unknown hfhe receipt class: " ^ other)