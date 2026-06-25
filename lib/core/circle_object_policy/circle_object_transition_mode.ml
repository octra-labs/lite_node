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
  | Open
  | Proof_required
  | Frozen

let string_of_t = function
  | Open -> "open"
  | Proof_required -> "proof_required"
  | Frozen -> "frozen"

let of_string = function
  | "open" -> Ok Open
  | "proof_required" -> Ok Proof_required
  | "frozen" -> Ok Frozen
  | other -> Error ("unknown object transition mode: " ^ other)