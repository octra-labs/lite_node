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
  | No_proof
  | Zero_receipt_v1
  | Range_v1
  | Range_receipt_v1
  | Bound_zero_v1
  | Bound_zero_receipt_v1

let string_of_t = function
  | No_proof -> "none"
  | Zero_receipt_v1 -> "zero_receipt_v1"
  | Range_v1 -> "range_v1"
  | Range_receipt_v1 -> "range_receipt_v1"
  | Bound_zero_v1 -> "bound_zero_v1"
  | Bound_zero_receipt_v1 -> "bound_zero_receipt_v1"

let of_string = function
  | "none" -> Ok No_proof
  | "zero_receipt_v1" -> Ok Zero_receipt_v1
  | "range_v1" -> Ok Range_v1
  | "range_receipt_v1" -> Ok Range_receipt_v1
  | "bound_zero_v1" -> Ok Bound_zero_v1
  | "bound_zero_receipt_v1" -> Ok Bound_zero_receipt_v1
  | other -> Error ("unknown hfhe proof kind: " ^ other)