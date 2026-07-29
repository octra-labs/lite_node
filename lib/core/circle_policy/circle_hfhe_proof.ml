(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

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