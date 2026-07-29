(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

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