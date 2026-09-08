(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type bits = C_bin.bits

type error =
  | Check of C_check.rule_error
  | Encode

val issue :
  C_rule.schedule -> epoch:Z.t -> C_low.prog -> (bits, error) result
val verify : C_rule.schedule -> epoch:Z.t -> bits -> bool
val text : error -> string