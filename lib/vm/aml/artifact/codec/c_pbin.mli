(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type bits = C_bin.bits

val valid : C_low.prog -> bool
val code : C_low.prog -> C_bin.code option
val get : C_bin.code -> C_low.prog option
val enc : C_low.prog -> bits option
val dec : bits -> C_low.prog option