(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type bits = C_bin.bits

val valid : C_proj.t -> bool
val code : C_proj.t -> C_bin.code option
val get : C_bin.code -> C_proj.t option
val enc : C_proj.t -> bits option
val dec : bits -> C_proj.t option