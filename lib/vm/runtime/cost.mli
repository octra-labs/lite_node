(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val add : int -> int -> int option
val product : int list -> int option
val scaled_product : int list -> divisor:int -> int option
val charge : used:int -> cost:int -> limit:int -> int option
val charge_z : used:int -> cost:Z.t -> limit:int -> int option