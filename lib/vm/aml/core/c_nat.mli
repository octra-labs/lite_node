(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

val max : int
val str_max : int
val zero : t
val one : t
val make : Z.t -> t option
val byte : Z.t -> int option
val of_int : int -> t option
val valid : t -> bool
val to_z : t -> Z.t
val to_int : t -> int
val text : t -> string
val equal : t -> t -> bool
val compare : t -> t -> int
val lt : t -> t -> bool
val le : t -> t -> bool
val add : t -> t -> t option
val sub : t -> t -> t option