(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = private {text : string; count : int; chars : int}

val plan : capacity:int -> fits:(count:int -> chars:int -> bool) -> string -> t option
val read : strict:bool -> t -> Z.t array option