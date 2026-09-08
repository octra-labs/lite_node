(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

val max : int
val make : unit -> t
val add : t -> string -> unit
val full : t -> bool
val get : t -> string
val clip : string -> string