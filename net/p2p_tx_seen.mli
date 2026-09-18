(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

val empty : t
val capacity : int
val lifetime : float
val size : t -> int
val step : t -> now:float -> string -> t * bool