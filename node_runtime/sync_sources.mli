(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type route = Head | Range
type outcome = Good | Neutral | Busy | Failed
type t
type row

val create : ?now:(unit -> float) -> ?jitter:(unit -> float) -> unit -> t
val plan : t -> route -> string list -> string list * string list
val start : t -> route -> string -> row
val finish : t -> row -> outcome -> unit
val error : string -> outcome