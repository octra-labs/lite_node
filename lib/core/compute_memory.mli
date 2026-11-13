(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t
type ticket
type stats = {bytes : int; speculative : int; jobs : int}

val create : limit:int -> speculative_limit:int -> slots:int -> t
val reserve : t -> speculative:bool -> int -> ticket option
val release : ticket -> unit
val stats : t -> stats
val limit_of_getenv : (string -> string option) -> int
val shared : t
val add : int -> int -> int
val strings : string list -> int