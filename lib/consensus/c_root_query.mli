(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type lease
type 'a t

val empty : 'a t
val join : epoch:int64 -> 'a t -> ('a t * lease, string) result
val listening : epoch:int64 -> 'a t -> bool
val add : epoch:int64 -> same:('a -> 'a -> bool) -> 'a -> 'a t -> 'a t
val read : lease -> 'a t -> 'a list
val leave : lease -> 'a t -> 'a t