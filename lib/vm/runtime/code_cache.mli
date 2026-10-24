(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type key
type 'a t

val create : unit -> 'a t
val key : mode:int -> point_ops:bool -> trusted:Program_attestation.key list -> string -> key
val fits : key -> 'a -> bool
val run : ?retain:('a -> 'a) -> 'a t -> key -> (unit -> ('a, 'e) result) -> ('a, 'e) result