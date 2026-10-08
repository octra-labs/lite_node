(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

val limit : Z.t
val consensus_id : string
val create : unit -> t
val used : t -> Z.t
val reserve : t -> Z.t -> bool
val key_decode : string -> Z.t option
val key_value : Octra_core.Fhe_image.key -> Z.t
val key_image : string -> Z.t option
val key_read_effort : ?active:bool -> string -> int option
val key_write_effort : ?active:bool -> Octra_core.Fhe_image.key -> int option
val key_effort : ?active:bool -> Z.t -> int option
val cipher_decode : string -> Z.t