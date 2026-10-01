(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

val limit : Z.t
val consensus_id : string
val create : unit -> t
val used : t -> Z.t
val reserve : t -> Z.t -> bool
val key_decode : string -> Z.t option
val key_value : Pvac_ffi.pubkey -> Z.t
val key_read_effort : string -> int option
val key_write_effort : Pvac_ffi.pubkey -> int option
val cipher_decode : string -> Z.t