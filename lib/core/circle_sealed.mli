(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type digest_mode = Compatible | Keyed

val digest_salt : circle_id:string -> key_id:string -> string
val read_key : circle_id:string -> key_id:string -> passphrase:string -> string
val digest : circle_id:string -> key_id:string -> passphrase:string -> string -> string
val write_digest :
  ?mode:digest_mode -> circle_id:string -> key_id:string -> passphrase:string -> string -> string
val matches_digest :
  circle_id:string -> key_id:string -> passphrase:string -> expected:string -> string -> bool