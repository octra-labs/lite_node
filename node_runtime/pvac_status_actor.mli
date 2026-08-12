(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type error =
  | Busy
  | Stopped
  | Read_failed

type stats = {
  cache_entries : int;
  queued : int;
  generation : int64;
}

type deps = {
  load_pubkey : addr:string -> string option Lwt.t;
  hash_pubkey : string -> string;
  classify_cipher : string -> Octra_core.Pvac_migration.cipher_class;
  classify_key : string option -> Octra_core.Pvac_migration.key_class;
}

type t

val request_capacity : int
val cache_capacity : int
val missing_capacity : int

val create : deps -> t

val query :
  t ->
  addr:string ->
  key_hash:string option ->
  cipher:string ->
  (Octra_core.Pvac_migration.status, error) result Lwt.t

val stats : t -> stats Lwt.t
val shutdown : t -> unit Lwt.t