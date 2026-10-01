(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  env : Lmdb.Env.t;
  meta : (string, string, [`Uni]) Lmdb.Map.t;
  receipts : (string, string, [`Uni]) Lmdb.Map.t;
  rejected : (string, string, [`Uni]) Lmdb.Map.t;
  addresses : (string, string, [`Dup | `Uni]) Lmdb.Map.t;
  epochs : (int32, string, [`Dup | `Uni]) Lmdb.Map.t;
}

exception Refused of string

val pending_key : string
val epoch_field : string -> string -> string -> string -> int
val journal : t -> [> `Read] Lmdb.Txn.t -> Aux_delta.t option
val write : t -> [`Read | `Write] Lmdb.Txn.t -> Aux_delta.key * Aux_delta.cell -> unit
val each : ('key, 'value, 'dup) Lmdb.Map.t -> [> `Read] Lmdb.Txn.t ->
  ('key -> 'value -> unit) -> unit
val with_write : t -> previous:Aux_delta.anchor option -> target:Aux_delta.anchor ->
  writes:(Aux_delta.key * Aux_delta.cell) list ->
  ([`Read | `Write] Lmdb.Txn.t -> 'a) -> 'a
val verify : t -> [> `Read] Lmdb.Txn.t -> head:Aux_delta.anchor option -> unit
val inspect : t -> head:Aux_delta.anchor option -> unit
val apply_restore : t -> [`Read | `Write] Lmdb.Txn.t -> head:Aux_delta.anchor option -> unit
val retire : t -> [`Read | `Write] Lmdb.Txn.t -> head:Aux_delta.anchor option -> unit