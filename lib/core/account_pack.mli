(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type data =
  | Old of Ledger_types.account
  | Parts of Ledger_types.account * string

type meta

type image = {
  data : string;
  meta : string option;
  parts : Blob_chunk.part list;
  id : string option;
}

val old : Ledger_types.account -> string
val data : string -> (data, string) result
val meta : string -> (meta, string) result
val ids : meta -> string list
val key : meta -> string
val head : Ledger_types.account -> (string * string) option
val image : Ledger_types.account -> image

val read :
  data ->
  meta:string option ->
  get:(string -> string option) ->
  (Ledger_types.account, string) result

val balance : string -> (Z.t, string) result