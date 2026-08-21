(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type state =
  | Missing
  | Ready of Sync_need.t
  | Invalid of string

type write =
  | Stored
  | Present of Sync_need.t

val path : string -> string

val read : data_dir:string -> chain:string -> state

val write :
  data_dir:string ->
  chain:string ->
  Sync_need.t ->
  (write, string) result