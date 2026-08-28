(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type part = {
  id : string;
  raw : string;
}

type t = {
  id : string;
  size : int;
  parts : part list;
}

val part_id : string -> string
val count_ok : size:int -> int -> bool
val cut : string -> t

val join :
  id:string ->
  size:int ->
  (string * string) list ->
  (string, string) result