(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type pos = {
  off : int;
  line : int;
  col : int;
}

type error = {
  first : pos;
  last : pos;
}

val erase : string -> (string, error) result
val text : error -> string