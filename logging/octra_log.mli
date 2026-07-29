(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type level = Octra_log_format.level =
  | FATAL
  | ERROR
  | WARN
  | INFO
  | TRACE

val _set_level : level -> unit

val level_of_string : string -> level option

val init_from_env : unit -> unit

val stdout : ('a, unit, string, unit) format4 -> 'a

val stderr : ('a, unit, string, unit) format4 -> 'a

val info : string -> ('a, unit, string, unit) format4 -> 'a

val warn : string -> ('a, unit, string, unit) format4 -> 'a

val error : string -> ('a, unit, string, unit) format4 -> 'a

val fatal : string -> ('a, unit, string, unit) format4 -> 'a

val trace : string -> ('a, unit, string, unit) format4 -> 'a