(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type level =
  | FATAL
  | ERROR
  | WARN
  | INFO
  | TRACE

val level_name : level -> string

val timestamp : float -> string

val render :
  color:bool ->
  timestamp:string ->
  level:level ->
  component:string ->
  message:string ->
  string