(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type decision =
  | Keep
  | Register of string

val env_name : string

val activation_epoch_of :
  (string -> string option) ->
  (int option, string) result

val activation_epoch_exn :
  (string -> string option) ->
  int option

val decide :
  activation_epoch:int option ->
  epoch:int ->
  stored:string option ->
  carried:string option ->
  decision