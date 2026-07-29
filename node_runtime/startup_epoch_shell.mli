(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type source =
  | Meta of int
  | Files of int
  | Genesis

type deps = {
  last_epoch_meta : unit -> string option;
  saved_epochs : unit -> int list;
  set_current_epoch : int -> unit;
}

val source :
  last_epoch_meta:string option ->
  saved_epochs:int list ->
  source

val current_epoch :
  source ->
  int

val run :
  deps ->
  int