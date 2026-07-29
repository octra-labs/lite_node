(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val run :
  enabled:bool ->
  store:Octra_core.Store_irmin.t ->
  exit_fatal:(unit -> unit) ->
  unit