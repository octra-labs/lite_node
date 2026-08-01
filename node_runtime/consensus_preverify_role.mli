(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type runner =
  Octra_core.Transaction.t list ->
  Octra_core.Preverify_worker.batch Lwt.t

type build
type validate

val build : runner -> build
val validate : runner -> validate

val run_build : build -> runner
val run_validate : validate -> runner