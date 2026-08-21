(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Transaction = Octra_core.Transaction

type runner =
  string -> Transaction.t list -> Octra_core.Preverify_worker.batch Lwt.t

type build = Build of runner
type validate = Validate of runner

let build runner = Build runner
let validate runner = Validate runner

let run_build (Build runner) = runner
let run_validate (Validate runner) = runner