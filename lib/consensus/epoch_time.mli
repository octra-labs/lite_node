(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = int64

type proposal_kind = Fresh | Reproposal

type rule = Historical | Uniform

val interval_ms : int64
val interval_seconds : float
val proposal_wait_budget_ms : int64
val of_seconds : float -> (t, string) result
val to_z : t -> Z.t
val next_delay_ms : now:float -> previous:float -> int64
val check :
  now:float ->
  previous:t option ->
  candidate:float ->
  (t, string) result
val check_reproposal :
  previous:t option ->
  candidate:float ->
  (t, string) result
val check_proposal :
  rule:rule ->
  kind:proposal_kind ->
  now:float ->
  previous:t option ->
  candidate:float ->
  (t, string) result