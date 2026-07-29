(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = int64

val of_seconds : float -> (t, string) result
val to_z : t -> Z.t
val check :
  now:float ->
  previous:t option ->
  candidate:float ->
  (t, string) result
val check_reproposal :
  previous:t option ->
  candidate:float ->
  (t, string) result