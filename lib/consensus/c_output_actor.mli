(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

type request =
  | Start
  | Join

type completion =
  | Continue
  | Stop

val idle : t
val request : t -> t * request
val complete : t -> t * completion
val fail : t -> t
val is_idle : t -> bool