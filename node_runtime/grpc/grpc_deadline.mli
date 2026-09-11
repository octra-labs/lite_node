(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val seconds :
  default:float ->
  limit:float ->
  string option ->
  (float, string) result