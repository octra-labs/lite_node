(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type shape = {
  slots : int;
  layers : int;
  edges : int;
}

val multiplication_effort :
  left:shape ->
  right:shape ->
  int option

val additional_effort :
  left:shape ->
  right:shape ->
  int option