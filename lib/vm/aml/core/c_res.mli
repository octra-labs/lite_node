(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val typ : C_type.t -> C_type.t -> C_type.t
val ok : err:C_type.t -> C_term.t -> C_term.t
val err : ok:C_type.t -> C_term.t -> C_term.t

val fold :
  C_term.t ->
  C_term.bind ->
  C_term.t ->
  C_term.bind ->
  C_term.t ->
  C_term.t