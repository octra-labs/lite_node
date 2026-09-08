(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type mode = Every | Any | Count | Sum

type error =
  | Low of C_low.error
  | Check of C_check.error
  | Fresh

val expand : C_syn.name -> C_syn.name -> mode -> C_syn.t -> C_syn.bind ->
  C_syn.t -> (C_syn.t, error) result
val make : mode -> C_syn.t -> C_syn.bind -> C_syn.t ->
  (C_syn.t, error) result
val lower : C_syn.bind list -> mode -> C_syn.t -> C_syn.bind -> C_syn.t ->
  (C_low.prog, error) result
val check : C_syn.bind list -> mode -> C_syn.t -> C_syn.bind -> C_syn.t ->
  (C_check.info, error) result
val text : error -> string