(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type bits = C_cert.bits

type error =
  | Lower of C_low.error
  | Cert of C_cert.error

val issue :
  C_rule.schedule -> epoch:Z.t -> C_syn.bind list -> C_syn.t ->
  (bits, error) result
val verify :
  C_rule.schedule -> epoch:Z.t -> C_syn.bind list -> C_syn.t -> bits -> bool
val text : error -> string