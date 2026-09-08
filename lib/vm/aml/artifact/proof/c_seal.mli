(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type bits = C_cert.bits

type error =
  | Source of C_parse.error
  | Cert of C_cert.error
  | Encode

type item = {
  core : bits;
  res : C_limit.t;
}

val make :
  C_rule.schedule -> epoch:Z.t -> string -> (bits, error) result
val decode : bits -> item option
val accept : C_rule.schedule -> epoch:Z.t -> string -> bits -> bool
val text : error -> string