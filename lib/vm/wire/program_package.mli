(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type source = {
  path : string;
  body : string;
}

type compiled = {
  package : string;
  envelope : string;
  result : Oct_compile.compile_result;
}

type admitted = {
  envelope : string;
  program : Admission.t;
}

type error

val error_message : error -> string

val compiler_profile_id : string
val standard_id : string

val compile_for :
  point_ops:bool ->
  main:string ->
  sources:source list ->
  (compiled, error) result

val compile :
  main:string ->
  sources:source list ->
  (compiled, error) result

val validate_base64 :
  string ->
  (unit, error) result

val admit_base64 :
  ?point_ops:bool ->
  string ->
  (admitted, error) result