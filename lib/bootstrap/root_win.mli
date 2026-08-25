(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val width : int
val name : string

val encode :
  Octra_consensus.C_types.finalize list ->
  string

val decode :
  string ->
  (Octra_consensus.C_types.finalize list, string) result

val bind :
  current:Octra_consensus.C_types.finalize ->
  validator_set:Octra_consensus.C_types.validator_set ->
  Octra_consensus.C_types.finalize ->
  (Octra_consensus.C_types.finalize, string) result

val verify :
  anchor:Octra_consensus.C_types.finalize ->
  Octra_consensus.C_types.finalize list ->
  ((int * string) list, string) result

val select :
  stored:string option ->
  signed:string option ->
  (string option, string) result

val read :
  string ->
  (Octra_consensus.C_types.finalize list, string) result