(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val complete :
  Contract_vm.instr array ->
  Program_type_flow.facts ->
  (Program_type_flow.facts, string) result

val effects :
  Contract_vm.instr array ->
  Program_type_flow.facts ->
  (string list, string) result

val verify :
  Contract_vm.instr array ->
  Program_type_flow.facts ->
  string list ->
  (unit, string) result