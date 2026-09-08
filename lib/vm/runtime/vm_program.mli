(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type scope_error =
  | Scope_start of int
  | Scope_overflow of int
  | Scope_duplicate of int
  | Scope_missing of int

val fix : Contract_vm.instr array -> Contract_vm.instr array
val scope :
  first:int ->
  Contract_vm.instr array ->
  (Contract_vm.instr array * int, scope_error) result
val scope_text : scope_error -> string
val entry : Contract_vm.instr array -> int option