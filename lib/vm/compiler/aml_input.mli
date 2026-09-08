(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type error =
  | Method_absent of string
  | Method_private of string
  | Arity of int * int
  | Value of int * string
  | Type of int * string

type core_value = private {
  vms : Contract_vm.v list;
  lits : C_emit.lit list;
  lit : C_emit.lit;
  value : C_eval.value;
}

val integer : string -> Z.t option
val tagged : string -> Contract_vm.v option
val detached : string -> Contract_vm.v option
val core : C_term.bind list -> string list -> (core_value list, error) result
val core_octb : C_type.t array -> string list -> (core_value list, error) result

val method_def :
  Oct_lang.contract ->
  string ->
  (Oct_lang.func_def, error) result

val parse :
  Oct_lang.contract ->
  string ->
  string list ->
  (Contract_vm.v list, error) result

val storage_kinds :
  Oct_lang.contract ->
  (string * Contract_vm.storage_kind) list

val error_text : error -> string