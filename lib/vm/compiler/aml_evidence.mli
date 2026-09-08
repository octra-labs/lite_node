(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

type error =
  | Effect of Aml_effect.error
  | Lower of C_octb.error
  | Layout_count of int * int
  | Registry
  | Form

val make : Aml_effect.sealed -> (t, error) result
val enc : t -> C_bin.bits option
val dec : Aml_effect.t -> C_bin.bits -> (t, error) result
val dec_any : C_bin.bits -> (t, error) result
val blocks : t -> Aml_effect.block list
val program : t -> C_low.prog
val output : t -> C_type.t
val runtime : t -> (Contract_vm.instr array, error) result
val text : error -> string