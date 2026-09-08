(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = private {
  name : string;
  declaration : Oct_lang.declaration;
  ast : Oct_lang.contract;
  code : Contract_vm.instr array;
  octb : string;
}

val compile_ast : Oct_lang.contract -> (t, string) result
val compile : string -> (t, string) result
val compile_multi : (string -> string option) -> string -> (t, string) result
val owns : string -> bool