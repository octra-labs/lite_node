(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = private {
  name : string;
  declaration : Oct_lang.declaration;
  ast : Oct_lang.contract;
  code : Contract_vm.instr array;
  octb : string;
}

val compile_ast : ?metered:bool -> ?loops:bool -> syntax:Oct_gen.syntax -> Oct_lang.contract -> (t, string) result
val compile : ?metered:bool -> ?loops:bool -> syntax:Oct_gen.syntax -> string -> (t, string) result
val compile_multi : ?metered:bool -> ?loops:bool -> program_only:bool -> syntax:Oct_gen.syntax -> (string -> string option) -> string -> (t, string) result
val owns : string -> bool
val check_loops : syntax:Oct_gen.syntax -> (string -> string option) -> string -> (unit, string) result