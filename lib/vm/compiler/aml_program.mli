(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = private {
  octb : string;
  code : Contract_vm.instr array;
}

val compile : string -> (t, string) result