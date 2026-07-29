(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type effect =
  | Memory_read
  | Memory_write
  | Storage_read
  | Storage_write
  | Call
  | Deploy
  | Transfer
  | Emit
  | Fhe
  | Journal

type t

val scan : Contract_vm.instr array -> t
val names : t -> string list