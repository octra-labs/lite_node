(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

type error =
  | Evidence of Aml_evidence.error
  | Scope of Vm_program.scope_error
  | Entry of int
  | Label_start of int
  | Label_space of int
  | Input of C_type.t
  | Output of C_type.t
  | Tail
  | Verify
  | Bits
  | Count of int
  | Form
  | Repeated of int
  | Absent of int
  | Differs of int
  | Overlap of int * int
  | Image

val make : entry:int -> first:int -> Aml_evidence.t -> (t, error) result
val code : t -> Contract_vm.instr array
val next : t -> int
val body : t -> int
val size : t -> int
val encode : image:string -> t list -> (string, error) result
val decode : string -> (t list, error) result
val verify :
  image:string -> Contract_vm.instr array -> string -> (unit, error) result
val text : error -> string