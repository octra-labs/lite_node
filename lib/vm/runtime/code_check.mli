(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

type profile =
  | Legacy
  | Program of Program_type_flow.facts

type error =
  | Decode_error of string
  | Verify_error of string
  | Unsafe_error of string

val of_code : ?point_ops:bool -> Contract_vm.instr array -> (t, error) result
val of_program : ?point_ops:bool -> ?facts:Program_type_flow.facts -> Contract_vm.instr array -> (t, error) result
val decode : ?view:bool -> ?point_ops:bool -> string -> (t, error) result
val decode_deploy : ?view:bool -> ?trusted:Program_attestation.key list -> ?point_ops:bool -> string -> (t, error) result
val decode_program : ?view:bool -> ?trusted:Program_attestation.key list -> ?point_ops:bool -> string -> (t, error) result
val decode_program_source : ?view:bool -> ?point_ops:bool -> string -> (t, error) result
val code : t -> Contract_vm.instr array
val effects : t -> Program_effects.t
val profile : t -> profile
val compiler_version : t -> string option
val scalar_cost : string -> Z.t option
val check_standard : point_ops:bool -> t -> (unit, error) result
val error_message : error -> string