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

val of_code : Contract_vm.instr array -> (t, error) result
val of_program : ?facts:Program_type_flow.facts -> Contract_vm.instr array -> (t, error) result
val decode : string -> (t, error) result
val decode_deploy : ?trusted:Program_attestation.key list -> string -> (t, error) result
val decode_program : ?trusted:Program_attestation.key list -> string -> (t, error) result
val decode_program_source : string -> (t, error) result
val code : t -> Contract_vm.instr array
val effects : t -> Program_effects.t
val profile : t -> profile
val error_message : error -> string