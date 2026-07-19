(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


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