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


module VM = Contract_vm

type host_float_hit = {
  pc : int;
  opcode : string;
}

let host_float_opcode = function
  | VM.EXP_LUT _ -> Some "EXP_LUT"
  | VM.SOFTMAX_INPLACE _ -> Some "SOFTMAX_INPLACE"
  | VM.LAYERNORM_INPLACE _ -> Some "LAYERNORM_INPLACE"
  | VM.RMSNORM_INPLACE _ -> Some "RMSNORM_INPLACE"
  | VM.SILU_INPLACE _ -> Some "SILU_INPLACE"
  | VM.ROPE_APPLY _ -> Some "ROPE_APPLY"
  | VM.MATMUL_FP _ -> Some "MATMUL_FP"
  | VM.RMSNORM_FP _ -> Some "RMSNORM_FP"
  | VM.SILU_FP _ -> Some "SILU_FP"
  | VM.ELEMWISE_MUL_FP _ -> Some "ELEMWISE_MUL_FP"
  | VM.RESIDUAL_ADD_FP _ -> Some "RESIDUAL_ADD_FP"
  | VM.ROPE_APPLY_FP _ -> Some "ROPE_APPLY_FP"
  | VM.LOAD_INT8_FP _ -> Some "LOAD_INT8_FP"
  | VM.VECDOT_FP _ -> Some "VECDOT_FP"
  | VM.ARGMAX_FP _ -> Some "ARGMAX_FP"
  | VM.ATTENTION_KV_FP _ -> Some "ATTENTION_KV_FP"
  | VM.APPEND_VEC_FP _ -> Some "APPEND_VEC_FP"
  | _ -> None

let program_only_opcode = function
  | VM.EXP_Q16 _ -> Some "EXP_Q16"
  | VM.SOFTMAX_Q16_INPLACE _ -> Some "SOFTMAX_Q16_INPLACE"
  | VM.LAYERNORM_Q16_INPLACE _ -> Some "LAYERNORM_Q16_INPLACE"
  | VM.RMSNORM_Q16_INPLACE _ -> Some "RMSNORM_Q16_INPLACE"
  | VM.SILU_Q16_INPLACE _ -> Some "SILU_Q16_INPLACE"
  | VM.ROPE_APPLY_Q16 _ -> Some "ROPE_APPLY_Q16"
  | VM.ATTENTION_KV_Q16 _ -> Some "ATTENTION_KV_Q16"
  | VM.VECDOT_Q16 _ -> Some "VECDOT_Q16"
  | VM.ELEMWISE_MUL_Q16 _ -> Some "ELEMWISE_MUL_Q16"
  | VM.RESIDUAL_ADD_Q16 _ -> Some "RESIDUAL_ADD_Q16"
  | VM.LOAD_INT8_Q16 _ -> Some "LOAD_INT8_Q16"
  | VM.APPEND_VEC_Q16 _ -> Some "APPEND_VEC_Q16"
  | VM.ARGMAX_Q16 _ -> Some "ARGMAX_Q16"
  | _ -> None

let uses_host_float op =
  Option.is_some (host_float_opcode op)

let first_host_float code =
  let hit = ref None in
  Array.iteri
    (fun pc op ->
      match !hit, host_float_opcode op with
      | None, Some opcode -> hit := Some { pc; opcode }
      | _ -> ())
    code;
  !hit

let consensus_safe code =
  Option.is_none (first_host_float code)

let require_consensus_safe code =
  match first_host_float code with
  | None -> Ok ()
  | Some hit -> Error hit

let first_program_only code =
  let hit = ref None in
  Array.iteri
    (fun pc op ->
      match !hit, program_only_opcode op with
      | None, Some opcode -> hit := Some { pc; opcode }
      | _ -> ())
    code;
  !hit

let require_legacy_safe code =
  match first_program_only code with
  | Some hit -> Error hit
  | None -> require_consensus_safe code

type legacy_error =
  | Program_only of host_float_hit
  | Consensus_unsafe of host_float_hit

let legacy_error code =
  match first_program_only code with
  | Some hit -> Some (Program_only hit)
  | None -> Option.map (fun hit -> Consensus_unsafe hit) (first_host_float code)

let require_program_safe = require_consensus_safe

let error_message hit =
  Printf.sprintf "consensus unsafe opcode %s at pc %d" hit.opcode hit.pc

let program_only_error_message hit =
  Printf.sprintf "Program-only opcode %s at pc %d" hit.opcode hit.pc