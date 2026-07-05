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

let error_message hit =
  Printf.sprintf "consensus unsafe opcode %s at pc %d" hit.opcode hit.pc