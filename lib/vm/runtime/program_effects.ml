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

type t = effect list

let of_instr = function
  | Contract_vm.MLOAD _ | Contract_vm.MLOADR _ -> Some Memory_read
  | Contract_vm.MSTORE _ | Contract_vm.MSTORER _ -> Some Memory_write
  | Contract_vm.SLOAD _ | Contract_vm.SLOADK _ -> Some Storage_read
  | Contract_vm.SSTORE _ | Contract_vm.SSTOREK _ | Contract_vm.SDEL _ | Contract_vm.SDELK _ -> Some Storage_write
  | Contract_vm.XCALL _ | Contract_vm.CALL_INT _ -> Some Call
  | Contract_vm.SPAWN _ | Contract_vm.SPAWN2 _ -> Some Deploy
  | Contract_vm.TRANSFER _ -> Some Transfer
  | Contract_vm.EMIT _ -> Some Emit
  | Contract_vm.FHE_LOAD_PK _
  | Contract_vm.FHE_ADD _
  | Contract_vm.FHE_SUB _
  | Contract_vm.FHE_MUL _
  | Contract_vm.FHE_SCALE _
  | Contract_vm.FHE_DIV_CONST _
  | Contract_vm.FHE_ADD_CONST _
  | Contract_vm.FHE_SUB_CONST _
  | Contract_vm.FHE_VERIFY_ZERO _
  | Contract_vm.FHE_VERIFY_RANGE _
  | Contract_vm.FHE_VERIFY_BOUND _
  | Contract_vm.GROTH16_VERIFY_BN254 _
  | Contract_vm.FHE_COMMIT _
  | Contract_vm.FHE_PEDERSEN _
  | Contract_vm.FHE_SER _
  | Contract_vm.FHE_DESER _
  | Contract_vm.FHE_SER_PK _
  | Contract_vm.FHE_DESER_PK _ -> Some Fhe
  | Contract_vm.CHECKPOINT
  | Contract_vm.ROLLBACK
  | Contract_vm.COMMIT -> Some Journal
  | _ -> None

let add effect effects =
  if List.mem effect effects then effects else effects @ [effect]

let scan code =
  Array.fold_left
    (fun effects instr ->
      match of_instr instr with
      | None -> effects
      | Some effect -> add effect effects)
    [] code

let names effects =
  List.map (function
    | Memory_read -> "memory_read"
    | Memory_write -> "memory_write"
    | Storage_read -> "storage_read"
    | Storage_write -> "storage_write"
    | Call -> "call"
    | Deploy -> "deploy"
    | Transfer -> "transfer"
    | Emit -> "emit"
    | Fhe -> "fhe"
    | Journal -> "journal") effects