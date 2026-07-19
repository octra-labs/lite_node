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


val complete :
  Contract_vm.instr array ->
  Program_type_flow.facts ->
  (Program_type_flow.facts, string) result

val effects :
  Contract_vm.instr array ->
  Program_type_flow.facts ->
  (string list, string) result

val verify :
  Contract_vm.instr array ->
  Program_type_flow.facts ->
  string list ->
  (unit, string) result