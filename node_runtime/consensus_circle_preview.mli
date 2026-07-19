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


val process_tx :
  backend:Octra_core.Epoch_exec.backend ->
  env:Octra_core.Epoch_exec.env ->
  program_trust:Octra_vm.Program_trust.t ->
  Octra_core.Transaction.t ->
  (Z.t, string * string) result Lwt.t