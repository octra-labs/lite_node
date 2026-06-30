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


module Epoch_exec = Octra_core.Epoch_exec
module Epochlog = Octra_core.Epochlog

type role =
  | Proposer
  | Validator
  | Proposer_validator

val role_label : role -> string
val role_of : proposer_addr:string -> active_validators:string list -> string -> role
val amount_of : role:role -> plan:Epoch_exec.reward_plan -> Z.t
val recipients :
  proposer_addr:string ->
  active_validators:string list ->
  plan:Epoch_exec.reward_plan ->
  Epochlog.reward_recipient list