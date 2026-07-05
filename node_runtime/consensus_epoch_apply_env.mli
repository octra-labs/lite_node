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

val validator_pubkeys :
  driver:Octra_consensus.C_driver.t option ->
  fallback:(unit -> (string * string) list) ->
  (string * string) list

val standard :
  epoch_id:int ->
  proposer_addr:string ->
  validator_pubkeys:(string * string) list ->
  prev_state_root:string ->
  ready_state_root_at:(int -> string option Lwt.t) ->
  ready_max_lag:int ->
  Epoch_exec.env