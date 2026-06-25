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


module C_driver = Octra_consensus.C_driver

type majority = {
  root : string;
  count : int;
}

val required_attesters :
  active_f:int ->
  validator_count:int ->
  configured:int ->
  int

val peer_root_majority :
  C_driver.epoch_root_response_record list ->
  majority option