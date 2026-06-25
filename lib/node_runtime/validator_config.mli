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


type transition =
  | No_transition
  | Already_active of {
      activate_epoch : int64;
    }
  | Scheduled of {
      activate_epoch : int64;
      n : int;
      quorum : int;
      fingerprint : string;
    }

type t = {
  allowed_pubkeys : string list;
  current_validator_list : Octra_consensus.C_types.validator_info list;
  next_validator_list : Octra_consensus.C_types.validator_info list;
  active_validator_list : Octra_consensus.C_types.validator_info list;
  active_vs : Octra_consensus.C_types.validator_set;
  scheduled_driver_config : Octra_consensus.C_driver.scheduled_validator_set_config option;
  light_scheduled_validator_set : Octra_consensus.C_config.scheduled option;
  consensus_config_hash : string;
  transition : transition;
}

val validator_info_of_entry :
  string ->
  Octra_consensus.C_types.validator_info option

val validator_list_of_entries :
  string list ->
  Octra_consensus.C_types.validator_info list

val driver_config_of_update :
  Octra_core.Validator_set_update.t ->
  Octra_consensus.C_driver.scheduled_validator_set_config option

val readiness_missing :
  runtime:Octra_core.Validator_ready_policy.runtime ->
  requirements:Octra_core.Validator_ready_policy.requirements ->
  update:Octra_core.Validator_set_update.t ->
  (Octra_core.Validator_set_update.validator_entry * string option) list ->
  string list

val fingerprint_of_validator_set :
  Octra_consensus.C_types.validator_set ->
  string

val light_scheduled_of_driver :
  Octra_consensus.C_driver.scheduled_validator_set_config option ->
  Octra_consensus.C_config.scheduled option

val build :
  chain_id:string ->
  consensus_mode:bool ->
  current_height:int64 ->
  current_entries:string list ->
  next_entries:string list ->
  chain_pending_entries:string list ->
  next_activation_epoch:int64 option ->
  t