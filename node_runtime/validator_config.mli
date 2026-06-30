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

type persistent_update_deps = {
  read_pending : unit -> string option Lwt.t;
  read_marker : string -> string option Lwt.t;
  log_invalid : string -> unit;
  log_waiting :
    activate_epoch:int64 ->
    missing:string list ->
    fingerprint:string ->
    unit;
}

type persistent_update_node_deps = {
  read_pending : unit -> string option Lwt.t;
  read_marker : string -> string option Lwt.t;
  warn : string -> unit;
  current_height : unit -> int64;
}

type self_membership =
  | Self_active
  | Self_scheduled
  | Self_observer of string
  | Self_refused of string

type quorum_admission =
  | Quorum_ok
  | Quorum_refused of string list
  | Quorum_unsafe_allowed of string

type startup_event =
  | Startup_info of string
  | Startup_warn of string
  | Startup_refuse of string list

type startup_event_deps = {
  info : string -> unit;
  warn : string -> unit;
  refuse : string list -> unit;
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

val pending_entries_of_raw :
  string option ->
  string list

val readiness_missing :
  runtime:Octra_core.Validator_ready_policy.runtime ->
  requirements:Octra_core.Validator_ready_policy.requirements ->
  update:Octra_core.Validator_set_update.t ->
  (Octra_core.Validator_set_update.validator_entry * string option) list ->
  string list

val load_persistent_update :
  persistent_update_deps ->
  runtime:Octra_core.Validator_ready_policy.runtime ->
  requirements:Octra_core.Validator_ready_policy.requirements ->
  current_height:int64 ->
  Octra_consensus.C_driver.scheduled_validator_set_config option Lwt.t

val load_node_persistent_update :
  persistent_update_node_deps ->
  runtime:Octra_core.Validator_ready_policy.runtime ->
  requirements:Octra_core.Validator_ready_policy.requirements ->
  Octra_consensus.C_driver.scheduled_validator_set_config option Lwt.t

val fingerprint_of_validator_set :
  Octra_consensus.C_types.validator_set ->
  string

val light_scheduled_of_driver :
  Octra_consensus.C_driver.scheduled_validator_set_config option ->
  Octra_consensus.C_config.scheduled option

val transition_message :
  current_height:int64 ->
  t ->
  string option

val self_membership :
  address:string ->
  voting:bool ->
  role_label:string ->
  t ->
  self_membership

val scheduled_self_warning :
  address:string ->
  voting:bool ->
  t ->
  string option

val quorum_admission :
  allow_unsafe:bool ->
  t ->
  quorum_admission

val startup_admission :
  address:string ->
  voting:bool ->
  role_label:string ->
  allow_unsafe_quorum:bool ->
  next_activation_epoch:int64 option ->
  t ->
  startup_event list

val node_startup_admission :
  getenv:(string -> string option) ->
  activation_epoch:(unit -> int64 option) ->
  address:string ->
  voting:bool ->
  role_label:string ->
  t ->
  startup_event list

val emit_startup_events :
  startup_event_deps ->
  startup_event list ->
  unit

val build :
  chain_id:string ->
  consensus_mode:bool ->
  current_height:int64 ->
  current_entries:string list ->
  next_entries:string list ->
  chain_pending_entries:string list ->
  next_activation_epoch:int64 option ->
  t