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


type gc_config = {
  max_overhead : int;
  space_overhead : int;
  minor_heap_size : int;
}

type network_config = {
  api_port : int;
  p2p_port : int;
  consensus_port : int;
  consensus_peers : string list;
  chain_id : string;
}

type local_wallet_admission =
  | Wallet_admitted
  | Missing_voting_validator
  | Missing_non_voting_validator
  | Pubkey_mismatch_voting
  | Pubkey_mismatch_non_voting

val gc_config :
  env:(string -> string option) ->
  gc_config

val network_config :
  env:(string -> string option) ->
  network_config

val node_start_messages :
  version:string ->
  validator:string ->
  storage:string ->
  store_path:string ->
  epoch_duration:float ->
  network_config ->
  string list

val log_node_start :
  info:(string -> unit) ->
  version:string ->
  validator:string ->
  storage:string ->
  store_path:string ->
  epoch_duration:float ->
  network_config ->
  unit

val data_dir :
  env:(string -> string option) ->
  string

val ensure_data_dir :
  init_mode:bool ->
  data_dir:string ->
  exit_fatal:(unit -> unit) ->
  unit

val load_wallet :
  path:string ->
  ensure:(string -> 'a) ->
  address:('a -> string) ->
  exit_fatal:(unit -> unit) ->
  'a

val validator_pubkeys_boot :
  current:(string * string) list ->
  next:(string * string) list ->
  fallback:(string * string) list ->
  (string * string) list

val local_wallet_admission :
  validator_env_present:bool ->
  validators:(string * string) list ->
  wallet_addr:string ->
  wallet_pub:string ->
  voting_consensus_mode:bool ->
  consensus_mode:bool ->
  local_wallet_admission

val admit_local_wallet :
  validator_env_present:bool ->
  validators:(string * string) list ->
  wallet_addr:string ->
  wallet_pub:string ->
  voting_consensus_mode:bool ->
  consensus_mode:bool ->
  role_label:string ->
  exit_fatal:(unit -> unit) ->
  unit

val admit_wallet_validators :
  validator_env_present:bool ->
  current:(string * string) list ->
  next:(string * string) list ->
  fallback:(string * string) list ->
  wallet_addr:string ->
  wallet_pub:string ->
  voting_consensus_mode:bool ->
  consensus_mode:bool ->
  role_label:string ->
  exit_fatal:(unit -> unit) ->
  (string * string) list

val configure_process :
  exit_fatal:(unit -> unit) ->
  unit

val apply_gc_config :
  gc_config ->
  unit

val start_gc_reporter :
  prune:(unit -> unit) ->
  unit