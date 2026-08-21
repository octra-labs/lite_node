(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type apply =
  txs:Octra_core.Transaction.t list ->
  receipts_json:string list ->
  proposer_info:Octra_core.Epochlog.proposer_info option ->
  reward:Consensus_reward_attribution.t ->
  epoch_ts:float ->
  parent_commit:Octra_consensus.C_types.parent_commit option ->
  unit Lwt.t

exception Fetch_retry of string

type head = {
  epoch : int64;
  root : string;
}

type record = {
  epoch_id : int64;
  prev_state_root : string;
  state_root : string;
  tx_list_hash : string;
  tx_hashes : string list;
  txs_json : string list;
  receipts_json : string list;
  receipt_root : string;
  epoch_ts : float;
  creator_addr : string;
  commit_round : int;
  reward_source : Octra_consensus.C_types.reward_source;
  finality : Octra_consensus.C_codec.catchup_finality;
}

type range =
  | Retry
  | Missing
  | Records of record list

type outcome =
  | Synced
  | Leader_stale of {
      local_head : int64;
      leader_head : int64;
    }
  | Need_range of {
      head : int64;
      target : int64;
    }

type sync_plan =
  | Fetch_range of int64
  | Ready of {
      ready_epoch : int64;
      state_root : string;
    }
  | Local_ahead of {
      local_head : int64;
      leader_head : int64;
    }
  | Root_mismatch of {
      local_root : string;
      leader_root : string;
      epoch : int64;
    }

type cursor = {
  epoch : int64;
  prev_root : string;
  eic : string;
  txid : int64;
}

type prepared = {
  record : record;
  txs : Octra_core.Transaction.t list;
  expected_eic : string;
  next_cursor : cursor;
  epoch_int : int;
  proposer_info : Octra_core.Epochlog.proposer_info option;
  reward : Consensus_reward_attribution.t;
}

type ready_marker = {
  path : string;
  staged_path : string;
  payload : Yojson.Safe.t;
  ready_epoch : int64;
  state_root : string;
  records_verified : int;
}

type ready_marker_config = {
  data_dir : string;
  consensus_role : string;
  chain_id : string;
  validator : string;
  validator_pubkey : string;
  priv_b64 : string;
  generated_at : unit -> float;
}

type ready_marker_write_deps = {
  write_text : path:string -> contents:string -> unit;
  rename : src:string -> dst:string -> unit;
  log_written : ready_marker -> unit;
}

type apply_deps = {
  chain_id : string;
  expected_validator_set_hash : int64 -> (string, string) result;
  current_epoch : unit -> int;
  put_proposer : int -> Octra_core.Epochlog.proposer_info -> unit;
  put_root : int -> string -> unit;
  stage_finality : prepared -> unit;
  promote_finality : unit -> unit;
  apply : apply;
  root : unit -> string;
  eic : unit -> string option;
}

type run_deps = {
  fetch_head : string -> Yojson.Safe.t Lwt.t;
  fetch_range :
    string ->
    from_epoch:int64 ->
    max_epochs:int ->
    Yojson.Safe.t Lwt.t;
  local_next : unit -> int64;
  local_root : unit -> string;
  cursor : from_epoch:int64 -> cursor;
  apply_range : cursor:cursor -> record list -> (cursor * int) Lwt.t;
  write_ready :
    base:string ->
    ready_epoch:int64 ->
    state_root:string ->
    records_verified:int ->
    unit;
  sleep : float -> unit Lwt.t;
  log_start : base:string -> unit;
  log_applied : applied:int -> unit;
  log_retry : phase:string -> delay:float -> error:string -> unit;
}

type node_deps = {
  chain_id : string;
  expected_validator_set_hash : int64 -> (string, string) result;
  fetch_json : string -> Yojson.Safe.t Lwt.t;
  current_epoch : unit -> int;
  local_root : unit -> string;
  base_eic_root : unit -> string;
  next_txid : unit -> int64;
  put_proposer : int -> Octra_core.Epochlog.proposer_info -> unit;
  put_root : int -> string -> unit;
  stage_finality : prepared -> unit;
  promote_finality : unit -> unit;
  apply : apply;
  local_eic : unit -> string option;
  write_ready :
    base:string ->
    ready_epoch:int64 ->
    state_root:string ->
    records_verified:int ->
    unit;
  sleep : float -> unit Lwt.t;
  now : unit -> float;
}

type node_runtime_deps = {
  env : string -> string option;
  expected_validator_set_hash : int64 -> (string, string) result;
  fetch_json : string -> Yojson.Safe.t Lwt.t;
  current_epoch : unit -> int;
  head : unit -> Octra_core.Head_manifest.t option;
  next_txid : unit -> int64;
  put_proposer : int -> Octra_core.Epochlog.proposer_info -> unit;
  put_root_raw : int -> string -> unit;
  write_entry : Octra_consensus.Finality_log.entry -> unit;
  apply : apply;
  sleep : float -> unit Lwt.t;
  now : unit -> float;
  data_dir : string;
  consensus_role : string;
  chain_id : string;
  validator : string;
  validator_pubkey : string;
  priv_b64 : string;
  require_sync : Sync_need.t -> unit;
}

type node_runtime_wiring = {
  env : string -> string option;
  expected_validator_set_hash : int64 -> (string, string) result;
  fetch_json : string -> Yojson.Safe.t Lwt.t;
  current_epoch : unit -> int;
  head : unit -> Octra_core.Head_manifest.t option;
  next_txid : unit -> int64;
  finality : Consensus_finality_state.callbacks;
  write_entry : Octra_consensus.Finality_log.entry -> unit;
  apply : apply;
  sleep : float -> unit Lwt.t;
  now : unit -> float;
  data_dir : string;
  consensus_role : string;
  chain_id : string;
  validator : string;
  validator_pubkey : string;
  priv_b64 : string;
  require_sync : Sync_need.t -> unit;
}

val configured_base :
  string option ->
  string option

val first_source :
  string option ->
  string option

val configured_join :
  (string -> string option) ->
  string option

val normalize_base : string -> string

val root_hex64 : string -> string

val local_root_from_head :
  Octra_core.Head_manifest.t option ->
  string

val base_eic_root_from_head :
  Octra_core.Head_manifest.t option ->
  string

val local_eic_from_head :
  Octra_core.Head_manifest.t option ->
  string option

val head_url : string -> string

val range_url :
  string ->
  from_epoch:int64 ->
  max_epochs:int ->
  string

val http_get_json :
  ?timeout:float ->
  string ->
  Yojson.Safe.t Lwt.t

val retry_delay : int -> float

val parse_head : Yojson.Safe.t -> head

val parse_range : from_epoch:int64 -> Yojson.Safe.t -> range

val range_response :
  source:string ->
  from_epoch:int64 ->
  max_epochs:int ->
  range ->
  Octra_consensus.C_driver.catchup_range_response_record option

val http_range :
  ?fetch_json:(string -> Yojson.Safe.t Lwt.t) ->
  (string -> string option) ->
  from_epoch:int64 ->
  max_epochs:int ->
  Octra_consensus.C_driver.catchup_range_response_record option Lwt.t

val http_head :
  ?fetch_json:(string -> Yojson.Safe.t Lwt.t) ->
  (string -> string option) ->
  int64 option Lwt.t

val sync_plan :
  local_next:int64 ->
  local_root:string ->
  head ->
  sync_plan

val prepare_record :
  chain_id:string ->
  expected_validator_set_hash:string ->
  cursor:cursor ->
  record ->
  prepared

val finality_entry :
  chain_id:string ->
  prepared ->
  Octra_consensus.Finality_log.entry

val apply_records :
  apply_deps ->
  cursor:cursor ->
  record list ->
  (cursor * int) Lwt.t

val run_catchup :
  run_deps ->
  string ->
  outcome Lwt.t

val run_node_catchup :
  node_deps ->
  string ->
  outcome Lwt.t

val node_deps_of_runtime :
  node_runtime_deps ->
  node_deps

val node_runtime_deps :
  node_runtime_wiring ->
  node_runtime_deps

val run_configured_node_catchup :
  node_runtime_deps ->
  unit Lwt.t

val run_configured_node_wiring :
  node_runtime_wiring ->
  unit Lwt.t

val ready_marker :
  data_dir:string ->
  consensus_role:string ->
  leader_rpc:string ->
  chain_id:string ->
  validator:string ->
  validator_pubkey:string ->
  priv_b64:string ->
  ready_epoch:int64 ->
  state_root:string ->
  records_verified:int ->
  generated_at:float ->
  ready_marker

val ready_marker_payload_text :
  ready_marker ->
  string

val write_ready_marker_with :
  ready_marker_write_deps ->
  ready_marker ->
  unit

val write_ready_marker :
  ready_marker_config ->
  base:string ->
  ready_epoch:int64 ->
  state_root:string ->
  records_verified:int ->
  unit