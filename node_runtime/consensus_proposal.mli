(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Transaction = Octra_core.Transaction

type limits = {
  max_txs : int;
  max_bytes : int;
  max_ou : Z.t;
}

type totals = {
  count : int;
  bytes : int;
  ou : Z.t;
}

type capped = {
  txs : Transaction.t list;
  skipped : int;
  totals : totals;
}

type verified_bundle = {
  txs : Transaction.t list;
  candidates : Transaction.t list;
  receipts_json : string list;
  rejections : Octra_core.Tx_outcome.rejection list;
  preverify : Octra_core.Preverify_commit.t;
}

type local_preverify_bundle = {
  batch : Octra_core.Preverify_worker.batch;
  ready_txs : Transaction.t list;
  receipts_json : string list;
  skipped_count : int;
  skipped_sample : string;
}

type build_preverify = {
  batch : Octra_core.Preverify_worker.batch;
  heavy_count : int;
  capped : capped;
  txs : Transaction.t list;
  tx_hashes : string list;
  receipts : Octra_core.Preverify_receipt.t list;
  skipped_count : int;
  skipped_sample : string;
}

type proposal_admission_result =
  | Proposal_admit
  | Proposal_defer

type layera_diag_context = {
  validator_addrs : string list;
  validators_sha : string;
  meta_current : string;
  meta_last : string;
  meta_supply : string;
  meta_emission : string;
}

type preview_status =
  | Preview_ok of {
      post_state_root : string;
    }
  | Preview_error of string

type preview_decision =
  | Preview_accept of {
      computed_root : string;
      preview_eic_root : string;
    }
  | Preview_root_mismatch of {
      expected_root : string;
      computed_root : string option;
      preview_eic_root : string;
    }
  | Preview_error_reject of string

type prev_root_decision =
  | Prev_root_match
  | Prev_root_mismatch of {
      waited_steps : int;
      streak_after : int;
      quarantine_reason : string option;
    }

type prev_root_wait_deps = {
  read_root : unit -> string Lwt.t;
  replay_stashed : unit -> unit Lwt.t;
  sleep : float -> unit Lwt.t;
}

type prev_root_sample = {
  initial_root : string;
  current_root : string;
  tries_left : int;
}

type build_preview_status =
  | Build_preview_ok of {
      post_state_root : string;
      confirmed : Transaction.t list;
      rejected : Transaction.t list;
    }
  | Build_preview_error of string

type build_preview_plan = {
  final_txs : Transaction.t list;
  final_hashes : string list;
  rejected_hashes : string list;
  proposed_state_root : string;
  preview_consensus_root : string option;
  preview_error : string option;
}

type build_preview_output = {
  plan : build_preview_plan;
  receipts_json : string list;
  rejected_count : int;
}

type proposal_roots = {
  prev_ledger_root : string;
  prev_state_root : string;
}

type tx_hash_admission =
  | Tx_hash_ok of string list
  | Tx_hash_mismatch

type proposal_envelope = {
  header : Octra_consensus.C_types.epoch_header;
  parent_commit : Octra_consensus.C_types.parent_commit option;
  proposal_id : string;
  txid_hi : int64;
  tx_hashes : string list;
  txs : Transaction.t list;
  receipts_json : string list;
  frozen_bundle : Consensus_bundle_cache.frozen;
}

type frozen_proposal = {
  header : Octra_consensus.C_types.epoch_header;
  tx_hashes : string list;
  txs : Transaction.t list;
  receipts_json : string list;
  proposal_id : string;
  proposal_id_short : string;
}

type build_preview_request = {
  epoch_id : int64;
  epoch_ts : float;
  proposal_id : string;
  expected_prev_root : string;
  prev_state_root : string;
  parent_commit : Octra_consensus.C_types.parent_commit option;
  proposer : string;
  validator_pubkeys : (string * string) list;
  preverify : Octra_core.Preverify_commit.t;
  txs : Transaction.t list;
}

type make_proposal_deps = {
  start_height : int64 -> unit Lwt.t;
  current_epoch : unit -> int;
  state_attested : unit -> bool;
  quarantine_active : unit -> bool;
  quarantine_reason : unit -> string;
  read_prev_ledger_root : unit -> string option Lwt.t;
  cached_head : unit -> Octra_core.Head_manifest.t option;
  current_round : unit -> int;
  parent_commit :
    epoch_id:int64 ->
    (Octra_consensus.C_types.parent_commit option, string) result;
  frozen_bundle : string -> Consensus_bundle_cache.frozen option;
  store_bundle :
    proposal_id:string ->
    tx_hashes:string list ->
    txs:Transaction.t list ->
    receipts_json:string list ->
    unit;
  staging_txs : unit -> Transaction.t list;
  admits_tx : Transaction.t -> bool;
  build_preverify_once :
    state_root:string ->
    tx_hashes:string list ->
    Transaction.t list ->
    Octra_core.Preverify_worker.batch Lwt.t;
  staging_total : unit -> int;
  proposer : unit -> string;
  validator_pubkeys : int64 -> (string * string) list;
  preview :
    build_preview_request ->
    (Octra_core.Epoch_exec.exec_result, string) result Lwt.t;
  prev_eic_root : unit -> string;
  next_txid : unit -> int64;
  set_proposal : Transaction.t list -> string list -> unit;
  head_txid_hi : unit -> int64 option;
  freeze : string -> Consensus_bundle_cache.frozen -> unit;
  now : unit -> float;
  previous_epoch_ts : int64 -> float option;
}

type verify_proposal_deps = {
  now : unit -> float;
  previous_epoch_ts : int64 -> float option;
  quarantine_active : unit -> bool;
  quarantine_reason : unit -> string;
  mark_quarantine : string -> unit;
  prev_root_streak : unit -> int;
  set_prev_root_streak : int -> unit;
  state_root_streak : unit -> int;
  set_state_root_streak : int -> unit;
  limits : limits;
  layera_diag_live : unit -> bool;
  layera_env_diag_context : unit -> layera_diag_context;
  layera_diag_context : validator_addrs:string list -> layera_diag_context;
  wait_prev_root : prev_root_wait_deps;
  max_prev_root_wait_tries : int;
  prev_root_wait_delay_seconds : float;
  quarantine_mismatch_threshold : int;
  staging_txs : unit -> Transaction.t list;
  cached_bundle :
    proposal_id:string ->
    Consensus_bundle_fetch.proposal_bundle option;
  validate_preverify_once :
    state_root:string ->
    tx_hashes:string list ->
    Transaction.t list ->
    Octra_core.Preverify_worker.batch Lwt.t;
  driver_available : unit -> bool;
  validate_bundle :
    header:Octra_consensus.C_types.epoch_header ->
    expected_hashes:string list ->
    Octra_consensus.C_driver.bundle_response_record ->
    Consensus_bundle_validation.accepted option;
  query_bundle :
    epoch_id:int64 ->
    proposal_id:string ->
    validate:(Octra_consensus.C_driver.bundle_response_record -> bool) ->
    Octra_consensus.C_driver.bundle_response_record option Lwt.t;
  store_bundle :
    proposal_id:string ->
    tx_hashes:string list ->
    txs:Transaction.t list ->
    receipts_json:string list ->
    unit;
  public_key_for_tx : Transaction.t -> string option;
  verify_address_pubkey : addr:string -> pubkey:string -> bool;
  verify_tx_signature : Transaction.t -> pubkey:string -> bool;
  validator_pubkeys : int64 -> (string * string) list;
  read_local_ledger_root : unit -> string Lwt.t;
  preview :
    build_preview_request ->
    (Octra_core.Epoch_exec.exec_result, string) result Lwt.t;
  prev_eic_root : unit -> string;
  next_txid : unit -> int64;
  root_to_raw32 : string -> string;
  set_proposal : Transaction.t list -> string list -> unit;
  verify_parent_commit :
    epoch_id:int64 ->
    Octra_consensus.C_types.parent_commit option ->
    (unit, string) result;
}

type before_precommit_deps = {
  chain_id : string;
  validator_set : unit -> Octra_consensus.C_types.validator_set;
  current_tx_hashes : unit -> string list;
  cached_bundle : string -> Consensus_bundle_cache.encoded option;
  sync_bundle :
    tx_hashes:string list ->
    txs:Transaction.t list ->
    receipts_json:string list ->
    unit;
  mark_unsynced : unit -> unit;
  write_pending : Octra_core.Wal.pending_commit -> unit;
  now : unit -> float;
}

type precommit_sync_plan =
  | Precommit_sync_current
  | Precommit_sync_missing of {
      pid_short : string;
    }
  | Precommit_sync_decoded of {
      pid_short : string;
      tx_hashes : string list;
      txs : Transaction.t list;
      receipts_json : string list;
    }
  | Precommit_sync_decode_failed of {
      pid_short : string;
      error : string;
    }

type reject_reason =
  | Missing_txs of {
      have : int;
      need : int;
    }
  | Disabled_operation of {
      hash : string;
      op_type : string;
    }
  | Underpriced_transaction of {
      hash : string;
      op_type : string;
      provided : Z.t;
      required : Z.t;
    }
  | Receipt_root_mismatch
  | Receipt_decode_failed of string
  | Preverify_gate_failed of string
  | Bundle_limit of {
      totals : totals;
      limits : limits;
    }
  | Invalid_tx_signature of {
      hash : string;
      from_addr : string;
    }

type verification_deps = {
  public_key_for_tx : Transaction.t -> string option;
  verify_address_pubkey : addr:string -> pubkey:string -> bool;
  verify_tx_signature : Transaction.t -> pubkey:string -> bool;
}

type validator_pubkeys = (string * string) list

type admission_plan =
  | Proceed
  | Defer_state_not_attested
  | Defer_quarantine of {
      reason : string;
    }
  | Realign_stale_height of {
      target_epoch : int64;
    }
  | Defer_apply_gap

val limits : max_txs:int -> max_bytes:int -> max_ou:Z.t -> limits

val wire_size : Transaction.t -> int

val totals : Transaction.t list -> totals

val select_staged :
  hash_tx:(Transaction.t -> string) ->
  hashes:string list ->
  Transaction.t list ->
  Transaction.t list

val within_limits : limits:limits -> Transaction.t list -> bool

val cap : limits:limits -> Transaction.t list -> capped

val local_preverify_bundle :
  run_many:(Transaction.t list -> Octra_core.Preverify_worker.batch Lwt.t) ->
  tx_hashes:string list ->
  Transaction.t list ->
  local_preverify_bundle Lwt.t

val check_local_bundle :
  expected_hashes:string list ->
  Consensus_bundle_fetch.proposal_bundle ->
  local_preverify_bundle ->
  (Consensus_bundle_fetch.proposal_bundle, string) result

val build_preverify :
  run_many:(Transaction.t list -> Octra_core.Preverify_worker.batch Lwt.t) ->
  limits:limits ->
  Transaction.t list ->
  build_preverify Lwt.t

val handle_proposal_admission :
  start_height:(int64 -> unit Lwt.t) ->
  epoch_id:int64 ->
  current_epoch:int ->
  state_attested:bool ->
  quarantine_active:bool ->
  quarantine_reason:string ->
  proposal_admission_result Lwt.t

val verify_bundle :
  verification_deps ->
  limits:limits ->
  header:Octra_consensus.C_types.epoch_header ->
  expected_tx_count:int ->
  Transaction.t list ->
  string list ->
  (verified_bundle, reject_reason) result

val validator_pubkeys :
  driver:Octra_consensus.C_driver.t option ->
  fallback:(unit -> validator_pubkeys) ->
  validator_pubkeys

val epoch_exec_env :
  chain_id:string ->
  epoch_id:int64 ->
  epoch_ts:float ->
  proposer:string ->
  validator_pubkeys:validator_pubkeys ->
  prev_state_root:string ->
  ready_state_root_at:(int -> string option Lwt.t) ->
  ready_max_lag:int ->
  Octra_core.Epoch_exec.env

val log_reject :
  epoch_id:int64 ->
  reject_reason ->
  unit

val wait_for_prev_root :
  prev_root_wait_deps ->
  target_root:string ->
  max_tries:int ->
  delay_seconds:float ->
  prev_root_sample Lwt.t

val preview_decision :
  root_to_raw32:(string -> string) ->
  epoch_id:int64 ->
  tx_hashes:string list ->
  tx_count:int ->
  start_txid:int64 ->
  prev_eic_root:string ->
  local_ledger_root:string ->
  proposed_state_root:string ->
  preview:preview_status ->
  preview_decision

val preview_status_of_result :
  (Octra_core.Epoch_exec.exec_result, string) result ->
  preview_status

val prev_eic_root_from_head :
  Octra_core.Head_manifest.t option ->
  string

val proposal_roots :
  root_to_raw32:(string -> string) ->
  ledger_head:string option ->
  cached_head:Octra_core.Head_manifest.t option ->
  proposal_roots

val prev_root_decision :
  epoch_id:int64 ->
  target_root:string ->
  current_root:string ->
  max_wait_tries:int ->
  tries_left:int ->
  current_streak:int ->
  quarantine_threshold:int ->
  prev_root_decision

val build_preview_plan :
  root_to_raw32:(string -> string) ->
  epoch_id:int64 ->
  start_txid:int64 ->
  prev_eic_root:string ->
  prev_ledger_root:string ->
  fallback_ledger_root:string ->
  input_txs:Transaction.t list ->
  preview:build_preview_status ->
  build_preview_plan

val build_preview_status_of_result :
  (Octra_core.Epoch_exec.exec_result, string) result ->
  build_preview_status

val build_preview_output :
  root_to_raw32:(string -> string) ->
  epoch_id:int64 ->
  start_txid:int64 ->
  prev_eic_root:string ->
  prev_ledger_root:string ->
  fallback_ledger_root:string ->
  input_txs:Transaction.t list ->
  batch:Octra_core.Preverify_worker.batch ->
  rejections:Octra_core.Tx_outcome.rejection list ->
  preview_result:(Octra_core.Epoch_exec.exec_result, string) result ->
  build_preview_output

val rejection_tuples :
  Octra_core.Epoch_exec.tx_reject list ->
  (Transaction.t * string * string) list

val verify_preview_partition :
  candidates:Transaction.t list ->
  confirmed:Transaction.t list ->
  rejections:Octra_core.Tx_outcome.rejection list ->
  (Octra_core.Epoch_exec.exec_result, string) result ->
  (unit, string) result

val raw32_to_hex : string -> string

val raw_hex8 : string -> string

val layera_validator_addrs :
  env:string option ->
  fallback:string ->
  string list

val layera_diag_context :
  validator_addrs:string list ->
  hash_validators:(string -> string) ->
  meta:(string -> string) ->
  layera_diag_context

val layera_env_diag_context :
  env:string option ->
  fallback:string ->
  hash_validators:(string -> string) ->
  meta:(string -> string) ->
  layera_diag_context

val log_layera_prev_mismatch :
  epoch_id:int64 ->
  round:int ->
  leader:string ->
  target_root:string ->
  our_root:string ->
  initial_root:string ->
  waited_steps:int ->
  layera_diag_context ->
  unit

val log_layera_preview_mismatch :
  epoch_id:int64 ->
  round:int ->
  proposer:string ->
  tx_count:int ->
  expected_root:string ->
  computed_root:string option ->
  tx_list_hash:string ->
  tx_hashes:string list ->
  layera_diag_context ->
  unit

val log_layera_preview_error :
  epoch_id:int64 ->
  round:int ->
  proposer:string ->
  error:string ->
  layera_diag_context ->
  unit

val log_build_preverify :
  epoch_id:int64 ->
  round:int ->
  limits:limits ->
  staging_total:int ->
  raw_count:int ->
  admitted_count:int ->
  prev_root:string ->
  build_preverify ->
  unit

val tx_list_hash : string list -> string

val tx_hash_admission :
  expected_tx_list_hash:string ->
  tx_hashes:string list ->
  tx_hash_admission

val build_header :
  chain_id:string ->
  epoch_id:int64 ->
  prev_state_root:string ->
  final_hashes:string list ->
  receipts_json:string list ->
  proposed_state_root:string ->
  creator_addr:string ->
  next_txid:int64 ->
  head_txid_hi:int64 option ->
  parent_commit_hash:string ->
  ts:float ->
  Octra_consensus.C_types.epoch_header

val build_proposal_envelope :
  chain_id:string ->
  epoch_id:int64 ->
  prev_state_root:string ->
  final_hashes:string list ->
  final_txs:Transaction.t list ->
  receipts_json:string list ->
  proposed_state_root:string ->
  creator_addr:string ->
  next_txid:int64 ->
  head_txid_hi:int64 option ->
  parent_commit:Octra_consensus.C_types.parent_commit option ->
  ts:float ->
  proposal_envelope

val frozen_proposal :
  Consensus_bundle_cache.frozen ->
  frozen_proposal

val log_frozen_proposal :
  epoch_id:int64 ->
  round:int ->
  frozen_proposal ->
  unit

val verify_proposal :
  verify_proposal_deps ->
  chain_id:string ->
  Octra_consensus.C_types.propose ->
  bool Lwt.t

val make_proposal :
  make_proposal_deps ->
  chain_id:string ->
  root_to_raw32:(string -> string) ->
  limits:limits ->
  epoch_id:int64 ->
  Octra_consensus.C_driver.proposal_plan option Lwt.t

val precommit_sync_plan :
  proposal_id:string ->
  current_tx_hashes:string list ->
  cached_bundle:Consensus_bundle_cache.encoded option ->
  precommit_sync_plan

val pending_commit :
  epoch_id:int64 ->
  round:int ->
  proposal_id:string ->
  proposed_state_root:string ->
  txid_hi:int64 ->
  ts:float ->
  validator_addr:string ->
  proposal_wire:string ->
  vote_wire:string ->
  tx_hashes:string list ->
  txs_json:string list ->
  receipts_json:string list ->
  Octra_core.Wal.pending_commit

val handle_before_precommit :
  before_precommit_deps ->
  epoch_id:int64 ->
  round:int ->
  proposal_id:string ->
  proposed_state_root:string ->
  txid_hi:int64 ->
  validator_addr:string ->
  proposal_wire:string ->
  vote_wire:string ->
  bool

val admission_plan :
  epoch_id:int64 ->
  current_epoch:int ->
  state_attested:bool ->
  quarantine_active:bool ->
  quarantine_reason:string ->
  admission_plan