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
  receipts_json : string list;
  preverify : Octra_core.Preverify_commit.t;
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

type tx_hash_admission =
  | Tx_hash_ok of string list
  | Tx_hash_mismatch

type proposal_envelope = {
  header : Octra_consensus.C_types.epoch_header;
  proposal_id : string;
  txid_hi : int64;
  tx_hashes : string list;
  txs : Transaction.t list;
  receipts_json : string list;
  frozen_bundle : Consensus_bundle_cache.frozen;
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

val within_limits : limits:limits -> Transaction.t list -> bool

val cap : limits:limits -> Transaction.t list -> capped

val verify_bundle :
  verification_deps ->
  limits:limits ->
  header:Octra_consensus.C_types.epoch_header ->
  expected_tx_count:int ->
  Transaction.t list ->
  string list ->
  (verified_bundle, reject_reason) result

val log_reject :
  epoch_id:int64 ->
  reject_reason ->
  unit

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

val raw32_to_hex : string -> string

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
  ts:float ->
  proposal_envelope

val precommit_sync_plan :
  proposal_id:string ->
  current_tx_hashes:string list ->
  cached_bundle:Consensus_bundle_cache.encoded option ->
  precommit_sync_plan

val admission_plan :
  epoch_id:int64 ->
  current_epoch:int ->
  state_attested:bool ->
  quarantine_active:bool ->
  quarantine_reason:string ->
  admission_plan