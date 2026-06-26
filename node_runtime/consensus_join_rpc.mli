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
  creator_addr : string;
  commit_round : int;
}

type range =
  | Retry
  | Records of record list

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
  proposal_id : string;
}

type ready_marker = {
  path : string;
  tmp_path : string;
  payload : Yojson.Safe.t;
  ready_epoch : int64;
  state_root : string;
  records_verified : int;
}

val normalize_base : string -> string

val root_hex64 : string -> string

val head_url : string -> string

val range_url :
  string ->
  from_epoch:int64 ->
  max_epochs:int ->
  string

val parse_head : Yojson.Safe.t -> head

val parse_range : from_epoch:int64 -> Yojson.Safe.t -> range

val sync_plan :
  local_next:int64 ->
  local_root:string ->
  head ->
  sync_plan

val prepare_record : cursor:cursor -> record -> prepared

val finality_entry :
  ts:float ->
  prepared ->
  Octra_consensus.Finality_log.entry

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