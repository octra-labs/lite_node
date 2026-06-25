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


val node_root_response :
  validator:string ->
  epoch:int ->
  Yojson.Safe.t

val error_response :
  error_type:string ->
  reason:string ->
  Yojson.Safe.t

val status_response :
  epoch:int ->
  validator:string ->
  root_count:int ->
  timestamp:float ->
  total_accounts:int ->
  total_supply:Z.t ->
  encrypted_supply:Z.t ->
  active_accounts:int ->
  head:Octra_core.Head_manifest.t option ->
  Yojson.Safe.t

val network_stats_response :
  total_accounts:int ->
  active_accounts:int ->
  total_supply:Z.t ->
  recent_tx_count:int ->
  staging_size:int ->
  latest_epochs:int list ->
  Yojson.Safe.t

val epoch_list_response :
  current_epoch:int ->
  Yojson.Safe.t ->
  Yojson.Safe.t

val epoch_summary_ids :
  Yojson.Safe.t ->
  int list

val epoch_current_response :
  epoch_id:int ->
  root_count:int ->
  Yojson.Safe.t

val epoch_get_response :
  Yojson.Safe.t option ->
  (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result

val epoch_summaries_response :
  Yojson.Safe.t list ->
  Yojson.Safe.t

val epoch_summaries_response_of_ids :
  find_summary:(int -> Yojson.Safe.t option) ->
  int list ->
  Yojson.Safe.t

val search_pending_tx :
  hash:string ->
  fields:(string * Yojson.Safe.t) list ->
  Yojson.Safe.t

val search_confirmed_tx :
  hash:string ->
  epoch:int ->
  Yojson.Safe.t

val search_account :
  addr:string ->
  balance:Z.t ->
  nonce:int ->
  Yojson.Safe.t

val search_epoch :
  epoch:int ->
  Yojson.Safe.t

type search_query =
  | Search_too_short
  | Search_hash of string
  | Search_address of string
  | Search_epoch of int
  | Search_unknown

val search_query :
  string ->
  search_query

type search_result =
  | Search_too_short_result
  | Search_pending_tx of string * (string * Yojson.Safe.t) list
  | Search_confirmed_tx of string * int
  | Search_account_result of string * Z.t * int
  | Search_epoch_result of int
  | Search_not_found of string

val search_response :
  search_result ->
  (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result

val search_result_of_query :
  pending_tx:(string -> (string * Yojson.Safe.t) list option) ->
  confirmed_tx:(string -> int option) ->
  account:(string -> (Z.t * int) option) ->
  epoch_exists:(int -> bool) ->
  string ->
  search_result

val staging_view_response :
  tx_rows:Yojson.Safe.t list ->
  Yojson.Safe.t

val staging_view_response_of_txs :
  tx_hash:('a -> string) ->
  tx_fields:('a -> (string * Yojson.Safe.t) list) ->
  'a list ->
  Yojson.Safe.t

val staging_removed_response :
  tx_hash:string ->
  Yojson.Safe.t

val staging_tx_row :
  hash:string ->
  fields:(string * Yojson.Safe.t) list ->
  Yojson.Safe.t

val txs_response :
  (int * string * string) list ->
  Yojson.Safe.t

val send_tx_accepted_response :
  tx_hash:string ->
  nonce:int ->
  ou_cost:Z.t ->
  pending_from_sender:int ->
  total_staging_size:int ->
  Yojson.Safe.t

val staging_stats_response :
  tx_count:int ->
  total_ou:Z.t ->
  max_ou:Z.t ->
  by_sender:(string, int * Z.t) Hashtbl.t ->
  Yojson.Safe.t

val webhooks_response :
  (string, Octra_core.Webhooks.config) Hashtbl.t ->
  Yojson.Safe.t

val webhook_events :
  (string * Yojson.Safe.t) list ->
  string list option

val webhook_registered_response :
  id:string ->
  Yojson.Safe.t

val webhook_unregistered_response :
  id:string ->
  Yojson.Safe.t

val gone_encrypt_balance : string

val gone_decrypt_balance : string

val gone_view_encrypted_balance : string

val gone_register_pvac_pubkey : string

val gone_private_transactions : string

val gone_pending_private_transfers : string

val gone_claim_private_transfer : string

val encrypted_cipher_response :
  addr:string ->
  cipher:string ->
  cipher_type:string ->
  Yojson.Safe.t

val private_transfer_disabled_response : Yojson.Safe.t

val gone_legacy_rest : string

val legacy_rest_path :
  meth:string ->
  path:string ->
  bool

val staging_ou_rpc_response :
  ou_values:Z.t list ->
  staging_ou:Z.t ->
  capacity:Z.t ->
  Yojson.Safe.t

val staging_ou_rpc_response_of_txs :
  tx_ou:('a -> Z.t) ->
  txs:'a list ->
  staging_ou:Z.t ->
  capacity:Z.t ->
  Yojson.Safe.t

val staging_ou_rest_response :
  ou_values:Z.t list ->
  max_ou:Z.t ->
  used_ou:Z.t ->
  capacity:Z.t ->
  Yojson.Safe.t

val recommended_fee_response :
  params:Yojson.Safe.t ->
  is_heavy:(Octra_core.Transaction.t -> bool) ->
  stealth_floor:Z.t ->
  jitter:(unit -> int) ->
  staging_ou:Z.t ->
  capacity:Z.t ->
  usage_pct:int ->
  staging_size:int ->
  Yojson.Safe.t

val recommended_fee_jitter :
  get_int:(string -> int -> int) ->
  pick:(int -> int) ->
  int