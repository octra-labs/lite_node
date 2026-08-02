(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val recent_txs_of_summary_rows : Yojson.Safe.t list -> (int * string) list

val recent_tx_to_yojson : int * string -> Yojson.Safe.t

val recent_tx_link_to_yojson : int * string -> Yojson.Safe.t

val rejected_tx_to_yojson : string * int * 'a -> Yojson.Safe.t

val recent_txs_to_yojson : (int * string) list -> Yojson.Safe.t list

val recent_txs_to_link_yojson : (int * string) list -> Yojson.Safe.t list

val rejected_txs_to_yojson : (string * int * 'a) list -> Yojson.Safe.t list

val index_incomplete :
  string ->
  (string * Yojson.Safe.t) list ->
  Octra_core.Rpc.rpc_error

val recent_incomplete_error :
  offset:int ->
  limit:int ->
  missing:int ->
  Octra_core.Rpc.rpc_error

val address_incomplete_error :
  addr:string ->
  offset:int ->
  limit:int ->
  missing:int ->
  Octra_core.Rpc.rpc_error

val token_incomplete_error :
  addr:string ->
  offset:int ->
  limit:int ->
  missing:int ->
  Octra_core.Rpc.rpc_error

val epoch_incomplete_error :
  epoch_id:int ->
  (string * Yojson.Safe.t) list ->
  Octra_core.Rpc.rpc_error

val epoch_index_status_fields :
  Octra_core.Store_chaindata.epoch_index_status ->
  (string * Yojson.Safe.t) list

val epoch_incomplete_status_error :
  epoch_id:int ->
  missing:int ->
  Octra_core.Store_chaindata.epoch_index_status option ->
  Octra_core.Rpc.rpc_error

val page_has_more : offset:int -> count:int -> total:int -> bool

val transaction_epoch_targets :
  ?epoch_param:int ->
  int list ->
  int list

type transactions_query = {
  transactions_epoch_param : int option;
  transactions_limit : int;
}

type 'a transactions_list_result =
  | Transactions_index_incomplete of int * (string * Yojson.Safe.t) list
  | Transactions_ready of (int * string * 'a) list

val transactions_query :
  Yojson.Safe.t ->
  transactions_query

val transaction_rows :
  load_epoch:(int -> (string * 'a) list) ->
  limit:int ->
  int list ->
  (int * string * 'a) list

val transactions_list_result :
  epoch_status:(int -> 'status option) ->
  status_ok:('status -> bool) ->
  status_fields:('status -> (string * Yojson.Safe.t) list) ->
  load_epoch:(int -> (string * 'a) list) ->
  Yojson.Safe.t ->
  int list ->
  'a transactions_list_result

val transactions_list_response :
  'a transactions_list_result ->
  (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result

type page = {
  limit : int;
  offset : int;
}

type epoch_page_request = {
  epoch_page_id : int;
  epoch_page_limit : int;
  epoch_page_offset : int;
}

type 'a epoch_page_status = {
  epoch_page_status : 'a;
  epoch_page_heal_checked : int;
  epoch_page_heal_repaired : int;
  epoch_page_heal_errors : int;
  epoch_page_heal_ms : float;
  epoch_page_retry_ms : float;
}

val epoch_page_request :
  limit_max:int ->
  Yojson.Safe.t ->
  (epoch_page_request, Octra_core.Rpc.rpc_error) result

val epoch_page_cache_ttl :
  current_epoch_id:int ->
  recent_ttl:float ->
  old_ttl:float ->
  epoch_id:int ->
  float

val epoch_page_cache_key :
  epoch_id:int ->
  start_txid:int64 ->
  tx_count:int ->
  limit:int ->
  offset:int ->
  string

val epoch_page_cache_plan :
  current_epoch_id:int ->
  recent_ttl:float ->
  old_ttl:float ->
  epoch_id:int ->
  header:(int64 * int) option ->
  limit:int ->
  offset:int ->
  (string * float) option

val epoch_page_cache_deadline :
  now:float ->
  ttl:float ->
  float

val epoch_page_cache_live :
  now:float ->
  deadline:float ->
  bool

val epoch_page_status_without_heal :
  'a ->
  'a epoch_page_status

val epoch_page_status_after_heal :
  status:'a ->
  heal_checked:int ->
  heal_repaired:int ->
  heal_errors:int ->
  heal_ms:float ->
  retry_ms:float ->
  'a epoch_page_status

val epoch_page_heal_should_log :
  'a epoch_page_status ->
  bool

val epoch_page_status_with_heal :
  is_incomplete:('a -> bool) ->
  recent_epochs:int ->
  now:(unit -> float) ->
  heal:(unit -> int * int * int) ->
  retry:(unit -> 'a) ->
  'a ->
  'a epoch_page_status

val rpc_page :
  ?limit_index:int ->
  ?offset_index:int ->
  ?limit_max:int ->
  ?default_limit:int ->
  Yojson.Safe.t ->
  page

type epoch_profile

val make_epoch_profile :
  epoch_id:int ->
  current_epoch_id:int ->
  limit:int ->
  offset:int ->
  incomplete:bool ->
  missing:int ->
  expected:int ->
  rows:int ->
  rejected:int ->
  has_more:bool ->
  status_ms:float ->
  heal_ms:float ->
  heal_checked:int ->
  heal_repaired:int ->
  heal_errors:int ->
  retry_ms:float ->
  rejected_ms:float ->
  json_ms:float ->
  total_ms:float ->
  epoch_profile

val make_epoch_incomplete_profile :
  epoch_id:int ->
  current_epoch_id:int ->
  limit:int ->
  offset:int ->
  missing:int ->
  expected:int ->
  rows:int ->
  status_ms:float ->
  heal_ms:float ->
  heal_checked:int ->
  heal_repaired:int ->
  heal_errors:int ->
  retry_ms:float ->
  total_ms:float ->
  epoch_profile

val make_epoch_incomplete_page_profile :
  epoch_id:int ->
  current_epoch_id:int ->
  limit:int ->
  offset:int ->
  missing:int ->
  expected:int ->
  rows:'a list ->
  status_ms:float ->
  heal_ms:float ->
  heal_checked:int ->
  heal_repaired:int ->
  heal_errors:int ->
  retry_ms:float ->
  total_ms:float ->
  epoch_profile

val make_epoch_incomplete_page_profile_with_status :
  epoch_id:int ->
  current_epoch_id:int ->
  limit:int ->
  offset:int ->
  missing:int ->
  expected:int ->
  rows:'a list ->
  status_ms:float ->
  page_status:'b epoch_page_status ->
  total_ms:float ->
  epoch_profile

val make_epoch_complete_profile :
  epoch_id:int ->
  current_epoch_id:int ->
  limit:int ->
  offset:int ->
  expected:int ->
  rows:int ->
  rejected:int ->
  has_more:bool ->
  status_ms:float ->
  heal_ms:float ->
  heal_checked:int ->
  heal_repaired:int ->
  heal_errors:int ->
  retry_ms:float ->
  rejected_ms:float ->
  json_ms:float ->
  total_ms:float ->
  epoch_profile

val make_epoch_complete_page_profile :
  epoch_id:int ->
  current_epoch_id:int ->
  limit:int ->
  offset:int ->
  expected:int ->
  rows:'a list ->
  rejected_rows:'b list ->
  status_ms:float ->
  heal_ms:float ->
  heal_checked:int ->
  heal_repaired:int ->
  heal_errors:int ->
  retry_ms:float ->
  rejected_ms:float ->
  json_ms:float ->
  total_ms:float ->
  epoch_profile

val make_epoch_complete_page_profile_with_status :
  epoch_id:int ->
  current_epoch_id:int ->
  limit:int ->
  offset:int ->
  expected:int ->
  rows:'a list ->
  rejected_rows:'b list ->
  status_ms:float ->
  page_status:'c epoch_page_status ->
  rejected_ms:float ->
  json_ms:float ->
  total_ms:float ->
  epoch_profile

val epoch_profile_should_warn :
  warn_ms:float ->
  epoch_profile ->
  bool

val epoch_profile_log_message :
  epoch_profile ->
  string

val epoch_profile_warning :
  warn_ms:float ->
  epoch_profile ->
  string option

type account_profile

type account_view = {
  account_json : Yojson.Safe.t;
  account_recent_count : int;
  account_rejected_count : int;
}

val make_account_profile :
  tag:string ->
  addr:string ->
  recent_count:int ->
  rejected_count:int ->
  lookup_ms:float ->
  recent_ms:float ->
  rejected_ms:float ->
  json_ms:float ->
  account_profile

val account_profile_log_message :
  account_profile ->
  string

val encrypted_balance_present :
  string option ->
  bool

val account_response :
  addr:string ->
  balance:string ->
  balance_raw:string ->
  nonce:int ->
  has_public_key:bool ->
  has_encrypted_balance:bool ->
  tx_count:int ->
  recent_txs:(int * string) list ->
  rejected_txs:(string * int * 'a) list ->
  Yojson.Safe.t

val account_view :
  addr:string ->
  balance:Z.t ->
  nonce:int ->
  has_public_key:bool ->
  encrypted_balance:string option ->
  tx_count:int ->
  recent_rows:Yojson.Safe.t list ->
  rejected_txs:(string * int * 'a) list ->
  account_view

val account_profile_from_view :
  tag:string ->
  addr:string ->
  view:account_view ->
  t0:float ->
  t1:float ->
  t2:float ->
  t3:float ->
  t4:float ->
  account_profile

val address_transactions_response :
  addr:string ->
  total:int ->
  offset:int ->
  limit:int ->
  transactions:Yojson.Safe.t list ->
  rejected:Yojson.Safe.t list ->
  dropped:Yojson.Safe.t list ->
  Yojson.Safe.t

val token_transactions_response :
  addr:string ->
  total:int ->
  offset:int ->
  limit:int ->
  has_more:bool ->
  incoming:int ->
  outgoing:int ->
  transactions:Yojson.Safe.t list ->
  Yojson.Safe.t

val rejected_response :
  addr:string ->
  total:int ->
  offset:int ->
  limit:int ->
  rejected:Yojson.Safe.t list ->
  Yojson.Safe.t

val epoch_transactions_response :
  (int * string * 'a) list ->
  Yojson.Safe.t

val recent_transactions_response :
  transactions:Yojson.Safe.t list ->
  Yojson.Safe.t

val recent_transactions_result :
  mask_rows:('a list -> Yojson.Safe.t list) ->
  page:page ->
  incomplete:bool ->
  missing:int ->
  rows:'a list ->
  (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result

val address_transactions_result :
  mask_rows:('a list -> Yojson.Safe.t list) ->
  addr:string ->
  page:page ->
  total:int ->
  incomplete:bool ->
  missing:int ->
  rows:'a list ->
  rejected:'a list ->
  dropped:Yojson.Safe.t list ->
  (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result

val token_transactions_result :
  mask_rows:('a list -> Yojson.Safe.t list) ->
  addr:string ->
  page:page ->
  total:int ->
  has_more:bool ->
  incoming:int ->
  outgoing:int ->
  incomplete:bool ->
  missing:int ->
  rows:'a list ->
  (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result

val rejected_transactions_result :
  mask_rows:('a list -> Yojson.Safe.t list) ->
  addr:string ->
  page:page ->
  total:int ->
  rows:'a list ->
  (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result

val epoch_transactions_page_response :
  epoch_id:int ->
  expected_confirmed_count:int ->
  offset:int ->
  limit:int ->
  transactions:Yojson.Safe.t list ->
  rejected:Yojson.Safe.t list ->
  Yojson.Safe.t

val epoch_transactions_page_response_of_rows :
  mask_rows:('a list -> Yojson.Safe.t list) ->
  epoch_id:int ->
  expected_confirmed_count:int ->
  offset:int ->
  limit:int ->
  rows:'a list ->
  rejected_rows:'a list ->
  Yojson.Safe.t

val http_address_summary_response :
  addr:string ->
  balance:string ->
  balance_raw:string ->
  nonce:int ->
  has_public_key:bool ->
  transaction_count:int ->
  recent_txs:(int * string) list ->
  Yojson.Safe.t