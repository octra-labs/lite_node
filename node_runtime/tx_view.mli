(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val display_from : Octra_core.Transaction.t -> string

val display_to : Octra_core.Transaction.t -> string

val mask_stealth_row : Yojson.Safe.t -> Yojson.Safe.t

val mask_stealth_rows : Yojson.Safe.t list -> Yojson.Safe.t list

val tx_fields :
  decode_message:(string -> string) ->
  Octra_core.Transaction.t ->
  (string * Yojson.Safe.t) list

val standard_fields :
  decode_message:(string -> string) ->
  from_addr:string ->
  to_addr:string ->
  amount_raw:string ->
  nonce:int ->
  ou:string ->
  timestamp:float ->
  message:string option ->
  (string * Yojson.Safe.t) list

val rpc_pending :
  hash:string ->
  fields:(string * Yojson.Safe.t) list ->
  Yojson.Safe.t

val rpc_confirmed :
  hash:string ->
  epoch:int ->
  fields:(string * Yojson.Safe.t) list ->
  Yojson.Safe.t

val rpc_rejected :
  hash:string ->
  from_addr:string ->
  to_addr:string ->
  amount:string ->
  nonce:int ->
  err_type:string ->
  reason:string ->
  epoch:int ->
  rejected_at:float ->
  Yojson.Safe.t

val rpc_dropped :
  hash:string ->
  reason:string ->
  detail:string ->
  dropped_at:float ->
  from_addr:string ->
  to_addr:string ->
  nonce:int ->
  ou:Z.t ->
  op_type:Octra_core.Transaction.op_type ->
  Yojson.Safe.t

type rejected_lookup = {
  rejected_from : string;
  rejected_to : string;
  rejected_amount : string;
  rejected_nonce : int;
  rejected_type : string;
  rejected_reason : string;
  rejected_epoch : int;
  rejected_at : float;
}

type dropped_lookup = {
  dropped_reason : string;
  dropped_detail : string;
  dropped_at : float;
  dropped_from : string;
  dropped_to : string;
  dropped_nonce : int;
  dropped_ou : Z.t;
  dropped_op : Octra_core.Transaction.op_type;
}

type transaction_lookup =
  | Lookup_pending of Octra_core.Transaction.t
  | Lookup_confirmed of int * string
  | Lookup_rejected of rejected_lookup
  | Lookup_dropped of dropped_lookup
  | Lookup_missing

val transaction_lookup :
  pending:Octra_core.Transaction.t option ->
  confirmed:(int * string) option ->
  rejected:(string * string * string * int * string * string * int * float) option ->
  dropped:(string * string * float * string * string * int * Z.t * Octra_core.Transaction.op_type) option ->
  transaction_lookup

val transaction_lookup_response :
  decode_message:(string -> string) ->
  hash:string ->
  transaction_lookup ->
  (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result

val rest_pending :
  hash:string ->
  fields:(string * Yojson.Safe.t) list ->
  Yojson.Safe.t

val rest_confirmed :
  hash:string ->
  epoch:int ->
  tx_json:string ->
  fields:(string * Yojson.Safe.t) list ->
  Yojson.Safe.t

val rest_rejected :
  hash:string ->
  from_addr:string ->
  to_addr:string ->
  amount:string ->
  nonce:int ->
  err_type:string ->
  reason:string ->
  epoch:int ->
  rejected_at:float ->
  Yojson.Safe.t

val staging_row :
  decode_message:(string -> string) ->
  Octra_core.Transaction.t ->
  Yojson.Safe.t

val staging_response :
  decode_message:(string -> string) ->
  Octra_core.Transaction.t list ->
  Yojson.Safe.t

val requires_heavy_preverify :
  Octra_core.Transaction.t ->
  bool

val submit_params :
  Yojson.Safe.t ->
  (Octra_core.Transaction.t, Octra_core.Rpc.rpc_error) result

val submit_batch_params :
  Yojson.Safe.t ->
  (Yojson.Safe.t list, Octra_core.Rpc.rpc_error) result

val decode_submit_batch_tx :
  Yojson.Safe.t ->
  (Octra_core.Transaction.t, string) result

val submit_rpc_error :
  string ->
  string ->
  Octra_core.Rpc.rpc_error

val submit_accepted :
  tx_hash:string ->
  Octra_core.Transaction.t ->
  Yojson.Safe.t

val submit_batch_row :
  Octra_core.Transaction.t ->
  (string, string * string) result ->
  Yojson.Safe.t

val submit_batch_decode_error :
  string ->
  Yojson.Safe.t

val admission_fee_check :
  min_ou:Z.t ->
  Octra_core.Transaction.t ->
  (unit, string) result

val low_fee_heavy_hashes :
  min_ou:Z.t ->
  Octra_core.Transaction.t list ->
  string list

val semantic_check :
  Octra_core.Transaction.t ->
  (unit, string) result

val staging_submit_admission :
  min_ou:Z.t ->
  min_relay_fee:Z.t ->
  Octra_core.Transaction.t ->
  (unit, string) result

type staging_submit_effects = {
  total_txs : int;
  total_ou : Z.t;
  max_ou : Z.t;
  relay_payload : string option;
}

val staging_submit_effects :
  relay:bool ->
  peer_count:int ->
  total_txs:int ->
  total_ou:Z.t ->
  max_ou:Z.t ->
  tx_hash:string ->
  staging_submit_effects

val address_route_admission :
  Octra_core.Transaction.t ->
  (unit, string * string) result

val self_route_admission :
  Octra_core.Transaction.t ->
  (unit, string * string) result

val address_validation_json :
  Octra_core.Ledger.t ->
  string ->
  Yojson.Safe.t

type signed_rpc_auth_error =
  | Rpc_malformed of string
  | Rpc_auth_error of string

val staging_remove_message : string -> string

type staging_remove_request = {
  staging_remove_hash : string;
  staging_remove_signature_b64 : string;
  staging_remove_pubkey_b64 : string;
}

val staging_remove_params :
  Yojson.Safe.t ->
  (staging_remove_request, Octra_core.Rpc.rpc_error) result

val staging_remove_auth :
  sender:string ->
  tx_hash:string ->
  signature_b64:string ->
  pubkey_b64:string ->
  (unit, string) result

val staging_remove_request_auth :
  sender:string ->
  staging_remove_request ->
  (unit, string) result

val public_key_registration_message : string -> string

val public_key_registration_auth :
  addr:string ->
  pubkey_b64:string ->
  signature_b64:string ->
  (unit, string) result

val encrypted_balance_message : string -> string

val encrypted_balance_auth :
  Yojson.Safe.t ->
  addr:string ->
  (unit, signed_rpc_auth_error) result

val staging_error : string -> string * string

val semantic_error_reason :
  Octra_core.Transaction.t ->
  string ->
  string

val call_params_ok :
  Octra_core.Transaction.t ->
  bool

val encrypted_payload_ok :
  Octra_core.Transaction.t ->
  bool

type payload_limits = {
  encrypted_data_len : int;
  message_len : int;
  message_zkp_len : int;
  message_validator_len : int;
  message_program_len : int;
}

val payload_admission :
  limits:payload_limits ->
  Octra_core.Transaction.t ->
  (unit, string * string) result

val bft_op_admission :
  bft_mode:bool ->
  Octra_core.Transaction.t ->
  (unit, string * string) result

val pre_route_admission :
  now:float ->
  max_timestamp_drift:float ->
  observer_rpc_mode:bool ->
  bft_mode:bool ->
  Octra_core.Transaction.t ->
  (unit, string * string) result

val signature_admission :
  account_public_key:string option ->
  Octra_core.Transaction.t ->
  (unit, string * string) result

val sender_admission :
  sender_exists:bool ->
  Octra_core.Transaction.t ->
  (unit, string * string) result

val submit_pre_signature_admission :
  now:float ->
  max_timestamp_drift:float ->
  observer_rpc_mode:bool ->
  bft_mode:bool ->
  limits:payload_limits ->
  sender_exists:bool ->
  Octra_core.Transaction.t ->
  (unit, string * string) result

val preverify_stealth_payload :
  Octra_core.Transaction.t ->
  Octra_core.Crypto.PrivateTransferV4.t option

val preverify_stealth_ranges :
  pubkey_blob:string ->
  sender_enc:string ->
  Octra_core.Crypto.PrivateTransferV4.t ->
  ((bool * bool), string) result Lwt.t

val post_signature_admission :
  preverify_has_capacity:bool ->
  preverify_pending:int ->
  preverify_pending_max:int ->
  bft_mode:bool ->
  confirmed_nonce:int ->
  bft_window:int ->
  Octra_core.Transaction.t ->
  (unit, string * string) result

val submit_signature_admission :
  account_public_key:string option ->
  preverify_has_capacity:bool ->
  preverify_pending:int ->
  preverify_pending_max:int ->
  bft_mode:bool ->
  confirmed_nonce:int ->
  bft_window:int ->
  Octra_core.Transaction.t ->
  (unit, string * string) result