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


type prev_root_check =
  | Prev_root_ok
  | Prev_root_mismatch of {
      local_raw : string;
      expected_prev : string;
    }

type prev_root_result =
  | Prev_root_checked
  | Prev_root_exited

type prev_root_effects = {
  fatal : string -> unit;
  exit : unit -> unit;
}

type prev_root_context = {
  epoch_id : int;
  local_pre_root : string;
  expected_prev : string option;
}

type post_root_check =
  | Post_root_ok
  | Post_root_missing
  | Post_root_mismatch of {
      actual_raw : string;
      expected_root : string;
    }

type account_diag = {
  addr : string;
  balance : string;
  nonce : int;
  pub : string;
  enc : string;
  decrypt_allowance : string;
}

type account_view = {
  balance : string;
  nonce : int;
  pub : string option;
  enc : string option;
  decrypt_allowance : string;
}

type account_diag_row =
  | Account_missing of string
  | Account_present of account_diag

type post_root_diag = {
  epoch_id : int;
  proposer_source : string;
  proposer : string;
  round : int;
  validators : int;
  validators_sha : string;
  tx_count : int;
  base_reward : string;
  total_reward : string;
  proposer_total : string;
  each_validator : string;
  remainder : string;
  prev_supply : string;
  emission_remaining : string;
  next_supply : string;
  next_emission : string;
  fees : string;
  accounts_hash : string;
  meta_hash : string;
  root : string;
  expected_root : string;
  actual_root : string;
  current_epoch_meta : string;
  last_epoch_meta : string;
  total_supply_meta : string;
  emission_remaining_meta : string;
  accounts : account_diag_row list;
}

type post_root_action =
  | Post_root_continue of string option
  | Post_root_fail of {
      diag_lines : string list;
      fatal_lines : string list;
    }

type post_root_result =
  | Post_root_checked
  | Post_root_exited

type post_root_effects = {
  warn : string -> unit;
  error : string -> unit;
  fatal : string -> unit;
  remove_expected_root : int -> unit;
  prune_expected_roots_before : int -> unit;
  exit : unit -> unit;
}

type post_root_context = {
  consensus_mode : bool;
  layera_diag : bool;
  epoch_id : int;
  current_epoch : int;
  actual_root : string;
  expected_root : string option;
  proposer_source : string;
  proposer : string;
  proposer_short : string;
  round : int;
  validators : int;
  validators_sha : string;
  tx_count : int;
  base_reward : string;
  total_reward : string;
  proposer_total : string;
  each_validator : string;
  remainder : string;
  prev_supply : string;
  emission_remaining : string;
  next_supply : string;
  next_emission : string;
  fees : string;
  accounts_hash : string;
  meta_hash : string;
  root : string;
  current_epoch_meta : string;
  last_epoch_meta : string;
  total_supply_meta : string;
  emission_remaining_meta : string;
  account_addrs : string list;
  short : string -> string;
  find_account : string -> account_view option;
}

type post_root_context_input = {
  consensus_mode : bool;
  layera_diag : bool;
  epoch_id : int;
  current_epoch : int;
  actual_root : string;
  expected_root : string option;
  proposer_source : string;
  proposer : string;
  round : int;
  validators : int;
  validators_sha : string;
  tx_count : int;
  plan : Octra_core.Epoch_exec.reward_plan;
  prev_supply : Z.t;
  emission_remaining : Z.t;
  confirmed_fees : Z.t;
  replay_subtrees : (string * string) list;
  layera_meta_pairs : (string * string) list;
  root : string;
  account_addrs : string list;
  short : string -> string;
  find_account : string -> account_view option;
}

type stage_batch_effects = {
  trace : string -> unit;
  warn : string -> unit;
  level_output : string -> unit;
  replay_output : string -> unit;
  write_marker : int -> string -> unit;
  set_meta : key:string -> value:string -> unit Lwt.t;
  flush_dirty : unit -> unit Lwt.t;
  dump_subtrees : unit -> (string * string) list Lwt.t;
  dump_meta : unit -> (string * string) list Lwt.t;
  batch_root : unit -> string option Lwt.t;
  fallback_root : unit -> string Lwt.t;
}

type stage_batch_live_effects = {
  data_dir : string;
  store : Octra_core.Store_irmin.t;
  ledger : Octra_core.Ledger.t;
  trace : string -> unit;
  warn : string -> unit;
  level_output : string -> unit;
  replay_output : string -> unit;
}

type stage_batch_context = {
  epoch_id : int;
  current_epoch : int;
  replay_trace : bool;
  layera_diag : bool;
  txs_in : int;
  confirmed : int;
  fees : string;
  base : string;
}

type stage_batch_result = {
  post_state_hash : string;
  replay_subtrees : (string * string) list;
  layera_meta_pairs : (string * string) list;
}

val raw32_of_pre_root : string -> string

val raw_hex8 : string -> string

val assoc_or_none : string -> (string * string) list -> string

val short_field : int -> string -> string

val account_diag :
  short:(string -> string) ->
  find_account:(string -> account_view option) ->
  string ->
  account_diag_row

val account_view_of_ledger_account :
  Octra_core.Ledger.account ->
  account_view

val post_root_context_of_input :
  post_root_context_input ->
  post_root_context

val check_prev_root :
  local_pre_root:string ->
  expected_prev:string ->
  prev_root_check

val check_post_root :
  actual_root:string ->
  expected_root:string ->
  post_root_check

val mismatch_line :
  epoch_id:int ->
  local_raw:string ->
  expected_prev:string ->
  string

val run_prev_root_check :
  prev_root_effects ->
  prev_root_context ->
  prev_root_result

val post_root_missing_line : int -> string

val level_trace_line :
  epoch_id:int ->
  txs_in:int ->
  confirmed:int ->
  fees:string ->
  base:string ->
  string

val replay_post_line :
  epoch_id:int ->
  base:string ->
  fees:string ->
  accounts:string ->
  meta:string ->
  root:string ->
  string

val missing_batch_tree_line : string

val post_root_diag_lines : post_root_diag -> string list

val post_root_mismatch_fatal_lines :
  epoch_id:int ->
  expected_root:string ->
  actual_root:string ->
  proposer:string ->
  tx_count:int ->
  string list

val post_root_action :
  consensus_mode:bool ->
  layera_diag:bool ->
  epoch_id:int ->
  actual_root:string ->
  expected_root:string option ->
  diag_lines:(expected_root:string -> actual_root:string -> string list) ->
  fatal_lines:(expected_root:string -> actual_root:string -> string list) ->
  post_root_action

val post_root_diag_of_context :
  post_root_context ->
  expected_root:string ->
  actual_root:string ->
  string list

val post_root_fatal_of_context :
  post_root_context ->
  expected_root:string ->
  actual_root:string ->
  string list

val run_post_root_check :
  post_root_effects ->
  post_root_context ->
  post_root_result

val run_post_root_input :
  post_root_effects ->
  post_root_context_input ->
  post_root_result

val live_stage_batch_effects :
  stage_batch_live_effects ->
  stage_batch_effects

val run_stage_batch :
  stage_batch_effects ->
  stage_batch_context ->
  stage_batch_result Lwt.t