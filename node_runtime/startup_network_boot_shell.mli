(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type wallet = {
  address : string;
  pub : string;
  priv : string;
}

type refs = {
  consensus_config_hash : string ref;
  consensus_validator_set : Octra_consensus.C_types.validator_set ref;
  scheduled_validator_set : Octra_consensus.C_config.scheduled option ref;
}

type deps = {
  env : string -> string option;
  read_active_validator_meta : unit -> string option;
  read_pending_validator_meta : unit -> string option;
  data_dir : string;
  store_path : string;
  current_epoch : int ref;
  chaindata : Octra_core.Store_chaindata.t;
  finality : Consensus_finality_state.callbacks;
  set_proposal : Octra_core.Transaction.t list -> string list -> unit;
  apply_finalized : Consensus_startup_sync.apply_finalized;
  consensus_role : string;
  recovery : bool;
  wallet : wallet;
  require_sync : Sync_need.t -> unit;
  exit_error : unit -> unit;
  exit_success : unit -> unit;
}

type t = {
  api_port : int;
  p2p_port : int;
  consensus_port : int;
  consensus_peers : string list;
  chain_id : string;
  consensus_config_hash_ref : string ref;
  consensus_validator_set_ref : Octra_consensus.C_types.validator_set ref;
  scheduled_validator_set_ref : Octra_consensus.C_config.scheduled option ref;
}

val create_refs :
  unit ->
  refs

val sync :
  recovery:bool ->
  (unit -> unit Lwt.t) ->
  unit

val run :
  deps ->
  t