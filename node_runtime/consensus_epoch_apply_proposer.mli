(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Epochlog = Octra_core.Epochlog

type source =
  | Env
  | Override
  | Pending
  | Finalized_header
  | Epochlog_disk
  | Round_robin_fallback

type missing =
  | Missing_consensus_proposer
  | Missing_fallback_validator

type selected = {
  source : source;
  proposer : Epochlog.proposer_info;
}

type request = {
  epoch_id : int;
  consensus_mode : bool;
  active_validators : string list;
  env_fee_recipient : string option;
  override_proposer : Epochlog.proposer_info option;
  pending_proposer : Epochlog.proposer_info option;
  finalized_header_proposer : Epochlog.proposer_info option;
  disk_epoch : Epochlog.epoch_header option;
}

type runtime_deps = {
  env_fee_recipient : unit -> string option;
  pending_proposer : unit -> Epochlog.proposer_info option;
  finalized : unit -> Octra_consensus.C_types.finalize option;
  disk_epoch : unit -> Epochlog.epoch_header option;
  log : string -> unit;
  fatal : string -> unit;
  short : string -> string;
}

type runtime_request = {
  runtime_epoch_id : int;
  runtime_consensus_mode : bool;
  runtime_active_validators : string list;
  runtime_override_proposer : Epochlog.proposer_info option;
}

type runtime_result = {
  source_label : string;
  proposer : Epochlog.proposer_info;
}

type node_deps = {
  env : string -> string option;
  finality : Consensus_finality_state.t;
  epoch_json : int -> string option;
  log : string -> unit;
  fatal : string -> unit;
  short : string -> string;
}

val source_label : source -> string
val valid_proposer : Epochlog.proposer_info -> bool
val proposer_from_addr : ?commit_round:int -> string -> Epochlog.proposer_info option
val proposer_from_disk_epoch : Epochlog.epoch_header -> Epochlog.proposer_info option
val proposer_from_finalized : Octra_consensus.C_types.finalize -> Epochlog.proposer_info option
val selected_log_line :
  epoch_id:int ->
  short:(string -> string) ->
  selected ->
  string option
val missing_line : epoch_id:int -> missing -> string
val choose : request -> (selected, missing) result
val choose_runtime :
  runtime_deps ->
  runtime_request ->
  (selected, missing) result
val run_runtime :
  runtime_deps ->
  runtime_request ->
  exit:(unit -> runtime_result) ->
  runtime_result
val run_node :
  node_deps ->
  runtime_request ->
  exit:(unit -> runtime_result) ->
  runtime_result