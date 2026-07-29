(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type plan = {
  header : Octra_consensus.C_types.epoch_header;
  commit_round : int;
  finalize : Octra_consensus.C_types.finalize;
  txs : Octra_core.Transaction.t list;
  tx_hashes : string list;
  epoch : int;
  proposer_info : Octra_core.Epochlog.proposer_info option;
  expected_root : string option;
}

val parse_header :
  default_chain_id:string ->
  Yojson.Safe.t ->
  Octra_consensus.C_types.epoch_header * int

val parse_bundle :
  Yojson.Safe.t ->
  Octra_core.Transaction.t list

val build_plan :
  header:Octra_consensus.C_types.epoch_header ->
  commit_round:int ->
  txs:Octra_core.Transaction.t list ->
  plan

val load_plan :
  default_chain_id:string ->
  header_path:string ->
  bundle_path:string option ->
  plan

type one_shot_deps = {
  current_epoch : unit -> int;
  store_finalized : int -> Octra_consensus.C_types.finalize -> unit;
  store_proposer : int -> Octra_core.Epochlog.proposer_info -> unit;
  store_expected_root : int -> string -> unit;
  set_proposal : Octra_core.Transaction.t list -> string list -> unit;
  write_finality : plan -> unit;
  apply : plan -> unit Lwt.t;
  exit_error : unit -> unit;
  exit_success : unit -> unit;
}

type node_one_shot_deps = {
  current_epoch : unit -> int;
  finality : Consensus_finality_state.callbacks;
  set_proposal : Octra_core.Transaction.t list -> string list -> unit;
  write_entry : Octra_consensus.Finality_log.entry -> unit;
  apply : plan -> unit Lwt.t;
  exit_error : unit -> unit;
  exit_success : unit -> unit;
}

val finality_log_entry :
  plan ->
  Octra_consensus.Finality_log.entry

val node_one_shot_deps :
  node_one_shot_deps ->
  one_shot_deps

val run_one_shot :
  env:(string -> string option) ->
  default_chain_id:string ->
  one_shot_deps ->
  unit Lwt.t