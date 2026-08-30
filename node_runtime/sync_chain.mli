(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type deps = {
  data_dir : string;
  chain_id : string;
  store : Octra_core.Store_irmin.t;
  chaindata : Octra_core.Store_chaindata.t;
  read_finality : int64 -> Consensus_finality_journal.read_result;
  certificate_path : unit -> string;
}

val bridge_range :
  head_epoch:int64 ->
  after_epoch:int64 ->
  through_epoch:int64 ->
  activate_epoch:int64 ->
  (int64 * int64) option

val build :
  deps ->
  head_epoch:int64 ->
  Octra_consensus.C_types.validator_set ->
  Octra_consensus.C_types.validator_set ->
  (Octra_bootstrap.Sync_anchor.step list, string) result Lwt.t