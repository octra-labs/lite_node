(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type prepared = {
  head : Octra_core.Head_manifest.t;
  checkpoint : Octra_bootstrap.State_sync_checkpoint.body;
  checkpoint_hash : string;
  anchor : Octra_bootstrap.Sync_anchor.t;
  trusted_validator_set : Octra_consensus.C_types.validator_set;
  roots : Octra_consensus.C_types.finalize list;
}

type deps = {
  data_dir : string;
  chain_id : string;
  store : Octra_core.Store_irmin.t;
  chaindata : Octra_core.Store_chaindata.t;
  wallet : Octra_core.Crypto.Wallet.t;
  config_hash : unit -> (string, string) result;
  trusted_validator_set :
    unit ->
    (Octra_consensus.C_types.validator_set, string) result;
  head : unit -> Octra_core.Head_manifest.t option;
  read_finality :
    int64 ->
    Consensus_finality_journal.read_result;
  read_root :
    int64 ->
    Consensus_finality_journal.read_result;
  exporter_set :
    unit ->
    (Octra_consensus.C_types.validator_set, string) result;
  certificate_path : unit -> string;
  now : unit -> float;
  sleep : float -> unit Lwt.t;
  info : string -> unit;
  warn : string -> unit;
}

val epoch_head :
  store:Octra_core.Store_irmin.t ->
  chaindata:Octra_core.Store_chaindata.t ->
  int64 ->
  Consensus_finality_journal.record ->
  (Octra_core.Head_manifest.t, string) result Lwt.t

val prepare :
  chain_id:string ->
  config_hash:string ->
  trusted_validator_set:Octra_consensus.C_types.validator_set ->
  validator_set:Octra_consensus.C_types.validator_set ->
  steps:Octra_bootstrap.Sync_anchor.step list ->
  head:Octra_core.Head_manifest.t ->
  Consensus_finality_journal.record ->
  (prepared, string) result

val retention_plan :
  retain:int ->
  current:string ->
  (string * int) list ->
  string list

val publisher_addresses :
  Octra_consensus.C_types.validator_set ->
  string list

val publisher_override :
  string option ->
  (bool, string) result

val exporter_wallet :
  force:bool ->
  Octra_consensus.C_types.validator_set ->
  Octra_core.Crypto.Wallet.t ->
  (unit, string) result

val capture_epochs :
  target:int64 ->
  Octra_core.Head_manifest.t option ->
  int64 list

val run :
  deps ->
  unit Lwt.t