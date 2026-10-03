(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type request = {
  data_dir : string;
  chain_id : string;
  config_hash : string;
  certificate : Octra_bootstrap.State_sync_manifest.certificate;
  validator_set : Octra_consensus.C_types.validator_set;
  exporter_set : Octra_consensus.C_types.validator_set;
  first_epoch : int;
  activation_epoch : int;
  addresses : string list;
  max_epochs : int;
  max_txs : int;
  max_records : int;
}

val read : request -> (Octra_core.Pvac_migration_admission.t, string) result