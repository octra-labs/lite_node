(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Transaction = Octra_core.Transaction

type accepted = {
  tx_hashes : string list;
  txs : Transaction.t list;
  receipts_json : string list;
  rejections : Octra_core.Tx_outcome.rejection list;
}

val finalized :
  header:Octra_consensus.C_types.epoch_header ->
  Octra_consensus.C_driver.bundle_response_record ->
  (accepted, string) result

val proposal :
  header:Octra_consensus.C_types.epoch_header ->
  expected_hashes:string list ->
  Octra_consensus.C_driver.bundle_response_record ->
  (accepted, string) result