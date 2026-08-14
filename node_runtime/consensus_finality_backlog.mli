(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type outcome =
  | Clean
  | Armed
  | Blocked

type deps = {
  read_backlog :
    unit ->
    (Consensus_finality_journal.record list, string) result;
  write_finality : Octra_consensus.C_types.finalize -> unit;
  store_finalized :
    epoch:int ->
    validator_set:Octra_consensus.C_types.validator_set ->
    Octra_consensus.C_types.finalize ->
    unit;
  store_proposer : Consensus_finalized_flow.proposer_info -> unit;
  store_expected_root : epoch:int -> root:string -> unit;
  store_bundle :
    proposal_id:string ->
    tx_hashes:string list ->
    txs:Octra_core.Transaction.t list ->
    receipts_json:string list ->
    unit;
  set_consensus_finalized : bool -> unit;
  clear_state_attested : unit -> unit;
  mark_quarantine : string -> unit;
}

val run : deps -> outcome