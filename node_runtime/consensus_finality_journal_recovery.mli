(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type outcome =
  | Continue
  | Armed
  | Blocked

type deps = {
  read_journal : unit -> Consensus_finality_journal.read_result;
  head_epoch : unit -> int;
  root_at_epoch : int -> string option;
  current_root : unit -> string option;
  write_finality : Octra_consensus.C_types.finalize -> unit;
  store_finalized :
    epoch:int ->
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
  set_proposal :
    Octra_core.Transaction.t list ->
    string list ->
    unit;
  reset_proposal_state : unit -> unit;
  set_consensus_finalized : bool -> unit;
  clear_state_attested : unit -> unit;
  commit_journal : unit -> unit;
  mark_quarantine : string -> unit;
}

val run :
  deps ->
  outcome