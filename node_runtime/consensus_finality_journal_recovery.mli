(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type outcome =
  | Continue
  | Armed
  | Blocked

type invalid_plan =
  | Drop_unapplied
  | Block_invalid

type deps = {
  read_journal : unit -> Consensus_finality_journal.read_result;
  read_pending_epoch : unit -> (int64 option, string) result;
  drop_invalid_unapplied : head_epoch:int -> (int, string) result;
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
  commit_journal : epoch:int64 -> state_root:string -> unit;
  mark_quarantine : string -> unit;
}

val run :
  deps ->
  outcome

val classify_invalid :
  head_epoch:int ->
  (int64 option, string) result ->
  invalid_plan