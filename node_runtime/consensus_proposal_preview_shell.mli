(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type backend = {
  run :
    epoch_id:int ->
    proposal_id:string ->
    expected_prev_root:string option ->
    preverify:Octra_core.Preverify_commit.t ->
    reward:Consensus_reward_attribution.t ->
    env:Octra_core.Epoch_exec.env ->
    txs:Octra_core.Transaction.t list ->
    (Octra_core.Epoch_exec.exec_result, string) result Lwt.t;
}

type deps = {
  chain_id : string;
  program_trust : Octra_vm.Program_trust.t;
  backend : backend;
  ready_state_root_at : int -> string option Lwt.t;
  ready_max_lag : int;
  warn : string -> unit;
}

val node_backend :
  program_trust:Octra_vm.Program_trust.t ->
  legacy_replay:
    (epoch:int ->
     address:string ->
     cipher:string ->
     Octra_core.Pvac_legacy_public_replay.decision) ->
  private_result_policy:
    (int -> Octra_core.Private_result_policy.t) ->
  max_fhe:int ->
  max_stealth:int ->
  Octra_core.Store_irmin.t ->
  Octra_core.Ledger.t ->
  backend

val run :
  deps ->
  ?catch_exn:bool ->
  Consensus_proposal.build_preview_request ->
  (Octra_core.Epoch_exec.exec_result, string) result Lwt.t