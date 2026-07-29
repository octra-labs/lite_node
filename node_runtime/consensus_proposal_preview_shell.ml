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

let node_backend
    ~program_trust
    ~legacy_replay
    ~private_result_policy
    ~max_fhe
    ~max_stealth
    store =
  {
    run = (fun ~epoch_id ~proposal_id ~expected_prev_root ~preverify ~reward
        ~env ~txs ->
      Octra_core.State_preview.with_preview
        ~base_store:store
        ~epoch_id
        ~proposal_id
        ?expected_prev_root
        (fun backend ->
          let private_transition =
            Octra_core.Private_transition.create
              ~preverify:(Some preverify)
              ~ledger:backend.Octra_core.Epoch_exec.ledger
              ~epoch_id
              ~result_policy:(private_result_policy epoch_id)
              ~legacy_replay
              ~limits:Octra_core.Private_transition.{
                max_fhe;
                max_stealth;
              }
          in
          let process_tx ~backend ~env
              (tx : Octra_core.Transaction.t) =
            if Octra_core.Transaction.bft_crypto_active ()
              && Octra_core.Transaction.bft_crypto_op tx.op_type
            then
              let open Lwt.Syntax in
              let* result =
                Octra_core.Private_transition.process
                  private_transition
                  ~backend
                  ~env
                  tx in
              Lwt.return
                (Result.map
                   (fun fee -> Octra_core.Epoch_exec.Confirmed fee)
                   result)
            else
              Consensus_vm_transition.process_tx
                ~preverify
                ~program_trust
                ~backend
                ~env
                tx
          in
          Octra_core.Epoch_exec.run_transition_rewarded
            ~reward
            ~preverify
            ~backend
            ~env
            ~txs
            ~process_tx));
  }

let run (deps : deps) =
  Consensus_driver_wiring.node_proposal_preview
    Consensus_driver_wiring.{
      chain_id = deps.chain_id;
      ready_state_root_at = deps.ready_state_root_at;
      ready_max_lag = deps.ready_max_lag;
      warn = deps.warn;
      run_preview = (fun request ~reward ~env ->
        deps.backend.run
          ~epoch_id:(Int64.to_int request.Consensus_proposal.epoch_id)
          ~proposal_id:request.proposal_id
          ~expected_prev_root:(Some request.expected_prev_root)
          ~preverify:request.preverify
          ~reward
          ~env
          ~txs:request.txs);
    }