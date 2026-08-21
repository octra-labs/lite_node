(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type backend = {
  run :
    epoch_id:int ->
    proposal_id:string ->
    expected_prev_root:string option ->
    preverify:Octra_core.Preverify_commit.t ->
    parent_commit:Octra_consensus.C_types.parent_commit option ->
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
    ~rules
    ~legacy_replay
    ~private_result_policy
    ~max_fhe
    ~max_stealth
    store
    ledger =
  {
    run = (fun ~epoch_id ~proposal_id ~expected_prev_root ~preverify
        ~parent_commit ~reward ~env ~txs ->
      match Octra_core.Rule_graph.circle rules ~epoch:epoch_id with
      | Error fault ->
        Lwt.return_error (Octra_core.Rule_graph.fault_message fault)
      | Ok circle_mode ->
        begin
          match
            Octra_core.Rule_graph.wasm_compute rules ~epoch:epoch_id
          with
          | Error fault ->
            Lwt.return_error (Octra_core.Rule_graph.fault_message fault)
          | Ok wasm_compute_mode ->
            begin
              match
                Octra_core.Rule_graph.object_cost rules ~epoch:epoch_id,
                Octra_core.Rule_graph.owner_migration rules ~epoch:epoch_id
              with
              | Error fault, _
              | _, Error fault ->
                Lwt.return_error (Octra_core.Rule_graph.fault_message fault)
              | Ok object_cost_mode, Ok owner_migration_mode ->
                let object_cost =
                  object_cost_mode = Octra_core.Rule_graph.Active in
                begin
                  match
                    Octra_core.Rule_graph.private_payload
                      rules
                      ~epoch:epoch_id
                  with
                  | Error fault ->
                    Lwt.return_error
                      (Octra_core.Rule_graph.fault_message fault)
                  | Ok private_payload_mode ->
                    match
                      Set_rule.bind
                        rules
                        ~chain_id:env.Octra_core.Epoch_exec.chain_id
                        ~parent:parent_commit
                        ~epoch:epoch_id
                    with
                    | Error error -> Lwt.return_error error
                    | Ok fold ->
                      Octra_core.State_preview.with_preview
                        ~base_store:store
                        ~base_ledger:ledger
                        ~epoch_id
                        ~proposal_id
                        ?expected_prev_root
                        (fun backend ->
                let backend = {
                  backend with
                  Octra_core.Epoch_exec.fold = fold;
                } in
                let private_transition =
                  Octra_core.Private_transition.create
                    ~preverify:(Some preverify)
                    ~ledger:backend.Octra_core.Epoch_exec.ledger
                    ~epoch_id
                    ~owner_migration_mode
                    ~field_policy:
                      (Octra_core.Private_ledger.field_policy_of_mode
                         private_payload_mode)
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
                      ~circle_mode
                      ~wasm_compute_mode
                      ~program_trust
                      ~object_cost
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
                  ~process_tx)
                end
            end
        end);
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
          ~parent_commit:request.parent_commit
          ~reward
          ~env
          ~txs:request.txs);
    }