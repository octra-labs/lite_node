(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Core = Octra_core
module Gate = Core.Preverify_commit
module Tx = Core.Transaction

type prepared = Consensus_proposal.prepared = {
  execution : Core.Epoch_exec.exec_result;
  preverify : Gate.t;
}

type 'a runner =
    epoch_id:int ->
    proposal_id:string ->
    expected_prev_root:string option ->
    preverify:Octra_core.Preverify_commit.t ->
    parent_commit:Octra_consensus.C_types.parent_commit option ->
    reward:Consensus_reward_attribution.t ->
    env:Octra_core.Epoch_exec.env ->
    txs:Octra_core.Transaction.t list ->
    ('a, string) result Lwt.t

type backend = {
  run : Core.Epoch_exec.exec_result runner;
  prepare : prepared runner;
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
    ?(private_artifacts = fun _ -> [])
    ?(key_artifacts = fun _ -> [])
    ~program_trust
    ~rules
    ~legacy_replay
    ~private_result_policy
    ~max_fhe
    ~max_stealth
    store
    ledger =
  let execute ~capture ~epoch_id ~proposal_id ~expected_prev_root ~preverify
      ~parent_commit ~reward ~env ~txs =
      let preverify =
        Octra_core.Preverify_commit.with_artifacts
          (private_artifacts txs)
          preverify
        |> Octra_core.Preverify_commit.with_keys (key_artifacts txs)
      in
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
                      begin
                        match fold epoch_id with
                        | Error error -> Lwt.return_error error
                        | Ok fold_ctx ->
                      Octra_core.State_preview.with_preview
                        ~base_store:store
                        ~base_ledger:ledger
                        ~proof_mode:fold_ctx.Octra_core.Epoch_exec.standard_mode
                        ~math:fold_ctx.math
                        ~fold
                        ~epoch_id
                        ~proposal_id
                        ?expected_prev_root
                        (fun backend ->
                let open Lwt.Syntax in
                let* checked =
                  if not capture then Lwt.return_ok ()
                  else match Gate.dup_by Tx.hash txs with
                    | Some hash -> Lwt.return_error ("duplicate_tx:" ^ hash)
                    | None ->
                      let used = List.map (fun lane -> lane, Core.Resource_lanes.zero)
                        Core.Resource_lanes.all in
                      let budget = List.fold_left (fun checked tx ->
                        Result.bind checked (fun used ->
                          Gate.check_work preverify used tx)) (Ok used) txs in
                      match budget with
                      | Error error -> Lwt.return_error error
                      | Ok _ -> Gate.check_bound backend.ledger preverify
                          (List.filter (fun tx -> tx.Tx.op_type <> Tx.CircleCall) txs)
                in
                match checked with
                | Error error -> Lwt.return_error ("preverify_commit_gate:" ^ error)
                | Ok () ->
                let* snapshot =
                  if capture then
                    Lwt.map Core.Preverify_worker.state_hash (Core.Ledger.hash backend.ledger)
                  else Lwt.return "" in
                let captured = ref [] in
                let private_transition =
                  Octra_core.Private_transition.create
                    ~preverify:(Some preverify)
                    ~ledger:backend.Octra_core.Epoch_exec.ledger
                    ~epoch_id
                    ~owner_migration_mode
                    ~proof_mode:fold_ctx.Octra_core.Epoch_exec.standard_mode
                    ~math:fold_ctx.math
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
                  Octra_core.Exec_resource.run ~hash:(Octra_core.Transaction.hash tx) (fun () ->
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
                    if capture && tx.op_type = Tx.CircleCall then
                      let* result, binding = Lwt.catch
                        (fun () -> Consensus_vm_transition.capture_circle
                          ~circle_mode ~wasm_compute_mode ~program_trust ~object_cost
                          ~backend ~env tx)
                        (function
                          | Octra_circle_runtime.Circle_exec.Execution_unavailable _ ->
                            Lwt.fail (Core.Exec_resource.Unavailable Host)
                          | error -> Lwt.fail error) in
                      let receipt = match binding with
                        | None -> Error "circle receipt capture missing"
                        | Some binding ->
                          let circle = Consensus_circle_preverify.circle_state snapshot binding in
                          match Core.Preverify_worker.circle_receipt tx circle with
                          | Core.Preverify_worker.Ready receipt -> Ok receipt
                          | Skip error | Defer error -> Error error in
                      (match result, receipt with
                       | Ok _, Ok receipt -> captured := receipt :: !captured
                       | Ok (Core.Epoch_exec.Confirmed _), Error error -> failwith error
                       | _ -> ());
                      Lwt.return result
                    else
                    Consensus_vm_transition.process_tx
                      ~preverify
                      ~circle_mode
                      ~wasm_compute_mode
                      ~program_trust
                      ~object_cost
                      ~backend
                      ~env
                      tx)
                in
                let* result = Octra_core.Epoch_exec.run_core
                  ~reward:(Some reward)
                  ~preverify:(if capture then None else Some preverify)
                  ~backend
                  ~env
                  ~txs
                  ~process_tx in
                match result with
                | Error _ as error -> Lwt.return error
                | Ok execution ->
                  if not capture then Lwt.return_ok { execution; preverify }
                  else
                    let receipts = preverify.Gate.receipts @ List.rev !captured in
                    let selected = List.filter_map (fun tx ->
                      List.find_opt (fun receipt ->
                        receipt.Core.Preverify_receipt.tx_hash = Tx.hash tx) receipts) txs in
                    let preverify = { preverify with Gate.receipts = selected } in
                    let checked = List.filter (fun tx ->
                      tx.Tx.op_type <> Tx.CircleCall
                      || Result.is_ok (Gate.receipt_for_tx preverify tx)) txs in
                    Lwt.return (Result.map (fun () -> { execution; preverify })
                      (Gate.check preverify checked)))
                      end
                end
            end
        end
  in
  {
    run = (fun ~epoch_id ~proposal_id ~expected_prev_root ~preverify
        ~parent_commit ~reward ~env ~txs ->
      Lwt.map (Result.map (fun result -> result.execution))
        (execute ~capture:false ~epoch_id ~proposal_id ~expected_prev_root
          ~preverify ~parent_commit ~reward ~env ~txs));
    prepare = execute ~capture:true;
  }

let preview (deps : deps) runner =
  Consensus_driver_wiring.node_proposal_preview
    Consensus_driver_wiring.{
      chain_id = deps.chain_id;
      ready_state_root_at = deps.ready_state_root_at;
      ready_max_lag = deps.ready_max_lag;
      warn = deps.warn;
      run_preview = (fun request ~reward ~env ->
        runner
          ~epoch_id:(Int64.to_int request.Consensus_proposal.epoch_id)
          ~proposal_id:request.proposal_id
          ~expected_prev_root:(Some request.expected_prev_root)
          ~preverify:request.preverify
          ~parent_commit:request.parent_commit
          ~reward
          ~env
          ~txs:request.txs);
    }

let run deps = preview deps deps.backend.run

let prepare deps = preview deps deps.backend.prepare

let prepare_at ~mode deps epoch =
  if epoch < 0L || epoch > Int64.of_int max_int then
    Error "invalid proposal epoch"
  else
    match mode ~epoch:(Int64.to_int epoch) with
    | Error fault -> Error (Core.Rule_graph.fault_message fault)
    | Ok Core.Rule_graph.Prior -> Ok None
    | Ok Core.Rule_graph.Active -> Ok (Some (prepare deps ~catch_exn:true))