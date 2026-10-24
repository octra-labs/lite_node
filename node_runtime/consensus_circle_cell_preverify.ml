(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Epoch_exec = Octra_core.Epoch_exec
module Rule_graph = Octra_core.Rule_graph
module State_preview = Octra_core.State_preview
module Store_irmin = Octra_core.Store_irmin
module Transaction = Octra_core.Transaction
module Transition = Octra_core.Circle_cell_transition

type runtime = {
  store : Store_irmin.t;
  ledger : Octra_core.Ledger.t;
  rules : Rule_graph.t;
  env : pre_state_root:string -> Epoch_exec.env;
}

let run runtime ~pre_state_hash ~pre_state_root tx =
  Octra_core.Preverify_worker.cell_check (fun () ->
  if
    not
      (String.equal
         pre_state_hash
         (Octra_core.Preverify_worker.state_hash pre_state_root))
  then
    Lwt.return_error (`Invalid "circle_cell_preverify_state_hash_mismatch")
  else
    let env = runtime.env ~pre_state_root in
    match
      Rule_graph.account_pack runtime.rules ~epoch:env.epoch_id,
      Rule_graph.standard runtime.rules ~epoch:env.epoch_id
    with
    | Error fault, _
    | _, Error fault ->
      Lwt.return_error (`Unavailable (Rule_graph.fault_message fault))
    | Ok account_mode, Ok proof_mode ->
    match Rule_graph.proof_key runtime.rules ~epoch:env.epoch_id with
    | Error fault -> Lwt.return_error (`Unavailable (Rule_graph.fault_message fault))
    | Ok key_mode ->
    let keyed = key_mode = Rule_graph.Active in
    match Rule_graph.math runtime.rules ~epoch:env.epoch_id with
    | Error fault -> Lwt.return_error (`Unavailable (Rule_graph.fault_message fault))
    | Ok math_mode ->
    let math = math_mode = Rule_graph.Active in
    let proposal_id = "circle-cell-" ^ Transaction.hash tx in
    let open Lwt.Syntax in
    let* preview =
        State_preview.with_preview
          ~base_store:runtime.store
          ~base_ledger:runtime.ledger
          ~proof_mode
          ~math
          ~fold:(fun epoch ->
            Result.map
              (fun ctx -> { ctx with Epoch_exec.account_mode; proof_key = key_mode })
              (Epoch_exec.prior_fold epoch))
          ~epoch_id:env.epoch_id
          ~proposal_id
          ~expected_prev_root:pre_state_root
          (fun backend ->
            let* () = backend.Epoch_exec.begin_batch account_mode in
            let* result = Lwt.finalize
              (fun () ->
                let* plan =
                  Transition.prepare
                    ~store:backend.store
                    ~ledger:backend.ledger
                    ~current_epoch:env.epoch_id
                    tx
                in
                match plan with
                | Error error -> Lwt.return_error (`Invalid error)
                | Ok plan ->
                  let* verified =
                    Transition.verify_classified
                      ~keyed ~math
                      ~strict:(proof_mode = Rule_graph.Active)
                      plan
                  in
                  begin
                    match verified with
                    | Error (Octra_core.Pvac_verify_worker.Proof_rejected error) ->
                      Lwt.return_error (`Invalid error)
                    | Error failure ->
                      Lwt.return_error (`Unavailable
                        (Octra_core.Pvac_verify_worker.verification_failure_message failure))
                    | Ok () ->
                      let* applied =
                        Epoch_exec.process_circle_operation_tx
                          ~expected_transition_hash:plan.transition_hash
                          ~backend
                          ~current_epoch:env.epoch_id
                          tx
                      in
                      begin
                        match applied with
                        | Ok _ -> Lwt.return_ok plan.transition_hash
                        | Error (error_type, reason) ->
                          Lwt.return_error (`Invalid (error_type ^ ":" ^ reason))
                      end
                  end)
              (fun () ->
                Store_irmin.abort_epoch_batch backend.store;
                Lwt.return_unit) in
            Lwt.return_ok result) in
    Lwt.return (match preview with
      | Ok result -> result
      | Error reason -> Error (`Unavailable reason)))