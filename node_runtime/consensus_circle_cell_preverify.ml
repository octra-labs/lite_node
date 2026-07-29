(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Epoch_exec = Octra_core.Epoch_exec
module State_preview = Octra_core.State_preview
module Store_irmin = Octra_core.Store_irmin
module Transaction = Octra_core.Transaction
module Transition = Octra_core.Circle_cell_transition

type runtime = {
  store : Store_irmin.t;
  env : pre_state_root:string -> Epoch_exec.env;
}

let run runtime ~pre_state_hash ~pre_state_root tx =
  if
    not
      (String.equal
         pre_state_hash
         (Octra_core.Preverify_worker.state_hash pre_state_root))
  then
    Lwt.return_error "circle_cell_preverify_state_hash_mismatch"
  else
    let env = runtime.env ~pre_state_root in
    let proposal_id = "circle-cell-" ^ Transaction.hash tx in
    let open Lwt.Syntax in
    Lwt.catch
      (fun () ->
        State_preview.with_preview
          ~base_store:runtime.store
          ~epoch_id:env.epoch_id
          ~proposal_id
          ~expected_prev_root:pre_state_root
          (fun backend ->
            let* () = backend.Epoch_exec.begin_batch () in
            Lwt.finalize
              (fun () ->
                let* plan =
                  Transition.prepare
                    ~store:backend.store
                    ~ledger:backend.ledger
                    ~current_epoch:env.epoch_id
                    tx
                in
                match plan with
                | Error error -> Lwt.return_error error
                | Ok plan ->
                  let* verified = Transition.verify plan in
                  begin
                    match verified with
                    | Error error -> Lwt.return_error error
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
                          Lwt.return_error (error_type ^ ":" ^ reason)
                      end
                  end)
              (fun () ->
                Store_irmin.abort_epoch_batch backend.store;
                Lwt.return_unit)))
      (fun _ -> Lwt.return_error "circle_cell_preverify_exception")