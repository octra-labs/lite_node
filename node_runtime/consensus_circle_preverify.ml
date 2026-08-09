(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Circle_exec = Octra_circle_runtime.Circle_exec
module Epoch_exec = Octra_core.Epoch_exec
module Preverify_receipt = Octra_core.Preverify_receipt
module Preverify_worker = Octra_core.Preverify_worker
module Rule_graph = Octra_core.Rule_graph
module State_preview = Octra_core.State_preview
module Store_irmin = Octra_core.Store_irmin
module Transaction = Octra_core.Transaction

type runtime = {
  store : Octra_core.Store_irmin.t;
  ledger : Octra_core.Ledger.t;
  program_trust : Octra_vm.Program_trust.t;
  rules : Rule_graph.t;
  env : pre_state_root:string -> Epoch_exec.env;
}

let circle_state pre_state_hash (binding : Circle_exec.hfhe_binding) =
  Preverify_receipt.{
    snapshot_hash = pre_state_hash;
    circle_id = binding.circle_id;
    code_hash = binding.code_hash;
    stable_root = binding.stable_root;
    public_reads_hash = binding.public_reads_hash;
    context_hash = binding.context_hash;
    transcript = binding.transcript;
  }

let run runtime ~pre_state_hash ~pre_state_root tx =
  if
    not
      (String.equal
         pre_state_hash
         (Preverify_worker.state_hash pre_state_root))
  then
    Lwt.return_error (`Invalid "circle_preverify_state_hash_mismatch")
  else
    let env = runtime.env ~pre_state_root in
    match Rule_graph.circle runtime.rules ~epoch:env.epoch_id with
    | Error fault ->
      Lwt.return_error (`Unavailable (Rule_graph.fault_message fault))
    | Ok circle_mode ->
    match Rule_graph.wasm_compute runtime.rules ~epoch:env.epoch_id with
    | Error fault ->
      Lwt.return_error (`Unavailable (Rule_graph.fault_message fault))
    | Ok wasm_compute_mode ->
    let proposal_id = "circle-" ^ Transaction.hash tx in
    let open Lwt.Syntax in
    let* result = Lwt.catch
      (fun () ->
        let* preview =
          State_preview.with_preview
            ~base_store:runtime.store
            ~base_ledger:runtime.ledger
            ~epoch_id:env.epoch_id
            ~proposal_id
            ~expected_prev_root:pre_state_root
            (fun backend ->
              let* () = backend.Epoch_exec.begin_batch () in
              Lwt.finalize
                (fun () ->
                  let* result =
                    Consensus_vm_transition.preverify_circle
                      ~circle_mode
                      ~wasm_compute_mode
                      ~backend
                      ~env
                      ~program_trust:runtime.program_trust
                      tx
                  in
                  Lwt.return_ok result)
                (fun () ->
                  Store_irmin.abort_epoch_batch backend.store;
                  Lwt.return_unit))
        in
        match preview with
        | Ok (Ok binding) -> Lwt.return_ok binding
        | Ok (Error reason) -> Lwt.return_error (`Invalid reason)
        | Error reason -> Lwt.return_error (`Unavailable reason))
      (fun error ->
        match error with
        | Circle_exec.Execution_unavailable reason ->
          Lwt.return_error (`Unavailable reason)
        | _ ->
          Lwt.return_error (`Unavailable "circle_preverify_exception"))
    in
    match result with
    | Ok binding ->
      Lwt.return_ok (circle_state pre_state_hash binding)
    | Error error ->
      Lwt.return_error error