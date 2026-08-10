(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Pool = Consensus_preverify_pool
module Private_ledger = Octra_core.Private_ledger
module Transaction = Octra_core.Transaction

type t =
  (Private_ledger.private_artifact, Private_ledger.prepared) Pool.t

let eligible tx =
  match tx.Transaction.op_type with
  | Transaction.EncryptOp
  | Transaction.DecryptOp
  | Transaction.StealthOp
  | Transaction.ClaimOp -> true
  | _ -> false

let create ~field_policy ~result_policy ledger =
  Pool.create
    ~max_running:Octra_core.Pvac_verify_worker.capacity
    ~max_queued:
      (Octra_core.Resource_lanes.preverify_queue_limit
         Octra_core.Resource_lanes.Pvac)
    {
      eligible;
      verify = (fun priority tx ->
        let open Lwt.Syntax in
        let fields = field_policy () in
        let policy = result_policy () in
        let* result =
          Private_ledger.preverify_private_artifact
            ~field_policy:fields
            ~worker_priority:priority
            ~result_policy:policy
            ledger
            tx
        in
        begin
          match result with
          | Error failure -> Lwt.return_error failure.Private_ledger.reason
          | Ok artifact ->
            let* binding =
              Private_ledger.bind_private_artifact
                ~field_policy:(field_policy ())
                ~result_policy:(result_policy ())
                ledger
                tx
                artifact
            in
            Lwt.return_ok
              (match binding with
               | Private_ledger.Private_bound _ ->
                 Pool.Verification_ready artifact
               | Private_ledger.Private_source_changed ->
                 Pool.Verification_stale
               | Private_ledger.Private_artifact_invalid rejection ->
                 Pool.Verification_rejected
                   (artifact,
                    rejection.Private_ledger.private_preverify_reason))
        end);
      bind = (fun tx artifact ->
        let open Lwt.Syntax in
        let* binding =
          Private_ledger.bind_private_artifact
            ~field_policy:(field_policy ())
            ~result_policy:(result_policy ())
            ledger
            tx
            artifact
        in
        Lwt.return
          (match binding with
           | Private_ledger.Private_bound prepared -> Pool.Bound prepared
           | Private_ledger.Private_source_changed -> Pool.Source_changed
           | Private_ledger.Private_artifact_invalid rejection ->
             Pool.Source_invalid
               rejection.Private_ledger.private_preverify_reason));
    }

let admit = Pool.admit
let observe = Pool.observe
let await = Pool.await
let retain = Pool.retain
let stats = Pool.stats