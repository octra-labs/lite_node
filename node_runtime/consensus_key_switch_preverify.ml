(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Pool = Consensus_preverify_pool
module Private_ledger = Octra_core.Private_ledger
module Transaction = Octra_core.Transaction

type t =
  (Private_ledger.key_switch_artifact, Private_ledger.prepared) Pool.t

let eligible_with field_policy tx =
  tx.Transaction.op_type = Transaction.KeySwitch
  && not
       (Private_ledger.key_switch_requests_legacy_audit
          ~field_policy
          tx)

let create ~field_policy ledger =
  Pool.create
    ~max_running:Octra_core.Pvac_verify_worker.capacity
    ~max_queued:
      (Octra_core.Resource_lanes.preverify_queue_limit
         Octra_core.Resource_lanes.Fhe)
    {
      eligible = (fun tx -> eligible_with (field_policy ()) tx);
      verify = (fun priority tx ->
        let open Lwt.Syntax in
        let fields = field_policy () in
        let* result =
          Private_ledger.preverify_key_switch_artifact
            ~field_policy:fields
            ~worker_priority:priority
            ledger
            tx
        in
        begin
          match result with
          | Error failure ->
            Lwt.return_error failure.Private_ledger.reason
          | Ok artifact ->
            let* binding =
              Private_ledger.bind_key_switch_artifact
                ~field_policy:(field_policy ())
                ledger
                tx
                artifact
            in
            Lwt.return_ok
              (match binding with
               | Private_ledger.Key_switch_bound _ ->
                 Pool.Verification_ready artifact
               | Private_ledger.Key_switch_source_changed ->
                 Pool.Verification_stale
               | Private_ledger.Key_switch_artifact_invalid failure ->
                 Pool.Verification_rejected
                   (artifact, failure.Private_ledger.reason))
        end);
      bind = (fun tx artifact ->
        let open Lwt.Syntax in
        let* binding =
          Private_ledger.bind_key_switch_artifact
            ~field_policy:(field_policy ())
            ledger
            tx
            artifact
        in
        Lwt.return
          (match binding with
           | Private_ledger.Key_switch_bound prepared -> Pool.Bound prepared
           | Private_ledger.Key_switch_source_changed -> Pool.Source_changed
           | Private_ledger.Key_switch_artifact_invalid failure ->
             Pool.Source_invalid failure.Private_ledger.reason));
    }

let admit = Pool.admit
let observe = Pool.observe
let await = Pool.await
let retain = Pool.retain
let stats = Pool.stats