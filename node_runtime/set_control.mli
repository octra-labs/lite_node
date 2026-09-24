(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val eligible :
  mode:Octra_core.Rule_graph.mode ->
  head:int64 ->
  bonded_epoch:int64 ->
  snapshot:(Status_read_rpc.enrollment_snapshot, string) result ->
  Octra_core.Transaction.t ->
  Set_post.eligibility

val bonded :
  control:Octra_core.Validator_control.t ->
  address:string ->
  pubkey:string ->
  (Status_read_rpc.enrollment_snapshot, string) result ->
  (bool, string) result