(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type policy = {
  ready_mode : Octra_core.Rule_graph.mode;
  seat_mode : Octra_core.Rule_graph.mode;
  cap_mode : Octra_core.Set_fold.cap_mode;
}

val policy :
  Octra_core.Rule_graph.t ->
  int ->
  (policy, string) result

val resolve :
  Octra_core.Rule_graph.t ->
  chain_id:string ->
  parent:Octra_consensus.C_types.parent_commit option ->
  int ->
  (Octra_core.Epoch_exec.fold_ctx, string) result

val bind :
  Octra_core.Rule_graph.t ->
  chain_id:string ->
  parent:Octra_consensus.C_types.parent_commit option ->
  epoch:int ->
  (int -> (Octra_core.Epoch_exec.fold_ctx, string) result, string) result