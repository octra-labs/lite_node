(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type policy = {
  live_mode : Octra_core.Rule_graph.mode;
  seat_mode : Octra_core.Rule_graph.mode;
  open_mode : Octra_core.Rule_graph.mode;
  account_mode : Octra_core.Rule_graph.mode;
  cap_mode : Octra_core.Set_fold.cap_mode;
}

val start : Octra_core.Rule_graph.t -> int64
val profile_start : Octra_core.Rule_graph.t -> int64

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