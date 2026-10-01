(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type staged

type network = {
  broadcast : Octra_net.P2p_frame.frame -> unit;
  post : Octra_core.Transaction.t -> (unit, Set_post.failure) result Lwt.t;
}

val stage :
  bft_mode:bool -> Node_rest_facade.runtime -> Octra_core.Ledger.t ->
  Octra_core.Transaction.t -> (staged, string) result

val put : retry:Set_post.retry -> Set_post.t -> staged -> unit

val retry :
  bft_mode:bool -> Node_rest_facade.runtime -> Octra_core.Ledger.t -> network ->
  Octra_core.Transaction.t -> (unit, Set_post.failure) result Lwt.t