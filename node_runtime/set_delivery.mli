(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type staged

type network = {
  broadcast : Octra_net.P2p_frame.frame -> unit;
  post : Octra_core.Transaction.t -> (unit, Set_post.failure) result Lwt.t;
}

val proof_fields : Octra_core.Set_fold.proof -> (string * Yojson.Safe.t) list
val mark_fields : ?base:(string * Yojson.Safe.t) list -> epoch:int64 -> chain_id:string -> address:string ->
  state:Octra_core.Set_fold.t -> Octra_core.Set_fold.proof list ->
  ((string * Yojson.Safe.t) list, string) result

val mark_fields_at : ?base:(string * Yojson.Safe.t) list -> store:Octra_core.Store_irmin.t ->
  head:(unit -> Octra_core.Head_manifest.t option) -> epoch:int64 -> state_root:string ->
  chain_id:string -> address:string -> Octra_core.Set_fold.proof list ->
  ((string * Yojson.Safe.t) list, string) result Lwt.t

val sign : signing:Octra_core.Tx_signature.t -> key:string -> Octra_core.Transaction.t -> (Octra_core.Transaction.t, string) result

val stage :
  bft_mode:bool -> Node_rest_facade.runtime -> Octra_core.Ledger.t ->
  Octra_core.Transaction.t -> (staged, string) result

val put : retry:Set_post.retry -> Set_post.t -> staged -> unit

val retry :
  bft_mode:bool -> Node_rest_facade.runtime -> Octra_core.Ledger.t -> network ->
  Octra_core.Transaction.t -> (unit, Set_post.failure) result Lwt.t