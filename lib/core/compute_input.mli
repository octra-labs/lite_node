(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type root = private {
  chain_id : string;
  epoch : int64;
  state : string;
  validators : Octra_consensus.C_types.validator_set;
  tree : Store_irmin.Store.tree;
}

type plan = private {call : root; model : root; session : string list; fee : Z.t}

val window : int
val input_path : string -> string list
val session_path : Octra_consensus.Resource_compute_certificate.request -> string list
val snapshot : Store_irmin.t -> root -> Store_irmin.read_snapshot
val remember : Store_irmin.t -> epoch:int -> state:string -> Octra_consensus.C_types.parent_commit -> unit Lwt.t
val retained : Store_irmin.t -> int64 -> root option Lwt.t
val retained_set : ?snapshot:Store_irmin.read_snapshot -> Store_irmin.t -> int64 ->
  Set_fold.mark_set option Lwt.t
val prepare : Store_irmin.t -> max_fee:Z.t -> header:Octra_consensus.Resource_compute_protocol.header -> (plan, string) result Lwt.t
val commit : Store_irmin.t -> string -> plan -> unit Lwt.t
val release : Store_irmin.t -> string -> unit Lwt.t
val read : Store_irmin.t -> header:Octra_consensus.Resource_compute_protocol.header -> ((root * root), string) result Lwt.t