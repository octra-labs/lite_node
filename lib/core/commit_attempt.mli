(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type prepared = {
  commit_id : string;
  planned_txid_hi : int64;
  planned_state_root : string;
}

type t

val read : epoch:int -> generation:int -> Commit_journal.record list -> (t, string) result
val active : t -> (prepared option, string) result
val retire : t -> (string list, string) result
val initial : Commit_journal.record list -> (string list, string) result