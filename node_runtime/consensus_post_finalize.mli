(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type deps = {
  deactivate_gap : unit -> unit;
  set_consensus_finalized : bool -> unit;
  committed_head_epoch : unit -> int;
  sleep : float -> unit Lwt.t;
  read_pre_finalize_root : unit -> string option;
  read_commit_root : unit -> string option Lwt.t;
  read_local_root_raw : unit -> string Lwt.t;
  commit_finality_journal : unit -> unit;
  remove_pending_finalized : epoch:int -> unit;
  apply_timeout_seconds : float;
  fatal_exit : unit -> unit;
}

val short_hex8 : string -> string

val run :
  deps ->
  epoch_id:int64 ->
  proposed_root:string ->
  unit Lwt.t