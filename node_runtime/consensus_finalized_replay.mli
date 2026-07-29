(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C_types = Octra_consensus.C_types

type deps = {
  current_epoch : unit -> int;
  catchup_active : unit -> bool;
  quarantine_active : unit -> bool;
  find_finalized : int -> C_types.finalize option;
  read_local_root_raw : unit -> string Lwt.t;
  apply_finalized : C_types.finalize -> unit Lwt.t;
}

type node_deps = {
  current_epoch : unit -> int;
  catchup_active : unit -> bool;
  quarantine_active : unit -> bool;
  finality : Consensus_finality_state.callbacks;
  read_local_root_raw : unit -> string Lwt.t;
  apply_finalized : C_types.finalize -> unit Lwt.t;
}

type runner = {
  drain_pending : unit -> unit Lwt.t;
  replay_stashed_while_safe : source:string -> unit Lwt.t;
}

val drain :
  deps ->
  unit Lwt.t

val replay_while_safe :
  deps ->
  source:string ->
  unit Lwt.t

val node_deps :
  node_deps ->
  deps

val node_runner :
  node_deps ->
  runner