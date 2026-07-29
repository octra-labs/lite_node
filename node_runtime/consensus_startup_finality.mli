(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Finality_log = Octra_consensus.Finality_log

type deps = {
  head : unit -> int;
  last_finality : unit -> Finality_log.entry option;
  mark_quarantine : string -> unit;
}

type node_runtime = {
  committed_head_epoch : unit -> int;
  current_epoch : int ref;
  last_finality : unit -> Finality_log.entry option;
  drop_uncommitted_after : int -> (int, string) result;
  mark_quarantine : string -> unit;
}

val handle_startup : deps -> unit

val node_normalizer :
  node_runtime ->
  source:string ->
  unit

val node_deps :
  node_runtime ->
  deps

val run_node_startup :
  node_runtime ->
  unit