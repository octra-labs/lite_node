(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type strict_deps = {
  window : int;
  first_epoch : unit -> int;
  last_epoch : unit -> int option;
  status_at : int -> Octra_core.Store_chaindata.epoch_index_status option;
  exit_fatal : unit -> unit;
}

type reindex_marker_deps = {
  marker_path : string;
  exists : string -> bool;
  exit_fatal : unit -> unit;
}

type startup_checks_deps = {
  int_value : string -> int -> int;
  first_epoch : unit -> int;
  last_epoch : unit -> int option;
  status_at : int -> Octra_core.Store_chaindata.epoch_index_status option;
  marker_path : string;
  marker_exists : string -> bool;
  irmin_stealth_counter : unit -> int64;
  chaindata_next_txid : unit -> int64;
  exit_fatal : unit -> unit;
}

type window =
  | No_epochs
  | Window of { from_epoch : int; to_epoch : int }

val check_window :
  window:int -> first_epoch:int -> last_epoch:int option -> window

val strict_failures :
  from_epoch:int ->
  to_epoch:int ->
  status_at:(int -> Octra_core.Store_chaindata.epoch_index_status option) ->
  (int * Octra_core.Store_chaindata.epoch_index_status option) list

val run_strict_verify : strict_deps -> unit
val run_reindex_marker_guard : reindex_marker_deps -> unit

val log_stealth_counter :
  irmin_stealth_counter:int64 -> chaindata_next_txid:int64 -> unit

val run_startup_checks : startup_checks_deps -> unit