(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type wallet = {
  address : string;
  pub : string;
}

type store_deps = {
  data_dir : string;
  store : Octra_core.Store_irmin.t;
  exit_fatal : unit -> unit;
}

type deps = {
  data_dir : string;
  store : Octra_core.Store_irmin.t;
  ledger : Octra_core.Ledger.t;
  chaindata : Octra_core.Store_chaindata.t;
  total_tx_count : int ref;
  observer_mode : bool;
  wallet : wallet;
  consensus_mode : bool;
  voting_consensus_mode : bool;
  consensus_port_configured : unit -> bool;
  validators : unit -> (string * string) list;
  int_value : string -> int -> int;
  env : string -> string option;
  exit_fatal : unit -> unit;
}

val meta_int :
  default:int ->
  string option ->
  int

val last_epoch_id :
  Octra_core.Epochlog.epoch_header option ->
  int option

val last_epoch_or :
  default:int ->
  Octra_core.Epochlog.epoch_header option ->
  int

val recovery_override_error :
  consensus_mode:bool ->
  skip_recovery:bool ->
  skip_reconcile:bool ->
  string option

val run_s :
  ?timeout:float ->
  'a Lwt.t ->
  'a

val run_store :
  store_deps ->
  unit

val run_node :
  deps ->
  int