(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type validator = string * string

type plan =
  | Genesis_bootstrap
  | Observer_existing
  | Create_joining_account
  | Skip_consensus_account
  | Account_ready

type deps = {
  ledger_empty : unit -> bool;
  observer_mode : bool;
  wallet_addr : string;
  wallet_pub : string;
  wallet_present : unit -> bool;
  consensus_port_configured : unit -> bool;
  validators : unit -> validator list;
  add_account :
    addr:string ->
    pub:string ->
    amount:Z.t ->
    (unit, string) result;
  set_meta : key:string -> value:string -> unit;
  flush_dirty : unit -> unit;
}

type genesis_amounts = {
  circulating : Z.t;
  emission_pool : Z.t;
  supply : Z.t;
  per_validator : Z.t;
}

val plan :
  ledger_empty:bool ->
  observer_mode:bool ->
  wallet_present:bool ->
  consensus_port_configured:bool ->
  plan

val genesis_amounts :
  validator_count:int ->
  genesis_amounts

val run :
  deps ->
  unit