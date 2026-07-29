(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type kind =
  | Int
  | Bool
  | String
  | Bytes
  | Bytes32
  | U64
  | U128
  | U256
  | Addr
  | Cipher
  | PubKey
  | Unknown

type entry = {
  target : int;
  mem : (int * kind) list;
  effects : string list;
}

type call = {
  owner : int;
  pc : int;
  target : int;
  kind : kind;
}

type capability =
  | View
  | Storage_read
  | Storage_write
  | Transfer
  | Deploy
  | Fhe

type xcall = {
  pc : int;
  method_name : string;
  inputs : kind list;
  output : kind;
  capabilities : capability list;
}

type facts = {
  root : (int * kind) list;
  storage : (string * kind) list;
  entries : entry list;
  calls : call list;
  xcalls : xcall list;
}

type error

val empty_facts : facts
val kind_name : kind -> string
val kind_of_name : string -> kind option
val capability_name : capability -> string
val capability_of_name : string -> capability option
val facts_hash : facts -> string
val check :
  ?max_steps:int ->
  ?facts:facts ->
  Contract_vm.instr array ->
  (unit, error) result
val error_message : error -> string