(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type deploy = {
  address : string;
  code_hash : string;
  bytecode_b64 : string;
  owner : string;
  ctype : string;
  admission : string;
  storage : (string, string) Hashtbl.t;
}

type upgrade = {
  address : string;
  expected_code_hash : string;
  code_hash : string;
  bytecode_b64 : string;
  owner : string;
  ctype : string;
  admission : string;
  version : string;
}

type snapshot
type t

val create : unit -> t
val snapshot : t -> snapshot
val restore : t -> snapshot -> unit
val discard : t -> unit
val add_deploy : t -> deploy -> unit
val find_deploy : t -> string -> deploy option
val has_deploy : t -> string -> bool
val add_upgrade : t -> upgrade -> unit
val find_upgrade : t -> string -> upgrade option
val has_upgrade : t -> string -> bool
val load_storage : t -> string -> (string, string) Hashtbl.t option
val checkout_storage :
  t ->
  string ->
  fallback:(unit -> (string, string) Hashtbl.t) ->
  (string, string) Hashtbl.t
val deploys : t -> deploy list
val upgrades : t -> upgrade list
val storage_entries : t -> (string * (string, string) Hashtbl.t) list