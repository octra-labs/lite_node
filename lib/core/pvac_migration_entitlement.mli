(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type entry = {
  address : string;
  source_cipher_hash : string;
  total : int;
  decision : Pvac_legacy_public_replay.decision;
}

type t

val disabled :
  chain_id:string ->
  t

val create :
  chain_id:string ->
  snapshot_epoch:int ->
  state_root:string ->
  activation_epoch:int ->
  entry list ->
  (t, string) result

val load_env :
  chain_id:string ->
  getenv:(string -> string option) ->
  (t, string) result

val source_cipher_hash :
  string ->
  string

val root :
  t ->
  string option

val activation_epoch :
  t ->
  int option

val snapshot_epoch :
  t ->
  int option

val state_root :
  t ->
  string option

val bind_snapshot :
  t ->
  (int -> (string, string) result) ->
  (unit, string) result

val entry_count :
  t ->
  int

val decision :
  t ->
  epoch:int ->
  address:string ->
  cipher:string ->
  Pvac_legacy_public_replay.decision

val find :
  t ->
  epoch:int ->
  address:string ->
  cipher:string ->
  (entry, string) result

val to_yojson :
  t ->
  (Yojson.Safe.t, string) result