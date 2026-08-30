(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val client_progress_body_cap : int

val chunk_read_limit : string option -> int

val certificate_path :
  data_dir:string ->
  string

val exporter_set :
  unit ->
  (Octra_consensus.C_types.validator_set, string) result

val configured_validator_set :
  unit ->
  (Octra_consensus.C_types.validator_set, string) result

val configured_config_hash :
  unit ->
  (string, string) result

val read_body :
  Cohttp_lwt.Body.t ->
  (string, string) result Lwt.t

val handle_manifest :
  data_dir:string ->
  chain_id:string ->
  config_hash:string ->
  validator_set:Octra_consensus.C_types.validator_set ->
  (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

val handle_chunk :
  data_dir:string ->
  chain_id:string ->
  config_hash:string ->
  validator_set:Octra_consensus.C_types.validator_set ->
  (string * string list) list ->
  (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

val handle :
  data_dir:string ->
  ledger:Octra_core.Ledger.t ->
  tree_ref:Octra_core.Tree.t ref ->
  validator:string ->
  chain_id:string ->
  config_hash:string ->
  validator_set:Octra_consensus.C_types.validator_set ->
  current_epoch:int ref ->
  chaindata:Octra_core.Store_chaindata.t ->
  encrypted_supply:(unit -> Z.t) ->
  Cohttp.Request.t ->
  Cohttp_lwt.Body.t ->
  (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t