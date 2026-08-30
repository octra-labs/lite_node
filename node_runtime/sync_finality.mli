(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type outcome =
  | Missing
  | Advanced
  | Current
  | Seeded

type fault =
  | Root of string
  | Journal of string

val reason : fault -> string

val local_head :
  raw_to_hex:(string -> string) ->
  head:int ->
  cached:Octra_core.Head_manifest.t option ->
  epoch_root:(int -> string option) ->
  string option * int64 option

val seed :
  data_dir:string ->
  chain_id:string ->
  floor:(Octra_core.History_floor.t option, string) result ->
  head:int ->
  root:string option ->
  txid:int64 option ->
  trusted:(unit -> (Octra_consensus.C_types.validator_set, string) result) ->
  exporters:(unit -> (Octra_consensus.C_types.validator_set, string) result) ->
  active:Octra_consensus.C_types.validator_set ->
  (outcome, fault) result