(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type source

val create :
  chain_id:string ->
  data_dir:string ->
  chaindata:Octra_core.Store_chaindata.t ->
  (string -> string option) ->
  (source, string) result

val required :
  source ->
  int64 ->
  bool

val load :
  source ->
  epoch_id:int64 ->
  (Octra_consensus.C_types.parent_commit option, string) result

val verify :
  source ->
  epoch_id:int64 ->
  Octra_consensus.C_types.parent_commit option ->
  (unit, string) result