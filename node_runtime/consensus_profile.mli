(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val standard : string

val compat_wire_profile : int

val compat_wire_rules_id : string

val standard_hash :
  chain_id:string ->
  (string -> string option) ->
  string

val hash :
  chain_id:string ->
  epoch:int ->
  (string -> string option) ->
  string

val switch_after :
  chain_id:string ->
  applied_epoch:int ->
  bool

val activation_graph_hash : chain_id:string -> string

val compat_hash :
  (string -> string option) ->
  string

val validate :
  (string -> string option) ->
  (unit, string) result