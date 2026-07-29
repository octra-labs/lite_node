(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type transfer = {
  from_addr : string;
  to_addr : string;
  amount : Z.t;
}

type snapshot

type t

val create :
  balance:(string -> Z.t) ->
  t

val effective :
  t ->
  string ->
  Z.t

val transfer :
  t ->
  from_addr:string ->
  to_addr:string ->
  amount:Z.t ->
  bool

val apply :
  t ->
  Call_plan.value_effect ->
  bool

val discard :
  t ->
  unit

val commit :
  t ->
  debit:(string -> Z.t -> (unit, string) result) ->
  credit:(string -> Z.t -> (unit, string) result) ->
  (unit, string) result

val snapshot :
  t ->
  snapshot

val restore :
  t ->
  snapshot ->
  unit