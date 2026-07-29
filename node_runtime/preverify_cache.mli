(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type result = Preverify_submit.result = {
  delta_ok : bool;
  balance_ok : bool;
  sender_enc_snapshot : string;
}

type state =
  | Missing
  | Pending
  | Ready
  | Failed of string

type gate =
  | Ready_gate
  | Failed_gate of string
  | Timeout_gate of string
  | Defer_gate of {
    next_count : int;
    status : string;
  }

val pending_count :
  unit ->
  int

val cache_ttl :
  unit ->
  float

val pending_ceiling :
  unit ->
  float

val configured_max_entries :
  unit ->
  int

val pending_max :
  unit ->
  int

val has_capacity :
  unit ->
  bool

val start_task :
  (unit -> result Lwt.t) ->
  result Lwt.t

val insert_with_cap :
  string ->
  result Lwt.t ->
  unit

val remove :
  string ->
  unit

val state :
  string ->
  state

val gate :
  state:state ->
  defer_count:int ->
  max_defer:int ->
  gate

val ready_result :
  string ->
  sender_enc_snapshot:string ->
  result option

val retain :
  (string -> bool) ->
  unit

val prune :
  ?ttl:float ->
  ?max_entries:int ->
  unit ->
  unit