(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type result = Preverify_submit.result = {
  delta_ok : bool;
  balance_ok : bool;
  sender_enc_snapshot : string;
}

type task_result = Preverify_submit.task_result =
  | Checked of result
  | Unavailable of Tx_view.preverify_unavailable

type state =
  | Missing
  | Pending
  | Ready
  | Unavailable_state of Tx_view.preverify_unavailable
  | Failed of string

type gate =
  | Ready_gate
  | Failed_gate of string
  | Timeout_gate of string
  | Unavailable_gate of {
    next_count : int;
    reason : Tx_view.preverify_unavailable;
  }
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
  string ->
  (unit -> task_result Lwt.t) ->
  Preverify_submit.admit

val insert_with_cap :
  string ->
  task_result Lwt.t ->
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