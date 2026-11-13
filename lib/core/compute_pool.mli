(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type priority =
  | Required
  | Speculative

type stats = {
  active : int;
  required_waiting : int;
  speculative_waiting : int;
  required_streak : int;
}

type t
type reservation
type reservation_state = Available | Borrowed | Closing | Released
type reservation_message = Borrow | Settle | Close
type reservation_effect = Grant | Deny | Free

val reservation_step :
  reservation_state -> reservation_message -> reservation_state * reservation_effect list

val create :
  ?memory:Compute_memory.t ->
  ?reserved_limit:int ->
  capacity:int ->
  required_limit:int ->
  speculative_limit:int ->
  required_burst:int ->
  unit ->
  t

val capacity : t -> int

val reserve : memory:Compute_memory.t -> bytes:int -> t -> reservation option
val close_reservation : reservation -> unit

val run_async :
  ?bytes:int ->
  ?reservation:reservation ->
  ?running_memory:Compute_memory.t ->
  t ->
  priority ->
  (unit -> 'a Lwt.t) ->
  'a option Lwt.t

val run_threaded :
  ?bytes:int ->
  t ->
  priority ->
  ('a -> 'b) ->
  'a ->
  'b option Lwt.t

val run_sync :
  ?bytes:int ->
  t ->
  priority ->
  (unit -> 'a) ->
  'a option

val try_run_sync :
  ?bytes:int ->
  t ->
  priority ->
  (unit -> 'a) ->
  'a option

val stats :
  t ->
  stats