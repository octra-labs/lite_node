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

val create :
  capacity:int ->
  required_limit:int ->
  speculative_limit:int ->
  required_burst:int ->
  unit ->
  t

val capacity : t -> int

val run_async :
  t ->
  priority ->
  (unit -> 'a Lwt.t) ->
  'a option Lwt.t

val run_threaded :
  t ->
  priority ->
  ('a -> 'b) ->
  'a ->
  'b option Lwt.t

val run_sync :
  t ->
  priority ->
  (unit -> 'a) ->
  'a option

val try_run_sync :
  t ->
  priority ->
  (unit -> 'a) ->
  'a option

val stats :
  t ->
  stats