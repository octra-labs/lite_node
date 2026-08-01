(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type 'prepared binding =
  | Bound of 'prepared
  | Source_changed
  | Source_invalid of string

type 'artifact verification =
  | Verification_ready of 'artifact
  | Verification_rejected of 'artifact * string
  | Verification_stale

type ('artifact, 'prepared) deps = {
  eligible : Octra_core.Transaction.t -> bool;
  verify :
    Octra_core.Compute_pool.priority ->
    Octra_core.Transaction.t ->
    ('artifact verification, string) result Lwt.t;
  bind :
    Octra_core.Transaction.t ->
    'artifact ->
    'prepared binding Lwt.t;
}

type ('artifact, 'prepared) t

type stats = {
  pending : int;
  queued : int;
  running : int;
  ready : int;
  invalid : int;
}

val create :
  ?now:(unit -> float) ->
  ?max_running:int ->
  ?max_queued:int ->
  ?max_complete:int ->
  ('artifact, 'prepared) deps ->
  ('artifact, 'prepared) t

val admit :
  ('artifact, 'prepared) t ->
  Octra_core.Transaction.t ->
  'prepared Octra_core.Preverify_availability.t

val observe :
  ('artifact, 'prepared) t ->
  Octra_core.Transaction.t ->
  'prepared Octra_core.Preverify_availability.t Lwt.t

val await :
  ('artifact, 'prepared) t ->
  Octra_core.Transaction.t ->
  'prepared Octra_core.Preverify_availability.t Lwt.t

val retain :
  ('artifact, 'prepared) t ->
  (string -> bool) ->
  unit

val stats :
  ('artifact, 'prepared) t ->
  stats