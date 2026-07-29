(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type effects = {
  abort_ledger : unit -> unit;
  abort_store : unit -> unit;
  abort_history : unit -> unit;
  fatal : string -> unit;
  exit : unit -> unit;
}

val run :
  effects ->
  (unit -> 'a Lwt.t) ->
  'a Lwt.t