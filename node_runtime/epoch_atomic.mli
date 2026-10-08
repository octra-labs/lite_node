(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type effects = {
  abort_ledger : unit -> unit;
  abort_store : unit -> unit;
  abort_history : unit -> unit;
  fatal : string -> unit;
  exit : exn -> unit;
}

val run :
  effects ->
  (unit -> 'a Lwt.t) ->
  'a Lwt.t

val exit_store : ?code:int -> Octra_core.Store_irmin.t -> unit -> 'a

val run_store :
  ?fatal:(string -> unit) ->
  store:Octra_core.Store_irmin.t ->
  ledger:Octra_core.Ledger.t ->
  chaindata:Octra_core.Store_chaindata.t ->
  (unit -> 'a Lwt.t) -> 'a Lwt.t