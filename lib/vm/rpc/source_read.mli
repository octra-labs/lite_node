(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type outcome = Checked | Different | Pending | Failed
type input = {
  stored : string option;
  record : Octra_core.Store_chaindata.program_record;
}
type state = {
  generation : int;
  active : string list;
  cache : (string * outcome) list;
  closed : bool;
}
type message = Ask of string | Finish of int * string * outcome | Stop
type effect = Reply of outcome | Check of int * string
val capacity : int
val cache_limit : int
val initial : state
val step : state -> message -> state * effect
val identity : input -> string
type t
val create : ?check:(input -> outcome Lwt.t) -> unit -> t
val query : t -> input -> outcome Lwt.t
val shutdown : t -> unit