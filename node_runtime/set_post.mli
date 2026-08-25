(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type deps = {
  now : unit -> float;
  wait : float -> unit Lwt.t;
  staged : string -> bool;
  landed : Octra_core.Transaction.t -> bool;
  post : Octra_core.Transaction.t -> (unit, string) result Lwt.t;
  warn : string -> unit;
}

type t

val period : float
val create : deps -> t
val put : t -> hash:string -> Octra_core.Transaction.t -> unit
val tick : t -> unit
val stop : t -> unit