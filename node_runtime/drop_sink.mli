(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

val create : now:(unit -> float) ->
  write:(Octra_core.Drop_record.t list -> (unit, string) result Lwt.t) -> t
val submit : t -> Octra_core.Drop_record.t list -> (unit, string) result
val shutdown : t -> unit Lwt.t
val finish : close:(unit -> unit) -> t -> unit Lwt.t