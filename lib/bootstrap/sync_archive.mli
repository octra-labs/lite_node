(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

val path : t -> string -> string
val owns : t -> string -> bool
val mark_stage : t -> string -> unit
val marked_stage : t -> string -> bool
val finish_publish : ?sync:(Unix.file_descr -> unit) -> t -> string -> unit
val publish_stage : ?sync:(Unix.file_descr -> unit) -> t -> string -> unit
val run : string -> (t -> 'a) -> ('a, string) result
val run_lwt :
  string ->
  (t -> ('a, string) result Lwt.t) ->
  ('a, string) result Lwt.t