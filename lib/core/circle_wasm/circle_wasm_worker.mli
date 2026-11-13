(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val serve : unit -> unit
val check : unit -> (unit, string) result Lwt.t
val frames : int
val memory_stats : unit -> Compute_memory.stats
val inbox_stats : unit -> Compute_memory.stats
val run : ?call:(string -> string Lwt.t) -> ?optional:bool -> string ->
  (string, Circle_wasm_native.run_error) result Lwt.t