(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type run_error =
  | Rejected of string
  | Unavailable of string

val max_input_bytes : int

val max_call_input_bytes : int

val error_message : run_error -> string

val run_json_classified : ?optional:bool -> string -> (string, run_error) result

val run_call : (string -> string Lwt.t) -> string -> (string, run_error) result Lwt.t

val run_json : string -> (string, string) result

type lease

val lease_new : unit -> lease
val lease_hold : lease -> string -> int
val lease_close : lease -> unit

type build

val build_new : unit -> build
val build_begin : build -> string -> int
val build_step : build -> string -> int * string
val build_close : build -> unit