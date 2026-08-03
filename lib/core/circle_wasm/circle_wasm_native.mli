(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type run_error =
  | Rejected of string
  | Unavailable of string

val max_input_bytes : int

val error_message : run_error -> string

val run_json_classified : string -> (string, run_error) result

val run_json : string -> (string, string) result