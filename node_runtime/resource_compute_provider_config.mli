(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type accelerator =
  | Cpu
  | Cuda
  | Metal
  | Rocm

type limits = {
  accelerator : accelerator;
  lanes : int;
  memory_bytes : int64;
  max_request_bytes : int;
  max_response_bytes : int;
  timeout_seconds : int;
}

type t =
  | Disabled
  | Enabled of limits

val accelerator_name : accelerator -> string
val of_inputs :
  cli_enabled:bool ->
  getenv:(string -> string option) ->
  (t, string) result