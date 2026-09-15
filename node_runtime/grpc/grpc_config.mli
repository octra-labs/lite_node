(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  host : string;
  port : int;
  max_request_bytes : int;
  submit_bytes : int option;
  max_response_bytes : int;
  max_streams : int;
  default_deadline_s : float;
  max_deadline_s : float;
}

type setting =
  | Disabled
  | Enabled of t

val of_env :
  (string -> string option) ->
  (setting, string) result