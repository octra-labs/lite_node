(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let int_value name default =
  match Sys.getenv_opt name with
  | Some s ->
    begin
      try int_of_string s with _ -> default
    end
  | None -> default

let flag name =
  match Sys.getenv_opt name with
  | Some s ->
    let value = String.lowercase_ascii (String.trim s) in
    value = "1" || value = "true" || value = "yes"
  | None -> false