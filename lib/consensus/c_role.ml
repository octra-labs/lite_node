(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t =
  | Single
  | Validator
  | Observer

let normalize s =
  String.lowercase_ascii (String.trim s)

let of_mode ?(cli_observer = false) mode =
  match mode with
  | Some s when normalize s = "observer" -> Observer
  | Some s when normalize s = "bft" && cli_observer -> Observer
  | Some s when normalize s = "bft" -> Validator
  | _ when cli_observer -> Observer
  | _ -> Single

let consensus_enabled = function
  | Single -> false
  | Validator | Observer -> true

let voting_enabled = function
  | Validator -> true
  | Single | Observer -> false

let is_observer = function
  | Observer -> true
  | Single | Validator -> false

let label = function
  | Single -> "single"
  | Validator -> "validator"
  | Observer -> "observer"