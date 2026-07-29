(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t =
  | Legacy
  | Recoverable

let env_name = "OCTRA_PRIVATE_RESULT_ACTIVATION_EPOCH"

let activation_epoch_of getenv =
  match getenv env_name with
  | None | Some "" -> Ok 0
  | Some raw ->
    try
      let epoch = int_of_string raw in
      if epoch < 0 then
        Error (env_name ^ " must be nonnegative")
      else
        Ok epoch
    with _ ->
      Error (env_name ^ " must be an integer")

let for_epoch ~activation_epoch epoch =
  if epoch < activation_epoch then Legacy else Recoverable