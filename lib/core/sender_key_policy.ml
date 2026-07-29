(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type decision =
  | Keep
  | Register of string

let env_name =
  Private_result_policy.env_name

let activation_epoch_of getenv =
  Result.map
    (fun epoch -> Some epoch)
    (Private_result_policy.activation_epoch_of getenv)

let activation_epoch_exn getenv =
  match activation_epoch_of getenv with
  | Ok value -> value
  | Error error -> failwith error

let decide ~activation_epoch ~epoch ~stored ~carried =
  match activation_epoch with
  | Some activation when epoch >= activation ->
    begin
      match stored, carried with
      | None, Some key -> Register key
      | _ -> Keep
    end
  | _ -> Keep