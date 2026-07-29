(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t =
  | Detached
  | Transport_bound
  | Relay_witnessed

let string_of_t = function
  | Detached -> "detached"
  | Transport_bound -> "transport_bound"
  | Relay_witnessed -> "relay_witnessed"

let of_string = function
  | "detached" -> Ok Detached
  | "transport_bound" -> Ok Transport_bound
  | "relay_witnessed" -> Ok Relay_witnessed
  | other -> Error ("unknown hfhe receipt class: " ^ other)