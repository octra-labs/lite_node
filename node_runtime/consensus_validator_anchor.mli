(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type source = {
  getenv : string -> string option;
  chain_id : string;
  current_height : unit -> int64;
  active_raw : unit -> string option;
  pending_raw : unit -> string option;
}

val expected_set :
  source ->
  epoch:int64 ->
  (Octra_consensus.C_types.validator_set, string) result