(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  role : Octra_consensus.C_role.t;
  label : string;
  consensus_enabled : bool;
  voting_enabled : bool;
  observer_enabled : bool;
}

val of_inputs : cli_observer:bool -> env_mode:string option -> t

val publisher : t -> t

val recovery : t -> t

val with_need : Sync_need.t option -> t -> t