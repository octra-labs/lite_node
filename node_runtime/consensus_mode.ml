(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  role : Octra_consensus.C_role.t;
  label : string;
  consensus_enabled : bool;
  voting_enabled : bool;
  observer_enabled : bool;
}

let of_inputs ~cli_observer ~env_mode =
  let role =
    Octra_consensus.C_role.of_mode ~cli_observer env_mode
  in
  {
    role;
    label = Octra_consensus.C_role.label role;
    consensus_enabled = Octra_consensus.C_role.consensus_enabled role;
    voting_enabled = Octra_consensus.C_role.voting_enabled role;
    observer_enabled = Octra_consensus.C_role.is_observer role;
  }