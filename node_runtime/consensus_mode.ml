(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


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