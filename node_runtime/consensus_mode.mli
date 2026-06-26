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

val of_inputs : cli_observer:bool -> env_mode:string option -> t