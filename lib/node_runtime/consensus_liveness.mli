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


type state

type sample = {
  height : int64;
  round : int;
  step : string;
  expected : int64;
  now : float;
  source : string;
  stall_sec : float;
  observer : bool;
  voting : bool;
  catchup_active : bool;
  quarantine_active : bool;
  state_attested : bool;
  pending_finalized : bool;
}

type reset = {
  height : int64;
  round : int;
  step : string;
  expected : int64;
  source : string;
  stall_sec : float;
  state_age : float;
  height_age : float;
  resets : int;
}

type result = {
  state : state;
  reset : reset option;
}

val create : now:float -> state

val record : state -> sample -> result