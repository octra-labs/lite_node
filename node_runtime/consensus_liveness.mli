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

type driver_snapshot = {
  height : int64;
  round : int;
  step : string;
}

val create : now:float -> state

val record : state -> sample -> result

val step_label : Octra_consensus.C_types.round_step -> string

val driver_snapshot :
  Octra_consensus.C_driver.t ->
  driver_snapshot

val record_snapshot :
  state ->
  driver_snapshot ->
  expected:int64 ->
  now:float ->
  source:string ->
  stall_sec:float ->
  observer:bool ->
  voting:bool ->
  catchup_active:bool ->
  quarantine_active:bool ->
  state_attested:bool ->
  pending_finalized:bool ->
  result