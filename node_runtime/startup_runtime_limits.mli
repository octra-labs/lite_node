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


type env = {
  int_value : string -> int -> int;
  opt : string -> string option;
}

type private_limits = {
  max_stealth_defer : int;
  max_stealth_per_epoch : int;
  max_fhe_per_epoch : int;
  stealth_inline_verify_allowed : bool;
}

type consensus_limits = {
  quarantine_mismatch_threshold : int;
  quarantine_poll_sec : float;
  quarantine_ahead_streak_threshold : int;
  quarantine_ahead_grace_epochs : int;
  quarantine_ahead_drift_tolerance : int;
  soft_catchup_max_lag : int;
  consensus_liveness_stall_sec : float;
  bundle_cache_cap : int;
}

val bool_flag :
  string option ->
  bool

val z_value :
  string option ->
  default:Z.t ->
  Z.t

val private_limits :
  env ->
  private_limits

val consensus_limits :
  env ->
  consensus_limits

val proposal_limits :
  env ->
  default_max_ou:Z.t ->
  Consensus_proposal.limits