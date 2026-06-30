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

let bool_flag = function
  | Some "1" | Some "true" -> true
  | _ -> false

let z_value raw ~default =
  match raw with
  | Some s -> (try Z.of_string s with _ -> default)
  | None -> default

let private_limits env =
  {
    max_stealth_defer = env.int_value "OCTRA_STEALTH_MAX_DEFER" 30;
    max_stealth_per_epoch = env.int_value "OCTRA_STEALTH_MAX_PER_EPOCH" 4;
    max_fhe_per_epoch = env.int_value "OCTRA_FHE_MAX_PER_EPOCH" 8;
    stealth_inline_verify_allowed =
      bool_flag (env.opt "OCTRA_STEALTH_INLINE_VERIFY");
  }

let consensus_limits env =
  {
    quarantine_mismatch_threshold =
      env.int_value "OCTRA_QUARANTINE_MISMATCH_THRESHOLD" 3;
    quarantine_poll_sec =
      float_of_int (env.int_value "OCTRA_QUARANTINE_POLL_SEC" 15);
    quarantine_ahead_streak_threshold =
      env.int_value "OCTRA_QUARANTINE_AHEAD_STREAK_THRESHOLD" 5;
    quarantine_ahead_grace_epochs =
      env.int_value "OCTRA_QUARANTINE_AHEAD_GRACE_EPOCHS" 32;
    quarantine_ahead_drift_tolerance =
      env.int_value "OCTRA_QUARANTINE_AHEAD_DRIFT_TOLERANCE" 2;
    soft_catchup_max_lag =
      env.int_value "OCTRA_CATCHUP_SOFT_MAX_LAG" 4;
    consensus_liveness_stall_sec =
      float_of_int (env.int_value "OCTRA_BFT_LIVENESS_STALL_SEC" 90);
    bundle_cache_cap =
      env.int_value "OCTRA_BUNDLE_CACHE_CAP" 4096;
  }

let proposal_limits env ~default_max_ou =
  let max_txs = env.int_value "OCTRA_BFT_PROPOSAL_MAX_TXS" 1000 in
  let max_bytes = env.int_value "OCTRA_BFT_PROPOSAL_MAX_BYTES" 5_000_000 in
  let max_ou =
    z_value (env.opt "OCTRA_BFT_PROPOSAL_MAX_OU") ~default:default_max_ou
  in
  Consensus_proposal.limits ~max_txs ~max_bytes ~max_ou