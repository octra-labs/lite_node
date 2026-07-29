(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

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

let bounded_int env name ~fallback ~low ~high =
  let value = env.int_value name fallback in
  if value < low || value > high then fallback else value

let bounded_z raw ~fallback ~high =
  let value = z_value raw ~default:fallback in
  if Z.sign value <= 0 || Z.gt value high then fallback else value

let private_limits env =
  {
    max_stealth_defer =
      bounded_int env "OCTRA_STEALTH_MAX_DEFER" ~fallback:30 ~low:0 ~high:10_000;
    max_stealth_per_epoch =
      bounded_int env "OCTRA_STEALTH_MAX_PER_EPOCH" ~fallback:4 ~low:0 ~high:128;
    max_fhe_per_epoch =
      bounded_int env "OCTRA_FHE_MAX_PER_EPOCH" ~fallback:8 ~low:0 ~high:128;
    stealth_inline_verify_allowed =
      bool_flag (env.opt "OCTRA_STEALTH_INLINE_VERIFY");
  }

let consensus_limits env =
  let liveness_floor =
    (Octra_consensus.C_engine.max_timeout_ms / 1000) + 30
  in
  {
    quarantine_mismatch_threshold =
      bounded_int env "OCTRA_QUARANTINE_MISMATCH_THRESHOLD"
        ~fallback:3 ~low:1 ~high:100;
    quarantine_poll_sec =
      float_of_int (bounded_int env "OCTRA_QUARANTINE_POLL_SEC"
        ~fallback:15 ~low:1 ~high:300);
    quarantine_ahead_streak_threshold =
      bounded_int env "OCTRA_QUARANTINE_AHEAD_STREAK_THRESHOLD"
        ~fallback:5 ~low:1 ~high:100;
    quarantine_ahead_grace_epochs =
      bounded_int env "OCTRA_QUARANTINE_AHEAD_GRACE_EPOCHS"
        ~fallback:32 ~low:0 ~high:100_000;
    quarantine_ahead_drift_tolerance =
      bounded_int env "OCTRA_QUARANTINE_AHEAD_DRIFT_TOLERANCE"
        ~fallback:2 ~low:0 ~high:10_000;
    soft_catchup_max_lag =
      bounded_int env "OCTRA_CATCHUP_SOFT_MAX_LAG"
        ~fallback:4 ~low:0 ~high:10_000;
    consensus_liveness_stall_sec =
      float_of_int (bounded_int env "OCTRA_BFT_LIVENESS_STALL_SEC"
        ~fallback:liveness_floor ~low:liveness_floor ~high:3_600);
    bundle_cache_cap =
      bounded_int env "OCTRA_BUNDLE_CACHE_CAP"
        ~fallback:4096 ~low:64 ~high:65_536;
  }

let proposal_limits env ~default_max_ou =
  let max_txs = bounded_int env "OCTRA_BFT_PROPOSAL_MAX_TXS"
    ~fallback:1000 ~low:1 ~high:10_000 in
  let max_bytes = bounded_int env "OCTRA_BFT_PROPOSAL_MAX_BYTES"
    ~fallback:5_000_000 ~low:1024 ~high:(32 * 1024 * 1024) in
  let max_ou =
    bounded_z (env.opt "OCTRA_BFT_PROPOSAL_MAX_OU")
      ~fallback:default_max_ou ~high:default_max_ou
  in
  Consensus_proposal.limits ~max_txs ~max_bytes ~max_ou

let multi_exec_max env =
  bounded_int env "OCTRA_MULTI_EXEC_MAX_CALLS" ~fallback:8 ~low:1 ~high:64