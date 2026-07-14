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


type deps = {
  source : Consensus_epoch_apply_source.node_deps;
  preflight : Consensus_epoch_apply_preflight.effects;
  epoch_env : Consensus_epoch_apply_env.node_env;
  now : unit -> float;
  staging_size : unit -> int;
  clear_spent_nonces : unit -> unit;
  log_processing :
    epoch_id:int ->
    tx_count:int ->
    deferred:int ->
    elapsed:float ->
    unit;
}

type request = {
  epoch_id : int;
  override_ordered_txs : Octra_core.Transaction.t list option;
  override_receipts_json : string list option;
  consensus_mode : bool;
  elapsed : float;
}

type result = {
  ordered_txs : Octra_core.Transaction.t list;
  epoch_receipts_json : string list;
  epoch_start : float;
  pending_tx_saves : (Octra_core.Transaction.t * int) list ref;
  confirmed_fees : Z.t ref;
  processed_hashes : string list ref;
  pre_state_hash : string;
  pre_consensus_root : string;
  epoch_env : Consensus_epoch_apply_env.node_env;
}

val run : deps -> request -> result Lwt.t