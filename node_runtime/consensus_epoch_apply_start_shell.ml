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


module Env = Consensus_epoch_apply_env
module Preflight = Consensus_epoch_apply_preflight
module Source = Consensus_epoch_apply_source
module Transaction = Octra_core.Transaction

type deps = {
  source : Source.node_deps;
  preflight : Preflight.effects;
  epoch_env : Env.node_env;
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
  override_ordered_txs : Transaction.t list option;
  override_receipts_json : string list option;
  consensus_mode : bool;
  elapsed : float;
}

type result = {
  ordered_txs : Transaction.t list;
  epoch_receipts_json : string list;
  epoch_start : float;
  pending_tx_saves : (Transaction.t * int) list ref;
  confirmed_fees : Z.t ref;
  processed_hashes : string list ref;
  pre_state_hash : string;
  pre_consensus_root : string;
  epoch_env : Env.node_env;
}

let source_request (request : request) =
  Source.{
    epoch_id = request.epoch_id;
    override_ordered_txs = request.override_ordered_txs;
    override_receipts_json = request.override_receipts_json;
    consensus_mode = request.consensus_mode;
  }

let run deps request =
  let ordered_txs, epoch_receipts_json =
    Source.run_node deps.source (source_request request)
  in
  let epoch_start = deps.now () in
  let pending_tx_saves : (Transaction.t * int) list ref = ref [] in
  let confirmed_fees = ref Z.zero in
  let processed_hashes = ref [] in
  let deferred = max 0 (deps.staging_size () - List.length ordered_txs) in
  deps.log_processing
    ~epoch_id:request.epoch_id
    ~tx_count:(List.length ordered_txs)
    ~deferred
    ~elapsed:request.elapsed;
  deps.clear_spent_nonces ();
  let open Lwt.Syntax in
  let* preflight =
    Preflight.run deps.preflight { Preflight.epoch_id = request.epoch_id }
  in
  let pre_state = preflight.Preflight.pre_state in
  Lwt.return {
    ordered_txs;
    epoch_receipts_json;
    epoch_start;
    pending_tx_saves;
    confirmed_fees;
    processed_hashes;
    pre_state_hash = pre_state.Env.ledger_root;
    pre_consensus_root = pre_state.Env.consensus_root;
    epoch_env = deps.epoch_env;
  }