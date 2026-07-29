(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Ledger = Octra_core.Ledger
module Metrics = Octra_core.Metrics
module Store_chaindata = Octra_core.Store_chaindata
module Store_irmin = Octra_core.Store_irmin
module Staging = Octra_core.Tx_staging
module Tree = Octra_core.Tree

type rpc_result = (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result Lwt.t

type read_ctx = {
  ledger : Ledger.t;
  store : Store_irmin.t;
  chaindata : Store_chaindata.t;
  tree_ref : Tree.t ref;
  validator_address : string;
  validator_pubkey : string;
  validator_priv_b64 : string;
  chain_id : string;
  program_trust_hash : string option;
  runtime_profile_hash : string option;
  validator_view_pub : string;
  validator_set : Octra_consensus.C_types.validator_set;
  scheduled_validator_set : Octra_consensus.C_config.scheduled option;
  current_epoch : int ref;
  total_tx_count : int ref;
  encrypted : unit -> Z.t;
  swarm : unit -> Octra_net.P2p_swarm.t option;
  driver_ref : Octra_consensus.C_driver.t option ref;
}

type 'handler dispatch_adapters = {
  status_read : (Yojson.Safe.t -> read_ctx -> rpc_result) -> 'handler;
}

let ok value =
  Lwt.return (Ok value)

let signer ~chain_id ~validator_addr ~validator_pubkey ~validator_priv_b64
    ~config_hash =
  Light_rpc.{
    chain_id;
    validator_addr;
    validator_pubkey;
    validator_priv_b64;
    config_hash;
  }

let node_version () =
  ok (Status_rpc.node_version ())

let validator_view_pubkey ~validator_view_pub ~validator_address =
  ok (Status_rpc.validator_view_pubkey ~validator_view_pub ~validator_address)

let epoch_tags store =
  let open Lwt.Syntax in
  let* tags = Store_irmin.list_epoch_tags store in
  ok (Status_rpc.epoch_tags ~tags ~keep_epochs:Store_irmin.gc_keep_epochs)

let signed_root signer head =
  match head with
  | None ->
    Lwt.return (Error (Octra_core.Rpc.not_found "HEAD not available"))
  | Some h ->
    match Light_rpc.signed_root signer h with
    | Error e ->
      Lwt.return (Error (Octra_core.Rpc.err (-32000) e None))
    | Ok root ->
      ok root

let active_validator_set ~driver_ref ~fallback =
  match !driver_ref with
  | None ->
    fallback
  | Some driver ->
    driver.Octra_consensus.C_driver.engine.Octra_consensus.C_engine.vs

let active_scheduled_validator_set ~driver_ref ~fallback =
  match !driver_ref with
  | None ->
    fallback
  | Some driver ->
    match driver.Octra_consensus.C_driver.config.scheduled_validator_set_config with
    | None ->
      fallback
    | Some cfg ->
      Some Octra_consensus.C_config.{
        activate_epoch = cfg.Octra_consensus.C_driver.activate_epoch;
        validator_set = cfg.validator_set;
      }

let current_validator_config ~chain_id ~program_trust_hash
    ~runtime_profile_hash ~driver_ref ~validator_set ~scheduled_validator_set =
  let validator_set = active_validator_set ~driver_ref ~fallback:validator_set in
  let scheduled =
    active_scheduled_validator_set
      ~driver_ref
      ~fallback:scheduled_validator_set
  in
  let config_hash =
    Octra_consensus.C_config.hash
      ~chain_id
      ~validator_set
      ?scheduled
      ?program_trust_hash
      ?runtime_profile_hash
      ()
  in
  validator_set, scheduled, config_hash

let validator_set_proof ~chain_id ~program_trust_hash
    ~runtime_profile_hash ~driver_ref ~validator_set ~scheduled_validator_set =
  let validator_set, scheduled, config_hash =
    current_validator_config
      ~chain_id
      ~program_trust_hash
      ~runtime_profile_hash
      ~driver_ref
      ~validator_set
      ~scheduled_validator_set
  in
  Lwt.return
    (Status_rpc.validator_set_proof
       ~chain_id
       ~config_hash
       ?program_trust_hash
       ?runtime_profile_hash
       ?scheduled
       validator_set)

let consensus_peer_states ~swarm ~driver_ref =
  let now = Unix.gettimeofday () in
  let diag =
    match swarm with
    | None ->
      None
    | Some swarm ->
      Some (Octra_net.P2p_swarm.peer_diagnostics swarm)
  in
  let peer_records =
    match !driver_ref with
    | None ->
      None
    | Some driver ->
      Some (Octra_consensus.C_driver.peer_state_snapshot driver)
  in
  ok (Status_rpc.consensus_peer_states ~now ~diag ~peer_records)

let node_status ~tree_ref ~validator =
  let t = !tree_ref in
  ok
    (Status_rpc.node_status
       ~epoch:t.Tree.epoch_id
       ~validator
       ~roots:(Tree.root_count t)
       ~timestamp:(Unix.gettimeofday ())
       ~head:(Octra_core.Head_manifest.get_cached ()))

let node_stats ~ledger ~chaindata ~current_epoch ~total_confirmed ~encrypted =
  let true_total = Ledger.get_total_supply ledger in
  let total_accounts = Ledger.length ledger in
  let active_accounts = Ledger.active_count ledger in
  let recent_epochs =
    List.init (min 10 current_epoch) (fun i -> current_epoch - 1 - i)
  in
  let recent_tx_count =
    Store_chaindata.recent_tx_count
      chaindata
      ~n_epochs:10
      ~current_epoch
  in
  ok
    (Status_rpc.node_stats
       ~current_epoch
       ~total_accounts
       ~active_accounts
       ~true_total
       ~encrypted
       ~max_supply:(Ledger.get_max_supply ())
       ~total_confirmed
       ~staging:(List.length (Staging.all ()))
       ~recent_tx_count
       ~latest_epochs:recent_epochs
       ~head:(Octra_core.Head_manifest.get_cached ()))

let signer_of_ctx ctx =
  let _, _, config_hash =
    current_validator_config
      ~chain_id:ctx.chain_id
      ~program_trust_hash:ctx.program_trust_hash
      ~runtime_profile_hash:ctx.runtime_profile_hash
      ~driver_ref:ctx.driver_ref
      ~validator_set:ctx.validator_set
      ~scheduled_validator_set:ctx.scheduled_validator_set
  in
  signer
    ~chain_id:ctx.chain_id
    ~validator_addr:ctx.validator_address
    ~validator_pubkey:ctx.validator_pubkey
    ~validator_priv_b64:ctx.validator_priv_b64
    ~config_hash

let node_version_params _params _ctx =
  node_version ()

let validator_view_pubkey_params _params ctx =
  validator_view_pubkey
    ~validator_view_pub:ctx.validator_view_pub
    ~validator_address:ctx.validator_address

let epoch_tags_params _params ctx =
  epoch_tags ctx.store

let signed_root_params _params ctx =
  signed_root
    (signer_of_ctx ctx)
    (Octra_core.Head_manifest.get_cached ())

let account_proof_params params ctx =
  Light_rpc.account_proof_params
    (signer_of_ctx ctx)
    ~store:ctx.store
    ~head:(Octra_core.Head_manifest.get_cached ())
    params

let validator_set_proof_params _params ctx =
  validator_set_proof
    ~chain_id:ctx.chain_id
    ~program_trust_hash:ctx.program_trust_hash
    ~runtime_profile_hash:ctx.runtime_profile_hash
    ~driver_ref:ctx.driver_ref
    ~validator_set:ctx.validator_set
    ~scheduled_validator_set:ctx.scheduled_validator_set

let epoch_proof_params params ctx =
  Light_rpc.epoch_proof_params ~chain_id:ctx.chain_id ctx.chaindata params

let tx_inclusion_proof_params params ctx =
  Light_rpc.tx_inclusion_proof_params ~chain_id:ctx.chain_id ctx.chaindata params

let consensus_peer_states_params _params ctx =
  consensus_peer_states
    ~swarm:(ctx.swarm ())
    ~driver_ref:ctx.driver_ref

let node_status_params _params ctx =
  node_status
    ~tree_ref:ctx.tree_ref
    ~validator:ctx.validator_address

let node_stats_params _params ctx =
  node_stats
    ~ledger:ctx.ledger
    ~chaindata:ctx.chaindata
    ~current_epoch:!(ctx.current_epoch)
    ~total_confirmed:!(ctx.total_tx_count)
    ~encrypted:(ctx.encrypted ())

let node_metrics_params _params _ctx =
  ok (Metrics.get_metrics ())

let core_dispatch adapters =
  [
    "node_version", adapters.status_read node_version_params;
    "node_status", adapters.status_read node_status_params;
    "node_stats", adapters.status_read node_stats_params;
    "node_metrics", adapters.status_read node_metrics_params;
  ]

let proof_dispatch adapters =
  [
    "octra_validatorViewPubkey", adapters.status_read validator_view_pubkey_params;
    "octra_epochTags", adapters.status_read epoch_tags_params;
    "octra_signedRoot", adapters.status_read signed_root_params;
    "octra_accountProof", adapters.status_read account_proof_params;
    "octra_validatorSetProof", adapters.status_read validator_set_proof_params;
    "octra_epochProof", adapters.status_read epoch_proof_params;
    "octra_txInclusionProof", adapters.status_read tx_inclusion_proof_params;
    "octra_consensusPeerStates", adapters.status_read consensus_peer_states_params;
  ]

let dispatch adapters =
  core_dispatch adapters @ proof_dispatch adapters