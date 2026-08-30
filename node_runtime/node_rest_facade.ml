(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Emission_policy = Octra_core.Emission_policy
module Ledger = Octra_core.Ledger
module Store_irmin = Octra_core.Store_irmin
module Store_chaindata = Octra_core.Store_chaindata
module Transaction = Octra_core.Transaction
module Staging = Octra_core.Tx_staging
module Tree = Octra_core.Tree
module WSServer = Octra_core.Ws_server
module Wallet = Octra_core.Crypto.Wallet

type runtime = {
  swarm_ref : Octra_net.P2p_swarm.t option ref;
  preverify_admit : Transaction.t -> (unit, string) result;
  save_drops : Staging.drop_record list -> unit;
  find_drop : string -> Octra_core.Tx_drop.row option;
  drops_by_addr :
    string ->
    limit:int ->
    offset:int ->
    Octra_core.Tx_drop.row list;
}

let cli_has_flag name =
  Array.exists (fun a -> a = name) Sys.argv

let encrypted_supply_aggregate store ledger () =
  let public_supply = Ledger.get_total_supply ledger in
  let total_supply =
    match
      Emission_policy.resolve_total
        ~policy:(Emission_policy.of_env Sys.getenv_opt)
        ~stored:(Ledger.run_s (Store_irmin.get_meta store "total_supply"))
        ~legacy:(Emission_policy.legacy_total Sys.getenv_opt)
        ~public_supply
    with
    | Ok value -> value
    | Error error -> failwith error
  in
  match
    Emission_policy.hidden_supply
      ~total_supply
      ~public_supply
  with
  | Ok hidden -> hidden
  | Error error -> failwith error

let notify_staging_update ?(total_txs = List.length (Staging.all ()))
    ?(total_ou = !Staging.total_ou) ?(max_ou = Staging.max_ou) () =
  WSServer.notify_staging_update ~total_txs ~total_ou ~max_ou

let sweep_low_fee_stealth () =
  let min_ou = Rest_read_rpc.stealth_floor () in
  let victims = Tx_view.low_fee_heavy_hashes ~min_ou (Staging.all ()) in
  List.iter (fun h ->
    ignore (Staging.remove_by_hash h);
    Preverify_cache.remove h
  ) victims;
  let n = List.length victims in
  if n > 0 then
    Log.info "staging" "sweep_low_fee_stealth removed = %d min_ou = %s"
      n
      (Z.to_string min_ou);
  n

let add_tx_to_staging ?(relay = true) ?(bft_mode = false) runtime ledger tx =
  match Tx_view.bft_op_admission ~bft_mode tx with
  | Error (_, reason) -> Error reason
  | Ok () ->
    match Tx_view.staging_submit_admission
            ~min_ou:(Rest_read_rpc.stealth_floor ())
            ~min_relay_fee:(Staging.min_relay_fee tx)
            tx with
    | Error e -> Error e
    | Ok _ ->
      let tx_hash = Transaction.hash tx in
      let lookup addr =
        Ledger.find_opt ledger addr
        |> Option.map (fun a -> (a.Ledger.balance, a.Ledger.nonce))
      in
      match Staging.add_smart ~lookup tx with
      | Error e -> Error e
      | Ok drops ->
        runtime.save_drops drops;
        begin
          let admission =
            try runtime.preverify_admit tx
            with exn ->
              Error
                ("pre_verify_unavailable reason = " ^ Printexc.to_string exn)
          in
          match admission with
          | Error reason ->
            ignore (Staging.remove_by_hash tx_hash);
            Preverify_cache.remove tx_hash;
            Log.warn "staging"
              "event = preverify_admit status = deferred tx = %s reason = %s"
              (Text.hash_short tx_hash)
              reason;
            Error reason
          | Ok () ->
            let swarm_opt = !(runtime.swarm_ref) in
            let peer_count =
              match swarm_opt with
              | Some swarm -> Octra_net.P2p_swarm.peer_count swarm
              | None -> 0
            in
            let effects =
              Tx_view.staging_submit_effects
                ~relay
                ~peer_count
                ~total_txs:(List.length (Staging.all ()))
                ~total_ou:!Staging.total_ou
                ~max_ou:Staging.max_ou
                ~tx_hash
            in
            WSServer.notify_new_tx tx;
            notify_staging_update
              ~total_txs:effects.Tx_view.total_txs
              ~total_ou:effects.total_ou
              ~max_ou:effects.max_ou
              ();
            begin
              match swarm_opt, effects.relay_payload with
              | Some swarm, Some payload ->
                Lwt.async (fun () ->
                  Octra_net.P2p_swarm.broadcast swarm
                    {
                      Octra_net.P2p_frame.msg_type =
                        Octra_net.P2p_frame.msg_tx_gossip;
                      payload;
                    })
              | _ -> ()
            end;
            Ok tx_hash
        end

let max_timestamp_drift = 300.0

let consensus_mode_flags () =
  Consensus_mode.of_inputs
    ~cli_observer:(cli_has_flag "--observer")
    ~env_mode:(Sys.getenv_opt "OCTRA_CONSENSUS_MODE")

let bft_mode () =
  (consensus_mode_flags ()).consensus_enabled

let observer_rejects_submissions () =
  (consensus_mode_flags ()).observer_enabled
  && not (Env.flag "OCTRA_OBSERVER_RELAY_SUBMITS")

let bft_pending_nonce_window () =
  max 1 (Env.int_value "OCTRA_BFT_PENDING_NONCE_WINDOW" 32)

let validate_and_submit_tx runtime ledger (tx : Transaction.t) =
  let now = Unix.gettimeofday () in
  match Tx_view.submit_pre_signature_admission
          ~now
          ~max_timestamp_drift
          ~observer_rpc_mode:(observer_rejects_submissions ())
          ~bft_mode:(bft_mode ())
          ~limits:Tx_view.payload_limits
          ~sender_exists:(Ledger.mem ledger tx.from)
          tx with
  | Error e -> Error e
  | Ok () ->
    let acc = Ledger.find ledger tx.from in
    match Tx_view.submit_signature_admission
            ~account_public_key:acc.Ledger.public_key
            ~preverify_has_capacity:(Preverify_cache.has_capacity ())
            ~preverify_pending:(Preverify_cache.pending_count ())
            ~preverify_pending_max:(Preverify_cache.pending_max ())
            ~bft_mode:(bft_mode ())
            ~confirmed_nonce:acc.Ledger.nonce
            ~bft_window:(bft_pending_nonce_window ())
            tx with
    | Error e -> Error e
    | Ok () ->
      match add_tx_to_staging ~bft_mode:(bft_mode ()) runtime ledger tx with
      | Error msg ->
        let (t, r) = Tx_view.staging_error msg in
        Error (t, r)
      | Ok tx_hash -> Ok tx_hash

let run_s = Node_rpc_server.run_s

let list_saved_epochs chaindata =
  Octra_core.Store_chaindata.list_epoch_ids chaindata
  |> List.sort_uniq compare
  |> List.rev

let start runtime ~port ~data_dir ~store ~ledger ~tree_ref ~wallet ~chain_id
    ~consensus_config_hash_ref ~consensus_validator_set_ref
    ~scheduled_validator_set_ref
    ~current_epoch ~total_tx_count ~validator_view_sk ~validator_view_pub
    ~program_trust ~migration_entitlements ~rules ~chaindata
    ~consensus_driver_ref
    ~epoch_visibility ~resource_compute =
  let deps = Node_rpc_server.{
    validate = validate_and_submit_tx runtime ledger;
    encrypted_supply = encrypted_supply_aggregate store ledger;
    notify_staging_update = (fun () -> notify_staging_update ());
    bft_mode;
    account_path_profile_enabled = Env.flag "OCTRA_PROFILE_ACCOUNT_PATHS";
    swarm = (fun () -> !(runtime.swarm_ref));
    find_drop = runtime.find_drop;
    drops_by_addr = runtime.drops_by_addr;
  } in
  Node_rpc_server.start Node_rpc_server.{
    port;
    data_dir;
    store;
    ledger;
    tree_ref;
    wallet;
    chain_id;
    consensus_config_hash_ref;
    consensus_validator_set_ref;
    scheduled_validator_set_ref;
    current_epoch;
    total_tx_count;
    validator_view_sk;
    validator_view_pub;
    program_trust;
    migration_entitlements;
    rules;
    chaindata;
    consensus_driver_ref;
    epoch_visibility;
    resource_compute;
    deps;
  }

let start_task runtime ~port ~data_dir ~store ~ledger ~tree_ref ~wallet
    ~chain_id ~consensus_config_hash_ref ~consensus_validator_set_ref
    ~scheduled_validator_set_ref ~current_epoch ~total_tx_count
    ~validator_view_sk ~validator_view_pub ~program_trust
    ~migration_entitlements ~rules ~chaindata
    ~consensus_driver_ref ~epoch_visibility ~resource_compute () =
  start
    runtime
    ~port
    ~data_dir
    ~store
    ~ledger
    ~tree_ref
    ~wallet
    ~chain_id
    ~consensus_config_hash_ref
    ~consensus_validator_set_ref
    ~scheduled_validator_set_ref
    ~current_epoch
    ~total_tx_count
    ~validator_view_sk
    ~validator_view_pub
    ~program_trust
    ~migration_entitlements
    ~rules
    ~chaindata
    ~consensus_driver_ref
    ~epoch_visibility
    ~resource_compute