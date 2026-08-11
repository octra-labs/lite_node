(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Wallet = Octra_core.Crypto.Wallet
module Ledger = Octra_core.Ledger
module Transaction = Octra_core.Transaction
module Tree = Octra_core.Tree
module Rpc = Octra_core.Rpc
module Store_irmin = Octra_core.Store_irmin
module Store_chaindata = Octra_core.Store_chaindata

type deps = {
  validate : Submit_rpc.validate;
  encrypted_supply : unit -> Z.t;
  notify_staging_update : unit -> unit;
  bft_mode : unit -> bool;
  account_path_profile_enabled : bool;
  swarm : unit -> Octra_net.P2p_swarm.t option;
  find_drop : string -> Octra_core.Tx_drop.row option;
  drops_by_addr :
    string ->
    limit:int ->
    offset:int ->
    Octra_core.Tx_drop.row list;
}

type config = {
  port : int;
  data_dir : string;
  store : Store_irmin.t;
  ledger : Ledger.t;
  tree_ref : Tree.t ref;
  wallet : Wallet.t;
  chain_id : string;
  consensus_config_hash_ref : string ref;
  consensus_validator_set_ref : Octra_consensus.C_types.validator_set ref;
  scheduled_validator_set_ref : Octra_consensus.C_config.scheduled option ref;
  current_epoch : int ref;
  total_tx_count : int ref;
  validator_view_sk : string;
  validator_view_pub : string;
  program_trust : Octra_vm.Program_trust.t;
  migration_entitlements : Octra_core.Pvac_migration_entitlement.t;
  rules : Octra_core.Rule_graph.t;
  chaindata : Store_chaindata.t;
  consensus_driver_ref : Octra_consensus.C_driver.t option ref;
  epoch_visibility : Epoch_visibility.t;
  resource_compute : Resource_compute_service.t;
  deps : deps;
}

type ctx = {
  ledger : Ledger.t;
  store : Store_irmin.t;
  chaindata : Store_chaindata.t;
  tree_ref : Tree.t ref;
  wallet : Wallet.t;
  chain_id : string;
  consensus_config_hash_ref : string ref;
  consensus_validator_set_ref : Octra_consensus.C_types.validator_set ref;
  scheduled_validator_set_ref : Octra_consensus.C_config.scheduled option ref;
  current_epoch : int ref;
  total_tx_count : int ref;
  validator_view_sk : string;
  validator_view_pub : string;
  program_trust : Octra_vm.Program_trust.t;
  migration_entitlements : Octra_core.Pvac_migration_entitlement.t;
  rules : Octra_core.Rule_graph.t;
  consensus_driver_ref : Octra_consensus.C_driver.t option ref;
  resource_compute : Resource_compute_service.t;
  token_rpc : Octra_vm.Token_rpc_actor.t;
  deps : deps;
} [@@warning "-69"]

type rpc_result = (Yojson.Safe.t, Rpc.rpc_error) Stdlib.result

let run_s p =
  match Lwt.state p with
  | Lwt.Return v -> v
  | Lwt.Fail e -> raise e
  | Lwt.Sleep ->
    let r = ref None in
    Lwt.on_any p (fun v -> r := Some (Ok v)) (fun e -> r := Some (Error e));
    let rec pump n =
      if n > 10000000 then failwith "run_s: irmin I/O timeout"
      else
        match !r with
        | Some (Ok v) -> v
        | Some (Error e) -> raise e
        | None ->
          ignore (Lwt_engine.iter false);
          pump (n + 1)
    in
    pump 0

let with_address = Rpc_read_adapters.with_address

let with_account =
  Rpc_read_adapters.with_account
    ~find_account:(fun ctx addr -> Ledger.find_opt ctx.ledger addr)

let status_read_ctx (ctx : ctx) =
  let runtime_profile_hash =
    if !(ctx.consensus_config_hash_ref) = String.make 32 '\x00' then None
    else Some (Consensus_profile.hash Sys.getenv_opt)
  in
  Status_read_rpc.{
    ledger = ctx.ledger;
    store = ctx.store;
    chaindata = ctx.chaindata;
    tree_ref = ctx.tree_ref;
    validator_address = ctx.wallet.Wallet.address;
    validator_pubkey = ctx.wallet.Wallet.pub;
    validator_priv_b64 = ctx.wallet.Wallet.priv;
    chain_id = ctx.chain_id;
    program_trust_hash = Octra_vm.Program_trust.config_hash ctx.program_trust;
    runtime_profile_hash;
    validator_view_pub = ctx.validator_view_pub;
    validator_set_ref = ctx.consensus_validator_set_ref;
    scheduled_validator_set_ref = ctx.scheduled_validator_set_ref;
    current_epoch = ctx.current_epoch;
    total_tx_count = ctx.total_tx_count;
    encrypted = ctx.deps.encrypted_supply;
    swarm = ctx.deps.swarm;
    driver_ref = ctx.consensus_driver_ref;
  }

let status_read f params ctx =
  f params (status_read_ctx ctx)

let octra_account params ctx =
  let started_at = Unix.gettimeofday () in
  with_account params ctx (fun addr account ->
    Account_read_rpc.account
      ctx.chaindata
      ~params
      ~profile_enabled:ctx.deps.account_path_profile_enabled
      ~started_at
      ~addr
      ~account)

let octra_supply _params ctx =
  Account_read_rpc.supply
    ctx.store
    ctx.ledger
    ~encrypted:(ctx.deps.encrypted_supply ())

let octra_total_transactions _params ctx =
  Account_read_rpc.total_transactions ~confirmed:!(ctx.total_tx_count)

let octra_submit params ctx =
  Submit_rpc.submit ~validate:ctx.deps.validate params

let octra_submit_batch params ctx =
  Submit_rpc.submit_batch ~validate:ctx.deps.validate params

let octra_transactions_by_epoch params ctx =
  History_read_rpc.transactions_by_epoch
    ctx.chaindata
    ~params
    ~current_epoch_id:(!(ctx.tree_ref)).Tree.epoch_id

let octra_transaction params ctx =
  History_read_rpc.transaction
    ~find_drop:ctx.deps.find_drop
    ctx.chaindata
    ~params

let octra_transactions_by_address params ctx =
  with_address params (fun addr ->
    History_read_rpc.transactions_by_address
      ~drops_by_addr:ctx.deps.drops_by_addr
      ctx.chaindata
      ~params
      ~addr)

let staging_remove params ctx =
  Submit_rpc.staging_remove
    ~find_tx:Octra_core.Tx_staging.find_by_hash
    ~remove_tx:Octra_core.Tx_staging.remove_by_hash
    ~notify:ctx.deps.notify_staging_update
    params

let ctx_store ctx = ctx.store

let ctx_ledger ctx = ctx.ledger

let ctx_chaindata ctx = ctx.chaindata

let ctx_current_epoch ctx = ctx.current_epoch

let store_read = Rpc_read_adapters.store_read ~store:ctx_store

let ledger_params_read =
  Rpc_read_adapters.ledger_params_read ~ledger:ctx_ledger

let ledger_address_lwt_read =
  Rpc_read_adapters.ledger_address_lwt_read ~ledger:ctx_ledger ~with_address

let ledger_params_no_ctx_read =
  Rpc_read_adapters.ledger_params_no_ctx_read

let store_address_lwt_read =
  Rpc_read_adapters.store_address_lwt_read ~store:ctx_store ~with_address

let store_params_lwt_read =
  Rpc_read_adapters.store_params_lwt_read ~store:ctx_store

let store_ledger_address_lwt_read =
  Rpc_read_adapters.store_ledger_address_lwt_read
    ~store:ctx_store
    ~ledger:ctx_ledger
    ~with_address

let chaindata_params_read =
  Rpc_read_adapters.chaindata_params_read ~chaindata:ctx_chaindata

let chaindata_address_read =
  Rpc_read_adapters.chaindata_address_read
    ~chaindata:ctx_chaindata
    ~with_address

let ledger_chaindata_params_read =
  Rpc_read_adapters.ledger_chaindata_params_read
    ~ledger:ctx_ledger
    ~chaindata:ctx_chaindata

let account_lwt_read =
  Rpc_read_adapters.account_lwt_read ~with_account

let store_label_read =
  Rpc_read_adapters.store_label_read ~store:ctx_store

let chaindata_read =
  Rpc_read_adapters.chaindata_read ~chaindata:ctx_chaindata

let epoch_read =
  Rpc_read_adapters.epoch_read ~store:ctx_store ~current_epoch:ctx_current_epoch

let no_ctx = Rpc_read_adapters.no_ctx

let no_params = Rpc_read_adapters.no_params

let json0_read = Rpc_read_adapters.json0_read ~param_json:Rpc.param_json

let program_info_read params ctx =
  Octra_vm.Contract_rpc.program_info_params
    ~store:ctx.store
    ~ledger:ctx.ledger
    params

let program_list_read _params ctx =
  Octra_vm.Contract_rpc.list_contracts
    ~store:ctx.store
    ~ledger:ctx.ledger

let circle_asset_public_read f params ctx =
  f
    ctx.store
    params
    ~current_epoch:!(ctx.current_epoch)
    ~reveal_sensitive_fields:false

let contract_save_abi _params _ctx =
  Lwt.return
    (Error
       (Rpc.err
          (-32601)
          "program ABI writes are disabled; use program verification"
          None))

let program_tokens_by_address params ctx =
  Octra_vm.Contract_rpc.tokens_by_address_params
    ~actor:ctx.token_rpc
    params

let fhe_pubkey_loader store addr =
  match run_s (Store_irmin.get_pvac_pubkey store addr) with
  | None -> None
  | Some blob ->
    match Octra_core.Pvac_registry.load_pubkey blob with
    | Ok pk -> Some pk
    | Error _ -> None

let make_view_ctx store ledger current_epoch_ref =
  Octra_vm.Contract_rpc.make_view_ctx
    ~store
    ~ledger
    ~current_epoch:!current_epoch_ref
    ~get_fhe_pubkey:(fhe_pubkey_loader store)

let contract_call params ctx =
  Octra_vm.Contract_rpc.call_params
    ~store:ctx.store
    ~ledger:ctx.ledger
    ~current_epoch:!(ctx.current_epoch)
    ~get_fhe_pubkey:(fhe_pubkey_loader ctx.store)
    ~storage_json:(Rpc_view.storage_assoc ~limit:4096)
    params

let circle_view params ctx =
  let view_ctx = make_view_ctx ctx.store ctx.ledger ctx.current_epoch in
  Circle_read_rpc.view_call_public_params
    ~trusted:(Octra_vm.Program_trust.keys ctx.program_trust)
    ctx.store params ~view_ctx

let circle_view_auth params ctx =
  let view_ctx = make_view_ctx ctx.store ctx.ledger ctx.current_epoch in
  Circle_read_rpc.view_call_auth
    ~trusted:(Octra_vm.Program_trust.keys ctx.program_trust)
    ctx.store params ~view_ctx

let octra_register_pvac_pubkey params ctx =
  Registration_rpc.pvac_pubkey
    ~existing:(Ledger.get_pvac_pubkey ctx.ledger)
    ~kat_mismatch:(fun ~addr ~got ~expected ->
      Octra_log.info "pvac"
        "aes_kat_mismatch addr = %s got = %s expected = %s"
        addr got expected)
    params

let octra_private_transfer params ctx =
  Submit_rpc.private_transfer ~validate:ctx.deps.validate params

let octra_register_public_key params ctx =
  Registration_rpc.public_key
    ~bft_mode:(ctx.deps.bft_mode ())
    ~register:(Ledger.register_public_key ctx.ledger)
    params

let octra_pvac_migration_status params ctx =
  match
    Octra_core.Rule_graph.owner_migration
      ctx.rules
      ~epoch:!(ctx.current_epoch)
  with
  | Error fault ->
    Lwt.return_error
      (Rpc.err
         (-32005)
         (Octra_core.Rule_graph.fault_message fault)
         None)
  | Ok owner_migration_mode ->
    with_account params ctx (fun addr account ->
      Account_read_rpc.pvac_migration_status
        ctx.store
        ctx.migration_entitlements
        ~epoch:!(ctx.current_epoch)
        ~owner_migration_mode
        ~addr
        ~account)

let status_dispatch_adapters =
  Status_read_rpc.{ status_read }

let account_dispatch_adapters =
  Account_read_rpc.{
    ledger_params_read;
    ledger_params_no_ctx_read;
    ledger_address_lwt_read;
    store_address_lwt_read;
    store_params_lwt_read;
    store_ledger_address_lwt_read;
    account_lwt_read;
    pvac_migration_status = octra_pvac_migration_status;
    account = octra_account;
    supply = octra_supply;
    total_transactions = octra_total_transactions;
  }

let history_dispatch =
  History_read_rpc.dispatch History_read_rpc.{
    transaction = octra_transaction;
    transactions_by_address = octra_transactions_by_address;
    chaindata_params_read;
    chaindata_address_read;
    transactions_by_epoch = octra_transactions_by_epoch;
  }

let rest_dispatch =
  let rest_epoch_current _params ctx =
    Rest_read_rpc.epoch_current ~tree_ref:ctx.tree_ref
  in
  let rest_epoch_list params ctx =
    Rest_read_rpc.epoch_list ~tree_ref:ctx.tree_ref ~params
  in
  Rest_read_rpc.dispatch Rest_read_rpc.{
    ledger_chaindata_params_read;
    chaindata_params_read;
    no_params;
    epoch_current = rest_epoch_current;
    epoch_list = rest_epoch_list;
    recommended_fee = (fun params _ctx ->
      Rest_read_rpc.recommended_fee_from_env ~params);
  }

let circle_dispatch =
  Circle_read_rpc.dispatch Circle_read_rpc.{
    store_read;
    epoch_read;
    public_asset_read = circle_asset_public_read;
    circle_view;
    circle_view_auth;
  }

let program_dispatch =
  Program_read_rpc.dispatch Program_read_rpc.{
    store_label_read;
    chaindata_read;
    no_ctx;
    json0_read;
    program_info = program_info_read;
    program_call = contract_call;
    program_list = program_list_read;
    program_save_abi = contract_save_abi;
    program_tokens_by_address;
  }

let effect_dispatch =
  Rpc_effect_dispatch.dispatch Rpc_effect_dispatch.{
    submit = octra_submit;
    submit_batch = octra_submit_batch;
    staging_remove;
    register_public_key = octra_register_public_key;
    register_pvac_pubkey = octra_register_pvac_pubkey;
    private_transfer = octra_private_transfer;
  }

let compute_dispatch = [
  "octra_circleComputeSubmit",
  (fun params ctx -> Resource_compute_rpc.submit ctx.resource_compute params);
  "octra_circleComputeStatus",
  (fun params ctx -> Resource_compute_rpc.status ctx.resource_compute params);
  "octra_circleComputeCancel",
  (fun params ctx -> Resource_compute_rpc.cancel ctx.resource_compute params);
]

let read_error =
  Rpc.err
    (-32012)
    "committed state changed during read; retry"
    None

let stable_read visibility handler params ctx =
  let open Lwt.Syntax in
  let* result =
    Epoch_visibility.read visibility (fun () -> handler params ctx)
  in
  match result with
  | Some value -> Lwt.return value
  | None -> Lwt.return (Error read_error)

let guard_reads visibility routes =
  List.map
    (fun (name, handler) ->
      name, stable_read visibility handler)
    routes

let dispatch visibility :
    (string * (Yojson.Safe.t -> ctx -> rpc_result Lwt.t)) list =
  let reads =
    Status_read_rpc.core_dispatch status_dispatch_adapters
    @ Account_read_rpc.public_dispatch account_dispatch_adapters
    @ history_dispatch
    @ rest_dispatch
    @ circle_dispatch
    @ program_dispatch
    @ Account_read_rpc.pvac_dispatch account_dispatch_adapters
    @ Status_read_rpc.proof_dispatch status_dispatch_adapters
    |> guard_reads visibility
  in
  let effects =
    effect_dispatch.submission
    @ effect_dispatch.staging
    @ effect_dispatch.mutation
    @ compute_dispatch
  in
  reads @ effects

let stable_http visibility handler req body =
  let open Lwt.Syntax in
  let* response =
    Epoch_visibility.read visibility (fun () -> handler req body)
  in
  match response with
  | Some value -> Lwt.return value
  | None ->
    Cohttp_lwt_unix.Server.respond_string
      ~status:`Service_unavailable
      ~body:"committed state changed during read; retry"
      ()

let state_sensitive_http req =
  match Cohttp.Request.meth req, Uri.path (Cohttp.Request.uri req) with
  | `GET, "/"
  | `GET, "/status"
  | `GET, "/state-sync/head"
  | `GET, "/state-sync/range" -> true
  | _ -> false

let start (cfg : config) =
  let { Wallet.address; _ } = cfg.wallet in
  let token_rpc = Octra_vm.Token_rpc_actor.create ~store:cfg.store () in
  let rpc_ctx = {
    ledger = cfg.ledger;
    store = cfg.store;
    chaindata = cfg.chaindata;
    tree_ref = cfg.tree_ref;
    wallet = cfg.wallet;
    chain_id = cfg.chain_id;
    consensus_config_hash_ref = cfg.consensus_config_hash_ref;
    consensus_validator_set_ref = cfg.consensus_validator_set_ref;
    scheduled_validator_set_ref = cfg.scheduled_validator_set_ref;
    current_epoch = cfg.current_epoch;
    total_tx_count = cfg.total_tx_count;
    validator_view_sk = cfg.validator_view_sk;
    validator_view_pub = cfg.validator_view_pub;
    program_trust = cfg.program_trust;
    migration_entitlements = cfg.migration_entitlements;
    rules = cfg.rules;
    consensus_driver_ref = cfg.consensus_driver_ref;
    resource_compute = cfg.resource_compute;
    token_rpc;
    deps = cfg.deps;
  } in
  let routes = dispatch cfg.epoch_visibility in
  let rpc_handler =
    Rpc_http.handle_rpc_post
      ~process:(fun meta body_str ->
        Rpc_dispatch.process_body meta body_str rpc_ctx routes)
  in
  let state_http req body =
    State_sync_http.handle
      ~data_dir:cfg.data_dir
      ~ledger:cfg.ledger
      ~tree_ref:cfg.tree_ref
      ~validator:address
      ~chain_id:cfg.chain_id
      ~config_hash:!(cfg.consensus_config_hash_ref)
      ~validator_set:!(cfg.consensus_validator_set_ref)
      ~current_epoch:cfg.current_epoch
      ~chaindata:cfg.chaindata
      ~encrypted_supply:cfg.deps.encrypted_supply
      req
      body
  in
  let state_http_handler req body =
    if state_sensitive_http req then
      stable_http cfg.epoch_visibility state_http req body
    else
      state_http req body
  in
  let callback =
    Rpc_http.route_rpc_or_fallback
      ~rpc_handler
      ~fallback_handler:state_http_handler
  in
  Lwt.finalize
    (fun () -> Rpc_http.create_server ~port:cfg.port ~callback)
    (fun () -> Octra_vm.Token_rpc_actor.shutdown token_rpc)