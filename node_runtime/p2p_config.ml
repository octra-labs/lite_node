(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  config_hash : string;
  binary_hash : string;
  require_binary_hash : bool;
  upgrade_plan : Octra_net.P2p_upgrade_plan.t option;
  handshake_allowed_pubkeys : string list;
  validator_pubkeys : string list;
  readiness_runtime : Octra_core.Validator_ready_policy.runtime;
}

type swarm_params = {
  listen_port : int;
  chain_id : string;
  node_id : string;
  node_addr : string;
  pubkey_raw : string;
  consensus_config_hash : string;
  bootstrap_peers : string list;
  max_peers : int;
  sign_fn : string -> string;
  best_epoch_fn : unit -> int64;
  best_root_fn : unit -> string;
  handshake : t;
}

type identity = {
  priv_raw_32 : string;
  pub_raw_32 : string;
  node_id : string;
  sign_fn : string -> string;
}

type env = {
  binary_hash_value : string option;
  require_binary_hash : bool;
  allow_dynamic_peers : bool;
  validator_ready_strict : bool;
  validator_ready_min_shadow_epochs : int;
}

type upgrade_log =
  | Upgrade_scheduled of {
      activate_epoch : int64;
      binary_hash : string;
      config_hash : string;
    }
  | Rollback_scheduled of {
      activate_epoch : int64;
      binary_hash : string;
      config_hash : string;
    }

type upgrade_readiness_state = {
  last_block_reason : string option;
}

type startup_config = {
  validator : Validator_config.t;
  env : env;
  upgrade_plan : Octra_net.P2p_upgrade_plan.t option;
  handshake : t;
  readiness_requirements : Octra_core.Validator_ready_policy.requirements;
  readiness_runtime : Octra_core.Validator_ready_policy.runtime;
  upgrade_logs : upgrade_log list;
}

type startup_runtime = {
  active_vs : Octra_consensus.C_types.validator_set;
  scheduled_driver_config : Octra_consensus.C_driver.scheduled_validator_set_config option;
  light_scheduled_validator_set : Octra_consensus.C_config.scheduled option;
  consensus_config_hash : string;
  handshake : t;
  readiness_requirements : Octra_core.Validator_ready_policy.requirements;
  readiness_runtime : Octra_core.Validator_ready_policy.runtime;
}

type node_startup_install_deps = {
  info : string -> unit;
  warn : string -> unit;
  set_consensus_config_hash : string -> unit;
  set_consensus_validator_set : Octra_consensus.C_types.validator_set -> unit;
  set_scheduled_validator_set : Octra_consensus.C_config.scheduled option -> unit;
}

type node_swarm_start_deps = {
  info : string -> unit;
  set_swarm : Octra_net.P2p_swarm.t -> unit;
}

type node_swarm_start = {
  swarm : Octra_net.P2p_swarm.t;
  params : swarm_params;
}

type node_stack_deps = {
  getenv : string -> string option;
  chain_id : string;
  consensus_mode : bool;
  current_height : int64;
  chain_active_raw : string option;
  chain_pending_raw : string option;
  chain_pending_entries : string list;
  install : node_startup_install_deps;
  swarm : node_swarm_start_deps;
  listen_port : int;
  node_addr : string;
  priv_b64 : string;
  pub_b64 : string;
  bootstrap_peers : string list;
  best_epoch_fn : unit -> int64;
  best_root_fn : unit -> string;
}

type node_stack = {
  startup : startup_config;
  runtime : startup_runtime;
  swarm_start : node_swarm_start;
}

type upgrade_readiness =
  | Upgrade_ready of upgrade_readiness_state
  | Upgrade_not_ready of {
      state : upgrade_readiness_state;
      reason : string;
      should_log : bool;
    }

let first32 raw =
  if String.length raw >= 32 then String.sub raw 0 32 else raw

let env_bool getenv name =
  getenv name = Some "1"

let env_int getenv name default =
  match getenv name with
  | None -> default
  | Some raw ->
    try int_of_string raw with _ -> default

let env_of_getenv getenv =
  {
    binary_hash_value = getenv "OCTRA_BINARY_HASH";
    require_binary_hash = env_bool getenv "OCTRA_P2P_REQUIRE_BINARY_HASH";
    allow_dynamic_peers = env_bool getenv "OCTRA_CONSENSUS_ALLOW_DYNAMIC_PEERS";
    validator_ready_strict = env_bool getenv "OCTRA_VALIDATOR_READY_STRICT";
    validator_ready_min_shadow_epochs =
      env_int getenv "OCTRA_VALIDATOR_READY_MIN_SHADOW_EPOCHS" 0;
  }

let identity ~priv_b64 ~pub_b64 =
  let priv_raw_32 = Base64.decode_exn priv_b64 |> first32 in
  let pub_raw_32 = Base64.decode_exn pub_b64 in
  if String.length priv_raw_32 <> 32 then invalid_arg "invalid validator private key";
  if String.length pub_raw_32 <> 32 then invalid_arg "invalid validator public key";
  {
    priv_raw_32;
    pub_raw_32;
    node_id = Octra_net.P2p_handshake.node_id_of_pubkey pub_raw_32;
    sign_fn = (fun msg ->
      Octra_consensus.C_hash.sign_ed25519 ~priv_raw:priv_raw_32 ~msg);
  }

let normalize_binary_hash = function
  | None -> Ok (String.make 32 '\x00')
  | Some raw -> Octra_net.P2p_upgrade_plan.normalize_hash "OCTRA_BINARY_HASH" raw

let handshake_allowed_pubkeys ~allow_dynamic_peers allowed_pubkeys =
  if allow_dynamic_peers then [] else allowed_pubkeys

let build ~chain_id ~consensus_config_hash ~allowed_pubkeys
    ~binary_hash_value ~require_binary_hash ~allow_dynamic_peers ~upgrade_plan =
  match normalize_binary_hash binary_hash_value with
  | Error e -> Error e
  | Ok binary_hash ->
    let handshake_allowed_pubkeys =
      handshake_allowed_pubkeys ~allow_dynamic_peers allowed_pubkeys
    in
    let readiness_runtime = Octra_core.Validator_ready_policy.{
      chain_id;
      binary_hash = Text.raw_to_hex binary_hash;
      config_hash = Text.raw_to_hex consensus_config_hash;
    } in
    Ok {
      config_hash = consensus_config_hash;
      binary_hash;
      require_binary_hash;
      upgrade_plan;
      handshake_allowed_pubkeys;
      validator_pubkeys = allowed_pubkeys;
      readiness_runtime;
    }

let build_from_env env ~chain_id ~consensus_config_hash ~allowed_pubkeys
    ~upgrade_plan =
  build
    ~chain_id
    ~consensus_config_hash
    ~allowed_pubkeys
    ~binary_hash_value:env.binary_hash_value
    ~require_binary_hash:env.require_binary_hash
    ~allow_dynamic_peers:env.allow_dynamic_peers
    ~upgrade_plan

let readiness_requirements env =
  if env.validator_ready_strict then
    Octra_core.Validator_ready_policy.strict
      ~min_shadow_epochs:(max 0 env.validator_ready_min_shadow_epochs)
      ()
  else Octra_core.Validator_ready_policy.relaxed

let upgrade_logs = function
  | None -> []
  | Some plan ->
    let upgrade =
      Upgrade_scheduled {
        activate_epoch = plan.Octra_net.P2p_upgrade_plan.activate_epoch;
        binary_hash = plan.target_binary_hash;
        config_hash = plan.target_config_hash;
      }
    in
    begin
      match plan.rollback with
      | None -> [upgrade]
      | Some rollback ->
        [
          upgrade;
          Rollback_scheduled {
            activate_epoch = rollback.rollback_epoch;
            binary_hash = rollback.rollback_binary_hash;
            config_hash = rollback.rollback_config_hash;
          };
      ]
    end

let upgrade_log_message = function
  | Upgrade_scheduled row ->
    Printf.sprintf
      "p2p upgrade scheduled activate_epoch = %Ld binary = %s config = %s"
      row.activate_epoch
      (Octra_consensus.C_config.short row.binary_hash)
      (Octra_consensus.C_config.short row.config_hash)
  | Rollback_scheduled row ->
    Printf.sprintf
      "p2p rollback scheduled activate_epoch = %Ld binary = %s config = %s"
      row.activate_epoch
      (Octra_consensus.C_config.short row.binary_hash)
      (Octra_consensus.C_config.short row.config_hash)

let startup_config ~env ~chain_id ~consensus_mode ~current_height
    ~current_entries ~next_entries ~chain_pending_entries
    ~next_activation_epoch ?program_trust_hash
    ?runtime_profile_hash ?active_raw ?pending_raw () =
  let base_validator =
    Validator_config.build_bound
      ~chain_id
      ~consensus_mode
      ~current_height
      ~current_entries
      ~next_entries
      ~chain_pending_entries
      ~next_activation_epoch
      ~program_trust_hash
      ~runtime_profile_hash
  in
  let validator =
    Validator_config.bind_persistent_updates
      ~chain_id
      ~consensus_mode
      ~current_height
      ~active_raw
      ~pending_raw
      base_validator
  in
  match validator with
  | Error error -> Error error
  | Ok validator ->
  let network_config_hash =
    Octra_consensus.C_config.network_hash
      ~chain_id
      ?program_trust_hash
      ?runtime_profile_hash
      ()
  in
  match Octra_net.P2p_upgrade_plan.of_env ~current_config_hash:network_config_hash with
  | Error e -> Error e
  | Ok upgrade_plan ->
    match
      build_from_env
        env
        ~chain_id
        ~consensus_config_hash:network_config_hash
        ~allowed_pubkeys:validator.allowed_pubkeys
        ~upgrade_plan
    with
    | Error e -> Error e
    | Ok handshake ->
      let handshake = {
        handshake with
        validator_pubkeys =
          List.map
            (fun validator ->
              validator.Octra_consensus.C_types.pubkey)
            validator.active_vs.validators;
      } in
      let readiness_requirements = readiness_requirements env in
      Ok {
        validator;
        env;
        upgrade_plan;
        handshake;
        readiness_requirements;
        readiness_runtime = handshake.readiness_runtime;
        upgrade_logs = upgrade_logs upgrade_plan;
      }

let node_startup_config ~getenv ~chain_id ~consensus_mode ~current_height
    ~chain_active_raw ~chain_pending_raw ~chain_pending_entries =
  match Consensus_profile.validate getenv with
  | Error error -> Error error
  | Ok () ->
    match Octra_vm.Program_trust.of_env getenv with
    | Error error -> Error (Octra_vm.Program_trust.error_message error)
    | Ok trust ->
      let validator_policy =
        Octra_core.Validator_policy.of_env_exn getenv
      in
      let env = env_of_getenv getenv in
      let env =
        if Octra_core.Validator_policy.lifecycle_enabled validator_policy then
          { env with allow_dynamic_peers = true }
        else
          env
      in
      let runtime_profile_hash = Consensus_profile.hash getenv in
      startup_config
        ~env
        ~chain_id
        ~consensus_mode
        ~current_height
        ~current_entries:(Validators.entries_of_env_name "OCTRA_VALIDATORS")
        ~next_entries:(Validators.entries_of_env_name "OCTRA_VALIDATORS_NEXT")
        ~chain_pending_entries
        ~next_activation_epoch:(Validators.activation_epoch_int64 ())
        ?program_trust_hash:(Octra_vm.Program_trust.config_hash trust)
        ~runtime_profile_hash
        ?active_raw:chain_active_raw
        ?pending_raw:chain_pending_raw
        ()

let install_node_startup_config deps ~current_height startup =
  let validator = startup.validator in
  Option.iter
    deps.warn
    (Validator_config.transition_message ~current_height validator);
  let active_vs = validator.active_vs in
  let scheduled_driver_config = validator.scheduled_driver_config in
  let light_scheduled_validator_set = validator.light_scheduled_validator_set in
  let consensus_config_hash = validator.consensus_config_hash in
  deps.info
    (Printf.sprintf
       "consensus config hash = %s n = %d quorum = %d"
       (Octra_consensus.C_config.short consensus_config_hash)
       active_vs.n
       active_vs.quorum);
  deps.set_consensus_config_hash consensus_config_hash;
  deps.set_consensus_validator_set active_vs;
  deps.set_scheduled_validator_set light_scheduled_validator_set;
  List.iter
    (fun row -> deps.warn (upgrade_log_message row))
    startup.upgrade_logs;
  {
    active_vs;
    scheduled_driver_config;
    light_scheduled_validator_set;
    consensus_config_hash;
    handshake = startup.handshake;
    readiness_requirements = startup.readiness_requirements;
    readiness_runtime = startup.readiness_runtime;
  }

let upgrade_readiness_state = {
  last_block_reason = None;
}

let upgrade_ready state ~epoch ~consensus_config_hash cfg =
  match
    Octra_net.P2p_upgrade_plan.ready
      ~epoch
      ~config_hash:consensus_config_hash
      ~binary_hash:cfg.binary_hash
      cfg.upgrade_plan
  with
  | Octra_net.P2p_upgrade_plan.Ready ->
    Upgrade_ready { last_block_reason = None }
  | Octra_net.P2p_upgrade_plan.Not_ready reason ->
    Upgrade_not_ready {
      state = { last_block_reason = Some reason };
      reason;
      should_log = state.last_block_reason <> Some reason;
    }

let upgrade_ready_checker ~log_blocked ~epoch ~consensus_config_hash cfg =
  let state = ref upgrade_readiness_state in
  fun () ->
    match upgrade_ready !state ~epoch:(epoch ()) ~consensus_config_hash cfg with
    | Upgrade_ready next ->
      state := next;
      true
    | Upgrade_not_ready row ->
      state := row.state;
      if row.should_log then log_blocked row.reason;
      false

let swarm_params ~listen_port ~chain_id ~node_addr ~identity
    ~consensus_config_hash ~bootstrap_peers ~best_epoch_fn ~best_root_fn
    ~handshake =
  {
    listen_port;
    chain_id;
    node_id = identity.node_id;
    node_addr;
    pubkey_raw = identity.pub_raw_32;
    consensus_config_hash;
    bootstrap_peers;
    max_peers = 32;
    sign_fn = identity.sign_fn;
    best_epoch_fn;
    best_root_fn;
    handshake;
  }

let startup_swarm_params ~listen_port ~chain_id ~node_addr ~identity
    ~bootstrap_peers ~best_epoch_fn ~best_root_fn
    (startup : startup_config) =
  swarm_params
    ~listen_port
    ~chain_id
    ~node_addr
    ~identity
    ~consensus_config_hash:startup.handshake.config_hash
    ~bootstrap_peers
    ~best_epoch_fn
    ~best_root_fn
    ~handshake:startup.handshake

let node_startup_swarm_params ~listen_port ~chain_id ~node_addr ~priv_b64
    ~pub_b64 ~bootstrap_peers ~best_epoch_fn ~best_root_fn
    (startup : startup_config) =
  let identity = identity ~priv_b64 ~pub_b64 in
  startup_swarm_params
    ~listen_port
    ~chain_id
    ~node_addr
    ~identity
    ~bootstrap_peers
    ~best_epoch_fn
    ~best_root_fn
    startup

let swarm_config (params : swarm_params) =
  Octra_net.P2p_swarm.{
    listen_port = params.listen_port;
    chain_id = params.chain_id;
    node_id = params.node_id;
    node_addr = params.node_addr;
    pubkey_raw = params.pubkey_raw;
    consensus_config_hash = params.consensus_config_hash;
    binary_hash = params.handshake.binary_hash;
    require_binary_hash = params.handshake.require_binary_hash;
    upgrade_plan = params.handshake.upgrade_plan;
    allowed_pubkeys = params.handshake.handshake_allowed_pubkeys;
    bootstrap_peers = params.bootstrap_peers;
    max_peers = params.max_peers;
    sign_fn = params.sign_fn;
    best_epoch_fn = params.best_epoch_fn;
    best_root_fn = params.best_root_fn;
  }

let create_swarm params =
  let swarm = Octra_net.P2p_swarm.create (swarm_config params) in
  if params.handshake.validator_pubkeys = [] then
    swarm
  else
    match
      Octra_net.P2p_swarm.set_validator_pubkeys
        swarm
        params.handshake.validator_pubkeys
    with
    | Ok () -> swarm
    | Error error -> invalid_arg error

let start_node_swarm deps ~listen_port ~chain_id ~node_addr ~priv_b64
    ~pub_b64 ~bootstrap_peers ~best_epoch_fn ~best_root_fn
    (startup : startup_config) =
  let params =
    node_startup_swarm_params
      ~listen_port
      ~chain_id
      ~node_addr
      ~priv_b64
      ~pub_b64
      ~bootstrap_peers
      ~best_epoch_fn
      ~best_root_fn
      startup
  in
  let swarm = create_swarm params in
  deps.info
    (Printf.sprintf
       "consensus P2P enabled port = %d peers = %d chain = %s"
       listen_port
       (List.length bootstrap_peers)
       chain_id);
  deps.set_swarm swarm;
  { swarm; params }

let node_stack deps =
  match
    node_startup_config
      ~getenv:deps.getenv
      ~chain_id:deps.chain_id
      ~consensus_mode:deps.consensus_mode
      ~current_height:deps.current_height
      ~chain_active_raw:deps.chain_active_raw
      ~chain_pending_raw:deps.chain_pending_raw
      ~chain_pending_entries:deps.chain_pending_entries
  with
  | Error e -> Error e
  | Ok startup ->
    let runtime =
      install_node_startup_config
        deps.install
        ~current_height:deps.current_height
        startup
    in
    let swarm_start =
      start_node_swarm
        deps.swarm
        ~listen_port:deps.listen_port
        ~chain_id:deps.chain_id
        ~node_addr:deps.node_addr
        ~priv_b64:deps.priv_b64
        ~pub_b64:deps.pub_b64
        ~bootstrap_peers:deps.bootstrap_peers
        ~best_epoch_fn:deps.best_epoch_fn
        ~best_root_fn:deps.best_root_fn
        startup
    in
    Ok {
      startup;
      runtime;
      swarm_start;
    }