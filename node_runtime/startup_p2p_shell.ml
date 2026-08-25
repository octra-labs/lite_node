(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type network = {
  chain_id : string;
  consensus_mode : bool;
  consensus_port : int;
  consensus_peers : string list;
}

type wallet = {
  address : string;
  priv_b64 : string;
  pub_b64 : string;
}

type install = {
  set_consensus_config_hash : string -> unit;
  set_consensus_validator_set : Octra_consensus.C_types.validator_set -> unit;
  set_scheduled_validator_set : Octra_consensus.C_config.scheduled option -> unit;
  set_swarm : Octra_net.P2p_swarm.t -> unit;
}

type deps = {
  getenv : string -> string option;
  info : string -> unit;
  warn : string -> unit;
  current_height : unit -> int64;
  read_active_validator_meta : unit -> string option;
  read_pending_validator_meta : unit -> string option;
  read_head_hash : unit -> string option;
  root_of_head_hash : string -> string;
  install : install;
}

type node_request = {
  getenv : string -> string option;
  info : string -> unit;
  warn : string -> unit;
  current_height : unit -> int64;
  read_active_validator_meta : unit -> string option;
  read_pending_validator_meta : unit -> string option;
  read_head_hash : unit -> string option;
  root_of_head_hash : string -> string;
  install : install;
  chain_id : string;
  consensus_mode : bool;
  consensus_port : int;
  consensus_peers : string list;
  address : string;
  priv_b64 : string;
  pub_b64 : string;
}

type admission_deps = {
  getenv : string -> string option;
  activation_epoch : unit -> int64 option;
  info : string -> unit;
  warn : string -> unit;
  refuse : string list -> unit;
}

type persistent_update_deps = {
  read_pending : unit -> string option Lwt.t;
  warn : string -> unit;
}

type node_view = {
  validator_config : Validator_config.t;
  active_vs : Octra_consensus.C_types.validator_set;
  scheduled_validator_set_config : Octra_consensus.C_driver.scheduled_validator_set_config option;
  consensus_config_hash : string;
  p2p_config : P2p_config.t;
  swarm_params : P2p_config.swarm_params;
  swarm : Octra_net.P2p_swarm.t;
}

type node_start_runtime = {
  getenv : string -> string option;
  info : string -> unit;
  warn : string -> unit;
  fatal : string -> unit;
  read_active_validator_meta : unit -> string option;
  read_pending_validator_meta : unit -> string option;
  read_head_hash : unit -> string option;
  root_of_head_hash : string -> string;
  install : install;
  activation_epoch : unit -> int64 option;
  chain_id : string;
  consensus_mode : bool;
  consensus_port : int;
  consensus_peers : string list;
  address : string;
  priv_b64 : string;
  pub_b64 : string;
  voting : bool;
  role_label : string;
  read_persistent_pending : unit -> string option Lwt.t;
  current_height : unit -> int64;
}

type node_start = {
  view : node_view;
  load_scheduled_validator_set_config :
    unit ->
    Octra_consensus.C_driver.scheduled_validator_set_config option Lwt.t;
  activate_validator_set :
    Octra_consensus.C_types.validator_set ->
    string ->
    unit Lwt.t;
  activate_validator_set_relief :
    Octra_consensus.C_types.validator_set ->
    string ->
    unit Lwt.t;
}

let raw32_zero = String.make 32 '\x00'

let install_refs ~consensus_config_hash ~consensus_validator_set
    ~scheduled_validator_set ~set_swarm =
  {
    set_consensus_config_hash = (fun hash ->
      consensus_config_hash := hash);
    set_consensus_validator_set = (fun vs ->
      consensus_validator_set := vs);
    set_scheduled_validator_set = (fun scheduled ->
      scheduled_validator_set := scheduled);
    set_swarm;
  }

let current_height (deps : deps) =
  deps.current_height ()

let best_root (deps : deps) =
  match deps.read_head_hash () with
  | Some hash -> deps.root_of_head_hash hash
  | None -> raw32_zero

let node_stack_deps (deps : deps) (network : network) (wallet : wallet) =
  let active_raw = deps.read_active_validator_meta () in
  let pending_raw = deps.read_pending_validator_meta () in
  P2p_config.{
    getenv = deps.getenv;
    chain_id = network.chain_id;
    consensus_mode = network.consensus_mode;
    current_height = current_height deps;
    chain_active_raw = active_raw;
    chain_pending_raw = pending_raw;
    chain_pending_entries =
      Validator_config.pending_entries_of_raw pending_raw;
    install = {
      info = deps.info;
      warn = deps.warn;
      set_consensus_config_hash = deps.install.set_consensus_config_hash;
      set_consensus_validator_set = deps.install.set_consensus_validator_set;
      set_scheduled_validator_set = deps.install.set_scheduled_validator_set;
    };
    swarm = {
      info = deps.info;
      set_swarm = deps.install.set_swarm;
    };
    listen_port = network.consensus_port;
    node_addr = wallet.address;
    priv_b64 = wallet.priv_b64;
    pub_b64 = wallet.pub_b64;
    bootstrap_peers = network.consensus_peers;
    best_epoch_fn = (fun () -> current_height deps);
    best_root_fn = (fun () -> best_root deps);
  }

let build (deps : deps) (network : network) (wallet : wallet) =
  P2p_config.node_stack (node_stack_deps deps network wallet)

let deps_of_request (request : node_request) =
  {
    getenv = request.getenv;
    info = request.info;
    warn = request.warn;
    current_height = request.current_height;
    read_active_validator_meta = request.read_active_validator_meta;
    read_pending_validator_meta = request.read_pending_validator_meta;
    read_head_hash = request.read_head_hash;
    root_of_head_hash = request.root_of_head_hash;
    install = request.install;
  }

let network_of_request (request : node_request) =
  {
    chain_id = request.chain_id;
    consensus_mode = request.consensus_mode;
    consensus_port = request.consensus_port;
    consensus_peers = request.consensus_peers;
  }

let wallet_of_request (request : node_request) =
  {
    address = request.address;
    priv_b64 = request.priv_b64;
    pub_b64 = request.pub_b64;
  }

let stack_view (stack : P2p_config.node_stack) =
  let P2p_config.{ startup; runtime; swarm_start } = stack in
  {
    validator_config = startup.validator;
    active_vs = runtime.active_vs;
    scheduled_validator_set_config = runtime.scheduled_driver_config;
    consensus_config_hash = runtime.consensus_config_hash;
    p2p_config = runtime.handshake;
    swarm_params = swarm_start.params;
    swarm = swarm_start.swarm;
  }

let build_node_view (request : node_request) =
  match
    build
      (deps_of_request request)
      (network_of_request request)
      (wallet_of_request request)
  with
  | Ok stack -> Ok (stack_view stack)
  | Error e -> Error e

let request_of_runtime runtime =
  {
    getenv = runtime.getenv;
    info = runtime.info;
    warn = runtime.warn;
    current_height = runtime.current_height;
    read_active_validator_meta = runtime.read_active_validator_meta;
    read_pending_validator_meta = runtime.read_pending_validator_meta;
    read_head_hash = runtime.read_head_hash;
    root_of_head_hash = runtime.root_of_head_hash;
    install = runtime.install;
    chain_id = runtime.chain_id;
    consensus_mode = runtime.consensus_mode;
    consensus_port = runtime.consensus_port;
    consensus_peers = runtime.consensus_peers;
    address = runtime.address;
    priv_b64 = runtime.priv_b64;
    pub_b64 = runtime.pub_b64;
  }

let admit_validator_startup (deps : admission_deps) ~address ~voting
    ~role_label validator =
  Validator_config.node_startup_admission
    ~getenv:deps.getenv
    ~activation_epoch:deps.activation_epoch
    ~address
    ~voting
    ~role_label
    validator
  |> Validator_config.emit_startup_events
       {
         info = deps.info;
         warn = deps.warn;
         refuse = deps.refuse;
       }

let load_persistent_update (deps : persistent_update_deps) =
  Validator_config.load_node_persistent_update
    Validator_config.{
      read_pending = deps.read_pending;
      warn = deps.warn;
    }

let admission_deps_of_runtime runtime =
  {
    getenv = runtime.getenv;
    activation_epoch = runtime.activation_epoch;
    info = runtime.info;
    warn = runtime.warn;
    refuse = (fun messages ->
      List.iter runtime.fatal messages;
      failwith "p2p_start_refused");
  }

let persistent_update_deps_of_runtime runtime =
  {
    read_pending = runtime.read_persistent_pending;
    warn = runtime.warn;
  }

let load_runtime_persistent_update runtime =
  let open Lwt.Syntax in
  let* config =
    load_persistent_update (persistent_update_deps_of_runtime runtime)
  in
  match config with
  | None -> Lwt.return_none
  | Some config ->
    let validator_set =
      Octra_consensus.C_types.validator_set_for_epoch
        ~chain_id:runtime.chain_id
        ~epoch_id:config.activate_epoch
        config.validator_set
    in
    Lwt.return_some { config with validator_set }

let validator_pubkeys validator_set =
  validator_set.Octra_consensus.C_types.validators
  |> List.map (fun validator ->
    validator.Octra_consensus.C_types.pubkey)

let same_validator_set left right =
  String.equal
    (Octra_consensus.C_config.validator_set_hash left)
    (Octra_consensus.C_config.validator_set_hash right)

let same_scheduled
    (left : Octra_consensus.C_driver.scheduled_validator_set_config option)
    (right : Octra_consensus.C_driver.scheduled_validator_set_config option) =
  match left, right with
  | None, None -> true
  | Some left, Some right ->
    Int64.equal left.activate_epoch right.activate_epoch
    && String.equal left.fingerprint right.fingerprint
    && same_validator_set left.validator_set right.validator_set
  | None, Some _
  | Some _, None -> false

let runtime_config_hash runtime view active scheduled =
  if runtime.consensus_mode then
    Octra_consensus.C_config.hash
      ~chain_id:runtime.chain_id
      ~validator_set:active
      ?scheduled
      ?program_trust_hash:view.validator_config.program_trust_hash
      ?runtime_profile_hash:view.validator_config.runtime_profile_hash
      ()
  else
    raw32_zero

let install_runtime_validator_config runtime view active scheduled =
  let config_hash = runtime_config_hash runtime view active scheduled in
  match
    Octra_net.P2p_swarm.set_validator_pubkeys
      view.swarm
      (validator_pubkeys active)
  with
  | Error error -> Lwt.fail_with error
  | Ok () ->
    runtime.install.set_consensus_validator_set active;
    runtime.install.set_scheduled_validator_set scheduled;
    runtime.install.set_consensus_config_hash config_hash;
    Lwt.return_unit

let start_node runtime =
  match build_node_view (request_of_runtime runtime) with
  | Error e ->
    runtime.fatal e;
    failwith e
  | Ok view ->
    if runtime.consensus_mode then
      admit_validator_startup
        (admission_deps_of_runtime runtime)
        ~address:runtime.address
        ~voting:runtime.voting
        ~role_label:runtime.role_label
        view.validator_config;
    let active = ref view.active_vs in
    let scheduled = ref view.scheduled_validator_set_config in
    let install scheduled_driver =
      let light =
        Validator_config.light_scheduled_of_driver scheduled_driver
      in
      let open Lwt.Syntax in
      let* () =
        install_runtime_validator_config
          runtime
          view
          !active
          light
      in
      scheduled := scheduled_driver;
      Lwt.return_unit
    in
    {
      view;
      load_scheduled_validator_set_config = (fun () ->
        let open Lwt.Syntax in
        let* loaded = load_runtime_persistent_update runtime in
        match loaded with
        | None -> Lwt.return_none
        | Some config ->
          let next =
            if same_validator_set !active config.validator_set
               && Int64.compare config.activate_epoch (runtime.current_height ()) <= 0 then
              None
            else
              Some config
          in
          let* () =
            if same_scheduled !scheduled next then Lwt.return_unit
            else install next
          in
          Lwt.return next);
      activate_validator_set = (fun validator_set fingerprint ->
        let next =
          match !scheduled with
          | Some config when
              same_validator_set
                validator_set
                (Octra_consensus.C_types.validator_set_for_epoch
                   ~chain_id:runtime.chain_id
                   ~epoch_id:config.activate_epoch
                   config.validator_set)
              && String.equal fingerprint config.fingerprint ->
            None
          | current -> current
        in
        active := validator_set;
        install next);
      activate_validator_set_relief = (fun validator_set _ ->
        active := validator_set;
        install !scheduled);
    }