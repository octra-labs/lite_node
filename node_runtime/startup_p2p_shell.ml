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
  current_epoch : unit -> int;
  read_pending_validator_meta : unit -> string option;
  read_head_hash : unit -> string option;
  root_of_head_hash : string -> string;
  install : install;
}

type node_request = {
  getenv : string -> string option;
  info : string -> unit;
  warn : string -> unit;
  current_epoch : unit -> int;
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
  read_marker : string -> string option Lwt.t;
  warn : string -> unit;
  current_height : unit -> int64;
}

type node_view = {
  validator_config : Validator_config.t;
  active_vs : Octra_consensus.C_types.validator_set;
  scheduled_validator_set_config : Octra_consensus.C_driver.scheduled_validator_set_config option;
  consensus_config_hash : string;
  p2p_config : P2p_config.t;
  readiness_requirements : Octra_core.Validator_ready_policy.requirements;
  readiness_runtime : Octra_core.Validator_ready_policy.runtime;
  swarm_params : P2p_config.swarm_params;
  swarm : Octra_net.P2p_swarm.t;
}

let raw32_zero = String.make 32 '\x00'

let current_height (deps : deps) =
  Int64.of_int (deps.current_epoch ())

let pending_entries (deps : deps) =
  deps.read_pending_validator_meta ()
  |> Validator_config.pending_entries_of_raw

let best_root (deps : deps) =
  match deps.read_head_hash () with
  | Some hash -> deps.root_of_head_hash hash
  | None -> raw32_zero

let node_stack_deps (deps : deps) (network : network) (wallet : wallet) =
  P2p_config.{
    getenv = deps.getenv;
    chain_id = network.chain_id;
    consensus_mode = network.consensus_mode;
    current_height = current_height deps;
    chain_pending_entries = pending_entries deps;
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
    current_epoch = request.current_epoch;
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
    readiness_requirements = runtime.readiness_requirements;
    readiness_runtime = runtime.readiness_runtime;
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

let admit_validator_startup deps ~address ~voting ~role_label validator =
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

let load_persistent_update (deps : persistent_update_deps) ~runtime
    ~requirements =
  Validator_config.load_node_persistent_update
    Validator_config.{
      read_pending = deps.read_pending;
      read_marker = deps.read_marker;
      warn = deps.warn;
      current_height = deps.current_height;
    }
    ~runtime
    ~requirements

let load_irmin_persistent_update ~store ~warn ~current_height ~runtime
    ~requirements =
  load_persistent_update
    {
      read_pending = (fun () ->
        Octra_core.Store_irmin.get_meta
          store
          Octra_core.Validator_set_update.pending_meta_key);
      read_marker = (fun key ->
        Octra_core.Store_irmin.get_meta store key);
      warn;
      current_height;
    }
    ~runtime
    ~requirements