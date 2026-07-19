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


val role_tasks :
  observer:bool ->
  observer_loop:(unit -> 'a) ->
  tick_loop:(unit -> 'a) ->
  'a list

val optional_task :
  'a option ->
  'a list

val task_plan :
  base_tasks:'a list ->
  observer:bool ->
  observer_loop:(unit -> 'a) ->
  tick_loop:(unit -> 'a) ->
  optional:'a option ->
  'a list

val node_tasks :
  p2p_task:'a ->
  rpc_task:'a ->
  observer:bool ->
  observer_loop:(unit -> 'a) ->
  tick_loop:(unit -> 'a) ->
  swarm_task:'a option ->
  'a list

type 'a launch_tasks = {
  p2p : unit -> 'a;
  rpc : unit -> 'a;
  swarm : unit -> 'a option;
  observer : bool;
  observer_loop : unit -> 'a;
  tick_loop : unit -> 'a;
}

type node_launch_deps = {
  p2p : unit -> unit Lwt.t;
  rpc : unit -> unit Lwt.t;
  observer : bool;
  tick_loop : unit -> unit Lwt.t;
  swarm : Octra_net.P2p_swarm.t option;
  swarm_deps : P2p_swarm_lifecycle.node_deps;
}

type node_launch_runtime = {
  p2p : unit -> unit Lwt.t;
  rpc : unit -> unit Lwt.t;
  observer : bool;
  tick_loop : unit -> unit Lwt.t;
  swarm : Octra_net.P2p_swarm.t option;
  guard : Octra_net.P2p_tx_gossip_guard.t;
  find_tx : string -> Octra_core.Transaction.t option;
  find_account : string -> Octra_core.Ledger.account option;
  add_tx : Octra_core.Transaction.t -> (string, string) result;
  now : unit -> float;
  max_drift : float;
  driver_ref : Octra_consensus.C_driver.t option ref;
  close_chaindata : unit -> unit;
  exit_fatal : unit -> unit;
}

type join_log = {
  fatal : string -> unit;
  warn : string -> unit;
}

val make_node_swarm_deps :
  observer:bool ->
  guard:Octra_net.P2p_tx_gossip_guard.t ->
  find_tx:(string -> Octra_core.Transaction.t option) ->
  find_account:(string -> Octra_core.Ledger.account option) ->
  add_tx:(Octra_core.Transaction.t -> (string, string) result) ->
  now:(unit -> float) ->
  max_drift:float ->
  driver_ref:Octra_consensus.C_driver.t option ref ->
  P2p_swarm_lifecycle.node_deps

val make_node_launch_deps :
  p2p:(unit -> unit Lwt.t) ->
  rpc:(unit -> unit Lwt.t) ->
  observer:bool ->
  tick_loop:(unit -> unit Lwt.t) ->
  swarm:Octra_net.P2p_swarm.t option ->
  swarm_deps:P2p_swarm_lifecycle.node_deps ->
  node_launch_deps

val make_node_launch_deps_with_swarm :
  p2p:(unit -> unit Lwt.t) ->
  rpc:(unit -> unit Lwt.t) ->
  observer:bool ->
  tick_loop:(unit -> unit Lwt.t) ->
  swarm:Octra_net.P2p_swarm.t option ->
  guard:Octra_net.P2p_tx_gossip_guard.t ->
  find_tx:(string -> Octra_core.Transaction.t option) ->
  find_account:(string -> Octra_core.Ledger.account option) ->
  add_tx:(Octra_core.Transaction.t -> (string, string) result) ->
  now:(unit -> float) ->
  max_drift:float ->
  driver_ref:Octra_consensus.C_driver.t option ref ->
  node_launch_deps

val launch_tasks :
  'a launch_tasks ->
  'a list

val optional_ref_async :
  'a option ref ->
  dispatch:('a -> 'b -> 'c -> unit Lwt.t) ->
  'b ->
  'c ->
  unit

val p2p_swarm_task :
  swarm:Octra_net.P2p_swarm.t option ->
  deps:P2p_swarm_lifecycle.deps ->
  unit Lwt.t option

val node_swarm_task :
  swarm:Octra_net.P2p_swarm.t option ->
  deps:P2p_swarm_lifecycle.node_deps ->
  unit Lwt.t option

val p2p_listen_task :
  listen:(port:int -> callback:('a -> unit Lwt.t) -> unit Lwt.t) ->
  port:int ->
  unit ->
  unit Lwt.t

val node_launch_tasks :
  node_launch_deps ->
  unit Lwt.t list

val observer_loop :
  unit ->
  unit Lwt.t

val default_join_log :
  join_log

val run_join :
  log:join_log ->
  tasks:unit Lwt.t list ->
  close_chaindata:(unit -> unit) ->
  exit_fatal:(unit -> unit) ->
  unit Lwt.t

val run_launch_tasks :
  unit Lwt.t launch_tasks ->
  close_chaindata:(unit -> unit) ->
  exit_fatal:(unit -> unit) ->
  unit Lwt.t

val run_node_launch_tasks :
  node_launch_deps ->
  close_chaindata:(unit -> unit) ->
  exit_fatal:(unit -> unit) ->
  unit Lwt.t

val run_node_runtime :
  node_launch_runtime ->
  unit Lwt.t