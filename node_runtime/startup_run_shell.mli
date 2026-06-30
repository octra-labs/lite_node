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

val observer_loop :
  unit ->
  unit Lwt.t

val run_join :
  tasks:unit Lwt.t list ->
  close_chaindata:(unit -> unit) ->
  exit_fatal:(unit -> unit) ->
  unit Lwt.t

val run_launch_tasks :
  unit Lwt.t launch_tasks ->
  close_chaindata:(unit -> unit) ->
  exit_fatal:(unit -> unit) ->
  unit Lwt.t