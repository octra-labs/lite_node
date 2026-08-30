(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Infix

let role_tasks ~observer ~observer_loop ~tick_loop =
  if observer then [observer_loop ()] else [tick_loop ()]

let optional_task = function
  | Some task -> [task]
  | None -> []

let task_plan ~base_tasks ~observer ~observer_loop ~tick_loop ~optional =
  base_tasks
  @ role_tasks ~observer ~observer_loop ~tick_loop
  @ optional_task optional

let transport_tasks ~p2p ~rpc ~swarm =
  match swarm with
  | Some _ -> [rpc ()]
  | None -> [p2p (); rpc ()]

let node_tasks ~p2p_task ~rpc_task ~observer ~observer_loop ~tick_loop
    ~swarm_task =
  task_plan
    ~base_tasks:[p2p_task; rpc_task]
    ~observer
    ~observer_loop
    ~tick_loop
    ~optional:swarm_task

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
  services : (unit -> unit Lwt.t) list;
  observer : bool;
  tick_loop : unit -> unit Lwt.t;
  swarm : Octra_net.P2p_swarm.t option;
  swarm_deps : P2p_swarm_lifecycle.node_deps;
}

type node_launch_runtime = {
  p2p : unit -> unit Lwt.t;
  rpc : unit -> unit Lwt.t;
  services : (unit -> unit Lwt.t) list;
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
  resource_compute : Resource_compute_service.t option;
  close_chaindata : unit -> unit;
  exit_fatal : unit -> unit;
}

type join_log = {
  fatal : string -> unit;
  warn : string -> unit;
}

let make_node_swarm_deps ~observer ~guard ~find_tx ~find_account ~add_tx
    ~now ~max_drift ~driver_ref ~resource_compute =
  P2p_swarm_lifecycle.{
    observer;
    guard;
    find_tx;
    find_account;
    add_tx;
    now;
    max_drift;
    driver_ref;
    resource_compute;
  }

let make_node_launch_deps ~p2p ~rpc ~services ~observer ~tick_loop ~swarm
    ~swarm_deps =
  { p2p; rpc; services; observer; tick_loop; swarm; swarm_deps }

let make_node_launch_deps_with_swarm ~p2p ~rpc ~services ~observer ~tick_loop
    ~swarm ~guard ~find_tx ~find_account ~add_tx ~now ~max_drift ~driver_ref
    ~resource_compute =
  make_node_launch_deps
    ~p2p
    ~rpc
    ~services
    ~observer
    ~tick_loop
    ~swarm
    ~swarm_deps:
      (make_node_swarm_deps
         ~observer
         ~guard
         ~find_tx
         ~find_account
         ~add_tx
         ~now
         ~max_drift
         ~driver_ref
         ~resource_compute)

let launch_tasks (deps : 'a launch_tasks) =
  node_tasks
    ~p2p_task:(deps.p2p ())
    ~rpc_task:(deps.rpc ())
    ~observer:deps.observer
    ~observer_loop:deps.observer_loop
    ~tick_loop:deps.tick_loop
    ~swarm_task:(deps.swarm ())

let optional_ref_async ref_value ~dispatch a b =
  match !ref_value with
  | Some value ->
    Lwt.async (fun () -> dispatch value a b)
  | None -> ()

let p2p_swarm_task ~swarm ~deps =
  Option.map (P2p_swarm_lifecycle.start deps) swarm

let node_swarm_task ~swarm ~deps =
  P2p_swarm_lifecycle.node_task ~swarm deps

let p2p_listen_task ~listen ~port =
  fun () -> listen ~port ~callback:(fun _ -> Lwt.return_unit)

let observer_loop () =
  let rec loop () =
    Lwt_unix.sleep 60.0 >>= fun () -> loop ()
  in
  loop ()

let node_launch_tasks (deps : node_launch_deps) =
  let swarm_task = node_swarm_task ~swarm:deps.swarm ~deps:deps.swarm_deps in
  task_plan
    ~base_tasks:
      (transport_tasks ~p2p:deps.p2p ~rpc:deps.rpc ~swarm:swarm_task
       @ List.map (fun service -> service ()) deps.services)
    ~observer:deps.observer
    ~observer_loop
    ~tick_loop:deps.tick_loop
    ~optional:swarm_task

let default_join_log =
  {
    fatal = Octra_log.fatal "init" "%s";
    warn = Octra_log.warn "init" "%s";
  }

let run_join ~log ~tasks ~close_chaindata ~exit_fatal =
  Lwt.catch
    (fun () -> Lwt.pick tasks)
    (fun e ->
      log.fatal
        (Printf.sprintf "event = lwt_main_failed reason = %s"
           (Printexc.to_string e));
      (try close_chaindata ()
       with close_error ->
         log.warn
           (Printf.sprintf "event = chaindata_close_failed reason = %s"
              (Printexc.to_string close_error)));
      log.warn "event = irmin_close_skipped reason = lwt_main_unwinding";
      exit_fatal ();
      Lwt.return_unit)

let run_launch_tasks (deps : unit Lwt.t launch_tasks) ~close_chaindata
    ~exit_fatal =
  run_join
    ~log:default_join_log
    ~tasks:(launch_tasks deps)
    ~close_chaindata
    ~exit_fatal

let run_node_launch_tasks (deps : node_launch_deps) ~close_chaindata
    ~exit_fatal =
  run_join
    ~log:default_join_log
    ~tasks:(node_launch_tasks deps)
    ~close_chaindata
    ~exit_fatal

let run_node_runtime (runtime : node_launch_runtime) =
  run_node_launch_tasks
    (make_node_launch_deps_with_swarm
       ~p2p:runtime.p2p
       ~rpc:runtime.rpc
       ~services:runtime.services
       ~observer:runtime.observer
       ~tick_loop:runtime.tick_loop
       ~swarm:runtime.swarm
       ~guard:runtime.guard
       ~find_tx:runtime.find_tx
       ~find_account:runtime.find_account
       ~add_tx:runtime.add_tx
       ~now:runtime.now
       ~max_drift:runtime.max_drift
       ~driver_ref:runtime.driver_ref
       ~resource_compute:runtime.resource_compute)
    ~close_chaindata:runtime.close_chaindata
    ~exit_fatal:runtime.exit_fatal