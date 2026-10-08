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
  follow : bool;
  tick_loop : unit -> unit Lwt.t;
  swarm : Octra_net.P2p_swarm.t option;
  swarm_deps : P2p_swarm_lifecycle.node_deps;
}

type node_launch_runtime = {
  p2p : unit -> unit Lwt.t;
  rpc : unit -> unit Lwt.t;
  services : (unit -> unit Lwt.t) list;
  observer : bool;
  follow : bool;
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

let make_node_launch_deps ~p2p ~rpc ~services ~observer ~follow ~tick_loop ~swarm
    ~swarm_deps =
  { p2p; rpc; services; observer; follow; tick_loop; swarm; swarm_deps }

let make_node_launch_deps_with_swarm ~p2p ~rpc ~services ~observer ~follow ~tick_loop
    ~swarm ~guard ~find_tx ~find_account ~add_tx ~now ~max_drift ~driver_ref
    ~resource_compute =
  make_node_launch_deps
    ~p2p
    ~rpc
    ~services
    ~observer
    ~follow
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

let idle ~observer ~follow =
  observer && not follow

let node_launch_tasks ?duty_head ?bft_mode (deps : node_launch_deps) =
  let swarm_task = P2p_swarm_lifecycle.node_task
    ?duty_head ?bft_mode ~swarm:deps.swarm deps.swarm_deps in
  task_plan
    ~base_tasks:
      (transport_tasks ~p2p:deps.p2p ~rpc:deps.rpc ~swarm:swarm_task
       @ List.map (fun service -> service ()) deps.services)
    ~observer:(idle ~observer:deps.observer ~follow:deps.follow)
    ~observer_loop
    ~tick_loop:deps.tick_loop
    ~optional:swarm_task

let default_join_log =
  {
    fatal = Octra_log.fatal "init" "%s";
    warn = Octra_log.warn "init" "%s";
  }

let exit_fatal () = Unix._exit 1

let exit_store = Epoch_atomic.exit_store

let require_sync ~data_dir ~chain ~store need =
  let status, stored = match Sync_mark.write ~data_dir ~chain need with
    | Ok Sync_mark.Stored -> "stored", need
    | Ok (Sync_mark.Present prior) -> "present", prior
    | Error reason ->
      Log.fatal "consensus" "event = sync_recovery status = rejected reason = %s" reason;
      exit_store store () in
  Log.fatal "consensus"
    "event = sync_recovery status = %s cause = %s epoch = %d head = %d action = exit"
    status (Sync_need.label stored.Sync_need.cause) stored.epoch stored.head;
  exit_store store ()

let run_join ~log ~tasks ~exit_fatal ~exit_refused =
  Lwt.catch
    (fun () -> Lwt.pick tasks)
    (fun e ->
      let exit = match e with
        | Octra_core.Private_ledger.Worker_stopped _ -> exit_refused
        | _ -> exit_fatal in
      Fun.protect ~finally:exit (fun () ->
        log.fatal
          (Printf.sprintf "event = lwt_main_failed reason = %s"
             (Printexc.to_string e));
        log.warn "event = store_ownership_retained reason = fatal_exit");
      Lwt.return_unit)

let run_launch_tasks ?(exit_refused = fun () -> Unix._exit 78)
    (deps : unit Lwt.t launch_tasks) ~exit_fatal =
  run_join
    ~log:default_join_log
    ~tasks:(launch_tasks deps)
    ~exit_refused
    ~exit_fatal

let run_node_launch_tasks ?duty_head ?bft_mode ?(exit_refused = fun () -> Unix._exit 78)
    (deps : node_launch_deps)
    ~exit_fatal =
  run_join
    ~log:default_join_log
    ~tasks:(node_launch_tasks ?duty_head ?bft_mode deps)
    ~exit_refused
    ~exit_fatal

let run_node_runtime ?duty_head ?bft_mode ?exit_refused (runtime : node_launch_runtime) =
  run_node_launch_tasks
    ?duty_head
    ?bft_mode
    ?exit_refused
    (make_node_launch_deps_with_swarm
       ~p2p:runtime.p2p
       ~rpc:runtime.rpc
       ~services:runtime.services
       ~observer:runtime.observer
       ~follow:runtime.follow
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
    ~exit_fatal:runtime.exit_fatal