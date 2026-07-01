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
  observer : bool;
  tick_loop : unit -> unit Lwt.t;
  swarm : Octra_net.P2p_swarm.t option;
  swarm_deps : P2p_swarm_lifecycle.node_deps;
}

let make_node_swarm_deps ~observer ~guard ~find_tx ~find_account ~add_tx
    ~now ~max_drift ~driver_ref =
  P2p_swarm_lifecycle.{
    observer;
    guard;
    find_tx;
    find_account;
    add_tx;
    now;
    max_drift;
    driver_ref;
  }

let make_node_launch_deps ~p2p ~rpc ~observer ~tick_loop ~swarm ~swarm_deps =
  { p2p; rpc; observer; tick_loop; swarm; swarm_deps }

let make_node_launch_deps_with_swarm ~p2p ~rpc ~observer ~tick_loop ~swarm
    ~guard ~find_tx ~find_account ~add_tx ~now ~max_drift ~driver_ref =
  make_node_launch_deps
    ~p2p
    ~rpc
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
         ~driver_ref)

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

let observer_loop () =
  let rec loop () =
    Lwt_unix.sleep 60.0 >>= fun () -> loop ()
  in
  loop ()

let node_launch_tasks (deps : node_launch_deps) =
  launch_tasks
    {
      p2p = deps.p2p;
      rpc = deps.rpc;
      observer = deps.observer;
      observer_loop;
      tick_loop = deps.tick_loop;
      swarm = (fun () -> node_swarm_task ~swarm:deps.swarm ~deps:deps.swarm_deps);
    }

let run_join ~tasks ~close_chaindata ~exit_fatal =
  Lwt.catch
    (fun () -> Lwt.join tasks)
    (fun e ->
      Octra_log.fatal "init" "lwt_main = failed reason = %s"
        (Printexc.to_string e);
      (try close_chaindata ()
       with close_error ->
         Octra_log.warn "init" "chaindata_close = failed reason = %s"
           (Printexc.to_string close_error));
      Octra_log.warn "init" "irmin_close = skipped reason = lwt_main_unwinding";
      exit_fatal ();
      Lwt.return_unit)

let run_launch_tasks (deps : unit Lwt.t launch_tasks) ~close_chaindata
    ~exit_fatal =
  run_join
    ~tasks:(launch_tasks deps)
    ~close_chaindata
    ~exit_fatal

let run_node_launch_tasks (deps : node_launch_deps) ~close_chaindata
    ~exit_fatal =
  run_join
    ~tasks:(node_launch_tasks deps)
    ~close_chaindata
    ~exit_fatal