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


type deps = {
  p2p_port : int;
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

let p2p p2p_port =
  Startup_run_shell.p2p_listen_task
    ~listen:Octra_core.P2p.listen
    ~port:p2p_port

let log_observer_mode observer =
  if observer then
    Log.info "init" "event = observer_mode epoch_production = false p2p_shadow = true"

let run deps =
  log_observer_mode deps.observer;
  Lwt_main.run (
    Startup_run_shell.run_node_runtime
      Startup_run_shell.{
        p2p = p2p deps.p2p_port;
        rpc = deps.rpc;
        observer = deps.observer;
        tick_loop = deps.tick_loop;
        swarm = deps.swarm;
        guard = deps.guard;
        find_tx = deps.find_tx;
        find_account = deps.find_account;
        add_tx = deps.add_tx;
        now = deps.now;
        max_drift = deps.max_drift;
        driver_ref = deps.driver_ref;
        close_chaindata = deps.close_chaindata;
        exit_fatal = deps.exit_fatal;
      })