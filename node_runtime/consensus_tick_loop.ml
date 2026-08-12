(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Plan = Consensus_tick_plan

type deps = {
  now : unit -> float;
  last_epoch_time : unit -> float;
  current_epoch : unit -> int;
  consensus_finalized : unit -> bool;
  find_finalized : int -> Octra_consensus.C_types.finalize option;
  cached_bundle_for_pid : string -> bool;
  header_has_empty_bundle : Octra_consensus.C_types.epoch_header -> bool;
  store_empty_bundle_for_header : Octra_consensus.C_types.epoch_header -> unit;
  queue_missing_bundle : target_epoch:int64 -> reason:string -> unit;
  warn : string -> unit;
  info : string -> unit;
  clear_finalized : unit -> unit;
  apply :
    finalize:Octra_consensus.C_types.finalize option ->
    now:float ->
    elapsed:float ->
    unit Lwt.t;
  sleep : float -> unit Lwt.t;
}

type node_runtime = {
  now : unit -> float;
  last_epoch_time : float ref;
  current_epoch : int ref;
  consensus_finalized : bool ref;
  finality : Consensus_finality_state.callbacks;
  bundle : Consensus_bundle_cache.node_runtime;
  queue_missing_bundle : target_epoch:int64 -> reason:string -> unit;
  warn : string -> unit;
  info : string -> unit;
  apply :
    finalize:Octra_consensus.C_types.finalize option ->
    now:float ->
    elapsed:float ->
    unit Lwt.t;
  sleep : float -> unit Lwt.t;
}

let tick_state (deps : deps) ~consensus_mode ~current_epoch =
  Plan.finalized_state
    ~consensus_mode
    ~consensus_finalized:(deps.consensus_finalized ())
    ~current_epoch
    ~find_finalized:deps.find_finalized
    ~cached_bundle_for_pid:deps.cached_bundle_for_pid
    ~header_has_empty_bundle:deps.header_has_empty_bundle

let store_empty_bundle (deps : deps) prepared =
  match prepared.Plan.empty_bundle_header with
  | Some header -> deps.store_empty_bundle_for_header header
  | None -> ()

let step (deps : deps) ~consensus_mode =
  let now = deps.now () in
  let current_epoch = deps.current_epoch () in
  let elapsed = now -. deps.last_epoch_time () in
  let prepared = tick_state deps ~consensus_mode ~current_epoch in
  let effective_duration =
    Epoch_cadence.duration_seconds
      ~commit_round:(Option.value prepared.commit_round ~default:0)
  in
  let tick_plan =
    Plan.plan
      ~consensus_mode
      ~epoch_duration:effective_duration
      ~elapsed
      ~finalized_state:prepared.state
  in
  Plan.apply_action
    ~current_epoch
    ~clear_trigger:
      (consensus_mode
       && prepared.state <> Plan.No_trigger
       && tick_plan.action <> Plan.Wait)
    ~store_empty_bundle:(fun () -> store_empty_bundle deps prepared)
    ~queue_missing_bundle:deps.queue_missing_bundle
    ~warn:deps.warn
    ~clear_finalized:deps.clear_finalized
    tick_plan.action;
  if tick_plan.log_tick then
    deps.info
      (Printf.sprintf
         "tick epoch = %d elapsed = %.2fs next_in = %.2fs"
         current_epoch
         elapsed
         (max 0. (effective_duration -. elapsed)));
  Lwt.bind
    (if Plan.should_apply tick_plan.action then
       deps.apply ~finalize:prepared.finalize ~now ~elapsed
     else
       Lwt.return_unit)
    (fun () -> Lwt.return tick_plan.next_sleep)

let rec run (deps : deps) ~consensus_mode =
  Lwt.bind
    (step deps ~consensus_mode)
    (fun next_sleep ->
      Lwt.bind
        (deps.sleep next_sleep)
        (fun () -> run deps ~consensus_mode))

let node_deps runtime =
  {
    now = runtime.now;
    last_epoch_time = (fun () -> !(runtime.last_epoch_time));
    current_epoch = (fun () -> !(runtime.current_epoch));
    consensus_finalized = (fun () -> !(runtime.consensus_finalized));
    find_finalized = runtime.finality.find_finalized;
    cached_bundle_for_pid = (fun pid ->
      runtime.bundle.cached_bundle pid <> None);
    header_has_empty_bundle = runtime.bundle.header_has_empty_bundle;
    store_empty_bundle_for_header = runtime.bundle.store_empty_bundle;
    queue_missing_bundle = runtime.queue_missing_bundle;
    warn = runtime.warn;
    info = runtime.info;
    clear_finalized = (fun () -> runtime.consensus_finalized := false);
    apply = runtime.apply;
    sleep = runtime.sleep;
  }

let run_node runtime ~consensus_mode =
  run (node_deps runtime) ~consensus_mode