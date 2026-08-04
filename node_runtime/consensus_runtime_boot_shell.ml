(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Epochlog = Octra_core.Epochlog
module Store_chaindata = Octra_core.Store_chaindata
module Tree = Octra_core.Tree

type deps = {
  current_epoch : int ref;
  chaindata : Store_chaindata.t;
  runtime_env : Startup_runtime_limits.env;
  default_max_ou : Z.t;
  now : unit -> float;
}

type t = {
  last_epoch_time : float ref;
  stealth_defer_count : (string, int) Hashtbl.t;
  private_limits : Startup_runtime_limits.private_limits;
  stealth_in_epoch_counter : int ref;
  fhe_in_epoch_counter : int ref;
  tree : Tree.t ref;
  swarm_opt : Octra_net.P2p_swarm.t option ref;
  consensus_finalized : bool ref;
  catchup_in_progress : bool ref;
  catchup_queue : Consensus_catchup_queue.t;
  consensus_state : Consensus_runtime_state.t;
  consensus_limits : Startup_runtime_limits.consensus_limits;
  driver_ref : Octra_consensus.C_driver.t option ref;
  consensus_liveness : Consensus_liveness.state ref;
  proposal_state : Consensus_proposal_state.t;
  finality_state : Consensus_finality_state.t;
  finality_callbacks : Consensus_finality_state.callbacks;
  proposal_bundles : Consensus_bundle_cache.t;
  proposal_bundle_runtime : Consensus_bundle_cache.node_runtime;
  bft_proposal_limits : Consensus_proposal.limits;
  current_consensus_round : unit -> int;
  clear_state_attested : unit -> unit;
  set_state_attested : head:int -> root:string -> unit;
  mark_quarantine : string -> unit;
  clear_quarantine : string -> unit;
  catchup_node_queue : Consensus_catchup_shell.node_queue;
}

let parent_commit_or_genesis = function
  | Some header -> header.Epochlog.parent_commit
  | None -> "GENESIS"

let last_commit chaindata =
  match Store_chaindata.get_last_epoch chaindata with
  | Some _ as header -> parent_commit_or_genesis header
  | None ->
      begin
        match Store_chaindata.history_floor chaindata with
        | Ok (Some floor) ->
            Octra_core.History_floor.parent_commit floor
            |> Octra_consensus.C_parent_commit.hash
            |> Octra_bootstrap.State_sync_checkpoint.raw_to_hex
        | Ok None -> "GENESIS"
        | Error reason -> failwith reason
      end

let driver_round driver_ref =
  match !driver_ref with
  | Some driver -> Octra_consensus.C_driver.current_round driver
  | None -> 0

let create deps =
  let private_limits =
    Startup_runtime_limits.private_limits deps.runtime_env
  in
  let consensus_limits =
    Startup_runtime_limits.consensus_limits deps.runtime_env
  in
  let tree =
    ref (Tree.create
      ~epoch_id:!(deps.current_epoch)
      ~parent_commit:(last_commit deps.chaindata))
  in
  let catchup_queue = Consensus_catchup_queue.create () in
  let consensus_state = Consensus_runtime_state.create () in
  let driver_ref = ref None in
  let finality_state = Consensus_finality_state.create () in
  let finality_callbacks =
    Consensus_finality_state.callbacks finality_state
  in
  let proposal_bundles =
    Consensus_bundle_cache.create
      ~cap:consensus_limits.bundle_cache_cap
  in
  let proposal_bundle_runtime =
    Consensus_bundle_cache.node_runtime proposal_bundles
  in
  let state_ops =
    Consensus_health_wiring.node_runtime_state_ops
      {
        state = consensus_state;
        current_epoch = (fun () -> !(deps.current_epoch));
      }
  in
  let catchup_in_progress = ref false in
  let catchup_node_queue =
    Consensus_catchup_shell.node_queue
      {
        queue = catchup_queue;
        catchup_active = catchup_in_progress;
        clear_state_attested = state_ops.clear_state_attested;
      }
  in
  {
    last_epoch_time = ref (deps.now ());
    stealth_defer_count = Hashtbl.create 16;
    private_limits;
    stealth_in_epoch_counter = ref 0;
    fhe_in_epoch_counter = ref 0;
    tree;
    swarm_opt = ref None;
    consensus_finalized = ref false;
    catchup_in_progress;
    catchup_queue;
    consensus_state;
    consensus_limits;
    driver_ref;
    consensus_liveness = ref (Consensus_liveness.create ~now:(deps.now ()));
    proposal_state = Consensus_proposal_state.create ();
    finality_state;
    finality_callbacks;
    proposal_bundles;
    proposal_bundle_runtime;
    bft_proposal_limits =
      Startup_runtime_limits.proposal_limits
        deps.runtime_env
        ~default_max_ou:deps.default_max_ou;
    current_consensus_round = (fun () -> driver_round driver_ref);
    clear_state_attested = state_ops.clear_state_attested;
    set_state_attested = state_ops.set_state_attested;
    mark_quarantine = state_ops.mark_quarantine;
    clear_quarantine = state_ops.clear_quarantine;
    catchup_node_queue;
  }