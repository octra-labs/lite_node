(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Broadcast = Consensus_epoch_apply_broadcast
module P2p_swarm = Octra_net.P2p_swarm
module Staging = Octra_core.Tx_staging
module Transaction = Octra_core.Transaction
module Tree = Octra_core.Tree

type deps = {
  trace : string -> unit;
  log_deferred : int -> unit;
  log_expired : int -> unit;
  log_broadcast : Broadcast.log_view -> peers:int -> unit;
  set_last_epoch_time : float -> unit;
  reset_tree : epoch_id:int -> parent_commit:string -> unit;
  remove_processed : string list -> unit;
  clear_deferred : unit -> int;
  reset_private_counters : unit -> unit;
  expire_old : unit -> Staging.drop_record list;
  save_drops : Staging.drop_record list -> unit;
  cleanup_dropped : unit -> unit;
  sweep_low_fee : unit -> unit;
  retain_live_preverify : unit -> unit;
  prune_preverify : unit -> unit;
  notify_staging_update : unit -> unit;
  peer_count : unit -> int;
  broadcast : Broadcast.message -> unit Lwt.t;
}

type ctx = {
  now : float;
  epoch_id : int;
  parent_commit : string;
  post_consensus_root : string;
  confirmed_count : int;
  producer : string;
  processed_hashes : string list;
  txs_serialized : string list;
}

type node_refs = {
  last_epoch_time : float ref;
  tree : Tree.t ref;
  deferred_stealth_txs : Transaction.t list ref;
  stealth_in_epoch_counter : int ref;
  fhe_in_epoch_counter : int ref;
  swarm_opt : P2p_swarm.t option ref;
  save_drops : Staging.drop_record list -> unit;
}

let refs ~last_epoch_time ~tree ~deferred_stealth_txs
    ~stealth_in_epoch_counter ~fhe_in_epoch_counter ~swarm_opt ~save_drops =
  {
    last_epoch_time;
    tree;
    deferred_stealth_txs;
    stealth_in_epoch_counter;
    fhe_in_epoch_counter;
    swarm_opt;
    save_drops;
  }

let cleanup deps ctx =
  deps.trace "step:cleanup";
  deps.set_last_epoch_time ctx.now;
  deps.reset_tree ~epoch_id:(ctx.epoch_id + 1) ~parent_commit:ctx.parent_commit;
  deps.remove_processed ctx.processed_hashes;
  let deferred = deps.clear_deferred () in
  if deferred > 0 then deps.log_deferred deferred;
  deps.reset_private_counters ();
  let expired = deps.expire_old () in
  if expired <> [] then begin
    deps.save_drops expired;
    deps.log_expired (List.length expired)
  end;
  deps.cleanup_dropped ();
  deps.sweep_low_fee ();
  deps.retain_live_preverify ();
  deps.prune_preverify ()

let notify deps ctx =
  deps.trace "step:ws_notify";
  deps.notify_staging_update ();
  let peers = deps.peer_count () in
  if peers <= 0 then
    Lwt.return_unit
  else
    let message =
      Broadcast.message
        ~epoch_id:ctx.epoch_id
        ~root:ctx.post_consensus_root
        ~confirmed_count:ctx.confirmed_count
        ~producer:ctx.producer
        ~txs_serialized:ctx.txs_serialized
    in
    let open Lwt.Syntax in
    let* () = deps.broadcast message in
    deps.log_broadcast (Broadcast.log_view message) ~peers;
    Lwt.return_unit

let run deps ctx =
  cleanup deps ctx;
  notify deps ctx

let node_deps refs =
  {
    trace = (fun event -> Log.trace "epoch" "%s" event);
    log_deferred = (fun count ->
      Log.info "epoch" "deferred_stealth_requeue count = %d" count);
    log_expired = (fun count ->
      Log.info "staging" "expired count = %d reason = ttl" count);
    log_broadcast = (fun view ~peers ->
      Log.info "consensus" "broadcast epoch = %d root = %s..%s peers = %d"
        view.Broadcast.epoch_id
        view.root_head
        view.root_tail
        peers);
    set_last_epoch_time = (fun value -> refs.last_epoch_time := value);
    reset_tree = (fun ~epoch_id ~parent_commit ->
      refs.tree := Tree.create ~epoch_id ~parent_commit);
    remove_processed = Staging.remove_processed;
    clear_deferred = (fun () ->
      let count = List.length !(refs.deferred_stealth_txs) in
      refs.deferred_stealth_txs := [];
      count);
    reset_private_counters = (fun () ->
      refs.stealth_in_epoch_counter := 0;
      refs.fhe_in_epoch_counter := 0);
    expire_old = Staging.expire_old;
    save_drops = refs.save_drops;
    cleanup_dropped = Staging.cleanup_dropped;
    sweep_low_fee = (fun () -> ignore (Node_rest_facade.sweep_low_fee_stealth ()));
    retain_live_preverify = (fun () ->
      Preverify_cache.retain (fun k -> Staging.find_by_hash k <> None));
    prune_preverify = Preverify_cache.prune;
    notify_staging_update = Node_rest_facade.notify_staging_update;
    peer_count = (fun () ->
      match !(refs.swarm_opt) with
      | Some swarm -> P2p_swarm.peer_count swarm
      | None -> 0);
    broadcast = (fun message ->
      match !(refs.swarm_opt) with
      | Some swarm -> P2p_swarm.broadcast swarm (Broadcast.frame message)
      | None -> Lwt.return_unit);
  }

let run_node refs ctx =
  run (node_deps refs) ctx