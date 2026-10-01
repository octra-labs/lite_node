(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

module SC = Store_chaindata
module SI = Store_irmin
module Head = Head_manifest
module Phase = Recovery_phase

type result = {
  marker_phase : string option;
  marker_epoch : int option;
  txlog_last_epoch : int;
  irmin_last_epoch_before : int;
  irmin_last_epoch_after : int;
  index_repaired : int;
  index_errors : string list;
  boundary_ok : bool;
  full_repair_skipped : bool;
  eic_checked : bool;
  eic_ok : bool;
  eic_errors : string list;
}

exception Refused of string

let refuse reason = raise (Refused reason)
let unwrap = function Ok value -> value | Error reason -> refuse reason

type effects = {
  sync_chain : SC.t -> unit;
  sync_index : Chaindata_index.t -> unit;
  sync_irmin : SI.t -> unit;
  write_head : string -> Head.t -> unit;
  sync_head : string -> unit;
  append_journal : string -> Commit_journal.record -> unit;
}

let effects = {
  append_journal = Commit_journal.append;
  sync_chain = SC.fsync;
  sync_index = Chaindata_index.sync;
  sync_irmin = (fun store -> SI.Store.flush store.SI.repo; SI.sync_branches store.store_path);
  write_head = Head.atomic_write;
  sync_head = (fun dir ->
    let file = Unix.openfile (Head.path dir) [Unix.O_RDONLY] 0 in
    Fun.protect ~finally:(fun () -> Unix.close file) (fun () -> Unix.fsync file);
    Epoch_commit_marker.sync_directory dir);
}

let selected = function
  | Phase.Empty _ -> None
  | Phase.Stay (head, _) | Phase.Trim (head, _) | Phase.Publish head | Phase.Cut (head, _, _) -> Some head

let completed chaindata input head (entry : Wal.entry) =
  if entry.Wal.epoch_id <= head.Head.epoch_id then begin
    let header = match Epochlog.get chaindata.SC.epochlog entry.epoch_id with
      | Some header -> header
      | None -> refuse "completed WAL epoch header is missing" in
    if header.start_txid <> entry.start_txid || header.tx_count <> entry.tx_count
       || entry.tx_count < 0 || entry.start_txid < 0L then
      refuse "completed WAL transaction range differs";
    let _, root = SC.get_epoch_index_commitment chaindata entry.epoch_id in
    let expected = match root with
      | None -> entry.post_state_root
      | Some root -> Epoch_index_commitment.folded_state_root
        ~ledger_state_root:entry.post_state_root ~epoch_index_root:root in
    if expected <> header.state_root then refuse "completed WAL post root differs";
    if entry.epoch_id = head.epoch_id
       && entry.post_state_root <> Head.ledger_state_root head then
      refuse "completed WAL post root differs from HEAD";
    if entry.epoch_id = head.epoch_id
       && input.Phase.irmin_parent <> Some (entry.parent_commit, entry.pre_state_root) then
      refuse "completed WAL predecessor differs from Irmin parent"
  end

let inspect ~data_dir ~chaindata ~store =
  let disk_head = match Head.load_result data_dir with
    | Head.Present head -> Some head
    | Head.Missing -> None
    | Head.Corrupt reason -> refuse ("invalid HEAD: " ^ reason) in
  let head = Option.map (fun (head : Head.t) ->
    let hash, root = SC.get_epoch_index_commitment chaindata head.epoch_id in
    match head.epoch_index_hash, head.epoch_index_root, head.ledger_state_root, hash, root with
    | None, None, Some ledger, Some hash, Some root
      when Epoch_index_commitment.folded_state_root ~ledger_state_root:ledger ~epoch_index_root:root
        = head.state_root -> {head with epoch_index_hash = Some hash; epoch_index_root = Some root}
    | _ -> head) disk_head in
  let* irmin_meta = SI.get_meta store "last_epoch" in
  let irmin_epoch = match irmin_meta with None -> -1 | Some value -> int_of_string value in
  let* irmin_root = SI.get_head_hash store in
  let* irmin_commit = SI.get_commit_hash store in
  let* current = SI.Store.Head.find store.SI.store in
  let* irmin_parent = match current with
    | Some commit -> (match SI.Store.Commit.parents commit with
      | [key] ->
        let* parent = SI.Store.Commit.of_key store.repo key in
        Lwt.return (Option.map (fun parent ->
          Irmin.Type.to_string SI.Store.Hash.t (SI.Store.Commit.hash parent),
          Irmin.Type.to_string SI.Store.Hash.t (SI.Store.Tree.hash (SI.Store.Commit.tree parent))) parent)
      | _ -> Lwt.return_none)
    | None -> Lwt.return_none in
  let chain_epoch = match SC.last_epoch_id chaindata with
    | Ok (Some epoch) -> epoch | Ok None -> -1 | Error reason -> refuse reason in
  let epoch_hash, epoch_root = SC.get_epoch_index_commitment chaindata irmin_epoch in
  let input = Phase.{head; irmin_epoch; irmin_root; irmin_commit; irmin_parent; chain_epoch;
    marker = Epoch_commit_marker.read_marker data_dir;
    wal = Wal.read_pending data_dir; journal = Commit_journal.read_all data_dir;
    last = Epochlog.get chaindata.SC.epochlog irmin_epoch; epoch_hash; epoch_root;
    tx_position = SC.txlog_position chaindata; epoch_offset = SC.epochlog_offset chaindata} in
  let plan = unwrap (Phase.decide input) in
  SC.verify_auxiliary chaindata (selected plan);
  (match plan with
  | Phase.Empty _ -> ()
  | Phase.Stay (head, _) | Phase.Trim (head, _) | Phase.Publish head | Phase.Cut (head, _, _) ->
    List.iter (completed chaindata input head) input.wal);
  let _, finish = Recovery_index.inspect chaindata (selected plan) in
  (match plan with
  | Phase.Publish head ->
    let previous = Option.get input.head in
    let expected = if head.txid_hi = previous.txid_hi then
      Option.bind previous.txlog_seg (fun segment ->
        Option.map (fun offset -> segment, offset) previous.txlog_off)
    else finish in
    if expected <> Some input.tx_position then refuse "forward journal suffix exceeds committed transactions"
  | Phase.Trim (head, _) ->
    unwrap (Head_cut.verify ~head ~index:chaindata.index ~txlog:chaindata.txlog
      ~epochlog:chaindata.epochlog);
    Trim_index.verify chaindata head
  | Phase.Cut (head, _, _) ->
    unwrap (Head_cut.verify ~head ~index:chaindata.index ~txlog:chaindata.txlog
      ~epochlog:chaindata.epochlog)
  | _ -> ());
  let* () = match selected plan with
    | None -> Lwt.return_unit
    | Some head ->
      let* tag = SI.Store.Branch.find store.repo (Printf.sprintf "epoch_%d" head.epoch_id) in
      (match tag with
      | None -> Lwt.return_unit
      | Some tag ->
        let commit = Irmin.Type.to_string SI.Store.Hash.t (SI.Store.Commit.hash tag) in
        if Some commit <> irmin_commit then Lwt.fail (Refused "epoch tag differs from current Irmin commit")
        else Lwt.return_unit) in
  let pending = Wal.read_pending_commits data_dir in
  Lwt.return (input, plan, pending, disk_head)

let recover_using ?(request = Phase.Resume) effects ~data_dir ~chaindata ~store =
  Epoch_commit_marker.require_recovery data_dir;
  let* input, plan, pending, disk_head = inspect ~data_dir ~chaindata ~store in
  let plan = unwrap (Phase.authorize request plan) in
  let head = selected plan in
  (match plan with
  | Phase.Trim (head, _) ->
    Txlog.truncate_to chaindata.txlog ~seg_id:(Option.get head.txlog_seg)
      ~offset:(Option.get head.txlog_off)
  | Phase.Cut (head, entry, _) ->
    ignore (SC.rollback_to_head chaindata ~head ~inflight_start_txid:entry.start_txid
      ~inflight_tx_count:entry.tx_count)
  | _ -> ());
  let _, repaired, legacy = Recovery_index.commit chaindata head in
  if legacy > 0 then Octra_log.warn "recovery"
    "event = unproven_legacy_prefix epochs = %d checks = ranges_locations_frames eic = unavailable" legacy;
  effects.sync_chain chaindata;
  effects.sync_index chaindata.index;
  let* () = match head with
    | None -> Lwt.return_unit
    | Some head -> SI.tag_epoch store head.epoch_id in
  effects.sync_irmin store;
  (match plan with
  | Phase.Publish head -> effects.write_head data_dir head
  | Phase.Stay (head, _) when Some head <> disk_head -> effects.write_head data_dir head
  | _ -> ());
  (match head with Some _ -> effects.sync_head data_dir | None -> ());
  (match head with Some head -> Head.set_cached head | None -> ());
  (match plan with
  | Phase.Publish head ->
    effects.append_journal data_dir (Commit_journal.Commit {
      commit_id = head.commit_id; generation = head.generation; ts = head.ts })
  | Phase.Cut (head, _, attempts) | Phase.Trim (head, attempts) | Phase.Stay (head, attempts) ->
    List.iter (fun commit_id -> effects.append_journal data_dir (Commit_journal.Abort {
      commit_id; reason = "recovery_to_head"; ts = head.ts })) attempts
  | Phase.Empty attempts ->
    let ts = Unix.gettimeofday () in
    List.iter (fun commit_id -> effects.append_journal data_dir (Commit_journal.Abort {
      commit_id; reason = "recovery_before_head"; ts })) attempts);
  SC.retire_auxiliary chaindata head;
  List.iter (fun (entry : Wal.entry) -> Wal.delete data_dir entry.epoch_id) input.wal;
  let cap = match head with None -> -1 | Some head -> head.epoch_id in
  List.iter (fun (entry : Wal.pending_commit) ->
    if entry.epoch_id <= cap then Wal.delete_pending_commit data_dir entry.epoch_id entry.round) pending;
  Epoch_commit_marker.clear_marker data_dir;
  let eic_checked = match head with
    | Some head -> head.epoch_index_root <> None | None -> false in
  Lwt.return {
    marker_phase = Option.map (fun marker -> marker.Epoch_commit_marker.phase) input.marker;
    marker_epoch = Option.map (fun marker -> marker.Epoch_commit_marker.epoch_id) input.marker;
    txlog_last_epoch = input.chain_epoch;
    irmin_last_epoch_before = input.irmin_epoch;
    irmin_last_epoch_after = input.irmin_epoch;
    index_repaired = repaired; index_errors = []; boundary_ok = true;
    full_repair_skipped = false; eic_checked; eic_ok = true; eic_errors = [];
  }

let recover ~data_dir ~chaindata ~store =
  recover_using effects ~data_dir ~chaindata ~store

let recover_cut ~data_dir ~chaindata ~store =
  recover_using ~request:Phase.Rollback effects ~data_dir ~chaindata ~store

let previous_eic_root chaindata epoch_id =
  if epoch_id <= 0 then Epoch_index_commitment.genesis_root
  else
    match Store_chaindata.get_meta chaindata (Printf.sprintf "eic_epoch_root:%d" (epoch_id - 1)) with
    | Some root -> root
    | None -> Epoch_index_commitment.genesis_root