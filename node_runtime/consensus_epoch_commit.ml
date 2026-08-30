(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Commit_journal = Octra_core.Commit_journal
module Epoch_boundary = Octra_core.Epoch_boundary
module Epoch_exec = Octra_core.Epoch_exec
module Epoch_index_commitment = Octra_core.Epoch_index_commitment
module Epochlog = Octra_core.Epochlog
module Head_manifest = Octra_core.Head_manifest
module Store_irmin = Octra_core.Store_irmin
module Store_chaindata = Octra_core.Store_chaindata
module Transaction = Octra_core.Transaction
module Wal = Octra_core.Wal

let planned_txid_hi ~next_txid ~confirmed_count head =
  if confirmed_count > 0 then
    Int64.sub next_txid 1L
  else
    match head with
    | Some h -> h.Head_manifest.txid_hi
    | None ->
      if Int64.compare next_txid 0L <= 0 then -1L
      else Int64.sub next_txid 1L

let prev_epoch_index_root = function
  | Some h ->
    (match h.Head_manifest.ledger_state_root, h.Head_manifest.epoch_index_root with
     | Some _, Some root -> root
     | _ -> Epoch_index_commitment.genesis_root)
  | None -> Epoch_index_commitment.genesis_root

type index_plan = {
  start_txid : int64;
  planned_txid_hi : int64;
  epoch_index_hash : string;
  epoch_index_root : string;
}

let index_plan ~next_txid ~confirmed_count ~confirmed_txs ~epoch_id head =
  let start_txid = Int64.sub next_txid (Int64.of_int confirmed_count) in
  let eic_items =
    List.mapi (fun i tx ->
      Epoch_index_commitment.item
        ~txid:(Int64.add start_txid (Int64.of_int i))
        ~hash:(Transaction.hash tx)
    ) confirmed_txs
  in
  let epoch_index_hash, epoch_index_root =
    Epoch_index_commitment.next_root
      ~prev:(prev_epoch_index_root head)
      ~epoch_id
      eic_items
  in
  {
    start_txid;
    planned_txid_hi = planned_txid_hi ~next_txid ~confirmed_count head;
    epoch_index_hash;
    epoch_index_root;
  }

let prepare_record ~commit_id ~prev_generation ~epoch_id ~planned_txid_hi
    ~planned_state_root ~ts =
  Commit_journal.Prepare {
    commit_id;
    prev_generation;
    epoch_id;
    planned_txid_hi;
    planned_state_root;
    ts;
  }

let commit_record ~commit_id ~generation ~ts =
  Commit_journal.Commit {
    commit_id;
    generation;
    ts;
  }

let wal_entry ~epoch_id ~pre_state_root ~post_state_root ~parent_commit
    ~start_txid ~tx_count ~finalized_by ~finalized_at
    ~irmin_last_epoch_before =
  Wal.{
    epoch_id;
    pre_state_root;
    post_state_root;
    parent_commit;
    start_txid;
    tx_count;
    finalized_by;
    finalized_at;
    irmin_last_epoch_before;
  }

type boundary_log = {
  level : [ `Info | `Warn ];
  line : string;
}

type commit_boundary = {
  boundary : Epoch_boundary.plan;
  irmin_last_before : int;
  log : boundary_log option;
}

type rollback_offsets = {
  head_epoch : int;
  txlog_seg : int;
  txlog_off : int;
  epochlog_off : int;
}

type rollback_refusal = {
  head_epoch : int;
  commit_epoch : int;
}

type rollback_ok = {
  tx_loc : int;
  epoch_meta : int;
  addr_tx : int;
  txid_loc : int;
}

type rollback_plan =
  | Rollback_to_head of rollback_offsets
  | Rollback_missing_offsets
  | Rollback_refused of rollback_refusal
  | Rollback_missing_head

type prepare_effects = {
  head : unit -> Head_manifest.t option;
  irmin_last_epoch : unit -> int;
  next_txid : unit -> int64;
  commit_id : int -> string;
}

type prepare_input = {
  epoch_id : int;
  finalized_at : float;
  pre_state_hash : string;
  confirmed_count : int;
  confirmed_txs : Transaction.t list;
}

type prepared_commit = {
  head_before_commit : Head_manifest.t option;
  boundary : Epoch_boundary.plan;
  irmin_last_before : int;
  log : boundary_log option;
  finalized_at : float;
  commit_id : string;
  index : index_plan;
  rollback : rollback_plan;
}

type rollback_effects = {
  rollback_to_head : rollback_offsets -> int * int * int * int;
  delete_wal : unit -> unit;
  clear_marker : unit -> unit;
  log : string -> unit;
}

type commit_progress_state = {
  chaindata_committed : bool;
  irmin_commit_started : bool;
  head_committed : bool;
}

type commit_progress = commit_progress_state ref

type failure_effects = {
  rollback : unit -> bool;
  abort_chaindata : unit -> unit;
  abort_irmin : unit -> unit;
  log : string -> unit;
  exit_later : unit -> unit Lwt.t;
}

type commit_effects = {
  append_journal : Commit_journal.record -> unit;
  chaos : string -> unit;
  write_marker : int -> string -> unit;
  write_wal : Wal.entry -> unit;
  write_receipts : epoch_id:int -> receipts:string list -> unit;
  set_epoch : Epochlog.epoch_header -> unit;
  set_epoch_index_commitment :
    epoch_id:int -> epoch_hash:string -> root:string -> unit;
  fsync_chaindata : unit -> unit;
  commit_chaindata_batch : unit -> unit;
  verify_history :
    epoch_id:int -> start_txid:int64 -> tx_count:int ->
    Store_chaindata.epoch_index_status;
  commit_irmin_batch : string -> unit Lwt.t;
  tag_epoch : int -> unit Lwt.t;
  irmin_commit_hash : unit -> string option Lwt.t;
  txlog_position : unit -> int * int;
  epochlog_offset : unit -> int;
  write_head : Head_manifest.t -> unit;
  cache_head : Head_manifest.t -> unit;
  delete_wal : int -> unit;
  delete_pending_commits : int -> unit;
  clear_marker : unit -> unit;
  trace : string -> unit;
  fatal : string -> unit;
  now : unit -> float;
}

type live_effects = {
  data_dir : string;
  store : Store_irmin.t;
  ledger : Octra_core.Ledger.t;
  chaindata : Store_chaindata.t;
  trace : string -> unit;
  fatal : string -> unit;
  log : string -> unit;
  exit : unit -> unit;
}

type commit_request = {
  epoch_id : int;
  pre_state_root : string;
  post_state_root : string;
  post_consensus_root : string;
  prev_state_root : string;
  parent_commit : string;
  start_txid : int64;
  tx_count : int;
  finalized_by : string;
  finalized_at : float;
  proposer : Epochlog.proposer_info;
  confirmed_fees : Z.t;
  plan : Epoch_exec.reward_plan;
  reward_recipients : Epochlog.reward_recipient list;
  reward_source : Octra_consensus.C_types.reward_source;
  epoch_receipts_json : string list;
  commit_id : string;
  prev_generation : int;
  planned_txid_hi : int64;
  epoch_index_hash : string;
  epoch_index_root : string;
  progress : commit_progress;
}

let short16 value =
  String.sub value 0 (min 16 (String.length value))

let head_ledger_root h =
  short16 (Head_manifest.ledger_state_root h)

let boundary_log ~pre_state_hash ~irmin_last_before_meta
    ~head_before_commit boundary =
  let pre_state = short16 pre_state_hash in
  match boundary.Epoch_boundary.kind, head_before_commit with
  | Epoch_boundary.Aligned, _ ->
    None
  | Epoch_boundary.Root_mismatch, Some h ->
    Some {
      level = `Warn;
      line =
        Printf.sprintf
          "event = wal_boundary action = refuse reason = head_root_mismatch head_epoch = %d head_root = %s live_root = %s irmin_meta = %d"
          h.Head_manifest.epoch_id
          (head_ledger_root h)
          pre_state
          irmin_last_before_meta;
    }
  | Epoch_boundary.Unexpected_head, Some h ->
    Some {
      level = `Warn;
      line =
        Printf.sprintf
          "event = wal_boundary action = fallback_irmin_meta head_epoch = %d ledger_root = %s pre_state = %s irmin_meta = %d"
          h.Head_manifest.epoch_id
          (head_ledger_root h)
          pre_state
          irmin_last_before_meta;
    }
  | Epoch_boundary.Missing_head, _ ->
    Some {
      level = `Warn;
      line =
        Printf.sprintf
          "event = wal_boundary action = fallback_irmin_meta head_epoch = missing pre_state = %s irmin_meta = %d"
          pre_state
          irmin_last_before_meta;
    }
  | _, None ->
    None

let commit_boundary ~commit_epoch ~pre_state_hash ~irmin_last_before_meta
    ~head_before_commit =
  let boundary_head =
    match head_before_commit with
    | Some h ->
      Some {
        Epoch_boundary.epoch_id = h.Head_manifest.epoch_id;
        ledger_root = Head_manifest.ledger_state_root h;
      }
    | None ->
      None
  in
  let boundary =
    Epoch_boundary.plan
      ~commit_epoch
      ~pre_root:pre_state_hash
      ~meta_epoch:irmin_last_before_meta
      boundary_head
  in
  {
    boundary;
    irmin_last_before = boundary.irmin_last_before;
    log =
      boundary_log
        ~pre_state_hash
        ~irmin_last_before_meta
        ~head_before_commit
        boundary;
  }

let rollback_plan ~commit_epoch ~(boundary : Epoch_boundary.plan) = function
  | Some h when h.Head_manifest.epoch_id = commit_epoch - 1
                && boundary.rollback_to_head ->
    (match h.Head_manifest.txlog_seg,
           h.Head_manifest.txlog_off,
           h.Head_manifest.epochlog_off with
     | Some txlog_seg, Some txlog_off, Some epochlog_off ->
       Rollback_to_head {
         head_epoch = h.Head_manifest.epoch_id;
         txlog_seg;
         txlog_off;
         epochlog_off;
       }
     | _ -> Rollback_missing_offsets)
  | Some h ->
    Rollback_refused {
      head_epoch = h.Head_manifest.epoch_id;
      commit_epoch;
    }
  | None ->
    Rollback_missing_head

let prepare_commit (effects : prepare_effects) (input : prepare_input) =
  let head_before_commit = effects.head () in
  let irmin_last_before_meta = effects.irmin_last_epoch () in
  let boundary =
    commit_boundary
      ~commit_epoch:input.epoch_id
      ~pre_state_hash:input.pre_state_hash
      ~irmin_last_before_meta
      ~head_before_commit
  in
  let index =
    index_plan
      ~next_txid:(effects.next_txid ())
      ~confirmed_count:input.confirmed_count
      ~confirmed_txs:input.confirmed_txs
      ~epoch_id:input.epoch_id
      head_before_commit
  in
  {
    head_before_commit;
    boundary = boundary.boundary;
    irmin_last_before = boundary.irmin_last_before;
    log = boundary.log;
    finalized_at = input.finalized_at;
    commit_id = effects.commit_id input.epoch_id;
    index;
    rollback =
      rollback_plan
        ~commit_epoch:input.epoch_id
        ~boundary:boundary.boundary
        head_before_commit;
  }

let rollback_start_log (r : rollback_offsets) =
  Printf.sprintf
    "event = rollback action = start head_epoch = %d txlog_seg = %d txlog_off = %d epochlog_off = %d"
    r.head_epoch
    r.txlog_seg
    r.txlog_off
    r.epochlog_off

let rollback_ok_log (r : rollback_ok) =
  Printf.sprintf
    "event = rollback action = ok tx_loc = %d epoch_meta = %d addr_tx = %d txid_loc = %d"
    r.tx_loc
    r.epoch_meta
    r.addr_tx
    r.txid_loc

let rollback_failed_log reason =
  Printf.sprintf "event = rollback action = failed reason = %s" reason

let rollback_refused_log (r : rollback_refusal) =
  Printf.sprintf
    "event = rollback action = refused head_epoch = %d commit_epoch = %d"
    r.head_epoch
    r.commit_epoch

let rollback_unavailable_log =
  "event = rollback action = unavailable recovery = startup"

let commit_failed_logs ~epoch_id ~reason =
  [
    Printf.sprintf
      "event = commit_failure epoch = %d reason = %s"
      epoch_id
      reason;
    "event = commit_failure marker = retained action = investigate_before_restart";
    "event = commit_failure abort = chaindata_irmin exit = 1";
  ]

let commit_progress () =
  ref {
    chaindata_committed = false;
    irmin_commit_started = false;
    head_committed = false;
  }

let mark_chaindata_committed progress =
  progress := { !progress with chaindata_committed = true }

let mark_irmin_commit_started progress =
  progress := { !progress with irmin_commit_started = true }

let mark_head_committed progress =
  progress := { !progress with head_committed = true }

let rollback_needed progress =
  let state = !progress in
  state.chaindata_committed
  && not state.irmin_commit_started
  && not state.head_committed

let handle_failure ~progress ~epoch_id ~(effects : failure_effects) exn =
  let reason = Printexc.to_string exn in
  commit_failed_logs ~epoch_id ~reason
  |> List.iter effects.log;
  if rollback_needed progress then begin
    let ok = effects.rollback () in
    if not ok then
      effects.log rollback_unavailable_log
  end;
  (try effects.abort_chaindata () with _ -> ());
  (try effects.abort_irmin () with _ -> ());
  Lwt.async effects.exit_later;
  Lwt.fail exn

let history_incomplete_lines ~epoch_id ~start_txid ~tx_count
    (status : Store_chaindata.epoch_index_status) =
  Printf.sprintf
    "event = history_incomplete epoch = %d start_txid = %Ld tx_count = %d missing_epoch_meta = %b missing_txid_loc = %d missing_tx_loc = %d missing_addr_refs = %d malformed = %d errors = %d"
    epoch_id
    start_txid
    tx_count
    status.missing_epoch_meta
    status.missing_txid_loc
    status.missing_tx_loc
    status.missing_addr_refs
    status.malformed_records
    (List.length status.errors)
  :: List.map
    (fun error ->
      Printf.sprintf
        "event = history_incomplete_detail epoch = %d detail = %s"
        epoch_id
        error)
    status.errors

let run_rollback ~(effects : rollback_effects) = function
  | Rollback_to_head r ->
    effects.log (rollback_start_log r);
    (try
      let tx_loc, epoch_meta, addr_tx, txid_loc = effects.rollback_to_head r in
      effects.log (rollback_ok_log { tx_loc; epoch_meta; addr_tx; txid_loc });
      effects.delete_wal ();
      effects.clear_marker ();
      true
    with ex ->
      effects.log (rollback_failed_log (Printexc.to_string ex));
      false)
  | Rollback_missing_offsets ->
    effects.log (rollback_failed_log "missing_head_offsets");
    false
  | Rollback_refused r ->
    effects.log (rollback_refused_log r);
    false
  | Rollback_missing_head ->
    effects.log (rollback_failed_log "missing_head");
    false

let live_rollback_effects deps ~commit_epoch ~start_txid ~tx_count =
  {
    rollback_to_head = (fun r ->
      Store_chaindata.rollback_to_head deps.chaindata
        ~head_epoch:r.head_epoch
        ~head_txlog_seg:r.txlog_seg
        ~head_txlog_off:r.txlog_off
        ~head_epochlog_off:r.epochlog_off
        ~inflight_start_txid:start_txid
        ~inflight_tx_count:tx_count);
    delete_wal = (fun () -> Wal.delete deps.data_dir commit_epoch);
    clear_marker = (fun () ->
      Octra_core.Epoch_commit_marker.clear_marker deps.data_dir);
    log = deps.log;
  }

let live_commit_effects deps =
  {
    append_journal = Commit_journal.append deps.data_dir;
    chaos = Octra_core.Chaos.inject;
    write_marker = Octra_core.Epoch_commit_marker.write_marker deps.data_dir;
    write_wal = Wal.write deps.data_dir;
    write_receipts = Octra_core.Preverify_receipt_store.write deps.data_dir;
    set_epoch = Store_chaindata.set_epoch deps.chaindata;
    set_epoch_index_commitment =
      Store_chaindata.set_epoch_index_commitment deps.chaindata;
    fsync_chaindata = (fun () -> Store_chaindata.fsync deps.chaindata);
    commit_chaindata_batch = (fun () ->
      Store_chaindata.commit_batch deps.chaindata);
    verify_history = (fun ~epoch_id ~start_txid ~tx_count ->
      Store_chaindata.verify_epoch_index_complete_raw deps.chaindata
        ~epoch_id
        ~start_txid
        ~tx_count);
    commit_irmin_batch = (fun message ->
      let open Lwt.Syntax in
      if not (Octra_core.Ledger.journal_active deps.ledger) then
        Lwt.fail_with "ledger journal is not active before irmin commit"
      else
        let* () = Store_irmin.commit_epoch_batch deps.store message in
        match Octra_core.Ledger.commit_journal deps.ledger with
        | Ok () -> Lwt.return_unit
        | Error error -> Lwt.fail_with error);
    tag_epoch = Store_irmin.tag_epoch deps.store;
    irmin_commit_hash = (fun () -> Store_irmin.get_commit_hash deps.store);
    txlog_position = (fun () -> Store_chaindata.txlog_position deps.chaindata);
    epochlog_offset = (fun () -> Store_chaindata.epochlog_offset deps.chaindata);
    write_head = Head_manifest.atomic_write deps.data_dir;
    cache_head = Head_manifest.set_cached;
    delete_wal = Wal.delete deps.data_dir;
    delete_pending_commits = Wal.delete_pending_commits_for_epoch deps.data_dir;
    clear_marker = (fun () ->
      Octra_core.Epoch_commit_marker.clear_marker deps.data_dir);
    trace = deps.trace;
    fatal = deps.fatal;
    now = Unix.gettimeofday;
  }

let live_failure_effects deps ~rollback =
  {
    rollback;
    abort_chaindata = (fun () -> Store_chaindata.abort_batch deps.chaindata);
    abort_irmin = (fun () ->
      ignore (Octra_core.Ledger.abort_journal deps.ledger);
      Store_irmin.abort_epoch_batch deps.store);
    log = deps.log;
    exit_later = (fun () ->
      let open Lwt.Syntax in
      let* () = Lwt_unix.sleep 0.5 in
      deps.exit ();
      Lwt.return_unit);
  }

let epoch_header ~epoch_id ~state_root ~prev_state_root ~parent_commit
    ~start_txid ~tx_count ~finalized_by ~finalized_at ~proposer
    ~confirmed_fees ~(plan : Epoch_exec.reward_plan) ~reward_recipients
    ~reward_source =
  Epochlog.{
    id = epoch_id;
    state_root;
    prev_state_root;
    parent_commit;
    start_txid;
    tx_count;
    finalized_by;
    finalized_at;
    proposer;
    fees_total = Z.to_string confirmed_fees;
    fees_burned = Z.to_string plan.fees_burned;
    base_reward = Z.to_string plan.base_reward;
    total_reward = Z.to_string plan.total_reward;
    proposer_reward = Z.to_string plan.proposer_total;
    validator_reward_each = Z.to_string plan.each_validator;
    reward_recipients;
    reward_source = Some reward_source;
  }

let head_manifest ~generation ~state_root ~ledger_root ~irmin_commit
    ~txid_hi ~txlog_seg ~txlog_off ~epochlog_off ~commit_id ~ts
    ~epoch_index_hash ~epoch_index_root =
  Head_manifest.{
    schema_version;
    generation;
    epoch_id = generation;
    state_root;
    ledger_state_root = Some ledger_root;
    irmin_commit;
    txid_hi;
    txlog_seg = Some txlog_seg;
    txlog_off = Some txlog_off;
    epochlog_off = Some epochlog_off;
    commit_id;
    ts;
    quorum_cert_hash = None;
    epoch_index_hash = Some epoch_index_hash;
    epoch_index_root = Some epoch_index_root;
  }

let history_incomplete_failure epoch_id =
  Printf.sprintf "history_incomplete epoch = %d" epoch_id

let run_commit_effects (effects : commit_effects) (request : commit_request) =
  let open Lwt.Syntax in
  let wal_entry =
    wal_entry
      ~epoch_id:request.epoch_id
      ~pre_state_root:request.pre_state_root
      ~post_state_root:request.post_state_root
      ~parent_commit:request.parent_commit
      ~start_txid:request.start_txid
      ~tx_count:request.tx_count
      ~finalized_by:request.finalized_by
      ~finalized_at:request.finalized_at
      ~irmin_last_epoch_before:request.prev_generation
  in
  effects.append_journal
    (prepare_record
       ~commit_id:request.commit_id
       ~prev_generation:request.prev_generation
       ~epoch_id:request.epoch_id
       ~planned_txid_hi:request.planned_txid_hi
       ~planned_state_root:request.post_consensus_root
       ~ts:request.finalized_at);
  effects.chaos "after_prepare";
  effects.write_wal wal_entry;
  effects.chaos "after_wal";
  effects.write_marker request.epoch_id "wal_written";
  effects.write_marker request.epoch_id "begin";
  effects.write_receipts
    ~epoch_id:request.epoch_id
    ~receipts:request.epoch_receipts_json;
  effects.set_epoch
    (epoch_header
       ~epoch_id:request.epoch_id
       ~state_root:request.post_consensus_root
       ~prev_state_root:request.prev_state_root
       ~parent_commit:request.parent_commit
       ~start_txid:request.start_txid
       ~tx_count:request.tx_count
       ~finalized_by:request.finalized_by
       ~finalized_at:request.finalized_at
       ~proposer:request.proposer
       ~confirmed_fees:request.confirmed_fees
       ~plan:request.plan
       ~reward_recipients:request.reward_recipients
       ~reward_source:request.reward_source);
  effects.set_epoch_index_commitment
    ~epoch_id:request.epoch_id
    ~epoch_hash:request.epoch_index_hash
    ~root:request.epoch_index_root;
  effects.fsync_chaindata ();
  effects.trace "event = commit_batch";
  effects.write_marker request.epoch_id "chaindata_begin";
  effects.chaos "after_chaindata_begin";
  effects.commit_chaindata_batch ();
  mark_chaindata_committed request.progress;
  effects.write_marker request.epoch_id "chaindata_committed";
  let history_status =
    effects.verify_history
      ~epoch_id:request.epoch_id
      ~start_txid:request.start_txid
      ~tx_count:request.tx_count
  in
  if not (Store_chaindata.epoch_index_status_ok history_status) then begin
    history_incomplete_lines
      ~epoch_id:request.epoch_id
      ~start_txid:request.start_txid
      ~tx_count:request.tx_count
      history_status
    |> List.iter effects.fatal;
    failwith (history_incomplete_failure request.epoch_id)
  end;
  effects.chaos "after_chaindata_committed";
  effects.trace "event = commit_epoch_batch";
  effects.write_marker request.epoch_id "irmin_begin";
  mark_irmin_commit_started request.progress;
  let* () =
    effects.commit_irmin_batch
      (Printf.sprintf "epoch_%d" request.epoch_id)
  in
  effects.write_marker request.epoch_id "irmin_committed";
  effects.chaos "after_irmin_committed";
  effects.trace "event = tag_epoch";
  let* () = effects.tag_epoch request.epoch_id in
  let* irmin_commit_hash = effects.irmin_commit_hash () in
  let txlog_seg, txlog_off = effects.txlog_position () in
  let epochlog_off_now = effects.epochlog_offset () in
  let new_head =
    head_manifest
      ~generation:request.epoch_id
      ~state_root:request.post_consensus_root
      ~ledger_root:request.post_state_root
      ~irmin_commit:irmin_commit_hash
      ~txid_hi:request.planned_txid_hi
      ~txlog_seg
      ~txlog_off
      ~epochlog_off:epochlog_off_now
      ~commit_id:request.commit_id
      ~ts:request.finalized_at
      ~epoch_index_hash:request.epoch_index_hash
      ~epoch_index_root:request.epoch_index_root
  in
  effects.chaos "before_head_write";
  effects.write_head new_head;
  effects.cache_head new_head;
  mark_head_committed request.progress;
  effects.chaos "after_head_write";
  effects.append_journal
    (commit_record
       ~commit_id:request.commit_id
       ~generation:request.epoch_id
       ~ts:(effects.now ()));
  effects.delete_wal request.epoch_id;
  effects.delete_pending_commits request.epoch_id;
  effects.clear_marker ();
  Lwt.return_unit

let run_commit ~effects ~failure_effects request =
  Lwt.catch
    (fun () -> run_commit_effects effects request)
    (fun exn ->
      handle_failure
        ~progress:request.progress
        ~epoch_id:request.epoch_id
        ~effects:failure_effects
        exn)