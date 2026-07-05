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


module Commit_journal = Octra_core.Commit_journal
module Epoch_boundary = Octra_core.Epoch_boundary
module Epoch_exec = Octra_core.Epoch_exec
module Epoch_index_commitment = Octra_core.Epoch_index_commitment
module Epochlog = Octra_core.Epochlog
module Head_manifest = Octra_core.Head_manifest
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
  | Epoch_boundary.Direct_head_write, Some h ->
    Some {
      level = `Info;
      line =
        Printf.sprintf
          "event = wal_boundary action = direct_head_write head_epoch = %d ledger_root = %s pre_state = %s irmin_meta = %d"
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

let handle_failure ~progress ~epoch_id ~effects exn =
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

let epoch_header ~epoch_id ~state_root ~prev_state_root ~parent_commit
    ~start_txid ~tx_count ~finalized_by ~finalized_at ~proposer
    ~confirmed_fees ~(plan : Epoch_exec.reward_plan) ~reward_recipients =
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
    base_reward = Z.to_string plan.base_reward;
    total_reward = Z.to_string plan.total_reward;
    proposer_reward = Z.to_string plan.proposer_total;
    validator_reward_each = Z.to_string plan.each_validator;
    reward_recipients;
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