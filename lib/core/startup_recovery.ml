(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

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

let pf fmt = Octra_log.info "recovery" fmt

let chain_last_epoch chaindata =
  match Store_chaindata.last_epoch_id chaindata with
  | Ok (Some epoch) -> epoch
  | Ok None -> -1
  | Error reason -> failwith reason

let schema_version = "v2_ascii64_int32be"
let schema_meta_key = "index_schema_version"
let repaired_upto_meta_key = "repaired_upto_epoch"

let handle_marker_phase ~data_dir ~store m =
  let ph = m.Epoch_commit_marker.phase in
  let ep = m.Epoch_commit_marker.epoch_id in
  pf "marker: phase=%s epoch=%d ts=%.0f" ph ep m.ts;
  match ph with
  | "stage_batch_begin" ->
    pf "phase=%s: staged metadata did not reach durable WAL boundary" ph;
    Epoch_commit_marker.clear_marker data_dir;
    Lwt.return_unit
  | "wal_written" ->
    pf "phase=%s: WAL replay owns recovery for epoch %d" ph ep;
    Epoch_commit_marker.clear_marker data_dir;
    Lwt.return_unit
  | "begin" | "chaindata_begin" ->
    pf "phase=%s: nothing fully committed (LMDB pages auto-rollback without fsync)" ph;
    Epoch_commit_marker.clear_marker data_dir;
    Lwt.return_unit
  | "chaindata_committed" | "irmin_begin" ->
    pf "phase=%s: chaindata committed epoch %d, Irmin did not finish" ph ep;
    pf "phase=%s: ledger will catch up via next epoch's set_meta" ph;
    Epoch_commit_marker.clear_marker data_dir;
    Lwt.return_unit
  | "irmin_committed" ->
    pf "phase=irmin_committed: re-running tag_epoch %d" ep;
    let* () = Store_irmin.tag_epoch store ep in
    Epoch_commit_marker.clear_marker data_dir;
    Lwt.return_unit
  | other ->
    pf "phase=%s UNKNOWN, clearing marker (operator review recommended)" other;
    Epoch_commit_marker.clear_marker data_dir;
    Lwt.return_unit

let schema_check chaindata ~irmin_last_epoch =
  let idx = Store_chaindata.index chaindata in
  let stored_version = Chaindata_index.get_meta idx schema_meta_key in
  let repaired_upto = Chaindata_index.get_meta idx repaired_upto_meta_key in
  match stored_version, repaired_upto with
  | Some v, Some upto_str when v = schema_version ->
    (try
       let upto = int_of_string upto_str in
       upto >= irmin_last_epoch
     with _ -> false)
  | _ -> false

let mark_schema_ok chaindata ~irmin_last_epoch =
  let idx = Store_chaindata.index chaindata in
  Chaindata_index.set_meta_direct idx schema_meta_key schema_version;
  Chaindata_index.set_meta_direct idx repaired_upto_meta_key (string_of_int irmin_last_epoch)

type eic_check = {
  eic_checked : bool;
  eic_ok : bool;
  eic_errors : string list;
}

let eic_skip = {
  eic_checked = false;
  eic_ok = true;
  eic_errors = [];
}

let eic_root_key epoch_id =
  Printf.sprintf "eic_epoch_root:%d" epoch_id

let previous_eic_root chaindata epoch_id =
  if epoch_id <= 0 then Epoch_index_commitment.genesis_root
  else
    match Store_chaindata.get_meta chaindata (eic_root_key (epoch_id - 1)) with
    | Some root -> root
    | None -> Epoch_index_commitment.genesis_root

let eic_err msg =
  {
    eic_checked = true;
    eic_ok = false;
    eic_errors = [msg];
  }

let verify_head_eic chaindata head =
  match head with
  | None -> eic_skip
  | Some h ->
      match h.Head_manifest.epoch_index_hash, h.Head_manifest.epoch_index_root with
      | None, None -> eic_skip
      | Some _, None -> eic_err "HEAD epoch_index_root missing"
      | None, Some _ -> eic_err "HEAD epoch_index_hash missing"
      | Some head_hash, Some head_root ->
          match Store_chaindata.get_epoch_header chaindata h.epoch_id with
          | None ->
              begin
                match Store_chaindata.history_floor chaindata with
                | Error reason -> eic_err reason
                | Ok None ->
                    eic_err
                      (Printf.sprintf
                         "epoch_meta missing for HEAD epoch=%d"
                         h.epoch_id)
                | Ok (Some floor) ->
                    let errors =
                      (if History_floor.epoch floor = h.epoch_id then []
                       else ["history floor epoch differs from HEAD"])
                      @ (if History_floor.state_root floor = h.state_root then []
                         else ["history floor state root differs from HEAD"])
                      @ (if History_floor.ledger_state_root floor
                              = Head_manifest.ledger_state_root h then []
                         else ["history floor ledger root differs from HEAD"])
                      @ (if History_floor.txid_hi floor = h.txid_hi then []
                         else ["history floor transaction high-water differs from HEAD"])
                      @ (if History_floor.epoch_index_hash floor = head_hash then []
                         else ["history floor epoch hash differs from HEAD"])
                      @ (if History_floor.epoch_index_root floor = head_root then []
                         else ["history floor epoch root differs from HEAD"])
                      @ (match Store_chaindata.validate_history_floor chaindata floor with
                         | Ok () -> []
                         | Error reason -> [reason])
                    in
                    {
                      eic_checked = true;
                      eic_ok = errors = [];
                      eic_errors = errors;
                    }
              end
          | Some eh ->
              let prev_root = previous_eic_root chaindata h.epoch_id in
              let status = Store_chaindata.verify_epoch_index_commitment_raw chaindata
                ~epoch_id:h.epoch_id
                ~start_txid:eh.Epochlog.start_txid
                ~tx_count:eh.Epochlog.tx_count
                ~prev_root
              in
              let errors =
                status.eic_errors
                @ (match status.eic_stored_hash with
                   | Some v when v = head_hash -> []
                   | Some v -> [Printf.sprintf "HEAD epoch_index_hash mismatch meta=%s head=%s" v head_hash]
                   | None -> ["meta epoch_index_hash missing"])
                @ (match status.eic_stored_root with
                   | Some v when v = head_root -> []
                   | Some v -> [Printf.sprintf "HEAD epoch_index_root mismatch meta=%s head=%s" v head_root]
                   | None -> ["meta epoch_index_root missing"])
                @ (match status.eic_actual_hash with
                   | Some v when v = head_hash -> []
                   | Some v -> [Printf.sprintf "actual epoch_index_hash mismatch actual=%s head=%s" v head_hash]
                   | None -> [])
                @ (match status.eic_actual_root with
                   | Some v when v = head_root -> []
                   | Some v -> [Printf.sprintf "actual epoch_index_root mismatch actual=%s head=%s" v head_root]
                   | None -> [])
                @ (match h.Head_manifest.ledger_state_root with
                   | Some ledger_root ->
                       let folded =
                         Epoch_index_commitment.folded_state_root
                           ~ledger_state_root:ledger_root
                           ~epoch_index_root:head_root
                       in
                       if folded = h.Head_manifest.state_root then []
                       else [Printf.sprintf "HEAD folded state_root mismatch folded=%s head=%s" folded h.state_root]
                   | None -> [])
              in
              {
                eic_checked = true;
                eic_ok = errors = [];
                eic_errors = errors;
              }

let rebuild_head_eic_fields chaindata ~epoch_id ~epoch_header ~ledger_state_root
    ~fallback_state_root ~prev_root_hint =
  let eh = epoch_header in
      let stored_hash, stored_root =
        Store_chaindata.get_epoch_index_commitment chaindata epoch_id
      in
      let prev_stored =
        epoch_id > 0
        && Store_chaindata.get_meta chaindata (eic_root_key (epoch_id - 1)) <> None
      in
      let prev_root =
        match prev_root_hint with
        | Some root -> root
        | None -> previous_eic_root chaindata epoch_id
      in
      let p1b_context = prev_stored || prev_root_hint <> None in
      match stored_hash, stored_root with
      | None, None when not p1b_context -> fallback_state_root, None, None
      | None, None | Some _, Some _ ->
          let status = Store_chaindata.verify_epoch_index_commitment_raw chaindata
            ~epoch_id
            ~start_txid:eh.Epochlog.start_txid
            ~tx_count:eh.Epochlog.tx_count
            ~prev_root
          in
          let status =
            if status.eic_ok then status
            else
              match stored_hash, stored_root, status.eic_actual_hash, status.eic_actual_root with
              | None, None, Some epoch_hash, Some root when p1b_context ->
                  Store_chaindata.set_epoch_index_commitment_direct chaindata
                    ~epoch_id
                    ~epoch_hash
                    ~root;
                  Store_chaindata.verify_epoch_index_commitment_raw chaindata
                    ~epoch_id
                    ~start_txid:eh.Epochlog.start_txid
                    ~tx_count:eh.Epochlog.tx_count
                    ~prev_root
              | _ -> status
          in
          if not status.eic_ok then
            failwith (Printf.sprintf "HEAD rebuild EIC failed epoch=%d errors=%s"
              epoch_id (String.concat "; " status.eic_errors));
          (match status.eic_actual_hash, status.eic_actual_root with
           | Some epoch_hash, Some epoch_root ->
               let folded = Epoch_index_commitment.folded_state_root
                 ~ledger_state_root
                 ~epoch_index_root:epoch_root
               in
               if String.length eh.Epochlog.state_root > 0
                  && eh.Epochlog.state_root <> folded then
                 failwith (Printf.sprintf
                   "HEAD rebuild folded root mismatch epoch=%d epochlog=%s folded=%s"
                   epoch_id eh.Epochlog.state_root folded);
               folded, Some epoch_hash, Some epoch_root
           | _ ->
               failwith (Printf.sprintf "HEAD rebuild EIC missing actual root epoch=%d" epoch_id))
      | _ ->
          failwith (Printf.sprintf "HEAD rebuild partial EIC metadata epoch=%d" epoch_id)

let repair_missing_head_eic data_dir chaindata =
  match Head_manifest.get_cached () with
  | Some h when h.Head_manifest.epoch_index_hash = None
                && h.Head_manifest.epoch_index_root = None ->
      (match Store_chaindata.get_epoch_header chaindata h.epoch_id with
       | None -> ()
       | Some eh ->
           let ledger_state_root = Head_manifest.ledger_state_root h in
           let state_root, epoch_index_hash, epoch_index_root =
             rebuild_head_eic_fields chaindata
               ~epoch_id:h.epoch_id
               ~epoch_header:eh
               ~ledger_state_root
               ~fallback_state_root:h.state_root
               ~prev_root_hint:None
           in
           match epoch_index_hash, epoch_index_root with
           | Some _, Some _ ->
               let repaired = {
                 h with
                 Head_manifest.state_root;
                 ledger_state_root = Some ledger_state_root;
                 epoch_index_hash;
                 epoch_index_root;
               } in
               Head_manifest.atomic_write data_dir repaired;
               Head_manifest.set_cached repaired;
               pf "HEAD EIC repaired in-place: epoch=%d state_root=%s eic=%s"
                 repaired.epoch_id
                 (String.sub repaired.state_root 0 (min 16 (String.length repaired.state_root)))
                 (match repaired.epoch_index_root with
                  | Some root -> String.sub root 0 (min 16 (String.length root))
                  | None -> "none")
           | _ -> ())
  | _ -> ()

let recover ~data_dir ~chaindata ~store =
  pf "starting check (idempotent)";

  let marker = Epoch_commit_marker.read_marker data_dir in

  let txlog_last_epoch =
    chain_last_epoch chaindata
  in

  let* irmin_last_str = Store_irmin.get_meta store "last_epoch" in
  let irmin_last_epoch_before =
    match irmin_last_str with
    | Some s -> (try int_of_string s with _ -> -1)
    | None -> -1
  in

  pf "snapshot: txlog_last=%d irmin_last=%d marker=%s"
    txlog_last_epoch irmin_last_epoch_before
    (match marker with
     | Some m -> Printf.sprintf "phase=%s epoch=%d" m.Epoch_commit_marker.phase m.Epoch_commit_marker.epoch_id
     | None -> "none");

  let* () = match marker with
    | None -> Lwt.return_unit
    | Some m -> handle_marker_phase ~data_dir ~store m
  in

  let head =
    match Head_manifest.load_result data_dir with
    | Head_manifest.Missing ->
      pf "HEAD: not present (first run on this binary or pre-Phase-6.2)";
      None
    | Head_manifest.Corrupt err ->
      pf "FATAL: HEAD.json present but unparseable: %s" err;
      pf "  Operator: restore HEAD.json from backup (the canonical visibility";
      pf "  pointer cannot be silently ignored once written). Refusing to start.";
      exit 1
    | Head_manifest.Present h ->
      Head_manifest.set_cached h;
      pf "HEAD: schema=v%d epoch=%d gen=%d state_root=%s txid_hi=%Ld commit_id=%s"
        h.Head_manifest.schema_version h.epoch_id h.generation
        (String.sub h.state_root 0 (min 16 (String.length h.state_root)))
        h.txid_hi h.commit_id;
      if h.epoch_id > irmin_last_epoch_before then begin
        pf "FATAL: HEAD ahead of Irmin: HEAD.epoch=%d > irmin_last=%d"
          h.epoch_id irmin_last_epoch_before;
        pf "  Visibility pointer references state not in Irmin.";
        pf "  Operator: investigate epochlog/Irmin tags, or restore HEAD from backup.";
        exit 1
      end;
      Some h
  in

  let journal = Commit_journal.read_all data_dir in
  let head_commit_id = match head with
    | Some h -> Some h.Head_manifest.commit_id
    | None -> None in
  let pending = Commit_journal.pending_prepares ?head_commit_id journal in
  if pending <> [] then
    pf "Commit journal: %d pending PREPAREs (uncommitted residue)" (List.length pending);
  List.iter (fun (cid, eid) ->
    pf "  pending: epoch=%d commit_id=%s (HEAD bounds visibility — safe to ignore)" eid cid
  ) pending;

  let rollback_wal_to_head ~reason (e : Wal.entry) (h : Head_manifest.t) =
    match h.txlog_seg, h.txlog_off, h.epochlog_off with
    | Some head_seg, Some head_off, Some head_eoff ->
      pf "WAL epoch=%d: %s; rolling back chaindata to HEAD ep=%d txlog=(seg=%d,off=%d) epochlog_off=%d"
        e.Wal.epoch_id reason h.Head_manifest.epoch_id head_seg head_off head_eoff;
      (try
        let (n_tx, n_ep, n_addr, n_txid) = Store_chaindata.rollback_to_head
          chaindata
          ~head_epoch:h.Head_manifest.epoch_id
          ~head_txlog_seg:head_seg
          ~head_txlog_off:head_off
          ~head_epochlog_off:head_eoff
          ~inflight_start_txid:e.Wal.start_txid
          ~inflight_tx_count:e.Wal.tx_count in
        pf "WAL rollback OK: removed %d tx_loc, %d epoch_meta, %d addr_tx, %d txid_loc"
          n_tx n_ep n_addr n_txid;
        Wal.delete data_dir e.epoch_id;
        true
      with ex ->
        pf "WAL rollback FAILED: %s" (Printexc.to_string ex);
        pf "  Operator review required — keeping WAL entry for retry";
        false)
    | _ ->
      pf "WAL epoch=%d: %s but HEAD lacks txlog/epochlog offsets — cannot rollback safely"
        e.Wal.epoch_id reason;
      false
  in

  let safe_head_rollback_reason (e : Wal.entry) (h : Head_manifest.t) ~chain_last =
    if e.epoch_id = h.epoch_id + 1
       && chain_last >= e.epoch_id
       && irmin_last_epoch_before = h.epoch_id
       && e.pre_state_root = Head_manifest.ledger_state_root h then
      Some "HEAD-consistent uncommitted residue"
    else None
  in

  let pending_wal = Wal.read_pending data_dir in
  if pending_wal <> [] then begin
    pf "WAL: found %d pending entries" (List.length pending_wal);
    let head_ep = match head with
      | Some h -> h.Head_manifest.epoch_id
      | None -> -1
    in
    List.iter (fun (e : Wal.entry) ->
      if e.epoch_id <= head_ep then begin
        pf "WAL epoch=%d ≤ HEAD.epoch=%d → already committed, deleting WAL entry"
          e.epoch_id head_ep;
        Wal.delete data_dir e.epoch_id
      end else begin
        let chain_last =
          chain_last_epoch chaindata
        in
        let action = Wal.decide_action ~entry:e
          ~chaindata_last_epoch:chain_last
          ~irmin_last_epoch:irmin_last_epoch_before in
        pf "WAL epoch=%d pre_state=%s post_state=%s txs=%d → action=%s"
          e.Wal.epoch_id
          (String.sub e.pre_state_root 0 (min 16 (String.length e.pre_state_root)))
          (String.sub e.post_state_root 0 (min 16 (String.length e.post_state_root)))
          e.tx_count
          (Wal.action_to_string action);
        match action with
        | Wal.Skip ->
          Wal.delete data_dir e.epoch_id
        | Wal.ForwardTagOnly ->

          Wal.delete data_dir e.epoch_id
        | Wal.ForwardReplayIrmin
        | Wal.ForwardReplayChaindataAndIrmin -> begin
          match head with
          | None ->
            pf "WAL epoch=%d: action=%s but HEAD missing — cannot rollback safely"
              e.Wal.epoch_id (Wal.action_to_string action);
            pf "  Operator: restore HEAD.json from backup OR reset chaindata"
          | Some h ->
            ignore (rollback_wal_to_head
              ~reason:(Wal.action_to_string action) e h)
        end
        | Wal.InconsistentState msg ->
          match head with
          | Some h ->
            (match safe_head_rollback_reason e h ~chain_last with
             | Some reason ->
               pf "WAL epoch=%d: InconsistentState(%s) but %s"
                 e.Wal.epoch_id msg reason;
               ignore (rollback_wal_to_head ~reason e h)
             | None ->
               pf "WAL epoch=%d: InconsistentState(%s) — operator review required"
                 e.Wal.epoch_id msg)
          | None ->
            pf "WAL epoch=%d: InconsistentState(%s) — operator review required"
              e.Wal.epoch_id msg
      end
    ) pending_wal
  end;

  let pending_commits = Wal.read_pending_commits data_dir in
  if pending_commits <> [] then begin
    pf "PENDING_COMMIT: found %d records (validator broadcast precommit but commit not durable)"
      (List.length pending_commits);
    let head_ep = match head with
      | Some h -> h.Head_manifest.epoch_id
      | None -> -1
    in
    List.iter (fun (p : Wal.pending_commit) ->
      if p.epoch_id <= head_ep then begin
        pf "PENDING_COMMIT epoch=%d round=%d ≤ HEAD.epoch=%d → quorum reached + commit durable, dropping"
          p.epoch_id p.round head_ep;
        Wal.delete_pending_commit data_dir p.epoch_id p.round
      end else begin
        pf "PENDING_COMMIT epoch=%d round=%d pid=%s txid_hi=%Ld validator=%s ts=%.3f"
          p.epoch_id p.round
          (String.sub p.proposal_id 0 (min 16 (String.length p.proposal_id)))
          p.txid_hi p.validator_addr p.ts;
        pf "  → no quorum cert local, peer-query not yet implemented (Phase 8.0.1b)";
        pf "  → keeping record; on next quorum or operator action it will be cleared"
      end
    ) pending_commits
  end;

  let head_lag_detected = match head with
    | Some h -> h.Head_manifest.epoch_id < irmin_last_epoch_before
    | None -> irmin_last_epoch_before >= 0 in
  let* () =
    if not head_lag_detected then Lwt.return_unit
    else begin
      let head_ep_str = match head with
        | Some h -> string_of_int h.Head_manifest.epoch_id
        | None -> "absent" in
      pf "HEAD-lag: HEAD=%s < irmin_last=%d → rebuilding HEAD from current state"
        head_ep_str irmin_last_epoch_before;
      let last_epoch_header = Store_chaindata.get_last_epoch chaindata in
      let* irmin_commit_opt = Store_irmin.get_commit_hash store in
      let* irmin_state_root_opt = Store_irmin.get_head_hash store in
      match last_epoch_header with
      | None ->
        pf "HEAD-rebuild aborted: chaindata Epochlog empty (cannot determine state)";
        Lwt.return_unit
      | Some eh ->
        let journal = Commit_journal.read_all data_dir in
        let prepare_for_epoch =
          List.fold_left (fun acc r ->
            match r with
            | Commit_journal.Prepare { commit_id; planned_txid_hi; epoch_id; _ }
              when epoch_id = irmin_last_epoch_before ->
              Some (commit_id, planned_txid_hi)
            | _ -> acc) None journal in
        let commit_id = match prepare_for_epoch with
          | Some (cid, _) -> cid
          | None -> Commit_journal.mk_commit_id irmin_last_epoch_before in
        let txid_hi = match prepare_for_epoch with
          | Some (_, hi) -> hi
          | None -> Int64.add eh.Epochlog.start_txid (Int64.of_int (eh.tx_count - 1)) in
        let (txlog_seg, txlog_off) = Store_chaindata.txlog_position chaindata in
        let epochlog_off_now = Store_chaindata.epochlog_offset chaindata in
        let ledger_state_root = match irmin_state_root_opt with
          | Some r when String.length r > 0 -> r
          | _ -> eh.state_root in
        let fallback_state_root =
          if String.length eh.state_root > 0 then eh.state_root
          else ledger_state_root in
        let state_root, epoch_index_hash, epoch_index_root =
          let prev_root_hint =
            match head with
            | Some h when h.Head_manifest.epoch_id = irmin_last_epoch_before - 1 ->
                h.Head_manifest.epoch_index_root
            | _ -> None
          in
          rebuild_head_eic_fields chaindata
            ~epoch_id:irmin_last_epoch_before
            ~epoch_header:eh
            ~ledger_state_root
            ~fallback_state_root
            ~prev_root_hint
        in
        let new_head : Head_manifest.t = {
          schema_version = Head_manifest.schema_version;
          generation = irmin_last_epoch_before;
          epoch_id = irmin_last_epoch_before;
          state_root;
          ledger_state_root = Some ledger_state_root;
          irmin_commit = irmin_commit_opt;
          txid_hi;
          txlog_seg = Some txlog_seg;
          txlog_off = Some txlog_off;
          epochlog_off = Some epochlog_off_now;
          commit_id;
          ts = eh.finalized_at;
          quorum_cert_hash = None;
          epoch_index_hash;
          epoch_index_root;
        } in
        Head_manifest.atomic_write data_dir new_head;
        Head_manifest.set_cached new_head;
        pf "HEAD rebuilt: epoch=%d state_root=%s eic=%s txid_hi=%Ld commit_id=%s"
          new_head.epoch_id
          (String.sub new_head.state_root 0 (min 16 (String.length new_head.state_root)))
          (match new_head.epoch_index_root with
           | Some root -> String.sub root 0 (min 16 (String.length root))
           | None -> "none")
          new_head.txid_hi new_head.commit_id;
        Lwt.return_unit
    end
  in

  let cap = irmin_last_epoch_before in

  let already_ok = (marker = None) && schema_check chaindata ~irmin_last_epoch:cap in
  let stats =
    if already_ok then begin
      pf "schema gate: %s + repaired_upto=%d ≥ irmin_last=%d → SKIP full repair"
        schema_version cap cap;
      { Store_chaindata.checked = 0; repaired = 0; errors = [] }
    end else begin
      pf "schema gate: not satisfied → running incremental tx_loc repair (cap=%d)" cap;
      Store_chaindata.verify_and_repair_tx_loc_only chaindata ~max_epoch:cap
    end
  in

  if stats.repaired > 0 then begin
    pf "TX_LOC REPAIR: checked=%d repaired=%d errors=%d"
      stats.checked stats.repaired (List.length stats.errors);
    List.iter (fun e -> pf "  error: %s" e) stats.errors
  end else if not already_ok then
    pf "tx_loc integrity OK (no repair needed)";

  if not already_ok then begin
    pf "rebuilding epoch_meta from Epochlog (cap=%d)" cap;
    let epochs = Store_chaindata.read_all_epochs chaindata in
    let idx = Store_chaindata.index chaindata in
    let n_capped = ref 0 in
    Chaindata_index.begin_write idx;
    List.iter (fun h ->
      if h.Epochlog.id <= cap then begin
        Chaindata_index.buffer_epoch idx h.Epochlog.id (Epochlog.epoch_to_json h);
        incr n_capped
      end
    ) epochs;
    Chaindata_index.commit_write idx;
    pf "epoch_meta rebuilt: %d entries (cap=%d, total in epochlog=%d)"
      !n_capped cap (List.length epochs);
    mark_schema_ok chaindata ~irmin_last_epoch:cap
  end;

  repair_missing_head_eic data_dir chaindata;

  let eic_check = verify_head_eic chaindata (Head_manifest.get_cached ()) in
  if eic_check.eic_checked then begin
    if eic_check.eic_ok then
      pf "EIC check OK for HEAD epoch"
    else begin
      pf "FATAL: EIC check failed for HEAD epoch";
      List.iter (fun err -> pf "  eic: %s" err) eic_check.eic_errors;
      exit 1
    end
  end else
    pf "EIC check skipped: HEAD has no epoch index commitment";

  let* irmin_last_str_after = Store_irmin.get_meta store "last_epoch" in
  let irmin_last_epoch_after =
    match irmin_last_str_after with
    | Some s -> (try int_of_string s with _ -> -1)
    | None -> -1
  in

  let chain_last_after =
    chain_last_epoch chaindata
  in

  let boundary_ok = (irmin_last_epoch_after = chain_last_after) in
  if boundary_ok then
    pf "boundary OK: irmin=%d chaindata=%d" irmin_last_epoch_after chain_last_after
  else
    pf "boundary MISMATCH: irmin=%d chaindata=%d (operator review)"
      irmin_last_epoch_after chain_last_after;

  Lwt.return {
    marker_phase = (match marker with Some m -> Some m.Epoch_commit_marker.phase | None -> None);
    marker_epoch = (match marker with Some m -> Some m.Epoch_commit_marker.epoch_id | None -> None);
    txlog_last_epoch;
    irmin_last_epoch_before;
    irmin_last_epoch_after;
    index_repaired = stats.repaired;
    index_errors = stats.errors;
    boundary_ok;
    full_repair_skipped = already_ok;
    eic_checked = eic_check.eic_checked;
    eic_ok = eic_check.eic_ok;
    eic_errors = eic_check.eic_errors;
  }