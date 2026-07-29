(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type result =
  | Repaired of {
      old_head : int;
      target : int;
      root : string;
    }
  | Snapshot_required of string

let hex_raw h =
  if String.length h <> 64 then invalid_arg "root hex length";
  String.init 32 (fun i ->
    Char.chr (int_of_string ("0x" ^ String.sub h (i * 2) 2)))

let root_raw root =
  hex_raw (Epoch_index_commitment.root_hex root)

let target_matches chaindata ~target ~root =
  match Store_chaindata.get_epoch_header chaindata target with
  | Some h ->
    (try root_raw h.Epochlog.state_root = root with _ -> false)
  | None -> false

let empty_after chaindata ~target ~head =
  Store_chaindata.epochs_empty_after chaindata
    ~from_epoch:target ~to_epoch:head

let run_empty ~data_dir ~store ~chaindata ~target ~root =
  let open Lwt.Syntax in
  let snapshot reason = Lwt.return (Snapshot_required reason) in
  match Head_manifest.get_cached () with
  | None -> snapshot "missing_head"
  | Some h ->
    match h.Head_manifest.txlog_seg,
          h.Head_manifest.txlog_off,
          Store_chaindata.epochlog_offset_after chaindata target,
          Store_chaindata.get_epoch_index_commitment chaindata target with
    | Some seg, Some off, Some eoff, (Some eic_hash, Some eic_root) ->
      let old_head = h.Head_manifest.epoch_id in
      let next_txid = Store_chaindata.next_txid chaindata in
      let* irmin_result = Store_irmin.rollback_to_epoch store target in
      begin
        match irmin_result with
        | Error err -> snapshot ("irmin:" ^ err)
        | Ok () ->
          let* ledger_root_opt = Store_irmin.get_head_hash store in
          match ledger_root_opt with
          | None -> snapshot "missing_ledger_root"
          | Some ledger_root ->
            let folded =
              Epoch_index_commitment.folded_state_root
                ~ledger_state_root:ledger_root
                ~epoch_index_root:eic_root
            in
            if root_raw folded <> root then
              snapshot "folded_root_mismatch"
            else begin
              let _ =
                Store_chaindata.rollback_to_head chaindata
                  ~head_epoch:target
                  ~head_txlog_seg:seg
                  ~head_txlog_off:off
                  ~head_epochlog_off:eoff
                  ~inflight_start_txid:next_txid
                  ~inflight_tx_count:0
              in
              Store_chaindata.fsync chaindata;
              let* () = Store_irmin.save_state_root store in
              let* commit_hash = Store_irmin.get_commit_hash store in
              let new_head : Head_manifest.t = {
                schema_version = Head_manifest.schema_version;
                generation = target;
                epoch_id = target;
                state_root = folded;
                ledger_state_root = Some ledger_root;
                irmin_commit = commit_hash;
                txid_hi = h.Head_manifest.txid_hi;
                txlog_seg = Some seg;
                txlog_off = Some off;
                epochlog_off = Some eoff;
                commit_id = Printf.sprintf "fork-rollback-%d" target;
                ts = Unix.gettimeofday ();
                quorum_cert_hash = None;
                epoch_index_hash = Some eic_hash;
                epoch_index_root = Some eic_root;
              } in
              Head_manifest.atomic_write data_dir new_head;
              Head_manifest.set_cached new_head;
              Lwt.return (Repaired { old_head; target; root })
            end
      end
    | _ -> snapshot "missing_offsets_or_eic"