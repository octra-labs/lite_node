(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type result =
  | Staged of {
      old_head : int;
      target : int;
      root : string;
    }
  | Snapshot_required of string

type resume =
  | Idle
  | Resumed of int

let hex_raw value =
  if String.length value <> 64 then invalid_arg "root hex length";
  String.init 32 (fun i ->
    Char.chr (int_of_string ("0x" ^ String.sub value (i * 2) 2)))

let root_hex root =
  Epoch_index_commitment.root_hex root

let root_raw root =
  hex_raw (root_hex root)

let target_matches chaindata ~target ~root =
  match Store_chaindata.get_epoch_header chaindata target with
  | Some head ->
    (try String.equal (root_raw head.Epochlog.state_root) root with _ -> false)
  | None -> false

let empty_after chaindata ~target ~head =
  Store_chaindata.epochs_empty_after chaindata
    ~from_epoch:target ~to_epoch:head

let head_at data_dir =
  match Head_manifest.load_result data_dir with
  | Head_manifest.Missing -> Error "missing_head"
  | Head_manifest.Corrupt reason -> Error ("head:" ^ reason)
  | Head_manifest.Present head -> Ok head

let stable_plan_state data_dir chaindata old target next_txid =
  match head_at data_dir, Store_chaindata.last_epoch_id chaindata with
  | Ok current, Ok (Some chain_head) ->
    current = old
    && chain_head = old.Head_manifest.epoch_id
    && empty_after chaindata ~target ~head:chain_head
    && Int64.equal (Store_chaindata.next_txid chaindata) next_txid
  | _ -> false

let plan ~data_dir ~store ~chaindata ~target ~root =
  let open Lwt.Syntax in
  match head_at data_dir with
  | Error reason -> Lwt.return_error reason
  | Ok old ->
    let old_head = old.Head_manifest.epoch_id in
    if target < 0 then Lwt.return_error "negative_target"
    else if target >= old_head then Lwt.return_error "target_not_behind_head"
    else if not (empty_after chaindata ~target ~head:old_head) then
      Lwt.return_error "non_empty_fork"
    else
      match Store_chaindata.epoch_boundary chaindata target with
      | Error reason -> Lwt.return_error ("target:" ^ reason)
      | Ok boundary ->
        let next_txid = Int64.succ boundary.txid_hi in
        let* binding = Store_irmin.epoch_binding store target in
        begin
          match binding with
          | Error reason -> Lwt.return_error ("irmin:" ^ reason)
          | Ok binding ->
            let folded =
              Epoch_index_commitment.folded_state_root
                ~ledger_state_root:binding.root
                ~epoch_index_root:boundary.index_root
            in
            let target_root = root_hex root in
            if Int64.equal boundary.txid_hi Int64.max_int then
              Lwt.return_error "target_txid_overflow"
            else if not (Int64.equal old.txid_hi boundary.txid_hi) then
              Lwt.return_error "target_txid_mismatch"
            else if not
              (String.equal (root_hex boundary.header.Epochlog.state_root) folded)
            then
              Lwt.return_error "epoch_root_mismatch"
            else if not (String.equal folded target_root) then
              Lwt.return_error "folded_root_mismatch"
            else if not
              (stable_plan_state data_dir chaindata old target next_txid)
            then
              Lwt.return_error "state_changed"
            else
              let head : Head_manifest.t = {
                schema_version = Head_manifest.schema_version;
                generation = target;
                epoch_id = target;
                state_root = folded;
                ledger_state_root = Some binding.root;
                irmin_commit = Some binding.commit;
                txid_hi = boundary.txid_hi;
                txlog_seg = Some boundary.txlog_seg;
                txlog_off = Some boundary.txlog_off;
                epochlog_off = Some boundary.epochlog_off;
                commit_id = Printf.sprintf "fork-repair-%d" target;
                ts = boundary.header.Epochlog.finalized_at;
                quorum_cert_hash = None;
                epoch_index_hash = Some boundary.index_hash;
                epoch_index_root = Some boundary.index_root;
              } in
              Lwt.return_ok Fork_repair_log.{
                source = old;
                target_root;
                next_txid;
                head;
              }
        end

let stage_empty ~data_dir ~store ~chaindata ~target ~root =
  let open Lwt.Syntax in
  let* planned = plan ~data_dir ~store ~chaindata ~target ~root in
  match planned with
  | Error reason -> Lwt.return (Snapshot_required reason)
  | Ok plan ->
    begin
      try
        Fork_repair_log.write data_dir plan;
        Lwt.return
          (Staged {
             old_head = plan.source.Head_manifest.epoch_id;
             target = plan.head.Head_manifest.epoch_id;
             root;
           })
      with exn ->
        Lwt.return
          (Snapshot_required ("repair_log:" ^ Printexc.to_string exn))
    end

let read_plan data_dir =
  match Fork_repair_log.read data_dir with
  | Fork_repair_log.Missing -> Ok None
  | Fork_repair_log.Invalid reason -> Error reason
  | Fork_repair_log.Ready plan -> Ok (Some plan)

let validate_binding plan binding =
  let head = plan.Fork_repair_log.head in
  match head.Head_manifest.ledger_state_root, head.irmin_commit,
        head.epoch_index_root with
  | Some ledger_root, Some commit, Some epoch_root ->
    let folded =
      Epoch_index_commitment.folded_state_root
        ~ledger_state_root:binding.Store_irmin.root
        ~epoch_index_root:epoch_root
    in
    if not (String.equal ledger_root binding.root) then
      Error "fork repair ledger binding mismatch"
    else if not (String.equal commit binding.commit) then
      Error "fork repair commit binding mismatch"
    else if not (String.equal folded plan.target_root) then
      Error "fork repair folded binding mismatch"
    else
      Ok ()
  | _ -> Error "fork repair binding is incomplete"

let source_ready data_dir plan =
  match head_at data_dir with
  | Error reason -> Error reason
  | Ok current when current = plan.Fork_repair_log.source -> Ok ()
  | Ok current when current = plan.head -> Ok ()
  | Ok _ -> Error "fork repair source HEAD changed"

let resume_store ~data_dir ~store =
  let open Lwt.Syntax in
  match read_plan data_dir with
  | Error reason -> Lwt.return_error reason
  | Ok None -> Lwt.return_ok Idle
  | Ok (Some plan) ->
    let target = plan.head.Head_manifest.epoch_id in
    begin
      match source_ready data_dir plan with
      | Error reason -> Lwt.return_error reason
      | Ok () ->
        let* binding = Store_irmin.epoch_binding store target in
        begin
          match binding with
          | Error reason -> Lwt.return_error reason
          | Ok binding ->
            begin
              match validate_binding plan binding with
              | Error reason -> Lwt.return_error reason
              | Ok () ->
                let* rolled = Store_irmin.rollback_to_epoch store target in
                begin
                  match rolled with
                  | Error reason -> Lwt.return_error reason
                  | Ok () ->
                    let* actual = Store_irmin.get_head_hash store in
                    begin
                      match actual, plan.head.ledger_state_root with
                      | Some actual, Some expected when String.equal actual expected ->
                        let* _ = Store_irmin.drop_epoch_tags_after store target in
                        let* () = Store_irmin.save_state_root store in
                        Head_manifest.atomic_write data_dir plan.head;
                        Head_manifest.set_cached plan.head;
                        Lwt.return_ok (Resumed target)
                      | Some _, Some _ ->
                        Lwt.return_error "fork repair restored root mismatch"
                      | _ -> Lwt.return_error "fork repair restored root is missing"
                    end
                end
            end
        end
    end

let resume_chain ~data_dir ~chaindata =
  match read_plan data_dir with
  | Error reason -> Error reason
  | Ok None -> Ok Idle
  | Ok (Some plan) ->
    let target = plan.head.Head_manifest.epoch_id in
    let target_head =
      match head_at data_dir with
      | Ok current when current = plan.head -> Ok ()
      | Ok _ -> Error "fork repair target HEAD mismatch"
      | Error reason -> Error reason
    in
    begin
      match target_head, Store_chaindata.last_epoch_id chaindata,
            plan.head.txlog_seg, plan.head.txlog_off,
            plan.head.epochlog_off, plan.head.epoch_index_hash,
            plan.head.epoch_index_root with
      | Ok (), Ok (Some head), Some txlog_seg, Some txlog_off, Some epochlog_off,
        Some epoch_hash, Some epoch_root ->
        if head < target then Error "fork repair chaindata is behind target"
        else if not (empty_after chaindata ~target ~head) then
          Error "fork repair chaindata fork is not empty"
        else if not (target_matches chaindata ~target ~root:(hex_raw plan.target_root)) then
          Error "fork repair chaindata target root mismatch"
        else if Store_chaindata.epochlog_offset_after chaindata target
                <> Some epochlog_off then
          Error "fork repair epochlog offset mismatch"
        else if Store_chaindata.get_epoch_index_commitment chaindata target
                <> (Some epoch_hash, Some epoch_root) then
          Error "fork repair epoch commitment mismatch"
        else begin
          ignore
            (Store_chaindata.rollback_to_head chaindata
               ~head_epoch:target
               ~head_txlog_seg:txlog_seg
               ~head_txlog_off:txlog_off
               ~head_epochlog_off:epochlog_off
               ~inflight_start_txid:plan.next_txid
               ~inflight_tx_count:0);
          Store_chaindata.fsync chaindata;
          match Store_chaindata.last_epoch_id chaindata with
          | Ok (Some epoch) when epoch = target -> Ok (Resumed target)
          | _ -> Error "fork repair chaindata boundary mismatch"
        end
      | Error reason, _, _, _, _, _, _ -> Error reason
      | _, Ok None, _, _, _, _, _ ->
        Error "fork repair chaindata head is missing"
      | _, Error reason, _, _, _, _, _ -> Error reason
      | _ -> Error "fork repair boundary is incomplete"
    end

let clear data_dir =
  Fork_repair_log.clear data_dir