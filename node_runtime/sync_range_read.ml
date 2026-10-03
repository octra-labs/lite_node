(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

module Sync = Octra_bootstrap.State_sync
module Parts = Octra_bootstrap.Range_part
module Store = Octra_core.Store_chaindata
module Index = Octra_core.Chaindata_index
module Epoch = Octra_core.Epochlog
module Range = Sync_range

exception Interrupted of Range.error

let transaction ~dir (seg_id, offset, len) =
  try
    let _, payload = Octra_core.Txlog.read_location ~dir ~seg_id ~offset ~len in
    let hash, json = Store.split_payload payload in
    Some (hash, Store.visible_tx_json hash json)
  with _ -> None

let read ~chaindata ~data_dir ~chain_id ~read_finality ~cancelled (query : Range.query) =
  let check () =
    if cancelled () then raise (Interrupted Range.Stopped);
    if Octra_core.Head_manifest.get_cached () <> query.head then
      raise (Interrupted Range.Changed)
  in
  let step run =
    check ();
    let* result = run () in
    check ();
    Lwt.return result
  in
  let worker run = step (fun () -> Lwt_preemptive.detach run ()) in
  let stop epoch reason =
    Log.warn "state_sync" "event = catchup_range_stop epoch = %Ld reason = %s" epoch reason
  in
  let index = Store.index chaindata in
  let dir = chaindata.Store.txlog.dir in
  let rec rows epoch position size found =
    check ();
    if position >= epoch.Epoch.tx_count then Lwt.return_ok (List.rev found)
    else
      let txid = Int64.add epoch.start_txid (Int64.of_int position) in
      match Index.get_txid_loc index txid with
      | None -> Lwt.return_error "transaction_missing"
      | Some (_, _, len) when len > Parts.full_max - size ->
        Lwt.return_error "range response exceeds full limit"
      | Some location ->
        let* value = worker (fun () -> transaction ~dir location) in
        match value with
        | None -> Lwt.return_error "transaction_missing"
        | Some ((_, json) as value) ->
          let size = size + 64 + String.length json in
          if size > Parts.full_max then Lwt.return_error "range response exceeds full limit"
          else rows epoch (position + 1) size (value :: found)
  in
  let load target =
    check ();
    let epoch_id = Int64.to_int target in
    match Index.get_epoch index epoch_id with
    | None -> Lwt.return_error "epoch_missing"
    | Some raw ->
      let* epoch = worker (fun () -> Epoch.epoch_of_json raw) in
      match epoch with
      | None -> Lwt.return_error "epoch_invalid"
      | Some epoch ->
        let* finality = step (fun () -> read_finality epoch_id) in
        match finality with
        | None -> Lwt.return_error "finality_missing"
        | Some finality ->
          let* values = rows epoch 0 0 [] in
          match values with
          | Error _ as error -> Lwt.return error
          | Ok values ->
            worker (fun () ->
              let receipts = match data_dir with
                | None -> []
                | Some base -> Option.value ~default:[]
                    (Octra_core.Preverify_receipt_store.read base ~epoch_id) in
              Sync.check_range_record ~chain_id ~epoch_id ~epoch ~finality ~rows:values ~receipts
                ~reward_source:(fun _ header ->
                  Consensus_reward_attribution.epoch_source
                    ~validator_activation_epoch:query.activation
                    ~validator_pubkeys:query.pubkeys header))
  in
  let result records next = match records with
    | [] -> `NotFound
    | _ -> `Ok (List.rev records, next)
  in
  let rec collect count size records =
    let target = Int64.add query.from_epoch (Int64.of_int count) in
    if count >= min 16 query.max_epochs then Lwt.return (result records (Some target))
    else
      let* record = load target in
      match record with
      | Error reason ->
        stop target reason;
        Lwt.return (result records (if reason = "epoch_missing" then Some target else None))
      | Ok (_, bytes) when records <> [] && size + bytes > 4_000_000 ->
        Lwt.return (result records (Some target))
      | Ok (record, bytes) -> collect (count + 1) (size + bytes) (record :: records)
  in
  Lwt.catch (fun () ->
    let* () = step Lwt.pause in
    let* records = Lwt.catch (fun () -> collect 0 0 []) (function
      | Interrupted _ as error -> Lwt.fail error
      | error -> Lwt.return (`Internal (Printexc.to_string error))) in
    worker (fun () ->
      let status, count = match records with
        | `Ok (rows, _) -> "ok", List.length rows
        | `NotFound -> "not_found", 0
        | `Internal _ -> "error", 0 in
      let json = Sync.range_response ~head:query.head ~from_epoch:query.from_epoch
        ~max_epochs:query.max_epochs records in
      Result.bind (Parts.encode json) (fun encoded ->
        Parts.render ?index:query.part encoded
        |> Result.map (fun body -> Range.{ body; status; records = count;
          encoded = if Parts.digest encoded = None then None else Some encoded }))
      |> Result.map_error (fun reason -> Range.Invalid reason))) (function
    | Interrupted error -> Lwt.return_error error
    | _ -> Lwt.return_error (Range.Invalid "range read failed"))

let create ~chaindata ~data_dir ~chain_id =
  let read_finality epoch =
    let* record = Lwt_preemptive.detach (fun () ->
      Consensus_finality_journal.read_committed_epoch ~chain_id
        ~epoch:(Int64.of_int epoch) data_dir) () in
    match record with
    | Consensus_finality_journal.Valid record ->
      Lwt.return_some Octra_consensus.C_codec.{
        finalize = record.finalize;
        validator_set = record.validator_set;
      }
    | Consensus_finality_journal.Missing -> Lwt.return_none
    | Consensus_finality_journal.Invalid reason ->
      Log.error "state_sync" "event = finality_read_failed epoch = %d reason = %s" epoch reason;
      Lwt.return_none
  in
  Range.create {
    now = (fun () -> Mtime.Span.to_float_ns (Mtime_clock.elapsed ()) /. 1e9);
    read = read ~chaindata ~data_dir:(Some data_dir) ~chain_id ~read_finality;
  }