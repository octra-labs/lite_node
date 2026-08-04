(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Checkpoint = State_sync_checkpoint
module Eic = Octra_core.Epoch_index_commitment
module Epochlog = Octra_core.Epochlog
module Finality = Octra_consensus.Finality_log
module Head = Octra_core.Head_manifest
module Store = Octra_core.Store_chaindata

open Lwt.Infix

let fail reason =
  Lwt.return_error reason

let verify_irmin checkpoint data_dir =
  let path = Filename.concat data_dir "irmin_store" in
  Lwt.catch
    (fun () ->
      Octra_core.Store_irmin.open_store ~readonly:true path >>= fun store ->
      Lwt.finalize
        (fun () ->
          Octra_core.Store_irmin.get_head_hash store >>= function
          | Some root when root = checkpoint.Checkpoint.ledger_state_root ->
              Lwt.return_ok root
          | Some _ -> fail "restored Irmin root does not match checkpoint"
          | None -> fail "restored Irmin store has no head")
        (fun () -> Octra_core.Store_irmin.close store))
    (fun exn -> fail ("restored Irmin verification failed: " ^ Printexc.to_string exn))

let verify_chaindata checkpoint ledger_root data_dir =
  try
    let store =
      Octra_core.Store_chaindata.open_chaindata
        ~readonly:true
        (Filename.concat data_dir "chaindata")
    in
    Fun.protect
      ~finally:(fun () -> Octra_core.Store_chaindata.close store)
      (fun () ->
        match Octra_core.Store_chaindata.history_floor store with
        | Error reason -> Error ("restored history floor is invalid: " ^ reason)
        | Ok None -> Error "restored history floor is missing"
        | Ok (Some floor) ->
            let module Floor = Octra_core.History_floor in
            let fields_match =
              Floor.chain_id floor = checkpoint.Checkpoint.chain_id
              && Int64.of_int (Floor.epoch floor) = checkpoint.epoch
              && Floor.state_root floor = checkpoint.state_root
              && Floor.ledger_state_root floor = checkpoint.ledger_state_root
              && Floor.txid_hi floor = checkpoint.txid_hi
              && Floor.config_hash floor = checkpoint.config_hash
              && Floor.validator_set_hash floor = checkpoint.validator_set_hash
              && Some (Floor.epoch_index_hash floor) = checkpoint.epoch_index_hash
              && Some (Floor.epoch_index_root floor) = checkpoint.epoch_index_root
            in
            if not fields_match then
              Error "restored history floor differs from checkpoint"
            else if ledger_root <> Floor.ledger_state_root floor then
              Error "restored ledger root differs from history floor"
            else
              Octra_core.Store_chaindata.validate_history_floor store floor)
  with exn ->
    Error ("restored chaindata verification failed: " ^ Printexc.to_string exn)

let verify checkpoint data_dir =
  match Checkpoint.validate checkpoint with
  | Error reason -> fail reason
  | Ok () ->
      match Octra_core.Head_manifest.load_result data_dir with
      | Octra_core.Head_manifest.Missing -> fail "restored HEAD.json is missing"
      | Octra_core.Head_manifest.Corrupt reason ->
          fail ("restored HEAD.json is corrupt: " ^ reason)
      | Octra_core.Head_manifest.Present head
        when not (Checkpoint.matches_head checkpoint head) ->
          fail "restored HEAD.json does not match checkpoint"
      | Octra_core.Head_manifest.Present _ ->
          verify_irmin checkpoint data_dir >>= function
          | Error _ as error -> Lwt.return error
          | Ok ledger_root -> Lwt.return (verify_chaindata checkpoint ledger_root data_dir)

type anchor = {
  header : Epochlog.epoch_header;
  index_hash : string;
  index_root : string;
}

let historical_epoch_limit = 500_000

let txid_hi header =
  if header.Epochlog.tx_count < 0 then Error "historical epoch has negative transaction count"
  else if Int64.compare header.start_txid 0L < 0 then
    Error "historical epoch has negative start transaction id"
  else if header.tx_count = 0 then
    if header.start_txid = 0L then Error "historical epoch has invalid transaction range"
    else Ok (Int64.pred header.start_txid)
  else
    let offset = Int64.of_int (header.tx_count - 1) in
    if Int64.compare header.start_txid (Int64.sub Int64.max_int offset) > 0 then
      Error "historical epoch transaction range overflows"
    else
      Ok (Int64.add header.start_txid offset)

let verify_historical_anchor checkpoint header finality status =
  let epoch = Int64.to_int checkpoint.Checkpoint.epoch in
  if header.Epochlog.id <> epoch then Error "historical epoch id mismatch"
  else if header.state_root <> checkpoint.state_root then
    Error "historical epoch state root mismatch"
  else if finality.Finality.height <> epoch then Error "historical finality height mismatch"
  else if finality.state_root <> checkpoint.state_root then
    Error "historical finality state root mismatch"
  else if finality.creator_addr <> header.proposer.creator_addr
       || finality.round <> header.proposer.commit_round then
    Error "historical finality proposer mismatch"
  else
    match txid_hi header with
    | Error _ as error -> error
    | Ok high when high <> checkpoint.txid_hi || finality.txid_hi <> high ->
        Error "historical transaction high-water mismatch"
    | Ok _ when not status.Store.eic_ok ->
        Error ("historical epoch index is invalid: "
          ^ String.concat "; " status.eic_errors)
    | Ok _ ->
        match status.eic_actual_hash, status.eic_actual_root,
          checkpoint.epoch_index_hash, checkpoint.epoch_index_root with
        | Some hash, Some root, Some expected_hash, Some expected_root
          when hash = expected_hash && root = expected_root ->
            let state_root =
              Eic.folded_state_root
                ~ledger_state_root:checkpoint.ledger_state_root
                ~epoch_index_root:root
            in
            if state_root <> checkpoint.state_root then
              Error "historical folded state root mismatch"
            else
              begin
                match checkpoint.quorum_cert_hash with
                | Some expected when finality.qc_hash <> Some expected ->
                    Error "historical quorum certificate mismatch"
                | _ -> Ok { header; index_hash = hash; index_root = root }
              end
        | Some _, Some _, Some _, Some _ ->
            Error "historical epoch index commitment mismatch"
        | _ -> Error "historical epoch index commitment is incomplete"

let verify_live_head head anchor =
  if head.Head.epoch_id <> anchor.header.Epochlog.id then
    Error "live HEAD epoch mismatch"
  else if head.state_root <> anchor.header.state_root then
    Error "live HEAD state root mismatch"
  else if head.epoch_index_hash <> Some anchor.index_hash
       || head.epoch_index_root <> Some anchor.index_root then
    Error "live HEAD epoch index mismatch"
  else
    match txid_hi anchor.header with
    | Error _ as error -> error
    | Ok high when high <> head.txid_hi -> Error "live HEAD transaction high-water mismatch"
    | Ok _ ->
        let state_root =
          Eic.folded_state_root
            ~ledger_state_root:(Head.ledger_state_root head)
            ~epoch_index_root:anchor.index_root
        in
        if state_root = head.state_root then Ok ()
        else Error "live HEAD folded state root mismatch"

let verify_historical checkpoint data_dir =
  match Checkpoint.validate checkpoint with
  | Error reason -> Error reason
  | Ok () when Int64.compare checkpoint.epoch (Int64.of_int max_int) > 0 ->
      Error "historical checkpoint epoch exceeds platform limit"
  | Ok () ->
      match Head.load_result data_dir with
      | Head.Missing -> Error "live HEAD.json is missing"
      | Head.Corrupt reason -> Error ("live HEAD.json is corrupt: " ^ reason)
      | Head.Present head ->
          let epoch = Int64.to_int checkpoint.epoch in
          let lag = head.epoch_id - epoch in
          if lag < 0 then Error "historical checkpoint is ahead of live HEAD"
          else if lag > historical_epoch_limit then
            Error "historical checkpoint exceeds verification range"
          else
            try
              let store =
                Store.open_chaindata
                  ~readonly:true
                  (Filename.concat data_dir "chaindata")
              in
              Fun.protect
                ~finally:(fun () -> Store.close store)
                (fun () ->
                  let finality = Finality.index data_dir in
                  match Store.get_bound_epoch_header store epoch,
                    Finality.find finality epoch with
                  | Error reason, _ -> Error ("historical epoch binding failed: " ^ reason)
                  | _, None -> Error "historical finality record is missing"
                  | Ok header, Some finality_entry ->
                      let previous =
                        Octra_core.Startup_recovery.previous_eic_root store epoch
                      in
                      let status =
                        Store.verify_epoch_index_commitment_raw
                          store
                          ~epoch_id:epoch
                          ~start_txid:header.start_txid
                          ~tx_count:header.tx_count
                          ~prev_root:previous
                      in
                      match verify_historical_anchor
                        checkpoint header finality_entry status with
                      | Error _ as error -> error
                      | Ok initial ->
                          let rec walk next anchor =
                            if next > head.epoch_id then verify_live_head head anchor
                            else
                              match Store.get_bound_epoch_header store next with
                              | Error reason ->
                                  Error ("historical continuity failed: " ^ reason)
                              | Ok current
                                when current.prev_state_root <> anchor.header.state_root ->
                                  Error "historical state root continuity mismatch"
                              | Ok current ->
                                  let status =
                                    Store.verify_epoch_index_commitment_raw
                                      store
                                      ~epoch_id:next
                                      ~start_txid:current.start_txid
                                      ~tx_count:current.tx_count
                                      ~prev_root:anchor.index_root
                                  in
                                  if not status.eic_ok then
                                    Error ("historical epoch index continuity failed: "
                                      ^ String.concat "; " status.eic_errors)
                                  else
                                    match status.eic_actual_hash, status.eic_actual_root with
                                    | Some index_hash, Some index_root ->
                                        walk (next + 1) {
                                          header = current;
                                          index_hash;
                                          index_root;
                                        }
                                    | _ ->
                                        Error "historical epoch index continuity is incomplete"
                          in
                          walk (epoch + 1) initial)
            with exn ->
              Error ("historical checkpoint verification failed: "
                ^ Printexc.to_string exn)