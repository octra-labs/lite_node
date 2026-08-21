(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Anchor = Octra_bootstrap.Sync_anchor
module C_config = Octra_consensus.C_config
module Checkpoint = Octra_bootstrap.State_sync_checkpoint
module Finality_log = Octra_consensus.Finality_log
module Floor = Octra_core.History_floor
module Journal = Consensus_finality_journal
module Manifest = Octra_bootstrap.State_sync_manifest
module State_sync = Octra_bootstrap.State_sync

type outcome =
  | Missing
  | Advanced
  | Current
  | Seeded

type fault =
  | Root of string
  | Journal of string

let reason = function
  | Root value
  | Journal value -> value

let ( let* ) value next =
  match value with
  | Ok item -> next item
  | Error _ as error -> error

let root value = Error (Root value)
let journal value = Error (Journal value)

let map_root = function
  | Ok value -> Ok value
  | Error reason -> root reason

let anchor_file path =
  try
    let stat = Unix.lstat path in
    if stat.Unix.st_kind = Unix.S_REG then Ok true
    else root "state sync finality anchor is not a regular file"
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
  | exn -> root (Printexc.to_string exn)

let preflight_log data_dir entry =
  try
    match Finality_log.last_entry_fast data_dir with
    | None -> Ok false
    | Some prior when prior.Finality_log.height < entry.Finality_log.height ->
      Ok false
    | Some prior when Finality_log.same_commitment prior entry ->
      Ok true
    | Some _ ->
      journal "finality log conflicts with checkpoint"
  with exn ->
    journal (Printexc.to_string exn)

let local_head ~raw_to_hex ~head ~cached ~epoch_root =
  match cached with
  | Some manifest when manifest.Octra_core.Head_manifest.epoch_id = head ->
    Some manifest.state_root, Some manifest.txid_hi
  | _ ->
    Option.map raw_to_hex (epoch_root head), None

let floor_matches (checkpoint : Checkpoint.body) floor =
  checkpoint.chain_id = Floor.chain_id floor
  && checkpoint.epoch = Int64.of_int (Floor.epoch floor)
  && checkpoint.state_root = Floor.state_root floor
  && checkpoint.ledger_state_root = Floor.ledger_state_root floor
  && checkpoint.txid_hi = Floor.txid_hi floor
  && checkpoint.config_hash = Floor.config_hash floor
  && checkpoint.validator_set_hash = Floor.validator_set_hash floor
  && checkpoint.epoch_index_hash = Some (Floor.epoch_index_hash floor)
  && checkpoint.epoch_index_root = Some (Floor.epoch_index_root floor)

let seed ~data_dir ~chain_id ~floor ~head ~root:local_root ~txid
    ~trusted ~exporters ~active =
  let path = State_sync.anchor_path data_dir in
  let* present = anchor_file path in
  if not present then Ok Missing
  else if head < 0 then root "state sync checkpoint local height is invalid"
  else
    let* floor = floor |> map_root in
    match floor with
    | None -> root "state sync history floor is missing"
    | Some floor when Floor.chain_id floor <> chain_id ->
      root "state sync history floor chain mismatch"
    | Some floor when head < Floor.epoch floor ->
      root "state sync history floor is ahead"
    | Some floor when head > Floor.epoch floor -> Ok Advanced
    | Some floor ->
      let* trusted = trusted () |> map_root in
      let* exporters = exporters () |> map_root in
      let* certificate = Manifest.load_certificate path |> map_root in
      let* certificate =
        Manifest.verify_reference_certificate
          ~validator_set:trusted
          ~exporter_set:exporters
          certificate
        |> map_root
      in
      let checkpoint = certificate.Manifest.checkpoint in
      if checkpoint.chain_id <> chain_id then
        root "state sync checkpoint chain mismatch"
      else if not (floor_matches checkpoint floor) then
        root "state sync checkpoint history floor mismatch"
      else if checkpoint.epoch <> Int64.of_int head then
        root "state sync checkpoint height mismatch"
      else
        match local_root with
        | None -> root "state sync checkpoint local root is missing"
        | Some local_root when checkpoint.state_root <> local_root ->
          root "state sync checkpoint local root mismatch"
        | Some _ when txid = None ->
          root "state sync checkpoint local txid is missing"
        | Some _ when txid <> Some checkpoint.txid_hi ->
          root "state sync checkpoint local txid mismatch"
        | Some _ ->
          begin
            match Manifest.finality certificate with
            | None -> root "state sync finality anchor is missing"
            | Some encoded ->
              let* anchor =
                Anchor.verify ~validator_set:trusted checkpoint encoded
                |> map_root
              in
              let anchor_set = Anchor.validator_set anchor in
              if C_config.validator_set_hash anchor_set
                 <> C_config.validator_set_hash active then
                root "state sync active validator set mismatch"
              else
                let finalize = Anchor.finality anchor in
                let entry = Finality_log.of_finalize finalize in
                let* log_current = preflight_log data_dir entry in
                begin
                  match
                    Journal.seed
                      ~chain_id
                      ~validator_set:active
                      ~finalize
                      data_dir
                  with
                  | Error reason -> journal reason
                  | Ok journal_state ->
                    begin
                      try
                        Finality_log.write data_dir entry;
                        match journal_state, log_current with
                        | Journal.Seed_current, true -> Ok Current
                        | _ -> Ok Seeded
                      with exn ->
                        journal (Printexc.to_string exn)
                    end
                end
          end