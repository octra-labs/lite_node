(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C_types = Octra_consensus.C_types
module C_config = Octra_consensus.C_config
module Parent = Octra_consensus.C_parent_commit
module Protocol = Octra_consensus.C_protocol
module Finality_log = Octra_consensus.Finality_log
module Journal = Consensus_finality_journal

type source = {
  chain_id : string;
  data_dir : string;
  activation_epoch : int64;
}

let create ~chain_id ~data_dir getenv =
  Protocol.activation_epoch_of getenv
  |> Result.map (fun activation_epoch ->
    { chain_id; data_dir; activation_epoch })

let required source epoch_id =
  Int64.compare epoch_id source.activation_epoch >= 0

let missing source epoch_id reason =
  if required source epoch_id then Error reason
  else Ok None

let expected_of_parent (parent : C_types.parent_commit) =
  let certificate = parent.certificate in
  Parent.{
    chain_id = certificate.chain_id;
    epoch_id = certificate.epoch_id;
    proposal_id = certificate.proposal_id;
    state_root = certificate.header.proposed_state_root;
    validator_set_hash =
      C_config.validator_set_hash parent.validator_set;
  }

let validate parent =
  match Parent.validate (expected_of_parent parent) parent with
  | Parent.Valid -> Ok (Parent.canonical parent)
  | Parent.Invalid reason -> Error ("invalid parent commit: " ^ reason)

let load source ~epoch_id =
  if Int64.compare epoch_id 0L <= 0 then
    missing source epoch_id "parent commit unavailable at genesis"
  else
    let expected_epoch = Int64.pred epoch_id in
    match Finality_log.last source.data_dir with
    | None ->
      missing source epoch_id "parent finality entry is missing"
    | Some entry ->
      let finality_epoch = Int64.of_int entry.Finality_log.height in
      if Int64.compare finality_epoch expected_epoch < 0 then
        missing source epoch_id "parent finality entry is behind"
      else if Int64.compare finality_epoch expected_epoch > 0 then
        Error "parent finality entry is ahead"
      else
        match
          Journal.read_committed_validated
            ~chain_id:source.chain_id
            ~entry
            source.data_dir
        with
        | Journal.Missing ->
          missing source epoch_id "parent commit journal is missing"
        | Journal.Invalid reason ->
          Error ("parent commit journal is invalid: " ^ reason)
        | Journal.Valid record ->
          let parent = C_types.{
            certificate =
              certificate_of_finalize record.Journal.finalize;
            validator_set = record.validator_set;
          } in
          Result.map Option.some (validate parent)

let verify source ~epoch_id incoming =
  match load source ~epoch_id with
  | Error _ as error -> error
  | Ok None ->
    begin
      match incoming with
      | None -> Ok ()
      | Some _ -> Error "unexpected parent commit"
    end
  | Ok (Some expected) ->
    begin
      match incoming with
      | None -> Error "parent commit is missing"
      | Some parent ->
        begin
          match Parent.validate (expected_of_parent expected) parent with
          | Parent.Invalid reason ->
            Error ("invalid parent commit: " ^ reason)
          | Parent.Valid ->
            if Parent.same_block parent expected then Ok ()
            else Error "parent commit does not match local finality"
        end
    end