(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Checkpoint = State_sync_checkpoint
module C_codec = Octra_consensus.C_codec
module C_config = Octra_consensus.C_config
module C_hash = Octra_consensus.C_hash
module C_types = Octra_consensus.C_types
module O = Octra_net.Oce1
module Parent = Octra_consensus.C_parent_commit

let width = 64
let name = "ready_roots"
let max_item = 4_000_000
let max_file = 32_000_000

let ( let* ) value f =
  match value with
  | Ok item -> f item
  | Error _ as error -> error

let protect f =
  try Ok (f ()) with exn -> Error (Printexc.to_string exn)

let encode values =
  O.encode (fun buffer ->
    O.put_list
      (fun target value ->
        O.put_bytes target (C_codec.encode_finalize value))
      buffer
      values)

let decode payload =
  let* values =
    protect (fun () ->
      O.decode
        (fun cursor ->
          O.get_list_bounded
            ~max:width
            (fun source ->
              O.get_bytes_bounded ~max:max_item source
              |> C_codec.decode_finalize)
            cursor)
        payload)
  in
  if encode values = payload then Ok values
  else Error "ready root window encoding is not exact"

let expected (parent : C_types.parent_commit) =
  let certificate = parent.certificate in
  Parent.{
    chain_id = certificate.chain_id;
    epoch_id = certificate.epoch_id;
    proposal_id = certificate.proposal_id;
    state_root = certificate.header.proposed_state_root;
    validator_set_hash = C_config.validator_set_hash parent.validator_set;
  }

let same_parent expected_parent finalize =
  let got = C_types.{
    certificate = C_types.certificate_of_finalize finalize;
    validator_set = expected_parent.C_types.validator_set;
  }
  in
  Parent.encode got = Parent.encode expected_parent

let bind ~current ~validator_set prior =
  match current.C_types.parent_commit with
  | None -> Error "ready root parent commit is missing"
  | Some parent ->
      let got = C_types.{
        certificate = C_types.certificate_of_finalize prior;
        validator_set;
      }
      in
      if C_hash.parent_commit_hash_opt prior.parent_commit
         <> prior.header.parent_commit_hash then
        Error "ready root stored parent hash differs"
      else if not (Parent.same_block parent got) then
        Error "ready root stored block differs"
      else
        match Parent.validate (expected parent) got with
        | Parent.Invalid reason -> Error ("ready root stored qc " ^ reason)
        | Parent.Valid ->
            let certificate = parent.certificate in
            Ok C_types.{
              chain_id = certificate.chain_id;
              epoch_id = certificate.epoch_id;
              commit_round = certificate.commit_round;
              header = certificate.header;
              proposal_id = certificate.proposal_id;
              precommits = certificate.precommits;
              parent_commit = prior.parent_commit;
            }

let link (current : C_types.finalize) (prior : C_types.finalize) =
  match current.C_types.parent_commit with
  | None -> Error "ready root parent commit is missing"
  | Some parent ->
      let expected_epoch = Int64.pred current.epoch_id in
      if Int64.compare current.epoch_id 0L <= 0 then
        Error "ready root epoch underflows"
      else if prior.epoch_id <> expected_epoch
           || prior.header.epoch_id <> expected_epoch then
        Error "ready root epochs are not consecutive"
      else if current.chain_id <> prior.chain_id
           || current.header.chain_id <> prior.header.chain_id then
        Error "ready root chain differs"
      else if current.header.prev_state_root
              <> prior.header.proposed_state_root then
        Error "ready root state link differs"
      else if C_hash.parent_commit_hash_opt current.parent_commit
              <> current.header.parent_commit_hash then
        Error "ready root parent hash differs"
      else if not (same_parent parent prior) then
        Error "ready root certificate differs"
      else
        match Parent.validate (expected parent) parent with
        | Parent.Invalid reason -> Error ("ready root qc " ^ reason)
        | Parent.Valid -> Ok ()

let root (finalize : C_types.finalize) =
  if Int64.compare finalize.C_types.epoch_id 0L < 0
     || Int64.compare finalize.epoch_id (Int64.of_int max_int) > 0 then
    Error "ready root epoch exceeds platform range"
  else
    Ok
      (Int64.to_int finalize.epoch_id,
       Checkpoint.raw_to_hex finalize.header.proposed_state_root)

let verify ~anchor values =
  let rec loop current roots = function
    | [] -> Ok (List.rev roots)
    | prior :: rest ->
        let* () = link current prior in
        let* item = root prior in
        loop prior (item :: roots) rest
  in
  if List.length values > width then Error "ready root window is too wide"
  else
    let* item = root anchor in
    loop anchor [item] values

let select ~stored ~signed =
  match stored, signed with
  | None, None -> Ok None
  | Some root, None | None, Some root -> Ok (Some root)
  | Some stored, Some signed when stored = signed -> Ok (Some stored)
  | Some _, Some _ -> Error "ready root sources differ"

let read path =
  match
    protect (fun () ->
      let channel = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr channel)
        (fun () ->
          let size = in_channel_length channel in
          if size < 0 || size > max_file then
            failwith "ready root window exceeds byte limit";
          really_input_string channel size))
  with
  | Error _ as error -> error
  | Ok payload -> decode payload