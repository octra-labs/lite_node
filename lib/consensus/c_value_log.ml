(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open C_types

type record = {
  proposal : propose;
  votes : vote list;
}

let check ~policy ~chain_id ~epoch_id ~validator_set record =
  let proposal = record.proposal in
  let pid = C_hash.proposal_id proposal.header in
  if proposal.epoch_id <> epoch_id
    || not (C_engine.proof_proposal_valid ~chain_id ~validator_set proposal)
    || not (List.for_all (fun (vote : vote) ->
        vote.round = proposal.round && vote.proposal_id = pid) record.votes)
  then Error "stored proposal statement differs"
  else C_vote_log.polc_check ~policy ~chain_id ~epoch_id ~validator_set record.votes

let save ?sync store ~policy ~chain_id ~epoch_id ~validator_set record =
  let open Result in
  bind (check ~policy ~chain_id ~epoch_id ~validator_set record) (fun () ->
      let wire = `List [
        `String (Base64.encode_exn (C_codec.encode_propose record.proposal));
        `List (List.map (fun vote -> `String
          (Base64.encode_exn (C_codec.encode_vote vote))) record.votes)
      ] |> Yojson.Safe.to_string in
      C_vote_log.value_write ?sync store wire)

let read store =
  Result.bind (C_vote_log.value_read store) (function
  | None -> Ok None
  | Some wire ->
    try
      match Yojson.Safe.from_string wire with
      | `List [`String proposal; `List votes] when List.length votes <= C_codec.max_validators ->
        let proposal = Base64.decode_exn proposal |> C_codec.decode_propose in
        let votes = List.map (function
          | `String vote -> Base64.decode_exn vote |> C_codec.decode_vote
          | _ -> failwith "stored proposal vote is invalid") votes in
        Ok (Some {proposal; votes})
      | _ -> Error "stored proposal record is invalid"
    with exn -> Error (Printexc.to_string exn))

let load store ~policy ~chain_id ~epoch_id ~validator_set =
  Result.bind (read store) (function
    | None -> Ok None
    | Some record when record.proposal.epoch_id < epoch_id -> Ok None
    | Some record -> Result.map (fun () -> Some record)
        (check ~policy ~chain_id ~epoch_id ~validator_set record))