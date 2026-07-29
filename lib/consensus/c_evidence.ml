(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open C_types

type vote_conflict = {
  first : vote;
  second : vote;
}

let max_vote_bytes = 1_024

let same_slot (left : vote) (right : vote) =
  left.chain_id = right.chain_id
  && left.epoch_id = right.epoch_id
  && left.round = right.round
  && left.vote_type = right.vote_type
  && left.validator = right.validator

let compare_vote (left : vote) (right : vote) =
  let by_proposal = String.compare left.proposal_id right.proposal_id in
  if by_proposal <> 0 then by_proposal
  else String.compare left.signature right.signature

let vote_conflict (left : vote) (right : vote) =
  if not (same_slot left right) || left.proposal_id = right.proposal_id then None
  else if compare_vote left right <= 0 then Some { first = left; second = right }
  else Some { first = right; second = left }

let vote_conflict_id evidence =
  Octra_net.Hash_domain.hash_encoded "octra:vote_conflict:v1" (fun buf ->
    Octra_net.Oce1.put_string buf (C_codec.encode_vote evidence.first);
    Octra_net.Oce1.put_string buf (C_codec.encode_vote evidence.second))

let vote_conflict_root evidence =
  evidence
  |> List.map vote_conflict_id
  |> List.sort_uniq String.compare
  |> fun ids ->
    Octra_net.Hash_domain.hash_encoded "octra:vote_conflict_root:v1" (fun buf ->
      Octra_net.Oce1.put_list Octra_net.Oce1.put_hash32 buf ids)

let vote_slot_id (vote : vote) =
  Octra_net.Hash_domain.hash_encoded "octra:vote_slot:v1" (fun buf ->
    Octra_net.Oce1.put_string buf vote.chain_id;
    Octra_net.Oce1.put_u64 buf vote.epoch_id;
    Octra_net.Oce1.put_u32_int buf vote.round;
    Octra_net.Oce1.put_u8 buf (vote_type_to_u8 vote.vote_type);
    Octra_net.Oce1.put_string buf vote.validator)

let verify_vote_conflict ~pubkey_raw evidence =
  match vote_conflict evidence.first evidence.second with
  | None -> false
  | Some canonical ->
    canonical = evidence
    && C_hash.verify_vote ~pubkey_raw evidence.first
    && C_hash.verify_vote ~pubkey_raw evidence.second

let encode_vote_conflict evidence =
  Octra_net.Oce1.encode (fun buf ->
    Octra_net.Oce1.put_string buf (C_codec.encode_vote evidence.first);
    Octra_net.Oce1.put_string buf (C_codec.encode_vote evidence.second))

let decode_vote_conflict payload =
  Octra_net.Oce1.decode (fun cursor ->
    let first =
      Octra_net.Oce1.get_string_bounded ~max:max_vote_bytes cursor
      |> C_codec.decode_vote
    in
    let second =
      Octra_net.Oce1.get_string_bounded ~max:max_vote_bytes cursor
      |> C_codec.decode_vote
    in
    match vote_conflict first second with
    | Some evidence -> evidence
    | None -> failwith "vote evidence is not a conflict") payload