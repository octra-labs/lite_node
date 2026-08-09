(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module NodeMap = Map.Make (String)

let max_chain_id_bytes = 128
let max_node_id_bytes = 128
let max_nonce_bytes = 128
let max_candidates = 256
let max_committee_size = 64

type commitment = {
  chain_id : string;
  offer_id : string;
  commit_epoch : int64;
  node_id : string;
  graph_root : string;
  model_root : string;
  program_root : string;
  executor_root : string;
  nonce_hash : string;
  signature : string;
}

type reveal = {
  chain_id : string;
  offer_id : string;
  commit_epoch : int64;
  reveal_epoch : int64;
  node_id : string;
  graph_root : string;
  model_root : string;
  program_root : string;
  executor_root : string;
  nonce : string;
  evidence_root : string;
  signature : string;
}

type reject =
  | Malformed
  | Chain_mismatch
  | Offer_mismatch
  | Epoch_mismatch
  | Identity_mismatch
  | Model_mismatch
  | Program_mismatch
  | Nonce_mismatch
  | Commitment_signature
  | Reveal_signature
  | Evidence

type member = {
  node_id : string;
  graph_root : string;
  model_root : string;
  program_root : string;
  executor_root : string;
  evidence_root : string;
  score : string;
}

type selection = {
  offer_id : string;
  target_epoch : int64;
  challenge : string;
  members : member list;
  root : string;
}

let hash32 value =
  String.length value = 32

let signature64 value =
  String.length value = 64

let bounded_non_empty limit value =
  let length = String.length value in
  length > 0 && length <= limit

let commitment_is_well_formed (commitment : commitment) =
  bounded_non_empty max_chain_id_bytes commitment.chain_id
  && hash32 commitment.offer_id
  && commitment.commit_epoch >= 0L
  && bounded_non_empty max_node_id_bytes commitment.node_id
  && hash32 commitment.graph_root
  && hash32 commitment.model_root
  && hash32 commitment.program_root
  && hash32 commitment.executor_root
  && hash32 commitment.nonce_hash
  && signature64 commitment.signature

let reveal_is_well_formed (reveal : reveal) =
  bounded_non_empty max_chain_id_bytes reveal.chain_id
  && hash32 reveal.offer_id
  && reveal.commit_epoch >= 0L
  && reveal.reveal_epoch >= 0L
  && bounded_non_empty max_node_id_bytes reveal.node_id
  && hash32 reveal.graph_root
  && bounded_non_empty max_nonce_bytes reveal.nonce
  && hash32 reveal.model_root
  && hash32 reveal.program_root
  && hash32 reveal.executor_root
  && hash32 reveal.evidence_root
  && signature64 reveal.signature

let nonce_hash nonce =
  Octra_net.Hash_domain.hash "octra:resource_compute_nonce" nonce

let commitment_sign_bytes (commitment : commitment) =
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_commitment" (fun buf ->
    Octra_net.Oce1.put_string buf commitment.chain_id;
    Octra_net.Oce1.put_hash32 buf commitment.offer_id;
    Octra_net.Oce1.put_u64 buf commitment.commit_epoch;
    Octra_net.Oce1.put_string buf commitment.node_id;
    Octra_net.Oce1.put_hash32 buf commitment.graph_root;
    Octra_net.Oce1.put_hash32 buf commitment.model_root;
    Octra_net.Oce1.put_hash32 buf commitment.program_root;
    Octra_net.Oce1.put_hash32 buf commitment.executor_root;
    Octra_net.Oce1.put_hash32 buf commitment.nonce_hash)

let commitment_id (commitment : commitment) =
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_commitment_id" (fun buf ->
    Octra_net.Oce1.put_hash32 buf (commitment_sign_bytes commitment);
    Octra_net.Oce1.put_sig64 buf commitment.signature)

let reveal_sign_bytes (commitment : commitment) (reveal : reveal) =
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_reveal" (fun buf ->
    Octra_net.Oce1.put_hash32 buf (commitment_id commitment);
    Octra_net.Oce1.put_u64 buf reveal.reveal_epoch;
    Octra_net.Oce1.put_string buf reveal.nonce;
    Octra_net.Oce1.put_hash32 buf reveal.evidence_root)

let reveal_id (commitment : commitment) (reveal : reveal) =
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_reveal_id" (fun buf ->
    Octra_net.Oce1.put_hash32 buf (reveal_sign_bytes commitment reveal);
    Octra_net.Oce1.put_sig64 buf reveal.signature)

let challenge ~chain_id ~source_epoch ~finality_hash =
  if
    not (bounded_non_empty max_chain_id_bytes chain_id)
    || source_epoch < 0L
    || not (hash32 finality_hash)
  then
    invalid_arg "resource compute challenge parameters";
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_challenge" (fun buf ->
    Octra_net.Oce1.put_string buf chain_id;
    Octra_net.Oce1.put_u64 buf source_epoch;
    Octra_net.Oce1.put_hash32 buf finality_hash)

let protected_bool evaluate =
  try evaluate () with _ -> false

let epoch_window ~target_epoch ~minimum_delay (commitment : commitment) (reveal : reveal) =
  commitment.commit_epoch < target_epoch
  && reveal.reveal_epoch < target_epoch
  && reveal.reveal_epoch >= commitment.commit_epoch
  && Int64.sub reveal.reveal_epoch commitment.commit_epoch >= minimum_delay

let validate_after_commitment
    ~chain_id
    ~target_epoch
    ~minimum_delay
    ~verify_reveal_signature
    ~verify_evidence
    (commitment : commitment)
    (reveal : reveal) =
  if not (reveal_is_well_formed reveal) then
    Error Malformed
  else if reveal.chain_id <> chain_id then
    Error Chain_mismatch
  else if reveal.offer_id <> commitment.offer_id then
    Error Offer_mismatch
  else if
    reveal.commit_epoch <> commitment.commit_epoch
    || not (epoch_window ~target_epoch ~minimum_delay commitment reveal)
  then
    Error Epoch_mismatch
  else if reveal.node_id <> commitment.node_id then
    Error Identity_mismatch
  else if reveal.graph_root <> commitment.graph_root then
    Error Model_mismatch
  else if reveal.model_root <> commitment.model_root then
    Error Model_mismatch
  else if reveal.program_root <> commitment.program_root then
    Error Program_mismatch
  else if reveal.executor_root <> commitment.executor_root then
    Error Program_mismatch
  else if nonce_hash reveal.nonce <> commitment.nonce_hash then
    Error Nonce_mismatch
  else if not (protected_bool (fun () -> verify_reveal_signature commitment reveal)) then
    Error Reveal_signature
  else if not (protected_bool (fun () -> verify_evidence reveal)) then
    Error Evidence
  else
    Ok reveal

let validate
    ~chain_id
    ~target_epoch
    ~minimum_delay
    ~verify_commitment_signature
    ~verify_reveal_signature
    ~verify_evidence
    (commitment : commitment)
    (reveal : reveal) =
  if not (commitment_is_well_formed commitment && reveal_is_well_formed reveal) then
    Error Malformed
  else if commitment.chain_id <> chain_id || reveal.chain_id <> chain_id then
    Error Chain_mismatch
  else if reveal.offer_id <> commitment.offer_id then
    Error Offer_mismatch
  else if
    reveal.commit_epoch <> commitment.commit_epoch
    || not (epoch_window ~target_epoch ~minimum_delay commitment reveal)
  then
    Error Epoch_mismatch
  else if reveal.node_id <> commitment.node_id then
    Error Identity_mismatch
  else if reveal.graph_root <> commitment.graph_root then
    Error Model_mismatch
  else if reveal.model_root <> commitment.model_root then
    Error Model_mismatch
  else if reveal.program_root <> commitment.program_root then
    Error Program_mismatch
  else if reveal.executor_root <> commitment.executor_root then
    Error Program_mismatch
  else if nonce_hash reveal.nonce <> commitment.nonce_hash then
    Error Nonce_mismatch
  else if not (protected_bool (fun () -> verify_commitment_signature commitment)) then
    Error Commitment_signature
  else
    validate_after_commitment
      ~chain_id
      ~target_epoch
      ~minimum_delay
      ~verify_reveal_signature
      ~verify_evidence
      commitment
      reveal

let member ~challenge (commitment : commitment) (reveal : reveal) =
  let score =
    Octra_net.Hash_domain.hash_encoded "octra:resource_compute_score" (fun buf ->
      Octra_net.Oce1.put_hash32 buf challenge;
      Octra_net.Oce1.put_hash32 buf (commitment_id commitment)) in
  {
    node_id = commitment.node_id;
    graph_root = commitment.graph_root;
    model_root = commitment.model_root;
    program_root = commitment.program_root;
    executor_root = commitment.executor_root;
    evidence_root = reveal.evidence_root;
    score;
  }

let compare_member left right =
  let score_order = String.compare left.score right.score in
  if score_order <> 0 then score_order
  else String.compare left.node_id right.node_id

let rec take count values =
  if count <= 0 then []
  else
    match values with
    | [] -> []
    | value :: rest -> value :: take (count - 1) rest

let group_by_node values node_id_of =
  List.fold_left
    (fun table value ->
      let node_id = node_id_of value in
      NodeMap.update
        node_id
        (function
         | None -> Some [value]
         | Some values -> Some (value :: values))
        table)
    NodeMap.empty
    values

let unique_by id_of values =
  List.fold_left
    (fun table value -> NodeMap.add (id_of value) value table)
    NodeMap.empty
    values
  |> NodeMap.bindings
  |> List.map snd

let valid_pairs
    ~chain_id
    ~offer_id
    ~target_epoch
    ~minimum_delay
    ~verify_commitment_signature
    ~verify_reveal_signature
    ~verify_evidence
    commitments
    reveals =
  let valid_commitments =
    commitments
    |> List.filter (fun commitment ->
      commitment_is_well_formed commitment
      && commitment.chain_id = chain_id
      && commitment.offer_id = offer_id
      && commitment.commit_epoch < target_epoch
      && protected_bool (fun () -> verify_commitment_signature commitment))
    |> unique_by commitment_id
    |> fun values -> group_by_node values (fun (commitment : commitment) -> commitment.node_id) in
  let reveals_by_node =
    reveals
    |> List.filter reveal_is_well_formed
    |> fun values -> group_by_node values (fun (reveal : reveal) -> reveal.node_id) in
  NodeMap.fold
    (fun node_id node_commitments pairs ->
      match node_commitments, NodeMap.find_opt node_id reveals_by_node with
      | [commitment], Some node_reveals ->
        let valid_reveals =
          node_reveals
          |> List.filter_map (fun reveal ->
            match
              validate_after_commitment
                ~chain_id
                ~target_epoch
                ~minimum_delay
                ~verify_reveal_signature
                ~verify_evidence
                commitment
                reveal
            with
            | Ok value -> Some value
            | Error _ -> None)
          |> unique_by (reveal_id commitment) in
        begin
          match valid_reveals with
          | [reveal] -> (commitment, reveal) :: pairs
          | _ -> pairs
        end
      | _ -> pairs)
    valid_commitments
    []

let selection_root ~offer_id ~target_epoch ~challenge members =
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_selection" (fun buf ->
    Octra_net.Oce1.put_hash32 buf offer_id;
    Octra_net.Oce1.put_u64 buf target_epoch;
    Octra_net.Oce1.put_hash32 buf challenge;
    Octra_net.Oce1.put_list
      (fun buf member ->
        Octra_net.Oce1.put_string buf member.node_id;
        Octra_net.Oce1.put_hash32 buf member.graph_root;
        Octra_net.Oce1.put_hash32 buf member.model_root;
        Octra_net.Oce1.put_hash32 buf member.program_root;
        Octra_net.Oce1.put_hash32 buf member.executor_root;
        Octra_net.Oce1.put_hash32 buf member.evidence_root;
        Octra_net.Oce1.put_hash32 buf member.score)
      buf
      members)

let select
    ~chain_id
    ~offer_id
    ~target_epoch
    ~challenge
    ~minimum_delay
    ~committee_size
    ~verify_commitment_signature
    ~verify_reveal_signature
    ~verify_evidence
    ~commitments
    ~reveals =
  if
    committee_size <= 0
    || committee_size > max_committee_size
    || minimum_delay <= 0L
    || target_epoch <= 0L
    || not (bounded_non_empty max_chain_id_bytes chain_id)
    || not (hash32 offer_id)
    || not (hash32 challenge)
    || List.length commitments > max_candidates
    || List.length reveals > max_candidates
  then
    invalid_arg "resource compute selection parameters";
  let members =
    valid_pairs
      ~chain_id
      ~offer_id
      ~target_epoch
      ~minimum_delay
      ~verify_commitment_signature
      ~verify_reveal_signature
      ~verify_evidence
      commitments
      reveals
    |> List.map (fun (commitment, reveal) -> member ~challenge commitment reveal)
    |> List.sort compare_member
    |> take committee_size in
  {
    offer_id;
    target_epoch;
    challenge;
    members;
    root = selection_root ~offer_id ~target_epoch ~challenge members;
  }