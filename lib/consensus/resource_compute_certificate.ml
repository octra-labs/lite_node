(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module NodeMap = Map.Make (String)
module ResultMap = Map.Make (String)

let max_chain_id_bytes = 128
let max_circle_id_bytes = 128
let max_node_id_bytes = 128
let max_selected = 64
let max_votes = 256
let max_request_bytes = 1_048_576
let max_response_bytes = 1_048_576

type request = {
  chain_id : string;
  epoch_id : int64;
  expires_epoch : int64;
  validator_set_root : string;
  selection_root : string;
  circle_id : string;
  state_root : string;
  model_epoch : int64;
  model_state_root : string;
  graph_root : string;
  model_root : string;
  program_root : string;
  executor_root : string;
  caller : string;
  session_id : string;
  sequence : int64;
  input_hash : string;
  input_bytes : int;
  max_output_bytes : int;
}

type vote = {
  request_id : string;
  node_id : string;
  output_hash : string;
  trace_root : string;
  output_bytes : int;
  steps : int64;
  operations : int64;
  signature : string;
}

type signer = {
  node_id : string;
  weight : Z.t;
  vote_id : string;
}

type certificate = {
  request_id : string;
  output_hash : string;
  trace_root : string;
  output_bytes : int;
  steps : int64;
  operations : int64;
  votes : vote list;
  signers : signer list;
  signed_weight : Z.t;
  root : string;
}

type reject =
  | Request_malformed
  | Chain_mismatch
  | Epoch_mismatch
  | Selection_malformed
  | Threshold_malformed

type decision =
  | Rejected of reject
  | Insufficient
  | Conflicting
  | Agreed of certificate

let hash32 value =
  String.length value = 32

let signature64 value =
  String.length value = 64

let bounded_non_empty limit value =
  let length = String.length value in
  length > 0 && length <= limit

let request_shape (request : request) =
  bounded_non_empty max_chain_id_bytes request.chain_id
  && request.epoch_id >= 0L
  && request.expires_epoch >= request.epoch_id
  && hash32 request.validator_set_root
  && hash32 request.selection_root
  && bounded_non_empty max_circle_id_bytes request.circle_id
  && hash32 request.state_root
  && request.model_epoch >= 0L
  && hash32 request.model_state_root
  && hash32 request.graph_root
  && hash32 request.model_root
  && hash32 request.program_root
  && hash32 request.executor_root
  && bounded_non_empty max_node_id_bytes request.caller
  && hash32 request.session_id
  && request.sequence >= 0L
  && hash32 request.input_hash
  && request.input_bytes > 0
  && request.input_bytes <= max_request_bytes
  && request.max_output_bytes > 0
  && request.max_output_bytes <= max_response_bytes

let request_id request =
  if not (request_shape request) then
    invalid_arg "resource compute request";
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_request" (fun buf ->
    Octra_net.Oce1.put_string buf request.chain_id;
    Octra_net.Oce1.put_u64 buf request.epoch_id;
    Octra_net.Oce1.put_u64 buf request.expires_epoch;
    Octra_net.Oce1.put_hash32 buf request.validator_set_root;
    Octra_net.Oce1.put_hash32 buf request.selection_root;
    Octra_net.Oce1.put_string buf request.circle_id;
    Octra_net.Oce1.put_hash32 buf request.state_root;
    Octra_net.Oce1.put_u64 buf request.model_epoch;
    Octra_net.Oce1.put_hash32 buf request.model_state_root;
    Octra_net.Oce1.put_hash32 buf request.graph_root;
    Octra_net.Oce1.put_hash32 buf request.model_root;
    Octra_net.Oce1.put_hash32 buf request.program_root;
    Octra_net.Oce1.put_hash32 buf request.executor_root;
    Octra_net.Oce1.put_string buf request.caller;
    Octra_net.Oce1.put_hash32 buf request.session_id;
    Octra_net.Oce1.put_u64 buf request.sequence;
    Octra_net.Oce1.put_hash32 buf request.input_hash;
    Octra_net.Oce1.put_u32_int buf request.input_bytes;
    Octra_net.Oce1.put_u32_int buf request.max_output_bytes)

let vote_shape (vote : vote) =
  hash32 vote.request_id
  && bounded_non_empty max_node_id_bytes vote.node_id
  && hash32 vote.output_hash
  && hash32 vote.trace_root
  && vote.output_bytes > 0
  && vote.output_bytes <= max_response_bytes
  && vote.steps > 0L
  && vote.operations > 0L
  && signature64 vote.signature

let vote_sign_bytes vote =
  if not (vote_shape vote) then
    invalid_arg "resource compute vote";
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_vote" (fun buf ->
    Octra_net.Oce1.put_hash32 buf vote.request_id;
    Octra_net.Oce1.put_string buf vote.node_id;
    Octra_net.Oce1.put_hash32 buf vote.output_hash;
    Octra_net.Oce1.put_hash32 buf vote.trace_root;
    Octra_net.Oce1.put_u32_int buf vote.output_bytes;
    Octra_net.Oce1.put_u64 buf vote.steps;
    Octra_net.Oce1.put_u64 buf vote.operations)

let vote_id vote =
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_vote_id" (fun buf ->
    Octra_net.Oce1.put_hash32 buf (vote_sign_bytes vote);
    Octra_net.Oce1.put_sig64 buf vote.signature)

let protected_bool evaluate =
  try evaluate () with _ -> false

let protected_weight weight_of node_id =
  try
    match weight_of node_id with
    | Some weight when Z.gt weight Z.zero -> Some weight
    | _ -> None
  with _ -> None

let add_group key value groups =
  ResultMap.update
    key
    (function
     | None -> Some [value]
     | Some values -> Some (value :: values))
    groups

let group_by_node (values : vote list) =
  List.fold_left
    (fun groups (value : vote) ->
      NodeMap.update
        value.node_id
        (function
         | None -> Some [value]
         | Some values -> Some (value :: values))
        groups)
    NodeMap.empty
    values

let unique_votes (values : vote list) =
  List.fold_left
    (fun votes value -> NodeMap.add (vote_id value) value votes)
    NodeMap.empty
    values
  |> NodeMap.bindings
  |> List.map snd

let unique_strings values =
  List.fold_left (fun values value -> NodeMap.add value () values) NodeMap.empty values

let result_key (vote : vote) =
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_result" (fun buf ->
    Octra_net.Oce1.put_hash32 buf vote.output_hash;
    Octra_net.Oce1.put_hash32 buf vote.trace_root;
    Octra_net.Oce1.put_u32_int buf vote.output_bytes;
    Octra_net.Oce1.put_u64 buf vote.steps;
    Octra_net.Oce1.put_u64 buf vote.operations)

let compare_weighted_vote ((left : vote), _) ((right : vote), _) =
  String.compare left.node_id right.node_id

let signed_weight values =
  List.fold_left (fun total (_, weight) -> Z.add total weight) Z.zero values

let certificate_root request_id (vote : vote) (signers : signer list) signed_weight =
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_certificate" (fun buf ->
    Octra_net.Oce1.put_hash32 buf request_id;
    Octra_net.Oce1.put_hash32 buf vote.output_hash;
    Octra_net.Oce1.put_hash32 buf vote.trace_root;
    Octra_net.Oce1.put_u32_int buf vote.output_bytes;
    Octra_net.Oce1.put_u64 buf vote.steps;
    Octra_net.Oce1.put_u64 buf vote.operations;
    Octra_net.Oce1.put_string buf (Z.to_string signed_weight);
    Octra_net.Oce1.put_list
      (fun buf signer ->
        Octra_net.Oce1.put_string buf signer.node_id;
        Octra_net.Oce1.put_string buf (Z.to_string signer.weight);
        Octra_net.Oce1.put_hash32 buf signer.vote_id)
      buf
      signers)

let certificate request_id (values : (vote * Z.t) list) =
  let values = List.sort compare_weighted_vote values in
  match values with
  | [] -> invalid_arg "resource compute certificate votes"
  | (vote, _) :: _ ->
    let signers =
      List.map
        (fun ((value : vote), weight) ->
          { node_id = value.node_id; weight; vote_id = vote_id value })
        values in
    let signed_weight = signed_weight values in
    {
      request_id;
      output_hash = vote.output_hash;
      trace_root = vote.trace_root;
      output_bytes = vote.output_bytes;
      steps = vote.steps;
      operations = vote.operations;
      votes = List.map fst values;
      signers;
      signed_weight;
      root = certificate_root request_id vote signers signed_weight;
    }

let validate_request ~chain_id ~current_epoch ~max_ttl request =
  if not (request_shape request) then Error Request_malformed
  else if request.chain_id <> chain_id then Error Chain_mismatch
  else if
    max_ttl <= 0L
    || current_epoch < request.epoch_id
    || current_epoch > request.expires_epoch
    || Int64.sub request.expires_epoch request.epoch_id > max_ttl
  then
    Error Epoch_mismatch
  else
    Ok ()

let decide
    ~chain_id
    ~current_epoch
    ~max_ttl
    ~selected
    ~signer_floor
    ~weight_floor
    ~weight_of
    ~verify_signature
    request
    votes =
  match validate_request ~chain_id ~current_epoch ~max_ttl request with
  | Error reject -> Rejected reject
  | Ok () ->
    let selected_set = unique_strings selected in
    if
      selected = []
      || List.length selected > max_selected
      || NodeMap.cardinal selected_set <> List.length selected
      || NodeMap.exists (fun node_id () -> not (bounded_non_empty max_node_id_bytes node_id)) selected_set
    then
      Rejected Selection_malformed
    else if
      signer_floor <= 0
      || signer_floor > List.length selected
      || not (Z.gt weight_floor Z.zero)
    then
      Rejected Threshold_malformed
    else if List.length votes > max_votes then
      Rejected Selection_malformed
    else
      let expected_request_id = request_id request in
      let valid_votes =
        List.filter
          (fun vote ->
            vote_shape vote
            && vote.request_id = expected_request_id
            && vote.output_bytes <= request.max_output_bytes
            && NodeMap.mem vote.node_id selected_set
            && protected_bool (fun () -> verify_signature vote))
          votes in
      let weighted_votes =
        NodeMap.fold
          (fun node_id node_votes values ->
            match unique_votes node_votes, protected_weight weight_of node_id with
            | [vote], Some weight -> (vote, weight) :: values
            | _ -> values)
          (group_by_node valid_votes)
          [] in
      let groups =
        List.fold_left
          (fun groups (((vote : vote), _) as value) ->
            add_group (result_key vote) value groups)
          ResultMap.empty
          weighted_votes in
      let qualified =
        ResultMap.fold
          (fun _ values accepted ->
            if
              List.length values >= signer_floor
              && Z.geq (signed_weight values) weight_floor
            then
              values :: accepted
            else
              accepted)
          groups
          [] in
      match qualified with
      | [] -> Insufficient
      | [values] -> Agreed (certificate expected_request_id values)
      | _ -> Conflicting