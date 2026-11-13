(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C_quorum_policy = Octra_consensus.C_quorum_policy

module C_types = struct
type validator_info = Octra_consensus.C_types.validator_info = {
  address : string;
  pubkey : string;
}

type validator_set = Octra_consensus.C_types.validator_set = {
  validators : validator_info list;
  weights : (string * Z.t) list;
  n : int;
  f : int;
  quorum : int;
  total_weight : Z.t;
  quorum_weight : Z.t;
  weighted : bool;
  authors : (int64 * (validator_info * Z.t) list) option;
}

let quorum_weight total_weight =
  Z.(add (div (mul total_weight (of_int 2)) (of_int 3)) one)

let build_validator_set ~weighted entries =
  let sorted =
    List.sort
      (fun (left, _) (right, _) ->
        String.compare left.address right.address)
      entries
  in
  let validators = List.map fst sorted in
  let weights =
    List.map
      (fun (validator, weight) -> validator.address, weight)
      sorted
  in
  let total_weight =
    List.fold_left
      (fun total (_, weight) -> Z.add total weight)
      Z.zero
      sorted
  in
  let n = List.length validators in
  let f = (n - 1) / 3 in
  let quorum =
    if n <= 0 then 0
    else if n < 4 then n
    else n - f in
  let quorum_weight =
    if Z.sign total_weight <= 0 then Z.zero
    else quorum_weight total_weight
  in
  {
    validators;
    weights;
    n;
    f;
    quorum;
    total_weight;
    quorum_weight;
    weighted;
    authors = None;
  }

module String_map = Map.Make (String)

let weight_index (vs : validator_set) =
  List.fold_right (fun (address, weight) index -> String_map.add address weight index)
    vs.weights String_map.empty

let weights_of_set (vs : validator_set) =
  let weights = weight_index vs in
  List.map
    (fun validator ->
      match String_map.find_opt validator.address weights with
      | Some weight -> validator, weight
      | None -> invalid_arg "validator weight missing")
    vs.validators

let sum_weights entries =
  List.fold_left
    (fun total (_, weight) -> Z.add total weight)
    Z.zero
    entries

let cap_entries cap entries =
  List.map
    (fun (validator, weight) -> validator, Z.min weight cap)
    entries

let largest_weight_sum count entries =
  entries
  |> List.map snd
  |> List.sort (fun left right -> Z.compare right left)
  |> fun weights ->
     let rec sum remaining total = function
       | _ when remaining <= 0 -> total
       | [] -> total
       | weight :: rest -> sum (remaining - 1) (Z.add total weight) rest
     in
     sum count Z.zero weights

let count_live_at_cap ~faults cap entries =
  let effective = cap_entries cap entries in
  let total = sum_weights effective in
  let unavailable = largest_weight_sum faults effective in
  let remaining = Z.sub total unavailable in
  Z.geq remaining (quorum_weight total)

let count_weight_cap (vs : validator_set) =
  let entries = weights_of_set vs in
  match entries with
  | [] -> Z.zero
  | (_, first) :: rest ->
    let maximum =
      List.fold_left
        (fun current (_, weight) -> Z.max current weight)
        first
        rest
    in
    if vs.f <= 0 || count_live_at_cap ~faults:vs.f maximum entries then
      maximum
    else
      let rec search lower upper =
        if Z.geq lower upper then lower
        else
          let middle =
            Z.div (Z.add (Z.add lower upper) Z.one) (Z.of_int 2)
          in
          if count_live_at_cap ~faults:vs.f middle entries then
            search middle upper
          else
            search lower (Z.sub middle Z.one)
      in
      search Z.one maximum

let count_capped_set (vs : validator_set) =
  if not vs.weighted || vs.n < 4 then vs
  else
    let entries = weights_of_set vs in
    let cap = count_weight_cap vs in
    build_validator_set ~weighted:true (cap_entries cap entries)

let validator_set_for_rule rule vs =
  match rule with
  | C_quorum_policy.Capped -> count_capped_set vs
  | C_quorum_policy.Prior | C_quorum_policy.Stake -> vs

let validator_set_for_epoch ?policy ~chain_id ~epoch_id vs =
  let policy = Option.value policy ~default:(C_quorum_policy.for_chain chain_id) in
  let rule = C_quorum_policy.select policy epoch_id in
  let vs = match vs.authors with
    | Some (height, _) when height <> epoch_id || rule <> C_quorum_policy.Stake ->
      {vs with authors = None}
    | _ -> vs in
  validator_set_for_rule rule vs

let pubkey_of_addr (vs : validator_set) (addr : string) : string option =
  match List.find_opt (fun v -> v.address = addr) vs.validators with
  | Some v -> Some v.pubkey
  | None -> None

let weight_of_addr (vs : validator_set) addr =
  List.assoc_opt addr vs.weights

let signed_weight (vs : validator_set) validators =
  let rec loop seen total = function
    | [] -> Some total
    | validator :: rest ->
      if List.mem validator seen then
        loop seen total rest
      else
        match weight_of_addr vs validator with
        | None -> None
        | Some weight ->
          loop
            (validator :: seen)
            (Z.add total weight)
            rest
  in
  loop [] Z.zero validators

let unique_signer_count validators =
  validators
  |> List.sort_uniq String.compare
  |> List.length

let has_weight_quorum vs validators =
  match signed_weight vs validators with
  | None -> false
  | Some weight -> Z.geq weight vs.quorum_weight

let has_quorum_values vs ~signer_count ~signed_weight =
  signer_count >= vs.quorum
  && Z.geq signed_weight vs.quorum_weight

let has_quorum vs validators =
  match signed_weight vs validators with
  | None -> false
  | Some weight ->
    has_quorum_values
      vs
      ~signer_count:(unique_signer_count validators)
      ~signed_weight:weight

let weights_valid rule vs =
  rule <> C_quorum_policy.Stake || vs.weighted

let has_quorum_for_rule rule vs validators =
  let effective = validator_set_for_rule rule vs in
  weights_valid rule vs &&
  match rule with
  | C_quorum_policy.Prior -> has_weight_quorum effective validators
  | C_quorum_policy.Capped | C_quorum_policy.Stake -> has_quorum effective validators

let has_quorum_at ?policy ~chain_id ~epoch_id vs validators =
  let policy = Option.value policy ~default:(C_quorum_policy.for_chain chain_id) in
  has_quorum_for_rule (C_quorum_policy.select policy epoch_id) vs validators

let quorum_reached_at ?policy ~chain_id ~epoch_id vs ~signer_count ~signed_weight =
  let policy = Option.value policy ~default:(C_quorum_policy.for_chain chain_id) in
  weights_valid (C_quorum_policy.select policy epoch_id) vs &&
  Z.geq signed_weight vs.quorum_weight
  &&
  (C_quorum_policy.select policy epoch_id = C_quorum_policy.Prior
   || signer_count >= vs.quorum)

let round_skip_weight vs =
  Z.(add (sub vs.total_weight vs.quorum_weight) one)

let round_skip_reached_at ?policy ~chain_id ~epoch_id vs ~signer_count ~signed_weight =
  let policy = Option.value policy ~default:(C_quorum_policy.for_chain chain_id) in
  weights_valid (C_quorum_policy.select policy epoch_id) vs &&
  Z.geq signed_weight (round_skip_weight vs)
  &&
  (C_quorum_policy.select policy epoch_id = C_quorum_policy.Prior
   || signer_count >= vs.f + 1)
end

module C_driver = struct
type epoch_root_response_record = Octra_consensus.C_driver.epoch_root_response_record = {
  responder_addr : string;
  responder_head_epoch : int64;
  state_root : string option;
}

type root_weights = {
  epoch_id : int64;
  stamp : string;
  weight_required : Z.t;
  count_required : int;
  count_limit : int;
  weight : string -> Z.t option;
  agrees : count:int -> weight:Z.t -> bool;
}

module Root_addresses = Map.Make (String)

let epoch_root_consistent ~epoch_id (record : epoch_root_response_record) =
  match record.state_root with
  | None -> true
  | Some _ -> record.responder_head_epoch >= epoch_id

let epoch_root_records ~epoch_id records =
  List.fold_left (fun entries (record : epoch_root_response_record) ->
    Root_addresses.update record.responder_addr (function
      | None -> Some (Some record)
      | Some (Some prior) when prior = record -> Some (Some prior)
      | Some _ -> Some None) entries) Root_addresses.empty records
  |> Root_addresses.bindings
  |> List.filter_map snd
  |> List.filter (epoch_root_consistent ~epoch_id)

let catchup_source_agreement_reached ?(count_floor=true) validator_set
    ~signer_count ~signed_weight =
  Z.geq signed_weight (C_types.round_skip_weight validator_set)
  && (not count_floor || signer_count >= validator_set.C_types.f + 1)

let split_prefix n values =
  let rec take selected remaining count =
    if count <= 0 then List.rev selected, remaining
    else
      match remaining with
      | [] -> List.rev selected, []
      | value :: rest -> take (value :: selected) rest (count - 1)
  in
  take [] values n

let catchup_query_waves validator_set peer_ids =
  let width = max 1 (validator_set.C_types.f + 2) in
  let first, rest = split_prefix width peer_ids in
  let second, rest = split_prefix width rest in
  [first; second; rest] |> List.filter (fun wave -> wave <> [])

let finality_query_peer_ids validator_set ~epoch_id records =
  let limit = max 1 (validator_set.C_types.f + 2) in
  records
  |> List.filter (fun (record : epoch_root_response_record) ->
    Int64.compare record.responder_head_epoch epoch_id >= 0)
  |> List.filter_map (fun (record : epoch_root_response_record) ->
    match C_types.pubkey_of_addr validator_set record.responder_addr with
    | Some pubkey ->
      Some (Octra_net.P2p_handshake.node_id_of_pubkey pubkey)
    | None -> None)
  |> List.sort_uniq String.compare
  |> List.filteri (fun index _ -> index < limit)

let root_weights rule validator_set ~epoch_id ~stamp ~weight =
  {
    epoch_id;
    stamp;
    weight_required = C_types.round_skip_weight validator_set;
    count_required = validator_set.C_types.f + 1;
    count_limit = max 0 (validator_set.C_types.n - 1);
    weight;
    agrees = (fun ~count ~weight ->
      catchup_source_agreement_reached
        ~count_floor:(rule <> C_quorum_policy.Prior)
        validator_set ~signer_count:count ~signed_weight:weight);
  }

let min_peers rule ~n_validators ~quorum =
  if n_validators <= 1 then 0
  else if
    rule = C_quorum_policy.Prior
  then 1
  else max 1 (quorum - 1)
end

module C_relief = struct
let lowest_weight count target =
  target.C_types.weights
  |> List.map snd
  |> List.sort Z.compare
  |> fun weights ->
    let rec sum left total = function
      | _ when left <= 0 -> total
      | [] -> total
      | weight :: rest -> sum (left - 1) (Z.add total weight) rest
    in
    sum count Z.zero weights

let check_intersection current target =
  let shared = current.C_types.quorum + target.C_types.quorum - current.n in
  if shared <= current.f then
    Error "validator relief count intersection is unsafe"
  else
    let by_count = lowest_weight shared target in
    let by_weight =
      Z.sub
        (Z.add current.quorum_weight target.quorum_weight)
        current.total_weight
    in
    let overlap = Z.max by_count by_weight in
    let faults = Z.sub current.total_weight current.quorum_weight in
    if Z.leq overlap faults then
      Error "validator relief weight intersection is unsafe"
    else
      Ok ()
end

module Probe = struct
type majority = {
  root : string;
  count : int;
  weight : Z.t;
}

type root_quorum =
  | No_root
  | Missing_quorum of majority
  | Matching_quorum of majority
  | Mismatching_quorum of majority

type in_sync_plan =
  | Peer_root_mismatch of majority
  | Accept_genesis
  | Missing_peer_root_quorum of int
  | Accept_current_root

module Addresses = Map.Make (String)

let eligible (weights : C_driver.root_weights) responses =
  C_driver.epoch_root_records ~epoch_id:weights.epoch_id responses
  |> List.filter_map (fun (r : C_driver.epoch_root_response_record) ->
    match weights.weight r.C_driver.responder_addr with
    | Some weight when Z.sign weight > 0 -> Some (r, weight)
    | None | Some _ -> None)

let root_order a b =
  let by_weight = Z.compare b.weight a.weight in
  if by_weight <> 0 then by_weight else
  let by_count = Int.compare b.count a.count in
  if by_count <> 0 then by_count else String.compare a.root b.root

let peer_roots ~weights responses =
  eligible weights responses
  |> List.fold_left (fun roots ((r : C_driver.epoch_root_response_record), weight) ->
    match r.C_driver.state_root with
    | Some root when String.length root = 32 ->
      Addresses.update root (function
        | None -> Some {root; count = 1; weight}
        | Some prior -> Some {root; count = prior.count + 1;
            weight = Z.add prior.weight weight}) roots
    | None | Some _ -> roots) Addresses.empty
  |> Addresses.bindings
  |> List.map snd
  |> List.sort root_order

let root_agrees (weights : C_driver.root_weights) root =
  weights.agrees ~count:root.count ~weight:root.weight

let peer_root_quorum ~weights ~required ~local_root responses =
  let roots = peer_roots ~weights responses in
  let own = List.find_opt (fun root -> root.root = local_root) roots in
  let foreign = List.filter (fun root ->
    root.root <> local_root && root_agrees weights root) roots in
  match foreign, own with
  | [other], own when
      (match own with None -> true | Some own ->
        not (root_agrees weights own) && Z.gt other.weight own.weight) ->
    Mismatching_quorum other
  | [], Some own when own.count >= required -> Matching_quorum own
  | _ -> match roots with [] -> No_root | first :: _ -> Missing_quorum first

let peer_heads ~weights ~ceiling responses =
  eligible weights responses |> List.filter (fun ((r : C_driver.epoch_root_response_record), _) ->
    r.C_driver.responder_head_epoch >= -1L
    && r.responder_head_epoch <= ceiling)

let head_order replies =
  replies |> List.map (fun ((r : C_driver.epoch_root_response_record), _) -> r.responder_head_epoch)
  |> List.sort_uniq (fun a b -> Int64.compare b a)

let head_support head replies =
  List.fold_left (fun (count, weight) ((r : C_driver.epoch_root_response_record), value) ->
    if r.C_driver.responder_head_epoch >= head then count + 1, Z.add weight value
    else count, weight) (0, Z.zero) replies

let peer_ahead ~weights ~head ~ceiling responses =
  let replies = peer_heads ~weights ~ceiling responses in
  let current = snd (head_support head replies) in
  let below = Z.sub (snd (head_support (-1L) replies)) current in
  if Z.sign weights.weight_required <= 0 || Z.lt below weights.weight_required
      || Z.geq current weights.weight_required then None
  else head_order replies
    |> List.find_opt (fun target -> target < head
      && Z.geq (snd (head_support target replies)) weights.weight_required)

let in_sync_plan ~required ~has_committed_root ~genesis_attested root_quorum =
  match root_quorum with
  | Mismatching_quorum m -> Peer_root_mismatch m
  | No_root | Missing_quorum _ when genesis_attested -> Accept_genesis
  | No_root when required = 0 && has_committed_root -> Accept_current_root
  | No_root -> Missing_peer_root_quorum 0
  | Missing_quorum m -> Missing_peer_root_quorum m.count
  | Matching_quorum _ -> Accept_current_root

let state_attest_required (weights : C_driver.root_weights) ~configured =
  if weights.count_limit = 0 then 0 else
    min weights.count_limit (max weights.count_required configured)
end

module Pending = struct
type decision =
  | Confirmed_elsewhere of {
      agreed : int;
      quorum : int;
    }
  | All_peers_missing of {
      responses : int;
    }
  | Inconclusive of {
      agreed : int;
      peers : int;
      responses : int;
      quorum : int;
    }

let root_matches promised_root_raw (r : C_driver.epoch_root_response_record) =
  match r.state_root, promised_root_raw with
  | Some local, Some promised -> local = promised
  | _ -> false

let all_missing responses =
  List.for_all
    (fun (r : C_driver.epoch_root_response_record) -> r.state_root = None)
    responses

let decide ~validator_count ~peer_quorum ~promised_root_raw responses =
  let agreed =
    List.filter (root_matches promised_root_raw) responses
    |> List.length
  in
  let response_count = List.length responses in
  let peers = validator_count - 1 in
  if agreed >= peer_quorum then
    Confirmed_elsewhere { agreed; quorum = peer_quorum }
  else if response_count >= peers && all_missing responses then
    All_peers_missing { responses = response_count }
  else
    Inconclusive {
      agreed;
      peers;
      responses = response_count;
      quorum = peer_quorum;
    }

let peer_quorum ~quorum = max 0 (quorum - 1)
end