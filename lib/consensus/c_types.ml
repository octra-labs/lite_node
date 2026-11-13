(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let proto_version_parent_legacy = C_protocol.parent_legacy_version
let proto_version_current = C_protocol.parent_current_version

type epoch_header = {
  proto_version : int;
  chain_id : string;
  epoch_id : int64;
  prev_state_root : string;
  tx_list_hash : string;
  receipt_root : string;
  proposed_state_root : string;
  parent_commit_hash : string;
  creator_addr : string;
  txid_hi : int64;
  ts : float;
}

type vote_type = Prevote | Precommit

type vote = {
  chain_id : string;
  epoch_id : int64;
  round : int;
  vote_type : vote_type;
  proposal_id : string;
  validator : string;
  signature : string;
}

type validator_info = {
  address : string;
  pubkey : string;
}

type validator_set = {
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

type eviction = {
  epoch : int64;
  source : validator_set;
  offenders : string list;
}

type reward_member = {
  reward_address : string;
  reward_public_key : string option;
  reward_weight : Z.t;
}

type reward_source = {
  reward_proposer_addr : string;
  reward_proposer_public_key : string option;
  reward_members : reward_member list;
}

type commit_certificate = {
  chain_id : string;
  epoch_id : int64;
  commit_round : int;
  header : epoch_header;
  proposal_id : string;
  precommits : vote list;
}

type parent_commit = {
  certificate : commit_certificate;
  validator_set : validator_set;
}

type propose = {
  chain_id : string;
  epoch_id : int64;
  round : int;
  valid_round : int option;
  header : epoch_header;
  tx_hashes : string list;
  parent_commit : parent_commit option;
  proposer : string;
  signature : string;
}

type finalize = {
  chain_id : string;
  epoch_id : int64;
  commit_round : int;
  header : epoch_header;
  proposal_id : string;
  precommits : vote list;
  parent_commit : parent_commit option;
}

type proof_kind = Zero | Bound | Range

type proof_cert = {
  task_id : string;
  result : bool;
  validator : string;
  signature : string;
}

type proof_qc = {
  task_id : string;
  result : bool;
  certs : (string * string) list;
}

type round_step = ProposeStep | PrevoteStep | PrecommitStep

type engine_state = {
  height : int64;
  round : int;
  step : round_step;
  locked_round : int;
  locked_value : epoch_header option;
  valid_round : int;
  valid_value : epoch_header option;
}

let initial_engine_state height = {
  height;
  round = 0;
  step = ProposeStep;
  locked_round = -1;
  locked_value = None;
  valid_round = -1;
  valid_value = None;
}

let certificate_of_finalize finalize = {
  chain_id = finalize.chain_id;
  epoch_id = finalize.epoch_id;
  commit_round = finalize.commit_round;
  header = finalize.header;
  proposal_id = finalize.proposal_id;
  precommits = finalize.precommits;
}

let valid_round_is_well_formed ~round = function
  | None -> true
  | Some valid_round -> valid_round >= 0 && valid_round < round

let vote_type_to_u8 = function Prevote -> 1 | Precommit -> 2
let vote_type_of_u8 = function 1 -> Prevote | 2 -> Precommit | _ -> failwith "bad vote_type"

let proof_kind_to_u8 = function Zero -> 1 | Bound -> 2 | Range -> 3
let proof_kind_of_u8 = function 1 -> Zero | 2 -> Bound | 3 -> Range | _ -> failwith "bad proof_kind"

let round_step_to_u8 = function ProposeStep -> 1 | PrevoteStep -> 2 | PrecommitStep -> 3
let round_step_of_u8 = function
  | 1 -> ProposeStep
  | 2 -> PrevoteStep
  | 3 -> PrecommitStep
  | _ -> failwith "bad round_step"

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
  let C_measure.{ n; f; quorum; total_weight; quorum_weight } =
    C_measure.shape ~seats:(List.length validators) ~total:total_weight
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

let make_validator_set (vals : validator_info list) =
  vals
  |> List.map (fun validator -> validator, Z.one)
  |> build_validator_set ~weighted:false

module String_set = Set.Make (String)
module String_map = Map.Make (String)

let make_weighted_validator_set entries =
  let rec validate addresses pubkeys = function
    | [] when String_set.is_empty addresses -> Error "validator set is empty"
    | [] -> Ok ()
    | (validator, weight) :: rest ->
      if validator.address = "" then
        Error "validator address is empty"
      else if String.length validator.pubkey <> 32 then
        Error "validator public key must contain 32 bytes"
      else if Z.sign weight <= 0 then
        Error "validator weight must be positive"
      else if String_set.mem validator.address addresses then
        Error "duplicate validator address"
      else if String_set.mem validator.pubkey pubkeys then
        Error "duplicate validator public key"
      else
        validate
          (String_set.add validator.address addresses)
          (String_set.add validator.pubkey pubkeys)
          rest
  in
  match validate String_set.empty String_set.empty entries with
  | Error _ as error -> error
  | Ok () -> Ok (build_validator_set ~weighted:true entries)

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
  C_measure.live ~total ~lost:(largest_weight_sum faults effective)

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

type set_error = Set_shape of string | Seat_cap

let set_error_message = function
  | Set_shape reason -> reason
  | Seat_cap -> "validator weight reaches blocking threshold"

let validate_validator_set_shape (vs : validator_set) =
  if not vs.weighted then Error (Set_shape "validator weights are not declared")
  else
    let ( let* ) = Result.bind in
    let rec entries members weights =
      match members, weights with
      | [], [] -> Ok []
      | member :: rest, (address, weight) :: remaining
        when member.address = address ->
        let* remaining = entries rest remaining in
        Ok ((member, weight) :: remaining)
      | _ -> Error (Set_shape "validator weight membership differs")
    in
    let members = List.sort (fun left right -> String.compare left.address right.address)
      vs.validators in
    let weights = List.sort (fun (left, _) (right, _) -> String.compare left right)
      vs.weights in
    let* entries = entries members weights in
    let build entries = Result.map_error (fun reason -> Set_shape reason)
      (make_weighted_validator_set entries) in
    let* checked = build entries in
    if vs.n <> checked.n || vs.f <> checked.f
       || vs.quorum <> checked.quorum
       || not (Z.equal vs.total_weight checked.total_weight)
       || not (Z.equal vs.quorum_weight checked.quorum_weight) then
      Error (Set_shape "validator set shape differs")
    else
      let* authors = match vs.authors with
        | None -> Ok None
        | Some (height, authors) ->
          let* authors = build authors in
          let authors = weights_of_set authors in
          let* _ = build (entries @ authors) in
          Ok (Some (height, authors)) in
      Ok {checked with authors}

let same_voting_set (left : validator_set) (right : validator_set) =
  match validate_validator_set_shape {left with authors = None},
    validate_validator_set_shape {right with authors = None} with
  | Ok left, Ok right -> left = right
  | Error _, _ | _, Error _ -> false

let structural_validator_set_for_epoch ?policy ~chain_id ~epoch_id vs =
  let policy = Option.value policy ~default:(C_quorum_policy.for_chain chain_id) in
  let effective = validator_set_for_epoch ~policy ~chain_id ~epoch_id vs in
  if not (C_quorum_policy.seatless policy epoch_id) then Ok effective
  else Result.bind (validate_validator_set_shape vs) (fun _ ->
    validate_validator_set_shape effective)

let validate_validator_set_for_epoch ?policy ~chain_id ~epoch_id vs =
  let policy = Option.value policy ~default:(C_quorum_policy.for_chain chain_id) in
  let ( let* ) = Result.bind in
  let* checked = structural_validator_set_for_epoch ~policy ~chain_id ~epoch_id vs in
  if not (C_quorum_policy.seatless policy epoch_id) then Ok checked
  else
    let shape = C_measure.shape ~seats:checked.n ~total:checked.total_weight in
    let limit = C_measure.minority shape in
    if List.exists (fun (_, weight) -> Z.geq weight limit) checked.weights then Error Seat_cap
    else Ok checked

let checked_validator_set_for_epoch ?policy ~chain_id ~epoch_id vs =
  validate_validator_set_for_epoch ?policy ~chain_id ~epoch_id vs
  |> Result.map_error set_error_message

type eviction_choice = Remove_set of validator_set * Z.t | Retain_set of Z.t

let plan_eviction ~offenders vs =
  let ( let* ) = Result.bind in
  let* checked = validate_validator_set_shape vs |> Result.map_error set_error_message in
  let unique = List.sort_uniq String.compare offenders in
  if unique = [] then Error "validator eviction requires offenders"
  else if List.length unique <> List.length offenders then Error "duplicate eviction offender"
  else if List.exists (fun address -> not (List.mem_assoc address checked.weights)) unique then
    Error "eviction offender is not an active validator"
  else
    let removed, remaining = List.partition (fun (member, _) ->
      List.mem member.address unique) (weights_of_set checked) in
    let weight = sum_weights removed in
    if not (C_measure.live ~total:checked.total_weight ~lost:weight) then
      Ok (Retain_set weight)
    else
      let* next = make_weighted_validator_set remaining in
      Ok (Remove_set (next, weight))

let evict_validator_set ~offenders vs =
  match plan_eviction ~offenders vs with
  | Ok (Remove_set (next, weight)) -> Ok (next, weight)
  | Ok (Retain_set _) -> Error "evicted weight reaches blocking threshold"
  | Error _ as error -> error

type set_choice = Active_set of validator_set | Pending_set of validator_set

let pending_set_for_epoch ?policy ~chain_id ~epoch_id ~active pending =
  let policy = Option.value policy ~default:(C_quorum_policy.for_chain chain_id) in
  let ( let* ) = Result.bind in
  let* active =
    if not (C_quorum_policy.seatless policy epoch_id) then Ok None
    else match active with
      | None -> Ok None
      | Some active ->
        structural_validator_set_for_epoch ~policy ~chain_id ~epoch_id active
        |> Result.map Option.some in
  match validate_validator_set_for_epoch ~policy ~chain_id ~epoch_id pending with
  | Ok set -> Ok (Pending_set set)
  | Error (Set_shape reason) -> Error (Set_shape reason)
  | Error Seat_cap ->
    match active with
    | None -> Error (Set_shape "active validator set is missing")
    | Some checked -> Ok (Active_set checked)

let pubkey_of_addr (vs : validator_set) (addr : string) : string option =
  match List.find_opt (fun v -> v.address = addr) vs.validators with
  | Some v -> Some v.pubkey
  | None -> None

let is_validator (vs : validator_set) (addr : string) : bool =
  List.exists (fun v -> v.address = addr) vs.validators

let weight_of_addr (vs : validator_set) addr =
  List.assoc_opt addr vs.weights

let author_entry ~chain_id ~epoch_id vs addr =
  match List.find_opt (fun v -> v.address = addr) vs.validators with
  | Some member -> Option.map (fun weight -> member, weight) (weight_of_addr vs addr)
  | None ->
    match vs.authors with
    | Some (height, entries) when height = epoch_id
      && C_quorum_policy.linear_weight ~chain_id ~epoch_id ->
      List.find_opt (fun (member, _) -> member.address = addr) entries
    | _ -> None

let is_author ~chain_id ~epoch_id vs addr =
  Option.is_some (author_entry ~chain_id ~epoch_id vs addr)

let source_set ~chain_id ~epoch_id vs =
  match vs.authors with
  | Some (height, entries) when height = epoch_id
      && C_quorum_policy.linear_weight ~chain_id ~epoch_id ->
    make_weighted_validator_set (weights_of_set vs @ entries) |> Result.to_option
  | _ -> None

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

let space_for_rule rule vs =
  C_measure.ruled rule ~declared:vs.weighted vs.weights

let space ?policy ~chain_id ~epoch_id vs =
  let policy = Option.value policy ~default:(C_quorum_policy.for_chain chain_id) in
  C_measure.space policy epoch_id ~declared:vs.weighted vs.weights

let weights_valid rule vs =
  C_measure.valid (space_for_rule rule vs)

let has_quorum_at ?policy ~chain_id ~epoch_id vs validators =
  let policy = Option.value policy ~default:(C_quorum_policy.for_chain chain_id) in
  let rule = C_quorum_policy.select policy epoch_id in
  let effective = validator_set_for_rule rule vs in
  match signed_weight effective validators with
  | None -> false
  | Some weight ->
    C_measure.meets (space ~policy ~chain_id ~epoch_id effective) C_measure.Quorum
      ~seats:(unique_signer_count validators) ~weight

let quorum_reached_at ?policy ~chain_id ~epoch_id vs ~signer_count ~signed_weight =
  C_measure.meets (space ?policy ~chain_id ~epoch_id vs) C_measure.Quorum
    ~seats:signer_count ~weight:signed_weight

let pair_at ?policy ~chain_id ~epoch_id vs validators =
  match signed_weight vs validators with
  | None -> false
  | Some weight ->
    C_measure.meets (space ?policy ~chain_id ~epoch_id vs) C_measure.Pair
      ~seats:(unique_signer_count validators) ~weight

let promise_at ?policy ~chain_id ~epoch_id vs responders =
  let weight = List.sort_uniq String.compare responders
    |> List.fold_left (fun sum address ->
      Z.add sum (Option.value (weight_of_addr vs address) ~default:Z.zero)) Z.zero in
  C_measure.meets (space ?policy ~chain_id ~epoch_id vs) C_measure.Promise
    ~seats:(List.length responders) ~weight

let round_skip_weight vs =
  C_measure.minority (C_measure.shape ~seats:vs.n ~total:vs.total_weight)

let round_skip_reached_at ?policy ~chain_id ~epoch_id vs ~signer_count ~signed_weight =
  C_measure.meets (space ?policy ~chain_id ~epoch_id vs) C_measure.Skip
    ~seats:signer_count ~weight:signed_weight

let weighted_leader (vs : validator_set) ~epoch_id ~round =
  let buffer = Buffer.create 256 in
  Octra_net.Oce1.put_string buffer "octra:weighted_leader:v1";
  Octra_net.Oce1.put_u64 buffer epoch_id;
  Octra_net.Oce1.put_u32_int buffer round;
  Octra_net.Oce1.put_list
    (fun buffer (validator : validator_info) ->
      Octra_net.Oce1.put_string buffer validator.address;
      Octra_net.Oce1.put_raw buffer validator.pubkey;
      match weight_of_addr vs validator.address with
      | None -> invalid_arg "validator weight missing"
      | Some weight ->
        Octra_net.Oce1.put_string buffer (Z.to_string weight))
    buffer
    vs.validators;
  let target =
    buffer
    |> Buffer.contents
    |> Digestif.SHA256.digest_string
    |> Digestif.SHA256.to_hex
    |> Z.of_string_base 16
    |> fun value -> Z.erem value vs.total_weight
  in
  let rec select offset = function
    | [] -> invalid_arg "validator leader selection failed"
    | validator :: rest ->
      let weight =
        match weight_of_addr vs validator.address with
        | None -> invalid_arg "validator weight missing"
        | Some weight -> weight
      in
      let next = Z.add offset weight in
      if Z.lt target next then validator
      else select next rest
  in
  select Z.zero vs.validators

let leader_of (vs : validator_set) ~epoch_id ~round =
  if vs.n <= 0 then
    invalid_arg "validator set is empty"
  else if round < 0 then
    invalid_arg "consensus round is negative"
  else if vs.weighted then
    weighted_leader vs ~epoch_id ~round
  else
    let idx = Int64.to_int (Int64.rem epoch_id (Int64.of_int vs.n)) in
    let idx = (idx + round) mod vs.n in
    List.nth vs.validators idx

let proposal_is_well_formed
    ~chain_id
    ~(validator_set : validator_set)
    (proposal : propose) =
  validator_set.n > 0
  && proposal.round >= 0
  && proposal.chain_id = chain_id
  && proposal.header.proto_version
     = C_protocol.version_for_epoch proposal.epoch_id
  && proposal.header.chain_id = proposal.chain_id
  && proposal.header.epoch_id = proposal.epoch_id
  && valid_round_is_well_formed
       ~round:proposal.round
       proposal.valid_round
  && (leader_of
        validator_set
        ~epoch_id:proposal.epoch_id
        ~round:proposal.round).address = proposal.proposer
  && match proposal.valid_round with
     | None -> proposal.header.creator_addr = proposal.proposer
     | Some _ -> is_author ~chain_id ~epoch_id:proposal.epoch_id
         validator_set proposal.header.creator_addr