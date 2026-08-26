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
  }

let make_validator_set (vals : validator_info list) =
  vals
  |> List.map (fun validator -> validator, Z.one)
  |> build_validator_set ~weighted:false

let make_weighted_validator_set entries =
  let rec validate addresses pubkeys = function
    | [] when addresses = [] -> Error "validator set is empty"
    | [] -> Ok ()
    | (validator, weight) :: rest ->
      if validator.address = "" then
        Error "validator address is empty"
      else if String.length validator.pubkey <> 32 then
        Error "validator public key must contain 32 bytes"
      else if Z.sign weight <= 0 then
        Error "validator weight must be positive"
      else if List.mem validator.address addresses then
        Error "duplicate validator address"
      else if List.mem validator.pubkey pubkeys then
        Error "duplicate validator public key"
      else
        validate
          (validator.address :: addresses)
          (validator.pubkey :: pubkeys)
          rest
  in
  match validate [] [] entries with
  | Error _ as error -> error
  | Ok () -> Ok (build_validator_set ~weighted:true entries)

let weights_of_set (vs : validator_set) =
  List.map
    (fun validator ->
      match List.assoc_opt validator.address vs.weights with
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

let resilient_at_cap ~faults cap entries =
  let effective = cap_entries cap entries in
  let total = sum_weights effective in
  let unavailable = largest_weight_sum faults effective in
  let remaining = Z.sub total unavailable in
  Z.geq remaining (quorum_weight total)

let effective_weight_cap (vs : validator_set) =
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
    if vs.f <= 0 || resilient_at_cap ~faults:vs.f maximum entries then
      maximum
    else
      let rec search lower upper =
        if Z.geq lower upper then lower
        else
          let middle =
            Z.div (Z.add (Z.add lower upper) Z.one) (Z.of_int 2)
          in
          if resilient_at_cap ~faults:vs.f middle entries then
            search middle upper
          else
            search lower (Z.sub middle Z.one)
      in
      search Z.one maximum

let resilient_validator_set (vs : validator_set) =
  if not vs.weighted || vs.n < 4 then vs
  else
    let entries = weights_of_set vs in
    let cap = effective_weight_cap vs in
    build_validator_set ~weighted:true (cap_entries cap entries)

let validator_set_for_epoch ~chain_id ~epoch_id vs =
  if C_quorum_policy.weight_cap_required ~chain_id ~epoch_id then
    resilient_validator_set vs
  else
    vs

let pubkey_of_addr (vs : validator_set) (addr : string) : string option =
  match List.find_opt (fun v -> v.address = addr) vs.validators with
  | Some v -> Some v.pubkey
  | None -> None

let is_validator (vs : validator_set) (addr : string) : bool =
  List.exists (fun v -> v.address = addr) vs.validators

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

let has_quorum_at ~chain_id ~epoch_id vs validators =
  let effective = validator_set_for_epoch ~chain_id ~epoch_id vs in
  if C_quorum_policy.count_floor_required ~chain_id ~epoch_id then
    has_quorum effective validators
  else
    has_weight_quorum effective validators

let quorum_reached_at ~chain_id ~epoch_id vs ~signer_count ~signed_weight =
  Z.geq signed_weight vs.quorum_weight
  &&
  (not (C_quorum_policy.count_floor_required ~chain_id ~epoch_id)
   || signer_count >= vs.quorum)

let round_skip_weight vs =
  Z.(add (sub vs.total_weight vs.quorum_weight) one)

let round_skip_reached_at ~chain_id ~epoch_id vs ~signer_count ~signed_weight =
  Z.geq signed_weight (round_skip_weight vs)
  &&
  (not (C_quorum_policy.count_floor_required ~chain_id ~epoch_id)
   || signer_count >= vs.f + 1)

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
     | Some _ -> is_validator validator_set proposal.header.creator_addr