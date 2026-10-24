(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type activation = {
  anchor_epoch : int;
  anchor_state_root : string;
  activation_epoch : int;
}

type rule = Prior | Capped | Stake

type t = { count_epoch : int64; stake_epoch : int64 option; round_epoch : int64 option }

let with_round_epoch policy epoch =
  if Option.fold ~none:false ~some:(fun round ->
    match policy.stake_epoch with None -> true | Some stake -> round < stake) epoch then
    invalid_arg "round activation order";
  {policy with round_epoch = epoch}

let make ~count_epoch ~stake_epoch ~round_epoch =
  if count_epoch < 0L || Option.fold ~none:false
    ~some:(fun epoch -> epoch <= count_epoch) stake_epoch then
    invalid_arg "quorum activation order";
  with_round_epoch {count_epoch; stake_epoch; round_epoch = None} round_epoch

let rule_at ~count_epoch ~stake_epoch ~epoch_id =
  if Int64.compare count_epoch 0L < 0
     || Option.fold ~none:false
          ~some:(fun epoch -> Int64.compare epoch count_epoch <= 0) stake_epoch
  then invalid_arg "quorum activation order";
  if Int64.compare epoch_id count_epoch < 0 then Prior
  else match stake_epoch with
    | Some epoch when Int64.compare epoch_id epoch >= 0 -> Stake
    | _ -> Capped

let rewind_at ~count_epoch ~stake_epoch ~from_epoch ~to_epoch =
  ignore (rule_at ~count_epoch ~stake_epoch ~epoch_id:from_epoch);
  let crosses epoch =
    Int64.compare from_epoch epoch >= 0 && Int64.compare to_epoch epoch < 0 in
  not (crosses count_epoch || Option.fold ~none:false ~some:crosses stake_epoch)

let devnet_chain_id = "octra-devnet-9871-cluster"

let devnet_activation = {
  anchor_epoch = 1_306_321;
  anchor_state_root =
    "c19278aec1ff53c4796aea354c9cc7845d8421fc7d6ffe971b5fecea29299a04";
  activation_epoch = 1_306_547;
}

let activation_for_chain chain_id =
  if String.equal chain_id devnet_chain_id then Some devnet_activation
  else None

let stake_activation_for_chain chain_id =
  if String.equal chain_id devnet_chain_id then Some {
    anchor_epoch = 1_692_238;
    anchor_state_root =
      "369bdf20cafef586867569d2db0a6d89f36811960e1445a52651856b00f8242a";
    activation_epoch = 1_737_333;
  } else None

let linear_epoch ~chain_id =
  Option.map (fun plan -> Int64.of_int plan.activation_epoch)
    (stake_activation_for_chain chain_id)

let round_epoch ~chain_id =
  if String.equal chain_id devnet_chain_id then Some 1_740_000L else None

let count_epoch chain_id =
  match activation_for_chain chain_id with
  | None -> 0L
  | Some activation -> Int64.of_int activation.activation_epoch

let for_chain chain_id =
  make ~count_epoch:(count_epoch chain_id) ~stake_epoch:(linear_epoch ~chain_id)
    ~round_epoch:(round_epoch ~chain_id)

let select policy epoch_id =
  rule_at ~count_epoch:policy.count_epoch ~stake_epoch:policy.stake_epoch ~epoch_id

let round_wait policy epoch_id =
  Option.fold ~none:false ~some:(fun epoch -> epoch_id >= epoch) policy.round_epoch

let round_wait_id = "quorum_timers:stored_vote_resume:durable_past_votes:checked_relief_limit:quorum_phase:signed_progress_retry:stalled_signed_step:past_precommits"

let rule ~chain_id ~epoch_id =
  select (for_chain chain_id) epoch_id

let active ~chain_id ~epoch_id = rule ~chain_id ~epoch_id <> Prior

let consensus_id ~chain_id =
  match activation_for_chain chain_id with
  | None -> "resilient:genesis"
  | Some activation ->
    String.concat ":" [
      "resilient";
      string_of_int activation.anchor_epoch;
      activation.anchor_state_root;
      string_of_int activation.activation_epoch;
    ]

let id_at policy ~chain_id ~epoch_id =
  let prior = consensus_id ~chain_id in
  match policy.stake_epoch with
  | Some epoch when select policy epoch_id = Stake ->
    prior ^ ":stake:" ^ Int64.to_string epoch
  | _ -> prior

let consensus_id_at ~chain_id ~epoch_id = id_at (for_chain chain_id) ~chain_id ~epoch_id

let epochs policy = List.sort_uniq Int64.compare
  (Option.to_list policy.stake_epoch @ Option.to_list policy.round_epoch)

let linear_weight ~chain_id ~epoch_id =
  rule ~chain_id ~epoch_id = Stake

let count_floor_required ~chain_id ~epoch_id =
  active ~chain_id ~epoch_id

let count_cap_required ~chain_id ~epoch_id =
  rule ~chain_id ~epoch_id = Capped

let rewind_allowed ~chain_id ~from_epoch ~to_epoch =
  rewind_at ~count_epoch:(count_epoch chain_id)
    ~stake_epoch:(linear_epoch ~chain_id) ~from_epoch ~to_epoch
  && not (Option.fold ~none:false
    ~some:(fun epoch -> from_epoch >= epoch && to_epoch < epoch) (round_epoch ~chain_id))