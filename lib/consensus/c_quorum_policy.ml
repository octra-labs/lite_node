(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type activation = {
  anchor_epoch : int;
  anchor_state_root : string;
  activation_epoch : int;
}

type rule = Prior | Capped | Stake

type stake = { epoch : int64; seatless : int64 option }

type t = { count_epoch : int64; stake : stake option; round_epoch : int64 option }

let with_round_epoch policy epoch =
  if Option.fold ~none:false ~some:(fun round ->
    match policy.stake with None -> true | Some stake -> round < stake.epoch) epoch then
    invalid_arg "round activation order";
  {policy with round_epoch = epoch}

let with_seatless_epoch policy epoch =
  match policy.stake, epoch with
  | None, None -> policy
  | None, Some _ -> invalid_arg "seatless activation requires stake"
  | Some stake, Some epoch when epoch < stake.epoch ->
    invalid_arg "seatless activation order"
  | Some stake, seatless -> {policy with stake = Some {stake with seatless}}

let make ~count_epoch ~stake_epoch ~round_epoch =
  if count_epoch < 0L || Option.fold ~none:false
    ~some:(fun epoch -> epoch <= count_epoch) stake_epoch then
    invalid_arg "quorum activation order";
  let stake = Option.map (fun epoch -> {epoch; seatless = None}) stake_epoch in
  with_round_epoch {count_epoch; stake; round_epoch = None} round_epoch

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

let seatless_activation_for_chain chain_id : activation option =
  if String.equal chain_id devnet_chain_id then Some {
    anchor_epoch = 1_829_338;
    anchor_state_root = "621bc1164a85fc654cc8aeb34e8b23e8146edbc8448c4fbba5e8f9debcdd70af";
    activation_epoch = 1_845_000;
  } else None

let for_chain chain_id =
  make ~count_epoch:(count_epoch chain_id) ~stake_epoch:(linear_epoch ~chain_id)
    ~round_epoch:(round_epoch ~chain_id)
  |> fun policy -> with_seatless_epoch policy
    (Option.map (fun value -> Int64.of_int value.activation_epoch)
      (seatless_activation_for_chain chain_id))

let select policy epoch_id =
  rule_at ~count_epoch:policy.count_epoch
    ~stake_epoch:(Option.map (fun stake -> stake.epoch) policy.stake) ~epoch_id

let seatless policy epoch_id =
  Option.fold ~none:false ~some:(fun stake ->
    Option.fold ~none:false ~some:(fun epoch -> epoch_id >= epoch) stake.seatless) policy.stake

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
  match policy.stake with
  | Some stake when select policy epoch_id = Stake ->
    let prior = prior ^ ":stake:" ^ Int64.to_string stake.epoch in
    begin match stake.seatless with
    | Some epoch when epoch_id >= epoch ->
      prior ^ ":seatless:" ^ Int64.to_string epoch ^ ":single_seat_survival"
    | _ -> prior
    end
  | _ -> prior

let consensus_id_at ~chain_id ~epoch_id = id_at (for_chain chain_id) ~chain_id ~epoch_id

let epochs policy = List.sort_uniq Int64.compare
  ((match policy.stake with None -> [] | Some stake -> stake.epoch :: Option.to_list stake.seatless)
    @ Option.to_list policy.round_epoch)

let linear_weight ~chain_id ~epoch_id =
  rule ~chain_id ~epoch_id = Stake

let rewind policy ~from_epoch ~to_epoch =
  not (List.exists (fun epoch -> from_epoch >= epoch && to_epoch < epoch)
    (policy.count_epoch :: epochs policy))

let rewind_allowed ~chain_id ~from_epoch ~to_epoch =
  rewind (for_chain chain_id) ~from_epoch ~to_epoch