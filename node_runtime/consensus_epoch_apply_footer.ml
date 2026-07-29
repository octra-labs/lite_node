(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Epoch_exec = Octra_core.Epoch_exec
module Emission_policy = Octra_core.Emission_policy
module Emission_schedule = Octra_core.Emission_schedule
module Rewards = Consensus_epoch_apply_rewards

type meta = {
  emission_remaining : Z.t;
  prev_supply : Z.t;
  supply_retired : Z.t;
}

type trace = {
  replay : bool;
  layera : bool;
}

type validator_context = {
  active : string list;
  count : int;
  sha : string;
}

type node_deps = {
  get_meta : string -> string option;
  set_meta : string -> string -> unit Lwt.t;
  policy : Emission_policy.t;
  schedule : Emission_schedule.t;
  legacy_total : string option;
  public_supply : unit -> Z.t;
  apply_footer :
    Epoch_exec.env ->
    Consensus_reward_attribution.t ->
    Epoch_exec.reward_plan ->
    unit Lwt.t;
  log : string -> unit;
}

type node_request = {
  epoch_id : int;
  epoch_ts : float;
  proposer_addr : string;
  validator_pubkeys : (string * string) list;
  reward : Consensus_reward_attribution.t;
  ready_state_root_at : int -> string option Lwt.t;
  ready_max_lag : int;
  confirmed_fees : Z.t;
  short : string -> string;
}

type node_result = {
  meta : meta;
  plan : Epoch_exec.reward_plan;
  reward_recipients : Octra_core.Epochlog.reward_recipient list;
}

let z_of_meta key = function
  | None -> Error ("missing " ^ key)
  | Some raw ->
    try Ok (Z.of_string raw)
    with _ -> Error ("invalid " ^ key)

let meta ~supply_retired ~emission_remaining ~total_supply =
  match Emission_policy.remaining emission_remaining with
  | Error error -> Error error
  | Ok emission_remaining ->
    match z_of_meta "total_supply" total_supply with
    | Error error -> Error error
    | Ok prev_supply ->
      begin
        match supply_retired with
        | None ->
          Ok {
            emission_remaining;
            prev_supply;
            supply_retired = Z.zero;
          }
        | Some raw ->
          begin
            match z_of_meta "supply_retired" (Some raw) with
            | Error error -> Error error
            | Ok retired when Z.sign retired < 0 ->
              Error "negative retired supply"
            | Ok supply_retired ->
              Ok { emission_remaining; prev_supply; supply_retired }
          end
      end

let runtime_meta deps ~emission ~total ~retired =
  match
    Emission_policy.runtime_state
      ~policy:deps.policy
      ~public_supply:(deps.public_supply ())
      ~emission
      ~total
      ~legacy:deps.legacy_total
  with
  | Error error -> Error error
  | Ok state ->
    meta
      ~supply_retired:retired
      ~emission_remaining:(Some (Z.to_string state.Emission_policy.emission_remaining))
      ~total_supply:(Some (Z.to_string state.total_supply))

let set_missing deps key current value =
  match current with
  | Some _ -> Lwt.return_unit
  | None -> deps.set_meta key (Z.to_string value)

let persist_meta deps ~emission ~total ~force_emission plan =
  let open Lwt.Syntax in
  let* () =
    if force_emission then
      deps.set_meta
        "emission_remaining"
        (Z.to_string plan.Epoch_exec.new_emission_remaining)
    else
      set_missing
        deps
        "emission_remaining"
        emission
        plan.Epoch_exec.new_emission_remaining
  in
  let* () =
    set_missing deps "total_supply" total plan.Epoch_exec.new_total_supply
  in
  if plan.Epoch_exec.supply_tracking_active then
    deps.set_meta
      Emission_schedule.retired_key
      (Z.to_string plan.Epoch_exec.new_supply_retired)
  else
    Lwt.return_unit

let reward_plan ~validator_count ~confirmed_fees meta =
  Epoch_exec.build_reward_plan
    ~fee_burn_active:false
    ~supply_retired:meta.supply_retired
    ~validator_count
    ~emission_remaining:meta.emission_remaining
    ~confirmed_fees
    ~prev_supply:meta.prev_supply

let scheduled_reward_plan deps request ~validator_count meta =
  let headroom =
    Z.sub Octra_core.Denomination.max_supply meta.prev_supply
  in
  let binding =
    Emission_schedule.bind
      ~schedule:deps.schedule
      ~epoch_id:request.epoch_id
      ~remaining:meta.emission_remaining
      ~headroom
      ~stored_standard:(deps.get_meta Emission_schedule.standard_key)
      ~stored_activation:(deps.get_meta Emission_schedule.activation_key)
      ~stored_initial:(deps.get_meta Emission_schedule.initial_key)
      ~stored_retired:(deps.get_meta Emission_schedule.retired_key)
  in
  match binding with
  | Error error -> Error error
  | Ok binding ->
    begin
      match
        Emission_schedule.reward
          ~schedule:deps.schedule
          ~epoch_id:request.epoch_id
          binding
      with
      | Error error -> Error error
      | Ok base_reward ->
        Result.map
          (fun plan -> plan, binding)
          (match base_reward with
           | Some base_reward ->
             Epoch_exec.build_reward_plan_with_base
               ~base_reward
               ~fee_burn_active:binding.Emission_schedule.active
               ~supply_retired:binding.Emission_schedule.retired
               ~validator_count
               ~emission_remaining:binding.Emission_schedule.remaining
               ~confirmed_fees:request.confirmed_fees
               ~prev_supply:meta.prev_supply
           | None ->
             Epoch_exec.build_reward_plan
               ~fee_burn_active:binding.Emission_schedule.active
               ~supply_retired:binding.Emission_schedule.retired
               ~validator_count
               ~emission_remaining:binding.Emission_schedule.remaining
               ~confirmed_fees:request.confirmed_fees
               ~prev_supply:meta.prev_supply)
    end

let trace ~env =
  {
    replay = env "OCTRA_REPLAY_TRACE" = Some "1";
    layera = env "OCTRA_LAYERA_DIAG" = Some "1";
  }

let validators_sha ~hash ~raw_to_hex validators =
  hash "octra:replay:validators:v1" (String.concat "," validators)
  |> raw_to_hex

let validator_context ~hash ~raw_to_hex validator_pubkeys =
  let active = List.map fst validator_pubkeys in
  {
    active;
    count = List.length active;
    sha = validators_sha ~hash ~raw_to_hex active;
  }

let replay_proposer_line ~epoch_id ~proposer_source ~proposer ~validators_sha =
  Printf.sprintf
    "event = replay_proposer epoch = %d proposer_source = %s proposer = %s validators_sha = %s"
    epoch_id
    proposer_source
    proposer
    validators_sha

let emit_replay_proposer trace ~emit ~epoch_id ~proposer_source ~proposer
    ~validators_sha =
  if trace.replay then
    emit
      (replay_proposer_line
        ~epoch_id
        ~proposer_source
        ~proposer
        ~validators_sha)

let reward_line ~fees ~proposer ~short ~validator_count plan =
  Printf.sprintf
    "event = reward base = %s fees = %s fee_burn = %s fees_rewarded = %s total = %s proposer = %s proposer_total = %s each = %s validators = %d"
    (Z.to_string plan.Epoch_exec.base_reward)
    (Z.to_string fees)
    (Z.to_string plan.fees_burned)
    (Z.to_string plan.fees_rewarded)
    (Z.to_string plan.total_reward)
    (short proposer)
    (Z.to_string plan.proposer_total)
    (Z.to_string plan.each_validator)
    validator_count

let env ~epoch_id ~epoch_ts ~proposer_addr ~validator_pubkeys
    ~ready_state_root_at ~ready_max_lag =
  Epoch_exec.{
    chain_id = "";
    epoch_id;
    proposer_addr;
    validator_addrs = List.map fst validator_pubkeys;
    validator_pubkeys;
    prev_state_root = "";
    epoch_ts;
    ready_state_root_at = Some ready_state_root_at;
    ready_max_lag;
  }

let run_node deps request =
  let open Lwt.Syntax in
  let validator_count = List.length request.reward.validators in
  let emission = deps.get_meta "emission_remaining" in
  let total = deps.get_meta "total_supply" in
  let retired = deps.get_meta Emission_schedule.retired_key in
  let meta =
    match runtime_meta deps ~emission ~total ~retired with
    | Ok meta -> meta
    | Error error -> failwith error
  in
  let plan =
    match
      scheduled_reward_plan
        deps
        request
        ~validator_count
        meta
    with
    | Ok value -> value
    | Error error -> failwith error
  in
  let plan, schedule_binding = plan in
  let reward_meta =
    {
      meta with
      emission_remaining = schedule_binding.Emission_schedule.remaining;
      supply_retired = schedule_binding.Emission_schedule.retired;
    }
  in
  let footer_env =
    env
      ~epoch_id:request.epoch_id
      ~epoch_ts:request.epoch_ts
      ~proposer_addr:request.proposer_addr
      ~validator_pubkeys:request.validator_pubkeys
      ~ready_state_root_at:request.ready_state_root_at
      ~ready_max_lag:request.ready_max_lag
  in
  let reward_credits =
    match Epoch_exec.reward_credits request.reward plan with
    | Ok credits -> credits
    | Error error -> failwith error
  in
  let reward_recipients = Rewards.recipients reward_credits in
  let* () = deps.apply_footer footer_env request.reward plan in
  let* () =
    Lwt_list.iter_s
      (fun (key, value) -> deps.set_meta key value)
      schedule_binding.Emission_schedule.writes
  in
  let* () =
    persist_meta
      deps
      ~emission
      ~total
      ~force_emission:schedule_binding.Emission_schedule.activated
      plan
  in
  deps.log
    (reward_line
       ~fees:request.confirmed_fees
       ~proposer:request.reward.proposer_addr
       ~short:request.short
       ~validator_count
       plan);
  Lwt.return { meta = reward_meta; plan; reward_recipients }