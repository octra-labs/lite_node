(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


module Epoch_exec = Octra_core.Epoch_exec
module Rewards = Consensus_epoch_apply_rewards

type meta = {
  emission_remaining : Z.t;
  prev_supply : Z.t;
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
  apply_footer : Epoch_exec.env -> Epoch_exec.reward_plan -> unit Lwt.t;
  log : string -> unit;
}

type node_request = {
  epoch_id : int;
  epoch_ts : float;
  proposer_addr : string;
  validator_pubkeys : (string * string) list;
  active_validators : string list;
  ready_state_root_at : int -> string option Lwt.t;
  ready_max_lag : int;
  validator_count : int;
  confirmed_fees : Z.t;
  short : string -> string;
}

type node_result = {
  meta : meta;
  plan : Epoch_exec.reward_plan;
  reward_recipients : Octra_core.Epochlog.reward_recipient list;
}

let z_of_meta = function
  | Some s ->
    (try Z.of_string s with _ -> Z.zero)
  | None -> Z.zero

let meta ~emission_remaining ~total_supply =
  {
    emission_remaining = z_of_meta emission_remaining;
    prev_supply = z_of_meta total_supply;
  }

let reward_plan ~validator_count ~confirmed_fees meta =
  Epoch_exec.build_reward_plan
    ~validator_count
    ~emission_remaining:meta.emission_remaining
    ~confirmed_fees
    ~prev_supply:meta.prev_supply

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
    "event = reward base = %s fees = %s total = %s proposer = %s proposer_total = %s each = %s validators = %d"
    (Z.to_string plan.Epoch_exec.base_reward)
    (Z.to_string fees)
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
  let meta =
    meta
      ~emission_remaining:(deps.get_meta "emission_remaining")
      ~total_supply:(deps.get_meta "total_supply")
  in
  let plan =
    reward_plan
      ~validator_count:request.validator_count
      ~confirmed_fees:request.confirmed_fees
      meta
  in
  let reward_recipients =
    Rewards.recipients
      ~proposer_addr:request.proposer_addr
      ~active_validators:request.active_validators
      ~plan
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
  let* () = deps.apply_footer footer_env plan in
  deps.log
    (reward_line
       ~fees:request.confirmed_fees
       ~proposer:request.proposer_addr
       ~short:request.short
       ~validator_count:request.validator_count
       plan);
  Lwt.return { meta; plan; reward_recipients }