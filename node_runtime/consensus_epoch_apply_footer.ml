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

type meta = {
  emission_remaining : Z.t;
  prev_supply : Z.t;
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

let replay_proposer_line ~epoch_id ~proposer_source ~proposer ~validators_sha =
  Printf.sprintf
    "event = replay_proposer epoch = %d proposer_source = %s proposer = %s validators_sha = %s"
    epoch_id
    proposer_source
    proposer
    validators_sha

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

let env ~epoch_id ~proposer_addr ~validator_pubkeys ~ready_state_root_at
    ~ready_max_lag =
  Epoch_exec.{
    chain_id = "";
    epoch_id;
    proposer_addr;
    validator_addrs = List.map fst validator_pubkeys;
    validator_pubkeys;
    prev_state_root = "";
    epoch_ts = float_of_int (epoch_id * 10);
    ready_state_root_at = Some ready_state_root_at;
    ready_max_lag;
  }