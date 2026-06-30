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
module Epochlog = Octra_core.Epochlog

type role =
  | Proposer
  | Validator
  | Proposer_validator

let role_label = function
  | Proposer -> "proposer"
  | Validator -> "validator"
  | Proposer_validator -> "proposer_validator"

let is_validator ~active_validators addr =
  List.exists (String.equal addr) active_validators

let role_of ~proposer_addr ~active_validators addr =
  match String.equal addr proposer_addr, is_validator ~active_validators addr with
  | true, true -> Proposer_validator
  | true, false -> Proposer
  | false, _ -> Validator

let amount_of ~role ~plan =
  match role with
  | Proposer -> plan.Epoch_exec.proposer_total
  | Validator -> plan.Epoch_exec.each_validator
  | Proposer_validator ->
    Z.add plan.Epoch_exec.proposer_total plan.Epoch_exec.each_validator

let recipient ~proposer_addr ~active_validators ~plan addr =
  let role = role_of ~proposer_addr ~active_validators addr in
  let amount = amount_of ~role ~plan in
  if Z.leq amount Z.zero then
    None
  else
    Some Epochlog.{
      reward_addr = addr;
      reward_role = role_label role;
      reward_amount = Z.to_string amount;
    }

let recipients ~proposer_addr ~active_validators ~plan =
  proposer_addr :: active_validators
  |> List.sort_uniq String.compare
  |> List.filter_map (recipient ~proposer_addr ~active_validators ~plan)