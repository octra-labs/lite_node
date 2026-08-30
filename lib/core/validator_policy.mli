(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type active = {
  activation_epoch : int;
  parameters : Validator_admission.parameters;
  snapshot_interval : int64;
  evidence_epochs : int64;
}

type t =
  | Inactive
  | Bonded of active

val env_name : string
val standard_name : string
val min_bond : Z.t
val max_validators : int
val activation_delay : int64
val snapshot_interval : int64
val unbonding_epochs : int64
val evidence_epochs : int64
val parameters : Validator_admission.parameters
val of_env : (string -> string option) -> (t, string) result
val of_env_exn : (string -> string option) -> t
val activation_epoch : t -> int option
val consensus_id : t -> string
val lifecycle_enabled : t -> bool
val snapshot_activation : t -> source_epoch:int64 -> int64 option