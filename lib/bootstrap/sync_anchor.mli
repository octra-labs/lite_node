(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type source =
  | Pending
  | Active
  | Relief of Octra_consensus.C_relief.mark

type step = {
  source : source;
  finalize : Octra_consensus.C_types.finalize;
  ledger_state_root : string;
  epoch_index_root : string;
  update : string;
  proof : string;
}

type t

type origin = private {
  chain_id : string;
  epoch_id : int64;
  state_root : string;
  validators : Octra_consensus.C_types.validator_set;
}

val max_steps : int

val make :
  steps:step list ->
  finalize:Octra_consensus.C_types.finalize ->
  validator_set:Octra_consensus.C_types.validator_set ->
  t

val steps : t -> step list

val step_bytes : step -> int
val encoded_size : t -> int

val restrict : Octra_consensus.C_types.validator_set ->
  Octra_consensus.C_types.finalize -> Octra_consensus.C_types.finalize option

val validator_set :
  t ->
  Octra_consensus.C_types.validator_set

val transition_base : t -> (int64 * Octra_consensus.C_types.validator_set, string) result

val relief : t -> (Octra_consensus.C_relief.mark option, string) result

val finality :
  t ->
  Octra_consensus.C_types.finalize

val encode : t -> string
val decode : string -> (t, string) result

val verify_path : ?policy:Octra_consensus.C_quorum_policy.t ->
  validator_set:Octra_consensus.C_types.validator_set -> string -> (t, string) result

val derive :
  ?policy:Octra_consensus.C_quorum_policy.t ->
  validator_set:Octra_consensus.C_types.validator_set ->
  t ->
  (Octra_consensus.C_types.validator_set, string) result

val derive_lwt :
  ?policy:Octra_consensus.C_quorum_policy.t ->
  ?cancelled:(unit -> bool) ->
  validator_set:Octra_consensus.C_types.validator_set ->
  t ->
  (Octra_consensus.C_types.validator_set, string) result Lwt.t

val compact_lwt :
  ?policy:Octra_consensus.C_quorum_policy.t ->
  ?cancelled:(unit -> bool) ->
  validator_set:Octra_consensus.C_types.validator_set ->
  t ->
  (t, string) result Lwt.t

val raw_validator_set :
  Octra_consensus.C_types.validator_set ->
  (Octra_consensus.C_types.validator_set, string) result

val encoded_validator_set :
  Octra_consensus.C_types.validator_set ->
  (Octra_consensus.C_types.validator_set, string) result

val finalize_hash : Octra_consensus.C_types.finalize -> string

val verify_finalize :
  ?policy:Octra_consensus.C_quorum_policy.t ->
  chain_id:string ->
  validator_set:Octra_consensus.C_types.validator_set ->
  Octra_consensus.C_types.finalize ->
  (unit, string) result

val verify_checkpoint :
  ?policy:Octra_consensus.C_quorum_policy.t ->
  State_sync_checkpoint.body ->
  t ->
  (unit, string) result

val verify :
  ?policy:Octra_consensus.C_quorum_policy.t ->
  validator_set:Octra_consensus.C_types.validator_set ->
  State_sync_checkpoint.body ->
  string ->
  (t, string) result

val verify_lwt :
  ?policy:Octra_consensus.C_quorum_policy.t ->
  ?cancelled:(unit -> bool) ->
  validator_set:Octra_consensus.C_types.validator_set ->
  State_sync_checkpoint.body ->
  string ->
  (t, string) result Lwt.t

val from_input : Octra_core.Compute_input.root -> origin

val verify_origin :
  ?policy:Octra_consensus.C_quorum_policy.t ->
  max_bytes:int ->
  trusted:Octra_consensus.C_types.validator_set ->
  chain_id:string -> epoch_id:int64 -> state_root:string -> set_root:string ->
  string -> (origin, string) result

val verify_origin_lwt :
  ?policy:Octra_consensus.C_quorum_policy.t ->
  ?cancelled:(unit -> bool) ->
  max_bytes:int ->
  trusted:Octra_consensus.C_types.validator_set ->
  chain_id:string -> epoch_id:int64 -> state_root:string -> set_root:string ->
  string -> (origin, string) result Lwt.t

val read_state :
  max_bytes:int -> origin:origin -> ledger_root:string -> index_root:string ->
  path:string list -> proof:string -> (string option, string) result

val read_state_lwt :
  ?cancelled:(unit -> bool) -> max_bytes:int -> origin:origin ->
  ledger_root:string -> index_root:string -> path:string list -> proof:string ->
  unit -> (string option, string) result Lwt.t