(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

type callbacks = {
  find_finalized : int -> Octra_consensus.C_types.finalize option;
  find_finalized_with_set :
    int ->
    (Octra_consensus.C_types.finalize
     * Octra_consensus.C_types.validator_set) option;
  has_finalized : int -> bool;
  store_finalized : epoch:int -> Octra_consensus.C_types.finalize -> unit;
  store_finalized_with_set :
    epoch:int ->
    validator_set:Octra_consensus.C_types.validator_set ->
    Octra_consensus.C_types.finalize ->
    unit;
  store_proposer : int -> Octra_core.Epochlog.proposer_info -> unit;
  store_flow_proposer : Consensus_finalized_flow.proposer_info -> unit;
  store_expected_root : epoch:int -> root:string -> unit;
  remove_finalized : epoch:int -> unit;
  remove_proposer : epoch:int -> unit;
  prune_after_epoch : int -> unit;
}

val create : unit -> t
val find_finalized : t -> int -> Octra_consensus.C_types.finalize option
val find_finalized_with_set :
  t ->
  int ->
  (Octra_consensus.C_types.finalize
   * Octra_consensus.C_types.validator_set) option
val has_finalized : t -> int -> bool
val store_finalized : t -> int -> Octra_consensus.C_types.finalize -> unit
val store_finalized_with_set :
  t ->
  int ->
  Octra_consensus.C_types.validator_set ->
  Octra_consensus.C_types.finalize ->
  unit
val remove_finalized : t -> int -> unit
val find_proposer : t -> int -> Octra_core.Epochlog.proposer_info option
val store_proposer : t -> int -> Octra_core.Epochlog.proposer_info -> unit
val store_flow_proposer : t -> Consensus_finalized_flow.proposer_info -> unit
val remove_proposer : t -> int -> unit
val prune_proposers_before : t -> int -> unit
val find_expected_root : t -> int -> string option
val store_expected_root : t -> int -> string -> unit
val remove_expected_root : t -> int -> unit
val prune_expected_roots_before : t -> int -> unit
val prune_after_epoch : t -> int -> unit
val callbacks : t -> callbacks