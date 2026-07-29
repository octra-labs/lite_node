(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = C_types.parent_commit

type expected = {
  chain_id : string;
  epoch_id : int64;
  proposal_id : string;
  state_root : string;
  validator_set_hash : string;
}

type verdict =
  | Valid
  | Invalid of string

type participant = {
  address : string;
  pubkey : string;
  weight : Z.t;
}

val canonical :
  t ->
  t

val encode :
  t ->
  string

val decode :
  string ->
  t

val hash :
  t ->
  string

val participants :
  t ->
  string list

val signed_weight :
  t ->
  Z.t option

val participant_set :
  t ->
  (participant list, string) result

val proposer :
  t ->
  (participant, string) result

val same_block :
  t ->
  t ->
  bool

val validate :
  expected ->
  t ->
  verdict