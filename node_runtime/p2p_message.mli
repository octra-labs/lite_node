(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type domain =
  | Legacy_epoch
  | Tx_gossip
  | Consensus
  | Resource_compute
  | Unknown of int

val legacy_epoch_broadcast : int

val consensus_types : int list

val is_consensus : int -> bool

val classify : int -> domain