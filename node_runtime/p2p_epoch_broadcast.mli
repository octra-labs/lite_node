(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  epoch : int64;
  root : string;
  declared_count : int32;
  producer : string;
  txs : Octra_core.Transaction.t list;
}

type observer_event =
  | Silent
  | Ignored of {
      epoch : int;
      tx_count : int;
      root : string;
      producer : string;
    }

val tx_count : t -> int

val root_short : string -> string

val producer_short : t -> string

val parse : string -> t

val observer_event :
  observer:bool ->
  string ->
  (observer_event, string) result