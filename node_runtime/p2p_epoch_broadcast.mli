(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type observer_event =
  | Silent
  | Ignored of {
      epoch : int;
      tx_count : int;
      root : string;
      producer : string;
    }

val root_short : string -> string

val observer_event :
  observer:bool ->
  string ->
  (observer_event, string) result