(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type limits = { fhe : int; stealth : int }
type t

val create : limits -> t
val reserve : t -> Transaction.t -> t option
val select : limits:limits -> inputs:Transaction.t list ->
  ready:Transaction.t list -> Transaction.t list