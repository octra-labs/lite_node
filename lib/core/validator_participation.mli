(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type minimum =
  | Any
  | Share of {
      numerator : int;
      denominator : int;
    }

val validate : minimum -> (unit, string) result
val required : minimum -> epochs:int -> int
val admits : minimum -> epochs:int -> signed:int -> bool
val consensus_id : minimum -> string