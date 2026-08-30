(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type policy

type outcome =
  | Published
  | Failed

type event =
  | Finalized of int64
  | Completed of {
      epoch : int64;
      outcome : outcome;
    }

type effect =
  | Capture of int64
  | Retain of int

type t

val policy : interval:int64 -> retain:int -> (policy, string) result
val default : policy
val init : published:int64 option -> t
val published : t -> int64 option
val running : t -> int64 option
val step : policy -> t -> event -> ((t * effect list), string) result