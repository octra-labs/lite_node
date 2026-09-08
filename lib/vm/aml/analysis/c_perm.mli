(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t =
  | Nil
  | Step
  | Close
  | Both

type op =
  | Use_step
  | Use_close

type set

type error =
  | Nat of Z.t
  | Dup of C_nat.t
  | Count of int * int
  | Cut_miss of C_nat.t
  | Miss of C_nat.t * op
  | Deny of C_nat.t * op * t
  | Gain of C_nat.t * t * t
  | Low of C_low.error
  | Check of C_check.error

val le : t -> t -> bool
val meet : t -> t -> t
val empty : set
val add : set -> Z.t -> t -> (set, error) result
val set : (Z.t * t) list -> (set, error) result
val vals : set -> (C_nat.t * t) list
val cut : set -> Z.t -> t -> (set, error) result
val check_info : set -> C_check.info -> (C_check.info, error) result
val check : set -> C_term.bind list -> C_term.t -> (C_check.info, error) result
val prog :
  set -> C_syn.bind list -> C_syn.t -> (C_low.prog * C_check.info, error) result
val text : error -> string