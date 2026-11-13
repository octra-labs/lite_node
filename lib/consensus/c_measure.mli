(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type space

type ask = Quorum | Skip | Source | Pair | Ahead | Start | Promise

type gap = Few_seats | Low_weight | No_proof

type shape = {
  n : int;
  f : int;
  quorum : int;
  total_weight : Z.t;
  quorum_weight : Z.t;
}

val shape : seats:int -> total:Z.t -> shape

val quota : Z.t -> Z.t

val live : total:Z.t -> lost:Z.t -> bool

val space :
  C_quorum_policy.t -> int64 -> declared:bool -> (string * Z.t) list -> space

val ruled : C_quorum_policy.rule -> declared:bool -> (string * Z.t) list -> space

val valid : space -> bool

val minority : shape -> Z.t

val meets : space -> ask -> seats:int -> weight:Z.t -> bool

val attested : space -> required:int -> seats:int -> weight:Z.t -> bool

val replies : space -> configured:int -> int

val wave : space -> int

val fanout : space -> int

val overlap : space -> (string * Z.t) list -> (unit, gap) result

val label : space -> string

val start_text : space -> string

val promise_text : space -> string