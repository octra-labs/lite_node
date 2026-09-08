(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type atom =
  | Read of C_nat.t
  | Write of C_nat.t
  | Emit of C_nat.t
  | Fail of C_nat.t
  | Close of C_nat.t

type t

val valid : atom -> bool
val compare : atom -> atom -> int
val empty : t
val one : atom -> t
val add : atom -> t -> t
val union : t -> t -> t
val of_list : atom list -> t
val to_list : t -> atom list
val equal : t -> t -> bool
val subset : t -> t -> bool
val atom_text : atom -> string
val text : t -> string