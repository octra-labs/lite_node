(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type cursor = { at : int list -> int; items : int list -> int list option }

module type Graph = sig
  type t
  val mapper : ?saved:(t * 'a) array -> ((int list -> 'a) -> t -> 'a) -> t -> 'a
  val compare : (cursor -> t -> t -> int list option) -> t -> t -> bool
end

module Host : Graph with type t = Oct_lang.typ
module Core : Graph with type t = C_syn.typ