(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t
type set

type view = {
  rel : C_idx.rel;
  left : C_idx.t;
  right : C_idx.t;
}

type error =
  | Count of int * int
  | Index of C_idx.error

val make : C_idx.rel -> C_idx.t -> C_idx.t -> t
val view : t -> view
val empty : set
val set : t list -> (set, error) result
val items : set -> t list
val hold : C_idx.env -> set -> (unit, error) result
val text : error -> string