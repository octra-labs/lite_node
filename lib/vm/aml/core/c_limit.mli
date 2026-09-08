(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type axis = Steps | Depth | Work

type t = private {
  steps : Z.t;
  depth : Z.t;
  work : Z.t;
}

val at : axis -> t -> Z.t
val valid : t -> bool
val text : t -> string
val zero : t
val one : t
val make : Z.t -> t
val make3 : steps:Z.t -> depth:Z.t -> work:Z.t -> t
val level : Z.t -> t
val effort : Z.t -> t
val used : steps:Z.t -> work:Z.t -> t
val add : t -> t -> t
val succ : t -> t
val max : t -> t -> t
val scale : Z.t -> t -> t
val le : t -> t -> bool
val equal : t -> t -> bool