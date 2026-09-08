(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type target =
  | Octb1
  | Ocps1

type src = private {
  path : string;
  body : string;
  deps : string list;
}

type root = private {
  path : string;
  name : string;
  feed : string;
}

type t

type part =
  | Sources
  | Roots
  | Deps

type error =
  | Count of part * int * int
  | Size of int * int
  | Path of string
  | Body of string * int * int
  | Dup_src of string
  | Dup_dep of string * string
  | Missing_dep of string * string
  | Missing_root of string
  | Dup_root of string
  | Name of string
  | Dup_name of string
  | Rule of C_rule.id

val source : path:string -> body:string -> deps:string list -> src
val root : path:string -> name:string -> root
val root_feed : path:string -> name:string -> feed:string -> root
val path_ok : string -> bool
val name_ok : string -> bool

val make :
  rule:C_rule.id -> target:target -> srcs:src list -> roots:root list ->
  (t, error) result

val srcs : t -> src list
val roots : t -> root list
val rule : t -> C_rule.id
val target : t -> target
val out : target -> root -> string
val outs : t -> string list
val text : error -> string