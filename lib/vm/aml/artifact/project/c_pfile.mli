(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

type image = private {
  project : C_proj.t;
  rules : C_rule.schedule;
}

type error =
  | Header
  | Line of int
  | Character of int * int * int
  | Comment of C_comments.error
  | Roots of int * int
  | Version of string
  | Bodies of int * int
  | Source of string * C_parse.error
  | Binds of string * C_decl.error
  | Feed of string * C_feed.error
  | Rule of C_rule.error
  | Project of C_proj.error

val parse : string -> (t, error) result
val paths : t -> string list
val make : t -> string list -> (image, error) result
val text : error -> string