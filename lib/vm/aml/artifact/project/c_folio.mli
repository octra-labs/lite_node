(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type bits = C_bin.bits

type part = private {
  path : string;
  name : string;
  octb : string;
  seal : bits;
  map : bits;
  live : bits;
}

type t = private {
  project : C_proj.t;
  epoch : Z.t;
  parts : part list;
}

type origin = private {
  src : C_proj.src;
  root : C_proj.root;
  part : part;
  input : C_feed.t option;
  spans : C_lex.span array;
  rows : C_live.slot list array;
}

type error =
  | Target
  | Rule of C_rule.error
  | Rule_id
  | Root of string
  | Feed of string * C_feed.error
  | Octb of string * C_octb.error
  | Seal of string * C_seal.error
  | Image
  | Header
  | Size of int * int
  | Epoch of Z.t
  | Form

val epoch_max : Z.t
val epoch_valid : Z.t -> bool
val part :
  path:string -> name:string -> octb:string -> seal:bits -> map:bits ->
  live:bits -> part
val form : project:C_proj.t -> epoch:Z.t -> parts:part list -> t option
val make : C_rule.schedule -> epoch:Z.t -> C_proj.t -> (t, error) result
val origins : t -> origin list option
val enc : t -> bits option
val dec : bits -> t option
val file : t -> string option
val of_file : string -> (t, error) result
val verify : string -> (t, error) result
val text : error -> string