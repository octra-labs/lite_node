(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type key =
  | Receipt of string
  | Rejected of string
  | Address of string * string
  | Epoch of int * string
  | Metadata of string

type cell = Value of string option | Member of bool
type anchor = { epoch : int; root : string; commit_id : string }
type row = { key : key; prior : cell; next : cell }
type t = private { previous : anchor option; target : anchor; rows : row list }
type decision = Restore | Retire

val seal : previous:anchor option -> target:anchor ->
  before:(key * cell) list -> after:(key * cell) list -> (t, string) result
val decide : head:anchor option -> t -> (decision, string) result
val restore : head:anchor option -> current:(key * cell) list -> t ->
  ((key * cell) list, string) result
val encode : t -> string
val decode : string -> (t, string) result