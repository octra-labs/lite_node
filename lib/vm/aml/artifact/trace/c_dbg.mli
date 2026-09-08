(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type point =
  | Pc of int
  | Line of int

type cfg

type choice =
  | Step
  | Pause
  | Limit

type session

type error =
  | Cap
  | Point
  | Form
  | Digest
  | Seen
  | Expect

val make : cap:int -> points:point list -> expect:C_emit.lit option ->
  (cfg, error) result
val cap : cfg -> int
val points : cfg -> point list
val expect : cfg -> C_emit.lit option
val decide : cfg -> skip:int option -> seen:int -> pc:int -> line:int -> choice
val check : cfg -> C_emit.lit -> bool
val lit : string -> C_emit.lit option
val lit_text : C_emit.lit -> string
val session : source:string -> code:string -> cfg:cfg -> seen:int -> trace:string ->
  (session, error) result
val source : session -> string
val code : session -> string
val config : session -> cfg
val seen : session -> int
val trace : session -> string
val encode : session -> string
val decode : string -> (session, error) result
val text : error -> string