(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val measure : fits:(int -> bool) -> string Seq.t -> int option
val find : string -> string -> int
val page :
  prefix:string -> after:string -> capacity:int ->
  iter:((string -> unit) -> unit) -> string list