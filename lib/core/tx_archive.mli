(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type proof =
  | Exact
  | Signed

type t

val decode : hash:string -> json:string -> (t, string) result
val tx : t -> Transaction.t
val proof : t -> proof
val visible_json : t -> string

val verify :
  public_key_of:(string -> string option) ->
  t ->
  (unit, string) result