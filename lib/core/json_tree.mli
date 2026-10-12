(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val read : string -> Yojson.Safe.t
val read_plain : string -> Yojson.Safe.t
val measure : limit:int -> Yojson.Safe.t -> int option
val write : ?sort:bool -> Yojson.Safe.t -> string