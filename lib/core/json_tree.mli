(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val read : string -> Yojson.Safe.t
val write : ?sort:bool -> Yojson.Safe.t -> string