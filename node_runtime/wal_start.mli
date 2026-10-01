(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type error = { path : string; reason : string }

val exit_code : int
val check : string -> (unit, error) result
val recover : data_dir:string -> (unit -> 'a) -> ('a, error) result