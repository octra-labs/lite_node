(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

val capture : string -> t option
val close : t -> unit
val active : t -> data_dir:string -> bool
val claim : t -> data_dir:string -> bool