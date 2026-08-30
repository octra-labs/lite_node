(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

val disk : data_dir:string -> t
val memory : unit -> t
val keep : t -> C_relief.mark -> (C_relief.mark, string) result
val latest : t -> through_height:int64 -> (C_relief.mark option, string) result
val prune : t -> through_epoch:int64 -> (unit, string) result