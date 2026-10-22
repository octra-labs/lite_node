(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

val finish :
  created:bool -> data_dir:string -> chain:string -> epoch:int ->
  store:Octra_core.Store_irmin.t -> chaindata:Octra_core.Store_chaindata.t -> t option

val consume : t -> data_dir:string -> chain:string -> epoch:int -> head:int -> bool