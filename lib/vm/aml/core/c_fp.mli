(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

val p : Z.t
val valid : Z.t -> bool
val make : Z.t -> t option
val of_z : Z.t -> t
val to_z : t -> Z.t
val equal : t -> t -> bool
val add : t -> t -> t
val mul : t -> t -> t
val text : t -> string