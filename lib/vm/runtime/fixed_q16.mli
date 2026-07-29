(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = Z.t

val scale : t
val trunc_mul : t -> t -> t
val in_range : t -> bool
val round16 : t -> t
val make_round : int -> (t -> t) option
val exp : t -> t
val scale_floor : t -> t -> t option
val inv_sqrt : t -> t option
val ln : t -> t option
val inverse_pow : t -> t -> t option
val sin_cos : t -> t * t
val silu : t -> t option
val rope : t array -> t -> int -> t array option
val dot : t array -> int -> t array -> int -> int -> t
val map2_checked : (t -> t -> t) -> t array -> t array -> t array option
val elementwise_mul : t array -> t array -> t array option
val residual_add : t array -> t array -> t array option
val attention : t array -> t array -> t array -> int -> int -> int -> int -> t array option
val layer : t array -> t array -> t array -> t array option
val rms : t array -> t array -> t array option