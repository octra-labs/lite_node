(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type split = {
  burned : Z.t;
  rewarded : Z.t;
}

val burn_numerator : Z.t
val burn_denominator : Z.t
val consensus_id : string
val split : active:bool -> Z.t -> (split, string) result