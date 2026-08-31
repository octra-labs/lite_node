(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val emission_divisor : Z.t
val emission_tail : Z.t
val proposer_numerator : Z.t
val proposer_denominator : Z.t
val compute_base : emission_remaining:Z.t -> Z.t
val split : Z.t -> ((Z.t * Z.t), string) result
val consensus_id : string