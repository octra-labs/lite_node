(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type split = {
  burned : Z.t;
  rewarded : Z.t;
}

val burn_numerator : Z.t
val burn_denominator : Z.t
val consensus_id : string

type market
type reserve

type payment = {
  charged : Z.t;
  refund : Z.t;
}

val market : target:Z.t -> capacity:Z.t -> speed:Z.t -> floor:Z.t ->
  ceiling:Z.t -> (market, string) result
val advance : market -> price:Z.t -> used:Z.t -> (Z.t, string) result
val reserve : price:Z.t -> work:Z.t -> cap:Z.t -> (reserve, string) result
val settle : reserve -> used:Z.t -> (payment, string) result

val split : active:bool -> Z.t -> (split, string) result