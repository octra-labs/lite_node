(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type split = {
  burned : Z.t;
  rewarded : Z.t;
}

let burn_numerator = Z.one
let burn_denominator = Z.of_int 5
let consensus_id = "fee_burn:1:5"

type market = {
  target : Z.t;
  capacity : Z.t;
  speed : Z.t;
  floor : Z.t;
  ceiling : Z.t;
}

type reserve = {
  price : Z.t;
  work : Z.t;
  held : Z.t;
}

type payment = {
  charged : Z.t;
  refund : Z.t;
}

let market ~target ~capacity ~speed ~floor ~ceiling =
  if Z.sign target <= 0 || Z.lt capacity target || Z.sign speed <= 0
     || Z.sign floor <= 0 || Z.lt ceiling floor then
    Error "invalid fee policy"
  else Ok {target; capacity; speed; floor; ceiling}

let advance market ~price ~used =
  if Z.lt price market.floor || Z.gt price market.ceiling then
    Error "invalid unit price"
  else if Z.sign used < 0 || Z.gt used market.capacity then
    Error "invalid work total"
  else
    let delta = Z.sub used market.target in
    let change = Z.div
      (Z.mul price (Z.abs delta))
      (Z.mul market.target market.speed) in
    let next =
      if Z.sign delta > 0 then Z.add price (Z.max Z.one change)
      else Z.sub price change in
    Ok (Z.min market.ceiling (Z.max market.floor next))

let reserve ~price ~work ~cap =
  if Z.sign price <= 0 || Z.sign work < 0 || Z.sign cap < 0 then
    Error "invalid fee offer"
  else
    let held = Z.mul price work in
    if Z.gt held cap then Error "fee cap exceeded"
    else Ok {price; work; held}

let settle reserve ~used =
  if Z.sign used < 0 || Z.gt used reserve.work then
    Error "execution work exceeded"
  else
    let charged = Z.mul reserve.price used in
    Ok {charged; refund = Z.sub reserve.held charged}

let split ~active fees =
  if Z.sign fees < 0 then
    Error "negative confirmed fees"
  else
    let burned =
      if active then
        Z.div
          (Z.mul fees burn_numerator)
          burn_denominator
      else
        Z.zero
    in
    Ok {
      burned;
      rewarded = Z.sub fees burned;
    }