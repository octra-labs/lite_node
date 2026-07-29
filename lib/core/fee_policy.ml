(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type split = {
  burned : Z.t;
  rewarded : Z.t;
}

let burn_numerator = Z.one
let burn_denominator = Z.of_int 5
let consensus_id = "fee_burn:1:5"

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