(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let emission_divisor = Z.of_int 18_198_732
let emission_tail = Z.of_int 10_000
let proposer_numerator = Z.of_int 7
let proposer_denominator = Z.of_int 10

let compute_base ~emission_remaining =
  if Z.leq emission_remaining Z.zero then Z.zero
  else
    let raw = Z.div emission_remaining emission_divisor in
    let reward = Z.max raw emission_tail in
    Z.min reward emission_remaining

let split total_reward =
  if Z.sign total_reward < 0 then Error "reward total is negative"
  else
    let proposer =
      Z.div
        (Z.mul total_reward proposer_numerator)
        proposer_denominator
    in
    Ok (proposer, Z.sub total_reward proposer)

let consensus_id =
  String.concat ":" [
    "reward";
    Z.to_string emission_divisor;
    Z.to_string emission_tail;
    Z.to_string proposer_numerator;
    Z.to_string proposer_denominator;
  ]