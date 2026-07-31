(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let minimum_ms =
  Int64.to_int Octra_consensus.Epoch_time.interval_ms
let minimum_seconds =
  Octra_consensus.Epoch_time.interval_seconds
let maximum_ms = 30_000
let round_step_ms = 2_000

let duration_ms ~commit_round =
  let penalty =
    max 0 commit_round
    |> Int64.of_int
    |> Int64.mul (Int64.of_int round_step_ms)
  in
  Int64.add (Int64.of_int minimum_ms) penalty
  |> Int64.min (Int64.of_int maximum_ms)
  |> Int64.to_int

let duration_seconds ~commit_round =
  duration_ms ~commit_round
  |> float_of_int
  |> fun value -> value /. 1000.