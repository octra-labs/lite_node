(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let minimum_ms = 10_000
let maximum_ms = 30_000
let round_step_ms = 2_000

let clamp_ms value =
  max minimum_ms (min maximum_ms value)

let minimum_ms_of env =
  match env "OCTRA_EPOCH_DURATION" with
  | None -> minimum_ms
  | Some raw ->
    try
      float_of_string raw
      *. 1000.
      |> Float.round
      |> int_of_float
      |> clamp_ms
    with _ ->
      minimum_ms

let minimum_seconds_of env =
  float_of_int (minimum_ms_of env) /. 1000.

let duration_ms ~minimum_ms:configured ~commit_round =
  let base = clamp_ms configured in
  let penalty =
    max 0 commit_round
    |> Int64.of_int
    |> Int64.mul (Int64.of_int round_step_ms)
  in
  Int64.add (Int64.of_int base) penalty
  |> Int64.min (Int64.of_int maximum_ms)
  |> Int64.to_int

let duration_seconds ~minimum_seconds ~commit_round =
  duration_ms
    ~minimum_ms:(int_of_float (Float.round (minimum_seconds *. 1000.)))
    ~commit_round
  |> float_of_int
  |> fun value -> value /. 1000.