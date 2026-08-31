(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type minimum =
  | Any
  | Share of {
      numerator : int;
      denominator : int;
    }

let validate = function
  | Any -> Ok ()
  | Share { numerator; denominator }
    when numerator > 0 && denominator > 0 && numerator <= denominator ->
    Ok ()
  | Share _ -> Error "validator participation share is invalid"

let required minimum ~epochs =
  if epochs <= 0 then 0
  else
    match minimum with
    | Any -> 1
    | Share { numerator; denominator } ->
      ((epochs * numerator) + denominator - 1) / denominator

let admits minimum ~epochs ~signed =
  signed >= required minimum ~epochs

let consensus_id = function
  | Any -> "any"
  | Share { numerator; denominator } ->
    String.concat ":" [
      "share";
      string_of_int numerator;
      string_of_int denominator;
    ]