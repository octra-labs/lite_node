(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let int_of_nonnegative value =
  if Z.sign value < 0 || Z.gt value (Z.of_int max_int) then None
  else Some (Z.to_int value)

let add left right =
  if left < 0 || right < 0 then None
  else int_of_nonnegative Z.(add (of_int left) (of_int right))

let product values =
  if List.exists (fun value -> value < 0) values then None
  else
    values
    |> List.fold_left (fun total value -> Z.mul total (Z.of_int value)) Z.one
    |> int_of_nonnegative

let scaled_product values ~divisor =
  if divisor <= 0 || List.exists (fun value -> value < 0) values then None
  else
    values
    |> List.fold_left (fun total value -> Z.mul total (Z.of_int value)) Z.one
    |> fun total -> Z.div total (Z.of_int divisor)
    |> int_of_nonnegative

let charge ~used ~cost ~limit =
  if used < 0 || cost < 0 || limit < 0 || used > limit || cost > limit - used then None
  else Some (used + cost)

let charge_z ~used ~cost ~limit =
  if used < 0 || limit < 0 || used > limit || Z.sign cost < 0 then None
  else
    let left = limit - used in
    if Z.gt cost (Z.of_int left) then None
    else Option.map (Int.add used) (int_of_nonnegative cost)