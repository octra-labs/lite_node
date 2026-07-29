(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let units_per_oct = Z.of_int 1_000_000

let max_supply = Z.mul (Z.of_int 1_000_000_000) units_per_oct

let format_balance amount =
  let units = Z.div amount units_per_oct in
  let remainder = Z.rem amount units_per_oct in
  if Z.equal remainder Z.zero then Z.to_string units
  else Printf.sprintf "%s.%06d" (Z.to_string units) (Z.to_int remainder)