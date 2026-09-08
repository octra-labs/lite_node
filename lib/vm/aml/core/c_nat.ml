(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = int

let max = C_rule.local.nat
let str_max = C_rule.local.str
let zero = 0
let one = 1
let max_z = Z.of_int max
let byte_z = Z.of_int 255

let make value =
  if Z.sign value < 0 || Z.gt value max_z then None else Some (Z.to_int value)

let byte value =
  if Z.sign value < 0 || Z.gt value byte_z then None else Some (Z.to_int value)

let valid value = value >= 0 && value <= max
let of_int value = if valid value then Some value else None
let to_z = Z.of_int
let to_int value = value
let text = string_of_int
let equal = Int.equal
let compare = Int.compare
let lt left right = valid left && valid right && left < right
let le left right = valid left && valid right && left <= right

let add left right =
  if valid left && valid right then make (Z.add (to_z left) (to_z right))
  else None

let sub left right =
  if valid left && valid right && left >= right then
    make (Z.sub (to_z left) (to_z right))
  else None