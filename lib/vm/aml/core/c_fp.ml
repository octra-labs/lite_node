(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = Z.t

let p = Z.sub (Z.shift_left Z.one 127) Z.one

let valid value =
  Z.sign value >= 0 && Z.lt value p

let make value =
  if valid value then Some value else None

let of_z value =
  let value = Z.erem value p in
  if Z.sign value < 0 then Z.add value p else value

let to_z value = value
let equal = Z.equal
let add left right = of_z (Z.add left right)
let mul left right = of_z (Z.mul left right)
let text = Z.to_string