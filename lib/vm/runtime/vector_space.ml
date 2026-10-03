(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type part = Array of int list | Repeat of int * part list

let rec words = function
  | Array shape ->
    Option.bind (Cost.product shape) (fun cells ->
      if cells > Sys.max_array_length then None
      else Some (if cells = 0 then Z.zero else Z.succ (Z.of_int cells)))
  | Repeat (count, parts) ->
    if count < 0 then None
    else Option.map (Z.mul (Z.of_int count)) (total parts)

and total parts =
  List.fold_left (fun sum part ->
    Option.bind sum (fun size -> Option.map (Z.add size) (words part)))
    (Some Z.zero) parts

let bytes parts =
  Option.bind (total parts) (fun size ->
    let size = Z.mul (Z.of_int 8) size in
    if Z.fits_int size then Some (Z.to_int size) else None)

let arrays count length = [Repeat (count, [Array [length]])]

let matmul m k n = [Array [m; k]; Array [k; n]; Array [m; n]]

let q16_matmul m k n =
  [Repeat (2, [Array [m; k]; Array [k; n]]); Array [m; n]]

let rope n =
  [Repeat (3, [Array [n]]); Array [n / 2]]

let attention tokens heads keys width =
  [Repeat (2, [Array [heads; width]]);
   Repeat (4, [Array [tokens; keys; width]]);
   Repeat (heads, [Repeat (tokens, [Array [width]]);
     Repeat (4, [Array [tokens]]); Array [width];
     Repeat (width, [Array [tokens]])]);
   Repeat (2, [Array [heads]]); Array [heads; width]]