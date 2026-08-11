(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type shape = {
  slots : int;
  layers : int;
  edges : int;
}

let base_effort = 10_000
let base_layer_pairs = 4
let edge_divisor = 8

let valid shape =
  shape.slots > 0 && shape.layers > 0 && shape.edges >= 0

let multiplication_effort ~left ~right =
  if not (valid left && valid right) || left.slots <> right.slots then
    None
  else
    match
      Cost.scaled_product
        [left.layers; right.layers; left.slots; base_effort]
        ~divisor:base_layer_pairs,
      Cost.add left.edges right.edges
    with
    | Some layer_effort, Some edges ->
      begin
        match
          Cost.scaled_product
            [edges; left.slots]
            ~divisor:edge_divisor
        with
        | Some edge_effort ->
          Option.map
            (max base_effort)
            (Cost.add layer_effort edge_effort)
        | None -> None
      end
    | None, _
    | _, None -> None

let additional_effort ~left ~right =
  match multiplication_effort ~left ~right with
  | Some effort when effort >= base_effort -> Some (effort - base_effort)
  | Some _
  | None -> None