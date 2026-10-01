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

type op = Copy | Join | Product

let cell_limit = Z.of_int 65_536
let layer_limit = Z.of_int 4_096
let word_limit = Z.of_int 1_048_576
let cell_effort = 16
let product_edges = 8
let sample_factor = 32
let memory_cell = 1024
let memory_item = 4096
let memory_word = 128

let sampling ~rows ~columns ~weight ~noise ~branches =
  if rows <= 0 || columns <= 0 || weight < 0 || weight > columns
     || noise < 0 || noise > rows || branches < product_edges then None
  else
    let words = Z.div (Z.add (Z.of_int rows) (Z.of_int 63)) (Z.of_int 64) in
    let noise = if noise < rows then noise + 1 else noise in
    let cost = Z.add
      (Z.mul (Z.of_int sample_factor) (Z.add (Z.of_int weight) (Z.of_int noise)))
      (Z.mul words (Z.of_int weight)) in
    if Z.fits_int cost then Some (Z.to_int cost) else None

let consensus_id = String.concat ":" [
  "fhe_projected_storage_sampling_traversal_child_budget";
  Z.to_string cell_limit; Z.to_string layer_limit; Z.to_string word_limit;
  string_of_int cell_effort; string_of_int product_edges;
  string_of_int base_effort; string_of_int base_layer_pairs; string_of_int edge_divisor;
  string_of_int sample_factor;
  string_of_int memory_cell; string_of_int memory_item; string_of_int memory_word;
  Fhe_memory.consensus_id;
]

let plan op ~base ~left_words ~right_words ~product_words ~sample_work ~left ~right =
  let valid shape = shape.slots > 0 && shape.layers >= 0 && shape.edges >= 0 in
  if base < 0 || not (valid left && valid right) || left.slots <> right.slots
     || left_words < 0 || right_words < 0 || product_words < 0 || sample_work < 0 then None
  else
    let slots = Z.of_int left.slots in
    let layers = Z.of_int left.layers in
    let edges = Z.of_int left.edges in
    let layers, edges = match op with
      | Copy -> layers, edges
      | Join -> Z.add layers (Z.of_int right.layers), Z.add edges (Z.of_int right.edges)
      | Product ->
        let pairs = Z.mul layers (Z.of_int right.layers) in
        Z.add pairs (Z.add layers (Z.of_int right.layers)),
        Z.add (Z.mul (Z.of_int product_edges) pairs) (Z.add edges (Z.of_int right.edges))
    in
    let cells = Z.mul slots (Z.succ (Z.add layers edges)) in
    let words = match op with
      | Copy -> Z.of_int left_words
      | Join -> Z.add (Z.of_int left_words) (Z.of_int right_words)
      | Product ->
        Z.add (Z.add (Z.of_int left_words) (Z.of_int right_words))
          (Z.mul (Z.of_int product_words)
            (Z.mul (Z.of_int product_edges) (Z.mul (Z.of_int left.layers) (Z.of_int right.layers))))
    in
    if Z.gt layers layer_limit || Z.gt cells cell_limit || Z.gt words word_limit then None
    else
      let storage_effort = Z.to_int (Z.add (Z.mul (Z.of_int cell_effort) cells) words) in
      let effort = match op with
        | Copy | Join -> Some storage_effort
        | Product when left.layers = 0 || right.layers = 0 -> Some (max base_effort storage_effort)
        | Product -> Option.map (max storage_effort) (multiplication_effort ~left ~right)
      in
      let traversal = match op with
        | Copy -> Z.zero
        | Join -> Z.mul layers (Z.succ layers)
        | Product -> Z.mul (Z.of_int 2) (Z.mul layers (Z.succ layers)) in
      let samples = match op with
        | Copy | Join -> Z.zero
        | Product -> Z.mul (Z.of_int sample_work)
            (Z.mul (Z.of_int product_edges) (Z.mul (Z.of_int left.layers) (Z.of_int right.layers))) in
      Option.bind effort (fun effort ->
        let total = Z.add (Z.of_int effort) (Z.add traversal samples) in
        let memory = Z.add (Z.of_int memory_item)
          (Z.add (Z.mul (Z.of_int memory_cell) cells)
            (Z.add (Z.mul (Z.of_int memory_item) (Z.add layers edges))
              (Z.mul (Z.of_int memory_word) words))) in
        if Z.fits_int total then Some (max 0 (Z.to_int total - base), memory) else None)

let work op ~base ~left_words ~right_words ~product_words ~sample_work ~left ~right =
  Option.map fst (plan op ~base ~left_words ~right_words ~product_words ~sample_work ~left ~right)