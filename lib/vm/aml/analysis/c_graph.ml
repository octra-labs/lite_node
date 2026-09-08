(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type sign = Pos | Neg

type rule =
  | Base
  | Prod of C_nat.t * C_nat.t

type layer = {
  tag : C_nat.t;
  rule : rule;
}

type edge = {
  layer : C_nat.t;
  index : C_nat.t;
  sign : sign;
  weight : Z.t list;
  sigma : bool list;
}

type cfg = {
  basis : C_nat.t;
  slots : C_nat.t;
  sigma : C_nat.t;
  edge_cap : Z.t;
}

type graph = {
  layers : layer list;
  edges : edge list;
}

type info = {
  graph : graph;
  merged : bool;
  layer_count : Z.t;
  edge_count : Z.t;
  removed : Z.t;
}

type error = Cfg | Layer | Edge | Shape

let prime = Z.pred (Z.shift_left Z.one 127)

let cfg ~basis ~slots ~sigma ~edge_cap =
  if C_nat.lt C_nat.zero basis
    && C_nat.lt C_nat.zero slots
    && C_nat.lt C_nat.zero sigma
    && Z.gt edge_cap Z.zero
  then Ok { basis; slots; sigma; edge_cap }
  else Error Cfg

let base ~tag = { tag; rule = Base }
let prod ~tag ~left ~right = { tag; rule = Prod (left, right) }

let field value = Z.geq value Z.zero && Z.lt value prime

let edge ~layer ~index ~sign ~weight ~sigma =
  if C_nat.valid layer && C_nat.valid index && List.for_all field weight then
    Ok { layer; index; sign; weight; sigma }
  else Error Edge

let graph layers edges = { layers; edges }

let layer_ok id value =
  C_nat.valid value.tag
  && match value.rule with
  | Base -> true
  | Prod (left, right) -> C_nat.lt left id && C_nat.lt right id

let edge_ok config count value =
  C_nat.lt value.layer count
  && C_nat.lt value.index config.basis
  && List.length value.weight = C_nat.to_int config.slots
  && List.length value.sigma = C_nat.to_int config.sigma
  && List.for_all field value.weight

let valid config value =
  match cfg ~basis:config.basis ~slots:config.slots ~sigma:config.sigma
      ~edge_cap:config.edge_cap with
  | Error _ -> false
  | Ok _ ->
      begin
        match C_nat.of_int (List.length value.layers) with
        | None -> false
        | Some count ->
            let rec layers id = function
              | [] -> true
              | item :: rest ->
                  layer_ok id item
                  && match C_nat.add id C_nat.one with
                  | Some next -> layers next rest
                  | None -> false
            in
            layers C_nat.zero value.layers
            && List.for_all (edge_ok config count) value.edges
      end

let mul_depth value =
  let parent id depths value =
    List.nth_opt depths (id - C_nat.to_int value - 1)
  in
  let rec walk id depths peak = function
    | [] ->
        begin
          match C_nat.of_int peak with
          | Some depth -> Ok depth
          | None -> Error Layer
        end
    | value :: rest ->
        begin
          match C_nat.of_int id with
          | None -> Error Layer
          | Some at when not (layer_ok at value) -> Error Layer
          | Some _ ->
              let depth =
                match value.rule with
                | Base -> Some 0
                | Prod (left, right) ->
                    begin
                      match parent id depths left, parent id depths right with
                      | Some lhs, Some rhs -> Some (1 + Int.max lhs rhs)
                      | _, _ -> None
                    end
              in
              begin
                match depth with
                | Some depth ->
                    walk (id + 1) (depth :: depths) (Int.max peak depth) rest
                | None -> Error Layer
              end
        end
  in
  match C_nat.of_int (List.length value.layers) with
  | Some _ -> walk 0 [] 0 value.layers
  | None -> Error Layer

let fadd left right = Z.erem (Z.add left right) prime

let rec wadd left right =
  match left, right with
  | [], [] -> []
  | x :: xs, y :: ys -> fadd x y :: wadd xs ys
  | _, _ -> []

let rec sxor left right =
  match left, right with
  | [], [] -> []
  | x :: xs, y :: ys -> Bool.equal x (not y) :: sxor xs ys
  | _, _ -> []

let nz weight sigma =
  List.exists (fun value -> not (Z.equal value Z.zero)) weight
  || List.exists Fun.id sigma

module Key = struct
  type t = C_nat.t * C_nat.t * sign

  let rank = function Pos -> 0 | Neg -> 1

  let compare (ll, li, ls) (rl, ri, rs) =
    let by_layer = C_nat.compare ll rl in
    if by_layer <> 0 then by_layer
    else
      let by_index = C_nat.compare li ri in
      if by_index <> 0 then by_index else Int.compare (rank ls) (rank rs)
end

module Cells = Map.Make (Key)

let merge values =
  let cells =
    List.fold_left (fun acc value ->
      let key = value.layer, value.index, value.sign in
      let weight, sigma =
        match Cells.find_opt key acc with
        | None -> value.weight, value.sigma
        | Some (prior_weight, prior_sigma) ->
            wadd prior_weight value.weight, sxor prior_sigma value.sigma
      in
      Cells.add key (weight, sigma) acc)
      Cells.empty values
  in
  Cells.bindings cells
  |> List.filter_map (fun ((layer, index, sign), (weight, sigma)) ->
       if nz weight sigma then Some { layer; index; sign; weight; sigma }
       else None)

let compact layers edges =
  let source = Array.of_list layers in
  let count = Array.length source in
  let live = Array.make count false in
  List.iter (fun value -> live.(C_nat.to_int value.layer) <- true) edges;
  let changed = ref true in
  while !changed do
    changed := false;
    Array.iteri (fun id value ->
      if live.(id) then
        match value.rule with
        | Base -> ()
        | Prod (left, right) ->
            let left = C_nat.to_int left in
            let right = C_nat.to_int right in
            if not live.(left) then begin
              live.(left) <- true;
              changed := true
            end;
            if not live.(right) then begin
              live.(right) <- true;
              changed := true
            end)
      source
  done;
  let ids = Array.make count (-1) in
  let next = ref 0 in
  Array.iteri (fun id keep ->
    if keep then begin
      ids.(id) <- !next;
      incr next
    end)
    live;
  let rec map_layers id acc =
    if id = count then Some (List.rev acc)
    else if not live.(id) then map_layers (id + 1) acc
    else
      let value = source.(id) in
      match value.rule with
      | Base -> map_layers (id + 1) (value :: acc)
      | Prod (left, right) ->
          let lnext = ids.(C_nat.to_int left) in
          let rnext = ids.(C_nat.to_int right) in
          if lnext < 0 || rnext < 0 then None
          else
            begin
              match C_nat.of_int lnext, C_nat.of_int rnext with
              | Some left, Some right ->
                  map_layers (id + 1)
                    ({ value with rule = Prod (left, right) } :: acc)
              | _, _ -> None
            end
  in
  let rec map_edges acc = function
    | [] -> Some (List.rev acc)
    | value :: rest ->
        let id = ids.(C_nat.to_int value.layer) in
        if id < 0 then None
        else
          begin
            match C_nat.of_int id with
            | Some layer -> map_edges ({ value with layer } :: acc) rest
            | None -> None
          end
  in
  match map_layers 0 [], map_edges [] edges with
  | Some layers, Some edges -> Some { layers; edges }
  | _, _ -> None

let norm config value =
  if not (valid config value) then Error Shape
  else
    let merged = Z.lt config.edge_cap (Z.of_int (List.length value.edges)) in
    let edges = if merged then merge value.edges else value.edges in
    match compact value.layers edges with
    | None -> Error Shape
    | Some graph when valid config graph ->
        let layer_count = List.length graph.layers in
        let edge_count = List.length graph.edges in
        Ok { graph; merged; layer_count = Z.of_int layer_count;
          edge_count = Z.of_int edge_count;
          removed = Z.of_int (List.length value.layers - layer_count) }
    | Some _ -> Error Shape

let text = function
  | Cfg -> "graph profile is invalid"
  | Layer -> "graph layer is invalid"
  | Edge -> "graph edge is invalid"
  | Shape -> "graph shape is invalid"