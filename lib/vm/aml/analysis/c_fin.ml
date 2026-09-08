(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type error =
  | Nodes of Z.t
  | Depth of int

type stat = {
  nodes : int;
  depth : int;
}

module Names = Set.Make (String)

let push depth values rest =
  List.fold_left (fun out value -> (depth, value) :: out) rest values

let stat term =
  let rec walk nodes high = function
    | [] -> Ok { nodes; depth = high }
    | (depth, _) :: _ when depth > C_check.max_depth -> Error (Depth depth)
    | _ when nodes >= C_check.max_nodes -> Error (Nodes (Z.of_int (nodes + 1)))
    | (depth, term) :: rest ->
      let next = depth + 1 in
      let rest =
        match term with
        | C_syn.KUnit | C_syn.KBool _ | C_syn.KInt _ | C_syn.KBytes _
        | C_syn.Var _ -> rest
        | C_syn.KVec (_, values) | C_syn.KSeq (_, _, values) ->
          push next values rest
        | C_syn.Let (_, value, body)
        | C_syn.Pair (value, body)
        | C_syn.Add (value, body)
        | C_syn.Sub (value, body)
        | C_syn.Mul (value, body)
        | C_syn.Div (value, body)
        | C_syn.Mod (value, body)
        | C_syn.Eq (_, value, body)
        | C_syn.Cmp (_, value, body)
        | C_syn.Cat (value, body)
        | C_syn.Vcat (value, body)
        | C_syn.Step (value, body) -> (next, value) :: (next, body) :: rest
        | C_syn.If (guard, yes, no) ->
          (next, guard) :: (next, yes) :: (next, no) :: rest
        | C_syn.Unpair (pair, _, _, body) ->
          (next, pair) :: (next, body) :: rest
        | C_syn.Fst value | C_syn.Snd value
        | C_syn.Inl (value, _) | C_syn.Inr (_, value)
        | C_syn.Act (_, value) | C_syn.Neg value | C_syn.Abs value
        | C_syn.Fit (_, value) | C_syn.Wide value | C_syn.Length value
        | C_syn.Take (_, value)
        | C_syn.Drop (_, value) | C_syn.At (_, value)
        | C_syn.Uncons value | C_syn.Close value -> (next, value) :: rest
        | C_syn.Case (value, _, yes, _, no) ->
          (next, value) :: (next, yes) :: (next, no) :: rest
        | C_syn.Vfold (vector, seed, fold) ->
          (next, vector) :: (next, seed) :: (next, fold.body) :: rest
      in
      walk (nodes + 1) (max high depth) rest
  in
  walk 0 0 [0, term]

let fit nodes depth =
  if Z.gt nodes (Z.of_int C_check.max_nodes) then Error (Nodes nodes)
  else if depth > C_check.max_depth then Error (Depth depth)
  else Ok ()

let add_name names name =
  Names.add (C_syn.name_text name) names

let add_bind names (bind : C_syn.bind) =
  add_name names bind.name

let add_terms names terms =
  let push values rest = List.fold_left (fun out value -> value :: out) rest values in
  let rec walk names = function
    | [] -> names
    | C_syn.Var name :: rest -> walk (add_name names name) rest
    | term :: rest ->
      let names, rest =
        match term with
        | C_syn.KUnit | C_syn.KBool _ | C_syn.KInt _ | C_syn.KBytes _ ->
          names, rest
        | C_syn.KVec (_, values) | C_syn.KSeq (_, _, values) ->
          names, push values rest
        | C_syn.Var _ -> names, rest
        | C_syn.Let (bind, value, body) ->
          add_bind names bind, value :: body :: rest
        | C_syn.If (guard, yes, no) -> names, guard :: yes :: no :: rest
        | C_syn.Pair (left, right)
        | C_syn.Add (left, right)
        | C_syn.Sub (left, right)
        | C_syn.Mul (left, right)
        | C_syn.Div (left, right)
        | C_syn.Mod (left, right)
        | C_syn.Cat (left, right)
        | C_syn.Vcat (left, right)
        | C_syn.Step (left, right) -> names, left :: right :: rest
        | C_syn.Unpair (pair, left, right, body) ->
          add_bind (add_bind names left) right, pair :: body :: rest
        | C_syn.Fst value | C_syn.Snd value
        | C_syn.Inl (value, _) | C_syn.Inr (_, value)
        | C_syn.Act (_, value) | C_syn.Neg value | C_syn.Abs value
        | C_syn.Fit (_, value) | C_syn.Wide value | C_syn.Length value
        | C_syn.Take (_, value)
        | C_syn.Drop (_, value) | C_syn.At (_, value)
        | C_syn.Uncons value | C_syn.Close value -> names, value :: rest
        | C_syn.Case (value, left, yes, right, no) ->
          add_bind (add_bind names left) right, value :: yes :: no :: rest
        | C_syn.Eq (_, left, right)
        | C_syn.Cmp (_, left, right) -> names, left :: right :: rest
        | C_syn.Vfold (vector, seed, fold) ->
          add_bind (add_bind names fold.item) fold.state,
          vector :: seed :: fold.body :: rest
      in
      walk names rest
  in
  walk names terms

let occupied used binds terms =
  let names = List.fold_left add_name Names.empty used in
  let names = List.fold_left add_bind names binds in
  add_terms names terms

let clear_set blocked names =
  let rec walk seen = function
    | [] -> true
    | name :: rest ->
      let key = C_syn.name_text name in
      if Names.mem key blocked || Names.mem key seen then false
      else walk (Names.add key seen) rest
  in
  walk Names.empty names

let clear used binds terms name =
  clear_set (occupied used binds terms) [name]

let clearn used binds terms names =
  clear_set (occupied used binds terms) names

let picks count used binds terms =
  if count < 0 then None
  else
    let blocked = occupied used binds terms in
    let rec walk left index out =
      if left = 0 then Some (List.rev out)
      else if index = C_check.max_inputs then None
      else
        match C_nat.of_int index with
        | None -> None
        | Some index ->
          let name = C_syn.dslot index in
          if Names.mem (C_syn.name_text name) blocked then
            walk left (C_nat.to_int index + 1) out
          else walk (left - 1) (C_nat.to_int index + 1) (name :: out)
    in
    walk count 0 []

let pick used binds terms =
  match picks 1 used binds terms with
  | Some [name] -> Some name
  | _ -> None

let pickn count used binds terms =
  picks count used binds terms