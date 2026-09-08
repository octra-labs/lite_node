(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t =
  | Lit of C_nat.t
  | Var of C_syn.name
  | Add of t * t
  | Mul of t * t
  | Max of t * t
  | Sub of t * t

type env = (C_syn.name * C_nat.t) list
type rel = Eq | Le

type view =
  | VLit of C_nat.t
  | VVar of C_syn.name
  | VAdd of t * t
  | VMul of t * t
  | VMax of t * t
  | VSub of t * t

type error =
  | Free of string
  | Dup of string
  | Nat of Z.t
  | Depth of int * int
  | Nodes of int * int
  | Vars of int * int
  | False of rel * C_nat.t * C_nat.t

let lit value = Option.map (fun value -> Lit value) (C_nat.make value)
let exact value = Lit value
let var name = Var name
let add left right = Add (left, right)
let mul left right = Mul (left, right)
let max left right = Max (left, right)
let sub left right = Sub (left, right)

let view = function
  | Lit value -> VLit value
  | Var name -> VVar name
  | Add (left, right) -> VAdd (left, right)
  | Mul (left, right) -> VMul (left, right)
  | Max (left, right) -> VMax (left, right)
  | Sub (left, right) -> VSub (left, right)

let empty = []
let depth_max = C_rule.local.tm_depth
let nodes_max = C_rule.local.tm_nodes
let vars_max = C_rule.local.inputs

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let env values =
  let count = List.length values in
  if count > vars_max then Error (Vars (vars_max, count))
  else
    let rec walk seen out = function
      | [] -> Ok (List.rev out)
      | (name, raw) :: rest ->
          if List.exists (C_syn.name_equal name) seen then
            Error (Dup (C_syn.name_text name))
          else
            begin
              match C_nat.make raw with
              | None -> Error (Nat raw)
              | Some value -> walk (name :: seen) ((name, value) :: out) rest
            end
    in
    walk [] [] values

let shape term =
  let rec walk nodes = function
    | [] -> Ok ()
    | (depth, _) :: _ when depth > depth_max ->
        Error (Depth (depth_max, depth))
    | _ when nodes >= nodes_max ->
        Error (Nodes (nodes_max, nodes + 1))
    | (depth, term) :: rest ->
        let next = depth + 1 in
        begin
          match term with
          | Lit _ | Var _ -> walk (nodes + 1) rest
          | Add (left, right) | Mul (left, right)
          | Max (left, right) | Sub (left, right) ->
              walk (nodes + 1) ((next, left) :: (next, right) :: rest)
        end
  in
  walk 0 [0, term]

let rec find name = function
  | [] -> None
  | (key, value) :: _ when C_syn.name_equal name key -> Some value
  | _ :: rest -> find name rest

let fit value =
  match C_nat.make value with
  | Some value -> Ok value
  | None -> Error (Nat value)

let rec run env = function
  | Lit value -> Ok value
  | Var name ->
      begin
        match find name env with
        | Some value -> Ok value
        | None -> Error (Free (C_syn.name_text name))
      end
  | Add (left, right) ->
      let* left = run env left in
      let* right = run env right in
      fit (Z.add (C_nat.to_z left) (C_nat.to_z right))
  | Mul (left, right) ->
      let* left = run env left in
      let* right = run env right in
      fit (Z.mul (C_nat.to_z left) (C_nat.to_z right))
  | Max (left, right) ->
      let* left = run env left in
      let* right = run env right in
      if C_nat.le left right then Ok right else Ok left
  | Sub (left, right) ->
      let* left = run env left in
      let* right = run env right in
      if C_nat.le left right then Ok C_nat.zero
      else
        begin
          match C_nat.sub left right with
          | Some value -> Ok value
          | None -> Error (Nat (Z.sub (C_nat.to_z left) (C_nat.to_z right)))
        end

let eval env term =
  let* () = shape term in
  run env term

let put env name term =
  if Option.is_some (find name env) then
    Error (Dup (C_syn.name_text name))
  else
    let count = List.length env + 1 in
    if count > vars_max then Error (Vars (vars_max, count))
    else
      let* value = eval env term in
      Ok ((name, value) :: env)

let pair env left right =
  let* left = eval env left in
  let* right = eval env right in
  Ok (left, right)

let eq env left right =
  let* left, right = pair env left right in
  Ok (C_nat.equal left right)

let le env left right =
  let* left, right = pair env left right in
  Ok (C_nat.le left right)

let hold env rel left right =
  let* left, right = pair env left right in
  let accepted =
    match rel with
    | Eq -> C_nat.equal left right
    | Le -> C_nat.le left right
  in
  if accepted then Ok () else Error (False (rel, left, right))

let rel_text = function Eq -> "=" | Le -> "<="

let text = function
  | Free name -> "index parameter is free = " ^ name
  | Dup name -> "duplicate index parameter = " ^ name
  | Nat value -> "index natural outside profile = " ^ Z.to_string value
  | Depth (limit, actual) ->
      Printf.sprintf "index depth limit = %d actual = %d" limit actual
  | Nodes (limit, actual) ->
      Printf.sprintf "index nodes limit = %d actual = %d" limit actual
  | Vars (limit, actual) ->
      Printf.sprintf "index parameter count limit = %d actual = %d" limit actual
  | False (rel, left, right) ->
      Printf.sprintf "index law is false relation = %s left = %s right = %s"
        (rel_text rel) (C_nat.text left) (C_nat.text right)