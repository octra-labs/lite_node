(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type typ =
  | Unit
  | Bool
  | Int
  | Num of C_type.sign * C_idx.t
  | Var of C_syn.name
  | Bytes of C_idx.t
  | Vec of C_idx.t * typ
  | Seq of C_idx.t * typ
  | Cap of Z.t
  | Pair of typ * typ
  | Result of typ * typ
  | Exact of C_syn.typ

type input = {
  name : string;
  mul : C_type.mul;
  typ : typ;
}

type error =
  | Name of string
  | Dup of string
  | Nat of Z.t
  | Depth of int * int
  | Nodes of int * int
  | Mode of string * C_type.mul
  | Count of int * int
  | Idx of C_idx.error
  | Low of C_low.error

module Names = Set.Make (String)

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let input name mul typ = { name; mul; typ }

let nat value =
  match C_nat.make value with
  | Some value -> Ok value
  | None -> Error (Nat value)

let shape typ =
  let rec walk nodes = function
    | [] -> Ok ()
    | (depth, _) :: _ when depth > C_type.max_depth ->
        Error (Depth (C_type.max_depth, depth))
    | _ when nodes >= C_type.max_nodes ->
        Error (Nodes (C_type.max_nodes, nodes + 1))
    | (depth, typ) :: rest ->
        let next = depth + 1 in
        begin
          match typ with
          | Unit | Bool | Int | Var _ | Exact _ -> walk (nodes + 1) rest
          | Num _ -> walk (nodes + 1) rest
          | Bytes _ -> walk (nodes + 1) rest
          | Cap kind ->
              let* _ = nat kind in
              walk (nodes + 1) rest
          | Vec (_, elem) -> walk (nodes + 1) ((next, elem) :: rest)
          | Seq (_, elem) -> walk (nodes + 1) ((next, elem) :: rest)
          | Pair (left, right) | Result (left, right) ->
              walk (nodes + 1) ((next, left) :: (next, right) :: rest)
        end
  in
  walk 0 [0, typ]

let rec lower env = function
  | Unit -> Ok C_syn.TUnit
  | Bool -> Ok C_syn.TBool
  | Int -> Ok C_syn.TInt
  | Num (sign, index) ->
      let* bits = Result.map_error (fun error -> Idx error) (C_idx.eval env index) in
      Ok (C_syn.TNum (sign, C_nat.to_z bits))
  | Var name -> Error (Name (C_syn.name_text name))
  | Bytes index ->
      let* len = Result.map_error (fun error -> Idx error) (C_idx.eval env index) in
      Ok (C_syn.TBytes (C_nat.to_z len))
  | Vec (index, elem) ->
      let* len = Result.map_error (fun error -> Idx error) (C_idx.eval env index) in
      let* elem = lower env elem in
      Ok (C_syn.TVec (C_nat.to_z len, elem))
  | Seq (index, elem) ->
      let* cap = Result.map_error (fun error -> Idx error) (C_idx.eval env index) in
      let* elem = lower env elem in
      Ok (C_syn.TSeq (C_nat.to_z cap, elem))
  | Cap kind ->
      let* kind = nat kind in
      Ok (C_syn.TCap (C_nat.to_z kind))
  | Pair (left, right) ->
      let* left = lower env left in
      let* right = lower env right in
      Ok (C_syn.TPair (left, right))
  | Result (ok, error) ->
      let* ok = lower env ok in
      let* error = lower env error in
      Ok (C_syn.TSum (ok, error))
  | Exact typ -> Ok typ

let typ_in env value =
  let* () = shape value in
  let* typ = lower env value in
  let* _ = Result.map_error (fun error -> Low error) (C_low.typ typ) in
  Ok typ

let typ value = typ_in C_idx.empty value

let res typ =
  let rec walk = function
    | [] -> false
    | C_syn.TUnit :: rest | C_syn.TBool :: rest | C_syn.TInt :: rest
    | C_syn.TNum _ :: rest
    | C_syn.TBytes _ :: rest -> walk rest
    | C_syn.TCap _ :: _ -> true
    | C_syn.TVec (len, _) :: rest when Z.equal len Z.zero -> walk rest
    | C_syn.TVec (_, elem) :: rest | C_syn.TSeq (_, elem) :: rest ->
        walk (elem :: rest)
    | C_syn.TPair (left, right) :: rest | C_syn.TSum (left, right) :: rest ->
        walk (left :: right :: rest)
  in
  walk [typ]

let binds_in env values =
  let count = List.length values in
  if count > C_check.max_inputs then
    Error (Count (C_check.max_inputs, count))
  else
    let rec walk names out = function
      | [] -> Ok (List.rev out)
      | value :: rest ->
          begin
            match C_syn.name value.name with
            | None -> Error (Name value.name)
            | Some _ when Names.mem value.name names -> Error (Dup value.name)
            | Some name ->
                let* typ = typ_in env value.typ in
                if res typ && value.mul <> C_type.One then
                  Error (Mode (value.name, value.mul))
                else
                  let bind = C_syn.bind name value.mul typ in
                  walk (Names.add value.name names) (bind :: out) rest
          end
    in
    walk Names.empty [] values

let binds values = binds_in C_idx.empty values

let prog_in env inputs body =
  let* inputs = binds_in env inputs in
  match C_low.prog inputs body with
  | Ok value -> Ok value
  | Error error -> Error (Low error)

let prog inputs body = prog_in C_idx.empty inputs body

let text error =
  let raw =
    match error with
    | Name name -> "invalid input name = " ^ name
    | Dup name -> "duplicate input name = " ^ name
    | Nat value -> "natural outside profile = " ^ Z.to_string value
    | Depth (limit, actual) ->
        Printf.sprintf "type depth limit = %d actual = %d" limit actual
    | Nodes (limit, actual) ->
        Printf.sprintf "type nodes limit = %d actual = %d" limit actual
    | Mode (name, mul) ->
        "resource input mode name = " ^ name ^ " mode = " ^ C_type.mul_text mul
    | Count (limit, actual) ->
        Printf.sprintf "input count limit = %d actual = %d" limit actual
    | Idx error -> C_idx.text error
    | Low error -> C_low.text error
  in
  C_text.clip raw