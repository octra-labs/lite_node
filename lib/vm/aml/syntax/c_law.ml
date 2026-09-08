(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  rel : C_idx.rel;
  left : C_idx.t;
  right : C_idx.t;
}

type set = t list

type view = {
  rel : C_idx.rel;
  left : C_idx.t;
  right : C_idx.t;
}

type error =
  | Count of int * int
  | Index of C_idx.error

let make rel left right : t = { rel; left; right }
let view (item : t) : view =
  { rel = item.rel; left = item.left; right = item.right }
let empty = []
let items value = value

let set items =
  let count = List.length items in
  if count > C_check.max_inputs then
    Error (Count (C_check.max_inputs, count))
  else Ok items

let hold env (items : set) =
  let rec walk (items : t list) =
    match items with
    | [] -> Ok ()
    | item :: rest ->
        begin
          match C_idx.hold env item.rel item.left item.right with
          | Ok () -> walk rest
          | Error error -> Error (Index error)
        end
  in
  walk items

let text = function
  | Count (limit, actual) ->
      Printf.sprintf "form law count limit = %d actual = %d" limit actual
  | Index error -> C_idx.text error