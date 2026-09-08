(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type target =
  | Octb1
  | Ocps1

type src = {
  path : string;
  body : string;
  deps : string list;
}

type root = {
  path : string;
  name : string;
  feed : string;
}

type t = {
  srcs : src list;
  roots : root list;
  rule : C_rule.id;
  target : target;
}

type part =
  | Sources
  | Roots
  | Deps

type error =
  | Count of part * int * int
  | Size of int * int
  | Path of string
  | Body of string * int * int
  | Dup_src of string
  | Dup_dep of string * string
  | Missing_dep of string * string
  | Missing_root of string
  | Dup_root of string
  | Name of string
  | Dup_name of string
  | Rule of C_rule.id

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let source ~path ~body ~deps = { path; body; deps }
let root ~path ~name = { path; name; feed = "" }
let root_feed ~path ~name ~feed = { path; name; feed }

let alpha value =
  match value with
  | 'a'..'z' | 'A'..'Z' | '_' -> true
  | _ -> false

let digit value = value >= '0' && value <= '9'

let path_char value =
  alpha value || digit value || value = '-' || value = '.'

let name_char value = alpha value || digit value || value = '-'

let seg_ok len dots = len > 0 && not (dots && len <= 2)

let path_ok value =
  let size = String.length value in
  let rec walk index len dots =
    if index = size then seg_ok len dots
    else
      let item = value.[index] in
      if item = '/' then
        seg_ok len dots && walk (index + 1) 0 true
      else
        path_char item
        && walk (index + 1) (len + 1) (dots && item = '.')
  in
  size > 0 && size <= C_rule.local.text && walk 0 0 true

let name_ok value =
  let size = String.length value in
  let rec walk index =
    index = size || name_char value.[index] && walk (index + 1)
  in
  size > 0 && size <= C_rule.local.name && walk 0

let rec mem item = function
  | [] -> false
  | value :: rest -> String.equal item value || mem item rest

let count part limit values =
  let rec walk total = function
    | [] -> Ok total
    | _ :: _ when total = limit -> Error (Count (part, limit, limit + 1))
    | _ :: rest -> walk (total + 1) rest
  in
  walk 0 values

let add limit total value =
  let size = String.length value in
  if size > limit - total then Error (Size (limit, total + size))
  else Ok (total + size)

let rec add_strings limit total = function
  | [] -> Ok total
  | value :: rest ->
      let* total = add limit total value in
      add_strings limit total rest

let add_src limit total (value : src) =
  let* total = add limit total value.path in
  let* total = add limit total value.body in
  add_strings limit total value.deps

let rec add_srcs limit total = function
  | [] -> Ok total
  | value :: rest ->
      let* total = add_src limit total value in
      add_srcs limit total rest

let add_root limit total (value : root) =
  let* total = add limit total value.path in
  let* total = add limit total value.name in
  add limit total value.feed

let rec add_roots limit total = function
  | [] -> Ok total
  | value :: rest ->
      let* total = add_root limit total value in
      add_roots limit total rest

let deps seen owner values =
  let* _ = count Deps C_rule.local.inputs values in
  let rec walk used = function
    | [] -> Ok ()
    | value :: _ when not (path_ok value) -> Error (Path value)
    | value :: _ when mem value used -> Error (Dup_dep (owner, value))
    | value :: _ when not (mem value seen) ->
        Error (Missing_dep (owner, value))
    | value :: rest -> walk (value :: used) rest
  in
  walk [] values

let check_srcs (values : src list) =
  let rec walk seen values =
    match (values : src list) with
    | [] -> Ok (List.rev seen)
    | value :: _ when not (path_ok value.path) -> Error (Path value.path)
    | value :: _ when String.length value.body > C_rule.local.str ->
        Error
          (Body (value.path, C_rule.local.str, String.length value.body))
    | value :: _ when mem value.path seen -> Error (Dup_src value.path)
    | value :: rest ->
        let* () = deps seen value.path value.deps in
        walk (value.path :: seen) rest
  in
  walk [] values

let check_roots paths (values : root list) =
  let rec walk used names values =
    match (values : root list) with
    | [] -> Ok ()
    | value :: _ when not (mem value.path paths) ->
        Error (Missing_root value.path)
    | value :: _ when not (name_ok value.name) -> Error (Name value.name)
    | value :: _ when mem value.path used -> Error (Dup_root value.path)
    | value :: _ when mem value.name names -> Error (Dup_name value.name)
    | value :: rest ->
        walk (value.path :: used) (value.name :: names) rest
  in
  walk [] [] values

let rule_ok value =
  C_rule.version value > 0
  && C_rule.same_limits (C_rule.id_limits value) C_rule.local

let make ~rule ~target ~srcs ~roots =
  let* src_count = count Sources C_rule.local.inputs srcs in
  let* root_count = count Roots C_rule.local.inputs roots in
  if src_count = 0 then Error (Count (Sources, C_rule.local.inputs, 0))
  else if root_count = 0 then Error (Count (Roots, C_rule.local.inputs, 0))
  else
    let* total = add_srcs C_rule.local.str 0 srcs in
    let* _ = add_roots C_rule.local.str total roots in
    if not (rule_ok rule) then Error (Rule rule)
    else
      let* paths = check_srcs srcs in
      let* () = check_roots paths roots in
      Ok { srcs; roots; rule; target }

let srcs value = value.srcs
let roots value = value.roots
let rule value = value.rule
let target value = value.target

let out target (value : root) =
  value.name ^
    match target with
    | Octb1 -> ".octb"
    | Ocps1 -> ".ocps"

let outs value = List.map (out value.target) value.roots

let part_text = function
  | Sources -> "sources"
  | Roots -> "roots"
  | Deps -> "dependencies"

let text = function
  | Count (part, limit, actual) ->
      Printf.sprintf "project %s count limit = %d actual = %d"
        (part_text part) limit actual
  | Size (limit, actual) ->
      Printf.sprintf "project size limit = %d actual = %d" limit actual
  | Path path -> "project path is invalid path = " ^ path
  | Body (path, limit, actual) ->
      Printf.sprintf "project source size path = %s limit = %d actual = %d"
        path limit actual
  | Dup_src path -> "project source repeats path = " ^ path
  | Dup_dep (path, dep) ->
      "project dependency repeats path = " ^ path ^ " dependency = " ^ dep
  | Missing_dep (path, dep) ->
      "project dependency is not earlier path = " ^ path ^ " dependency = "
      ^ dep
  | Missing_root path -> "project root is absent path = " ^ path
  | Dup_root path -> "project root repeats path = " ^ path
  | Name name -> "project output name is invalid name = " ^ name
  | Dup_name name -> "project output name repeats name = " ^ name
  | Rule rule -> "project rule is unsupported id = " ^ C_rule.id_text rule