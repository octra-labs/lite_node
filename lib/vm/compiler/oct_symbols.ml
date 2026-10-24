(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Oct_lang

let first key entries =
  let table = Hashtbl.create (List.length entries) in
  List.iter (fun entry ->
    let name = key entry in
    if not (Hashtbl.mem table name) then Hashtbl.add table name entry) entries;
  table

type t = {
  structs : (string, struct_def) Hashtbl.t;
  enums : (string, enum_def) Hashtbl.t;
  consts : (string, const_def) Hashtbl.t;
  state : (string, state_field) Hashtbl.t;
  events : (string, event_def) Hashtbl.t;
  errors : (string, error_def) Hashtbl.t;
  funcs : (string, func_def) Hashtbl.t;
  aliases : (int * string, string) Hashtbl.t;
}

let create structs enums consts state events errors funcs = {
  structs = first (fun value -> value.sd_name) structs;
  enums = first (fun value -> value.en_name) enums;
  consts = first (fun value -> value.c_name) consts;
  state = first (fun value -> value.sf_name) state;
  events = first (fun value -> value.ev_name) events;
  errors = first (fun value -> value.err_name) errors;
  funcs = first (fun value -> value.fn_name) funcs;
  aliases = Hashtbl.create 16;
}

let resolve symbols mode name =
  let special name = match name with
    | "epoch" -> mode land 1 <> 0
    | "epoch_time" -> mode land 2 <> 0
    | "value" -> mode land 4 <> 0
    | _ -> false in
  let rec walk active path name =
    let finish target =
      List.iter (fun name -> Hashtbl.replace symbols.aliases (mode, name) target) path;
      Ok target in
    match Hashtbl.find_opt symbols.aliases (mode, name) with
    | Some target -> finish target
    | None when Oct_scope.Names.mem name active -> Error name
    | None ->
      let next = if special name then None else
        match Hashtbl.find_opt symbols.consts name with
        | Some {c_typ; c_value = EVar next; _}
          when mode land 8 = 0 || c_typ = TVoid -> Some next
        | _ -> None in
      match next with
      | None -> finish name
      | Some next -> walk (Oct_scope.Names.add name active) (name :: path) next in
  walk Oct_scope.Names.empty [] name

let check ~metered ~functions ~consts =
  if metered && functions > Program_limits.max_functions then
    Error "Program function limit exceeded"
  else if metered && consts > Bytecode.max_consts then
    Error "Program constant limit exceeded"
  else Ok ()