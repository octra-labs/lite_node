(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Oct_lang

let rec type_name = function
  | TMap (key, item) -> "map[" ^ type_name key ^ "]" ^ type_name item
  | TList item -> "list[" ^ type_name item ^ "]"
  | TOption item -> "option[" ^ type_name item ^ "]"
  | TTuple items -> "(" ^ String.concat "," (List.map type_name items) ^ ")"
  | value -> typ_to_string value

let inputs params =
  `List (List.map (fun param -> `String (type_name param.p_typ)) params)

let names params =
  `List (List.map (fun param -> `String param.p_name) params)

let method_json fn =
  `Assoc [
    "name", `String fn.fn_name;
    "inputs", inputs fn.fn_params;
    "input_names", names fn.fn_params;
    "output", `String (type_name fn.fn_ret);
    "view", `Bool (fn.fn_view || fn.fn_pure);
    "payable", `Bool fn.fn_payable;
  ]

let event_json event =
  let fields =
    List.map
      (fun (name, kind, indexed) ->
        `Assoc [
          "name", `String name;
          "type", `String (type_name kind);
          "indexed", `Bool indexed;
        ])
      event.ev_fields
  in
  `Assoc ["name", `String event.ev_name; "fields", `List fields]

let to_json ast =
  let functions =
    ast.funcs
    |> List.filter (fun fn -> fn.fn_vis = Public)
    |> List.map method_json
  in
  `Assoc [
    "declaration", `String (declaration_to_string ast.declaration);
    "constructor", Option.fold ~none:`Null ~some:method_json ast.ctor;
    "functions", `List functions;
    "events", `List (List.map event_json ast.events);
  ]

let encode ast = Yojson.Safe.to_string (to_json ast)