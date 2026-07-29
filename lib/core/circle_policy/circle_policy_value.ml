(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let normalized_bool value =
  String.lowercase_ascii (String.trim value)

let string_value = function
  | Some value -> Some (String.trim value)
  | None -> None

let bool_value = function
  | Some value ->
    let value = normalized_bool value in
    value = "1" || value = "true" || value = "yes"
  | None -> false

let int64_value = function
  | Some value ->
    begin
      try Some (Int64.of_string (String.trim value))
      with _ -> None
    end
  | None -> None

let int_value = function
  | Some value ->
    begin
      try Some (int_of_string (String.trim value))
      with _ -> None
    end
  | None -> None

let json_string_list values =
  Yojson.Safe.to_string (`List (List.map (fun value -> `String value) values))

let json_int_list values =
  Yojson.Safe.to_string (`List (List.map (fun value -> `Int value) values))

let json_string_int_assoc values =
  Yojson.Safe.to_string (`Assoc (List.map (fun (key, value) -> key, `Int value) values))

let string_list_value = function
  | None -> []
  | Some value ->
    begin
      try
        match Yojson.Safe.from_string value with
        | `List items ->
          List.filter_map (function `String item -> Some item | _ -> None) items
        | _ -> []
      with _ ->
        []
    end

let int_list_value = function
  | None -> None
  | Some value ->
    begin
      try
        match Yojson.Safe.from_string value with
        | `List items ->
          let values =
            List.filter_map
              (function
                | `Int item -> Some item
                | `Intlit item ->
                  begin
                    try Some (int_of_string item)
                    with _ -> None
                  end
                | `String item ->
                  begin
                    try Some (int_of_string item)
                    with _ -> None
                  end
                | _ -> None)
              items in
          Some values
        | _ -> None
      with _ ->
        None
    end

let string_int_assoc_value = function
  | None -> []
  | Some value ->
    begin
      try
        match Yojson.Safe.from_string value with
        | `Assoc items ->
          List.filter_map
            (fun (key, value) ->
              match value with
              | `Int item -> Some (key, item)
              | `Intlit item ->
                begin
                  try Some (key, int_of_string item)
                  with _ -> None
                end
              | `String item ->
                begin
                  try Some (key, int_of_string item)
                  with _ -> None
                end
              | _ -> None)
            items
        | _ -> []
      with _ ->
        []
    end

let set_or_clear storage_tbl key value_opt =
  match value_opt with
  | Some value -> Hashtbl.replace storage_tbl key value
  | None -> Hashtbl.remove storage_tbl key