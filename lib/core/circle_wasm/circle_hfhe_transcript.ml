(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type entry = {
  method_name : string;
  request_hash : string;
  response_hash : string;
  result : bool option;
}

type mode =
  | Direct
  | Capture
  | Consume of entry list

let schema = "octra_circle_hfhe_transcript_v1"
let max_entries = 16
let max_verifiers = 2

let verify_method = function
  | "fhe_verify_zero"
  | "fhe_verify_range"
  | "fhe_verify_bound" -> true
  | _ -> false

let hex64 value =
  String.length value = 64
  && String.for_all
       (function
         | '0' .. '9'
         | 'a' .. 'f' -> true
         | _ -> false)
       value

let valid_entry entry =
  entry.method_name <> ""
  && String.length entry.method_name <= 64
  && hex64 entry.request_hash
  && hex64 entry.response_hash
  && (verify_method entry.method_name = Option.is_some entry.result)

let validate entries =
  if List.length entries > max_entries then
    Error "circle_hfhe_transcript_limit"
  else if
    List.fold_left
      (fun count entry ->
        if verify_method entry.method_name then count + 1 else count)
      0
      entries
    > max_verifiers
  then
    Error "circle_hfhe_verifier_limit"
  else if List.for_all valid_entry entries then
    Ok ()
  else
    Error "circle_hfhe_transcript_invalid"

let entry_json entry =
  `Assoc [
    "method", `String entry.method_name;
    "request_hash", `String entry.request_hash;
    "response_hash", `String entry.response_hash;
    "result",
    begin
      match entry.result with
      | Some value -> `Bool value
      | None -> `Null
    end;
  ]

let entries_json entries =
  `List (List.map entry_json entries)

let canonical entries =
  Yojson.Safe.to_string
    (`Assoc [
      "schema", `String schema;
      "entries", entries_json entries;
    ])

let hash entries =
  Digestif.SHA256.digest_string
    ("octra:circle_hfhe_transcript:v1\000" ^ canonical entries)
  |> Digestif.SHA256.to_hex

let string_field name fields =
  match List.assoc_opt name fields with
  | Some (`String value) -> Ok value
  | _ -> Error ("circle_hfhe_" ^ name ^ "_invalid")

let entry_of_json = function
  | `Assoc fields ->
    begin
      match
        string_field "method" fields,
        string_field "request_hash" fields,
        string_field "response_hash" fields,
        List.assoc_opt "result" fields
      with
      | Ok method_name, Ok request_hash, Ok response_hash, Some (`Bool result) ->
        let entry = {
          method_name;
          request_hash;
          response_hash;
          result = Some result;
        } in
        if valid_entry entry then Ok entry
        else Error "circle_hfhe_entry_invalid"
      | Ok method_name, Ok request_hash, Ok response_hash, Some `Null ->
        let entry = {
          method_name;
          request_hash;
          response_hash;
          result = None;
        } in
        if valid_entry entry then Ok entry
        else Error "circle_hfhe_entry_invalid"
      | Error e, _, _, _
      | _, Error e, _, _
      | _, _, Error e, _ -> Error e
      | _ -> Error "circle_hfhe_result_invalid"
    end
  | _ -> Error "circle_hfhe_entry_invalid"

let entries_of_json = function
  | `List values ->
    let rec loop acc = function
      | [] ->
        let entries = List.rev acc in
        begin
          match validate entries with
          | Ok () -> Ok entries
          | Error e -> Error e
        end
      | value :: rest ->
        begin
          match entry_of_json value with
          | Ok entry -> loop (entry :: acc) rest
          | Error e -> Error e
        end
    in
    loop [] values
  | _ -> Error "circle_hfhe_entries_invalid"

let mode_name = function
  | Direct -> "direct"
  | Capture -> "capture"
  | Consume _ -> "consume"

let mode_entries = function
  | Consume entries -> entries
  | Direct | Capture -> []