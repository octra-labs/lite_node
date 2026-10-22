(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  expected_code_hash : string;
  bytecode_raw : string;
}

let schema = "octra_program_upgrade_v1"

let is_hex = function
  | '0' .. '9'
  | 'a' .. 'f' -> true
  | _ -> false

let is_hash value =
  String.length value = 64 && String.for_all is_hex value

let one name fields =
  match List.filter (fun (key, _) -> String.equal key name) fields with
  | [(_, value)] -> Ok value
  | [] -> Error (name ^ " missing")
  | _ -> Error (name ^ " duplicated")

let parse_message_for schema raw =
  try
    match Octra_core.Json_tree.read raw with
    | `Assoc fields when List.length fields = 2 ->
      begin
        match one "schema" fields, one "expected_code_hash" fields with
        | Ok (`String parsed_schema), Ok (`String expected_code_hash) ->
          if not (String.equal parsed_schema schema) then
            Error "program upgrade schema mismatch"
          else if not (is_hash expected_code_hash) then
            Error "program upgrade expected code hash invalid"
          else
            Ok expected_code_hash
        | _ ->
          Error "program upgrade payload invalid"
      end
    | `Assoc _ ->
      Error "program upgrade payload fields invalid"
    | _ ->
      Error "program upgrade payload must be an object"
  with _ ->
    Error "program upgrade payload invalid"

let parse_message raw = parse_message_for schema raw

let is_source (tx : Octra_core.Transaction.t) =
  match tx.message with
  | Some raw -> Result.is_ok (parse_message_for Program_record.schema raw)
  | None -> false

let source_request (tx : Octra_core.Transaction.t) =
  let ( let* ) = Result.bind in
  if not (Z.equal tx.amount Z.zero) then Error "program source value must be zero"
  else match tx.message, tx.encrypted_data with
  | Some message, Some data ->
    let* hash = parse_message_for Program_record.schema message in
    let* sources = Program_record.decode_sources data in
    Ok (hash, sources)
  | _ -> Error "program source payload missing"

let source_message ~expected_code_hash =
  Yojson.Safe.to_string (`Assoc [
    "schema", `String Program_record.schema;
    "expected_code_hash", `String expected_code_hash;
  ])

let parse (tx : Octra_core.Transaction.t) =
  if not (Z.equal tx.amount Z.zero) then
    Error "program upgrade value must be zero"
  else
    match tx.message, tx.encrypted_data with
    | Some message, Some bytecode_b64 ->
      begin
        match parse_message message with
        | Error _ as error -> error
        | Ok expected_code_hash ->
          begin
            try
              let bytecode_raw = Base64.decode_exn bytecode_b64 in
              if bytecode_raw = "" then
                Error "program upgrade bytecode empty"
              else
                Ok { expected_code_hash; bytecode_raw }
            with _ ->
              Error "program upgrade bytecode invalid"
          end
      end
    | _ ->
      Error "program upgrade payload missing"

let message ~expected_code_hash =
  Yojson.Safe.to_string
    (`Assoc [
      "schema", `String schema;
      "expected_code_hash", `String expected_code_hash;
    ])