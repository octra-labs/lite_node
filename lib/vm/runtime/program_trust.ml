(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = Program_attestation.key list

type error =
  | Invalid_entry
  | Invalid_key_id
  | Invalid_public_key
  | Duplicate_key_id

let error_message = function
  | Invalid_entry -> "invalid program release key entry"
  | Invalid_key_id -> "invalid program release key id"
  | Invalid_public_key -> "invalid program release public key"
  | Duplicate_key_id -> "duplicate program release key id"

let key_id_valid value =
  let length = String.length value in
  length > 0
  && length <= 64
  && String.for_all
       (fun ch -> Char.code ch >= 33 && Char.code ch <= 126)
       value

let entry raw =
  match String.index_opt raw '=' with
  | None -> Error Invalid_entry
  | Some pos ->
    let id = String.trim (String.sub raw 0 pos) in
    let encoded = String.trim (String.sub raw (pos + 1) (String.length raw - pos - 1)) in
    if not (key_id_valid id) then Error Invalid_key_id
    else
      match Base64.decode encoded with
      | Error _ -> Error Invalid_public_key
      | Ok public_key when String.length public_key <> 32 -> Error Invalid_public_key
      | Ok public_key ->
        if Octra_ed25519.safe public_key then
          Ok { Program_attestation.id; public_key }
        else
          Error Invalid_public_key

let of_env getenv =
  match getenv "OCTRA_PROGRAM_RELEASE_KEYS" with
  | None -> Ok []
  | Some raw when String.trim raw = "" -> Ok []
  | Some raw ->
    let rec read seen acc = function
      | [] -> Ok (List.rev acc)
      | item :: rest ->
        (match entry item with
         | Error error -> Error error
         | Ok key when List.mem key.Program_attestation.id seen ->
           Error Duplicate_key_id
         | Ok key -> read (key.id :: seen) (key :: acc) rest)
    in
    read [] [] (String.split_on_char ',' raw)

let keys value = value

let empty = []

let frame value = string_of_int (String.length value) ^ ":" ^ value

let fingerprint value =
  let entries =
    keys value
    |> List.sort (fun left right ->
      String.compare left.Program_attestation.id right.id)
  in
  let encoded =
    entries
    |> List.map (fun key ->
      frame key.Program_attestation.id ^ frame key.public_key)
    |> String.concat ""
  in
  Digestif.SHA256.(digest_string (frame "octra:program_trust:v1" ^ encoded) |> to_raw_string)

let config_hash value =
  match keys value with
  | [] -> None
  | _ -> Some (fingerprint value)