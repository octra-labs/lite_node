(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type key = {
  id : string;
  public_key : string;
}

type error =
  | Invalid_certificate
  | Invalid_key_id
  | Invalid_private_key
  | Missing
  | Unknown_key
  | Invalid_signature

let schema = "octra_program_attestation_v1"
let domain = "octra:program_attestation:v1\000"

let error_message = function
  | Invalid_certificate -> "invalid program attestation certificate"
  | Invalid_key_id -> "invalid program attestation key id"
  | Invalid_private_key -> "invalid program attestation private key"
  | Missing -> "program compiler attestation missing"
  | Unknown_key -> "program compiler attestation key is not trusted"
  | Invalid_signature -> "program compiler attestation signature invalid"

let valid_key_id value =
  let length = String.length value in
  length > 0
  && length <= 64
  && String.for_all (fun ch -> Char.code ch >= 33 && Char.code ch <= 126) value

let payload ~key_id cert =
  if not (valid_key_id key_id) then Error Invalid_key_id
  else
    try
      match Octra_core.Json_tree.read cert with
      | `Assoc fields ->
        let fields = List.filter (fun (key, _) -> key <> "attestation") fields in
        Ok (domain ^ key_id ^ "\000" ^ Octra_core.Json_tree.write ~sort:true (`Assoc fields))
      | _ -> Error Invalid_certificate
    with
    | (Stack_overflow | Out_of_memory) as error -> raise error
    | _ -> Error Invalid_certificate

let certificate_fields cert =
  try
    match Octra_core.Json_tree.read cert with
    | `Assoc fields -> Ok fields
    | _ -> Error Invalid_certificate
  with
  | (Stack_overflow | Out_of_memory) as error -> raise error
  | _ -> Error Invalid_certificate

let field name fields =
  match List.filter (fun (key, _) -> key = name) fields with
  | [(_, value)] -> Some value
  | _ -> None

let string_field name fields =
  match field name fields with
  | Some (`String value) -> Some value
  | _ -> None

let attestation_fields fields =
  match List.filter (fun (key, _) -> key = "attestation") fields with
  | [] -> Error Missing
  | [(_, `Assoc values)] -> Ok values
  | _ -> Error Invalid_signature

let attach ~key_id ~private_key cert =
  if not (valid_key_id key_id) then Error Invalid_key_id
  else
    match payload ~key_id cert, certificate_fields cert with
    | Ok message, Ok fields ->
      (match Mirage_crypto_ec.Ed25519.priv_of_octets private_key with
       | Error _ -> Error Invalid_private_key
       | Ok key ->
         let signature = Mirage_crypto_ec.Ed25519.sign ~key message in
         let proof = `Assoc [
           "schema", `String schema;
           "key_id", `String key_id;
           "signature", `String (Base64.encode_exn signature);
         ] in
         let fields = List.filter (fun (name, _) -> name <> "attestation") fields in
         let fields = List.rev_append (List.rev fields) ["attestation", proof] in
         Ok (Octra_core.Json_tree.write (`Assoc fields)))
    | Error error, _
    | _, Error error -> Error error

let verify ~trusted cert =
  match certificate_fields cert with
  | Error error -> Error error
  | Ok fields ->
    (match attestation_fields fields with
     | Error error -> Error error
     | Ok proof ->
       (match string_field "schema" proof, string_field "key_id" proof,
              string_field "signature" proof with
        | Some proof_schema, Some key_id, Some encoded
          when proof_schema = schema && valid_key_id key_id ->
          (match payload ~key_id cert with
           | Error error -> Error error
           | Ok message ->
             (match List.find_opt (fun key -> key.id = key_id) trusted with
              | None -> Error Unknown_key
              | Some key ->
                (try
                   match Base64.decode encoded with
                   | Ok signature
                     when String.length signature = 64
                          && Octra_ed25519.verify ~pub:key.public_key ~msg:message signature ->
                     Ok ()
                   | _ -> Error Invalid_signature
                 with
                 | (Stack_overflow | Out_of_memory) as error -> raise error
                 | _ -> Error Invalid_signature)))
        | _ -> Error Invalid_signature))