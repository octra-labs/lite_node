(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type circle_proof =
  | Circle_none
  | Circle_zero
  | Circle_bound_zero
  | Circle_range

type circle_cell = {
  pubkey : string;
  cipher : string;
  ciphertext_commitment : string;
  proof_kind : circle_proof;
  proof : string;
  amount_commitment : string;
}

type request =
  | Ping
  | Encrypt of {
      pubkey : string;
      cipher : string;
      amount : Z.t;
      proof : string;
      commitment : string;
      blinding : string;
    }
  | Claim of {
      pubkey : string;
      cipher : string;
      proof : string;
      commitment : string;
    }
  | Key_switch_claim of {
      pubkey : string;
      cipher : string;
      proof : string;
      commitment : string;
    }
  | Range of {
      pubkey : string;
      cipher : string;
      proof : string;
    }
  | Zero of {
      pubkey : string;
      cipher : string;
      proof : string;
    }
  | Range_bound of {
      pubkey : string;
      cipher : string;
      proof : string;
      commitment : string;
    }
  | Circle_cell of circle_cell

type response = {
  request_hash : string;
  accepted : bool;
  reason : string;
}

let schema = "octra_pvac_verify"

let max_request_bytes = 64 * 1024 * 1024

let max_response_bytes = 64 * 1024

let max_pubkey_bytes = 8 * 1024 * 1024

let max_value_bytes = 16 * 1024 * 1024

let bind result f =
  match result with
  | Ok value -> f value
  | Error error -> Error error

let field name fields =
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> Error (name ^ "_missing")

let string_field name fields =
  bind (field name fields) (function
    | `String value -> Ok value
    | _ -> Error (name ^ "_invalid"))

let bool_field name fields =
  bind (field name fields) (function
    | `Bool value -> Ok value
    | _ -> Error (name ^ "_invalid"))

let lower_hex_char = function
  | '0' .. '9'
  | 'a' .. 'f' -> true
  | _ -> false

let lower_hex_64 value =
  String.length value = 64
  && String.for_all lower_hex_char value

let bounded name limit value =
  if String.length value > limit then Error (name ^ "_too_large")
  else Ok value

let encode_pubkey pubkey =
  Base64.encode_exn pubkey

let decode_pubkey encoded =
  if String.length encoded > ((max_pubkey_bytes * 4) / 3) + 8 then
    Error "pubkey_too_large"
  else
    try
      let raw = Base64.decode_exn encoded in
      bounded "pubkey" max_pubkey_bytes raw
    with _ ->
      Error "pubkey_invalid"

let circle_proof_name = function
  | Circle_none -> "none"
  | Circle_zero -> "zero"
  | Circle_bound_zero -> "bound_zero"
  | Circle_range -> "range"

let circle_proof_of_name = function
  | "none" -> Ok Circle_none
  | "zero" -> Ok Circle_zero
  | "bound_zero" -> Ok Circle_bound_zero
  | "range" -> Ok Circle_range
  | _ -> Error "proof_kind_invalid"

let request_fields request =
  let common op pubkey =
    [
      "schema", `String schema;
      "op", `String op;
      "pubkey", `String (encode_pubkey pubkey);
    ]
  in
  match request with
  | Ping ->
    [
      "schema", `String schema;
      "op", `String "ping";
    ]
  | Encrypt value ->
    common "encrypt" value.pubkey @ [
      "cipher", `String value.cipher;
      "amount", `String (Z.to_string value.amount);
      "proof", `String value.proof;
      "commitment", `String value.commitment;
      "blinding", `String value.blinding;
    ]
  | Claim value ->
    common "claim" value.pubkey @ [
      "cipher", `String value.cipher;
      "proof", `String value.proof;
      "commitment", `String value.commitment;
    ]
  | Key_switch_claim value ->
    common "key_switch_claim" value.pubkey @ [
      "cipher", `String value.cipher;
      "proof", `String value.proof;
      "commitment", `String value.commitment;
    ]
  | Range value ->
    common "range" value.pubkey @ [
      "cipher", `String value.cipher;
      "proof", `String value.proof;
    ]
  | Zero value ->
    common "zero" value.pubkey @ [
      "cipher", `String value.cipher;
      "proof", `String value.proof;
    ]
  | Range_bound value ->
    common "range_bound" value.pubkey @ [
      "cipher", `String value.cipher;
      "proof", `String value.proof;
      "commitment", `String value.commitment;
    ]
  | Circle_cell value ->
    common "circle_cell" value.pubkey @ [
      "cipher", `String value.cipher;
      "ciphertext_commitment", `String value.ciphertext_commitment;
      "proof_kind", `String (circle_proof_name value.proof_kind);
      "proof", `String value.proof;
      "amount_commitment", `String value.amount_commitment;
    ]

let request_json request =
  `Assoc (request_fields request)

let canonical_request request =
  Yojson.Safe.to_string (request_json request)

let request_hash request =
  Digestif.SHA256.digest_string (schema ^ "\000" ^ canonical_request request)
  |> Digestif.SHA256.to_hex

let parse_amount fields =
  bind (string_field "amount" fields) (fun raw ->
    if String.length raw > 32 then Error "amount_invalid"
    else
      try Ok (Z.of_string raw)
      with _ -> Error "amount_invalid")

let parse_value name fields =
  bind (string_field name fields) (bounded name max_value_bytes)

let request_of_json = function
  | `Assoc fields ->
    bind (string_field "schema" fields) (fun parsed_schema ->
    if parsed_schema <> schema then Error "schema_invalid"
    else
      bind (string_field "op" fields) (fun op ->
        match op with
        | "ping" ->
          Ok Ping
        | "encrypt" ->
          bind (string_field "pubkey" fields) (fun encoded_pubkey ->
          bind (decode_pubkey encoded_pubkey) (fun pubkey ->
          bind (parse_value "cipher" fields) (fun cipher ->
          bind (parse_amount fields) (fun amount ->
          bind (parse_value "proof" fields) (fun proof ->
          bind (parse_value "commitment" fields) (fun commitment ->
          bind (parse_value "blinding" fields) (fun blinding ->
            Ok
              (Encrypt {
                 pubkey;
                 cipher;
                 amount;
                 proof;
                 commitment;
                 blinding;
               }))))))))
        | "claim" ->
          bind (string_field "pubkey" fields) (fun encoded_pubkey ->
          bind (decode_pubkey encoded_pubkey) (fun pubkey ->
          bind (parse_value "cipher" fields) (fun cipher ->
          bind (parse_value "proof" fields) (fun proof ->
          bind (parse_value "commitment" fields) (fun commitment ->
            Ok (Claim { pubkey; cipher; proof; commitment }))))))
        | "key_switch_claim" ->
          bind (string_field "pubkey" fields) (fun encoded_pubkey ->
          bind (decode_pubkey encoded_pubkey) (fun pubkey ->
          bind (parse_value "cipher" fields) (fun cipher ->
          bind (parse_value "proof" fields) (fun proof ->
          bind (parse_value "commitment" fields) (fun commitment ->
            Ok
              (Key_switch_claim {
                 pubkey;
                 cipher;
                 proof;
                 commitment;
               }))))))
        | "range" ->
          bind (string_field "pubkey" fields) (fun encoded_pubkey ->
          bind (decode_pubkey encoded_pubkey) (fun pubkey ->
          bind (parse_value "cipher" fields) (fun cipher ->
          bind (parse_value "proof" fields) (fun proof ->
            Ok (Range { pubkey; cipher; proof })))))
        | "zero" ->
          bind (string_field "pubkey" fields) (fun encoded_pubkey ->
          bind (decode_pubkey encoded_pubkey) (fun pubkey ->
          bind (parse_value "cipher" fields) (fun cipher ->
          bind (parse_value "proof" fields) (fun proof ->
            Ok (Zero { pubkey; cipher; proof })))))
        | "range_bound" ->
          bind (string_field "pubkey" fields) (fun encoded_pubkey ->
          bind (decode_pubkey encoded_pubkey) (fun pubkey ->
          bind (parse_value "cipher" fields) (fun cipher ->
          bind (parse_value "proof" fields) (fun proof ->
          bind (parse_value "commitment" fields) (fun commitment ->
            Ok (Range_bound { pubkey; cipher; proof; commitment }))))))
        | "circle_cell" ->
          bind (string_field "pubkey" fields) (fun encoded_pubkey ->
          bind (decode_pubkey encoded_pubkey) (fun pubkey ->
          bind (parse_value "cipher" fields) (fun cipher ->
          bind
            (parse_value "ciphertext_commitment" fields)
            (fun ciphertext_commitment ->
          bind (string_field "proof_kind" fields) (fun proof_kind_raw ->
          bind (circle_proof_of_name proof_kind_raw) (fun proof_kind ->
          bind (parse_value "proof" fields) (fun proof ->
          bind
            (parse_value "amount_commitment" fields)
            (fun amount_commitment ->
              Ok
                (Circle_cell {
                   pubkey;
                   cipher;
                   ciphertext_commitment;
                   proof_kind;
                   proof;
                   amount_commitment;
                 })))))))))
        | _ ->
          Error "op_invalid"))
  | _ ->
    Error "request_invalid"

let request_of_string raw =
  if String.length raw > max_request_bytes then Error "request_too_large"
  else
    try Yojson.Safe.from_string raw |> request_of_json
    with _ -> Error "request_json_invalid"

let response_json response =
  `Assoc [
    "schema", `String schema;
    "request_hash", `String response.request_hash;
    "accepted", `Bool response.accepted;
    "reason", `String response.reason;
  ]

let canonical_response response =
  Yojson.Safe.to_string (response_json response)

let response_of_json = function
  | `Assoc fields ->
    bind (string_field "schema" fields) (fun parsed_schema ->
    if parsed_schema <> schema then Error "schema_invalid"
    else
      bind (string_field "request_hash" fields) (fun request_hash ->
      bind (bool_field "accepted" fields) (fun accepted ->
      bind (string_field "reason" fields) (fun reason ->
        if not (lower_hex_64 request_hash) then Error "request_hash_invalid"
        else if String.length reason > 4096 then Error "reason_too_large"
        else Ok { request_hash; accepted; reason }))))
  | _ ->
    Error "response_invalid"

let response_of_string raw =
  if String.length raw > max_response_bytes then Error "response_too_large"
  else
    try Yojson.Safe.from_string raw |> response_of_json
    with _ -> Error "response_json_invalid"