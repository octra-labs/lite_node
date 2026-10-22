(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type digest_mode = Compatible | Keyed

let text value =
  if not (String.is_valid_utf_8 value) then invalid_arg "invalid sealed text";
  value

let json_string ~escape_del value =
  let buffer = Buffer.create (String.length value + 2) in
  Buffer.add_char buffer '"';
  String.iter (function
    | '"' -> Buffer.add_string buffer "\\\""
    | '\\' -> Buffer.add_string buffer "\\\\"
    | '\b' -> Buffer.add_string buffer "\\b"
    | '\012' -> Buffer.add_string buffer "\\f"
    | '\n' -> Buffer.add_string buffer "\\n"
    | '\r' -> Buffer.add_string buffer "\\r"
    | '\t' -> Buffer.add_string buffer "\\t"
    | value when Char.code value < 32 || (escape_del && value = '\127') ->
      Buffer.add_string buffer (Printf.sprintf "\\u%04x" (Char.code value))
    | value -> Buffer.add_char buffer value) (text value);
  Buffer.add_char buffer '"';
  Buffer.contents buffer

let salt_bytes ~escape_del ~circle_id ~key_id =
  "[\"octra:circle:sealed_digest\","
  ^ json_string ~escape_del circle_id ^ ","
  ^ json_string ~escape_del key_id ^ "]"

let digest_salt = salt_bytes ~escape_del:true

let xor left right =
  String.init (String.length left) (fun index ->
    Char.chr (Char.code left.[index] lxor Char.code right.[index]))

let derive_key ~passphrase ~salt =
  let passphrase = text passphrase in
  let hmac value = Digestif.SHA256.(hmac_string ~key:passphrase value |> to_raw_string) in
  let first = hmac (salt ^ "\000\000\000\001") in
  let rec loop value previous remaining =
    if remaining = 0 then value
    else
      let next = hmac previous in
      loop (xor value next) next (remaining - 1)
  in
  loop first first 119999

let read_key ~circle_id ~key_id ~passphrase =
  derive_key ~passphrase
    ~salt:("octra:circle:sealed_read:v1:" ^ text circle_id ^ ":" ^ text key_id)

let digest_bytes ~salt ~passphrase plaintext =
  let key = derive_key ~passphrase ~salt in
  Digestif.SHA256.(hmac_string ~key plaintext |> to_hex)

let digest ~circle_id ~key_id ~passphrase plaintext =
  digest_bytes ~salt:(digest_salt ~circle_id ~key_id) ~passphrase plaintext

let plaintext_digest plaintext = Digestif.SHA256.(digest_string plaintext |> to_hex)

let write_digest ?(mode = Compatible) ~circle_id ~key_id ~passphrase plaintext =
  let _ = text circle_id, text key_id, text passphrase in
  match mode with
  | Compatible -> plaintext_digest plaintext
  | Keyed -> digest ~circle_id ~key_id ~passphrase plaintext

let matches_digest ~circle_id ~key_id ~passphrase ~expected plaintext =
  let _ = text circle_id, text key_id, text passphrase in
  String.length expected = 64
  && String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) expected
  && (Eqaf.equal expected (plaintext_digest plaintext)
      || Eqaf.equal expected (digest ~circle_id ~key_id ~passphrase plaintext)
      || ((String.contains circle_id '\127' || String.contains key_id '\127')
          && Eqaf.equal expected (digest_bytes
            ~salt:(salt_bytes ~escape_del:false ~circle_id ~key_id) ~passphrase plaintext)))