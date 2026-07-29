(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let magic = "OCPG"
let version = 2
let header_size = 16
let max_code = 16 * 1024 * 1024
let max_cert = 1024 * 1024

let is_program raw =
  String.length raw >= String.length magic
  && String.sub raw 0 (String.length magic) = magic

type t = {
  code : string;
  cert : string;
}

type error =
  | Too_large of string
  | Too_short
  | Bad_magic
  | Bad_version of int
  | Bad_flags of int
  | Bad_length
  | Trailing_bytes

let error_message = function
  | Too_large name -> Printf.sprintf "%s too large" name
  | Too_short -> "program envelope too short"
  | Bad_magic -> "bad program envelope magic"
  | Bad_version value -> Printf.sprintf "unsupported program envelope version: %d" value
  | Bad_flags value -> Printf.sprintf "unsupported program envelope flags: %d" value
  | Bad_length -> "invalid program envelope length"
  | Trailing_bytes -> "program envelope trailing bytes"

let put_u16 buf value =
  Buffer.add_char buf (Char.chr (value land 0xff));
  Buffer.add_char buf (Char.chr ((value lsr 8) land 0xff))

let put_u32 buf value =
  Buffer.add_char buf (Char.chr (value land 0xff));
  Buffer.add_char buf (Char.chr ((value lsr 8) land 0xff));
  Buffer.add_char buf (Char.chr ((value lsr 16) land 0xff));
  Buffer.add_char buf (Char.chr ((value lsr 24) land 0xff))

let get_u16 raw pos =
  Char.code raw.[pos] lor (Char.code raw.[pos + 1] lsl 8)

let get_u32 raw pos =
  Char.code raw.[pos]
  lor (Char.code raw.[pos + 1] lsl 8)
  lor (Char.code raw.[pos + 2] lsl 16)
  lor (Char.code raw.[pos + 3] lsl 24)

let encode ~code ~cert =
  let code_len = String.length code in
  let cert_len = String.length cert in
  if code_len > max_code then Error (Too_large "program bytecode")
  else if cert_len > max_cert then Error (Too_large "program certificate")
  else
    let buf = Buffer.create (header_size + code_len + cert_len) in
    Buffer.add_string buf magic;
    put_u16 buf version;
    put_u16 buf 0;
    put_u32 buf code_len;
    put_u32 buf cert_len;
    Buffer.add_string buf code;
    Buffer.add_string buf cert;
    Ok (Buffer.contents buf)

let decode raw =
  let len = String.length raw in
  if len < header_size then Error Too_short
  else if String.sub raw 0 4 <> magic then Error Bad_magic
  else
    let ver = get_u16 raw 4 in
    let flags = get_u16 raw 6 in
    let code_len = get_u32 raw 8 in
    let cert_len = get_u32 raw 12 in
    if ver <> version then Error (Bad_version ver)
    else if flags <> 0 then Error (Bad_flags flags)
    else if code_len > max_code then Error (Too_large "program bytecode")
    else if cert_len > max_cert then Error (Too_large "program certificate")
    else if code_len > len - header_size then Error Bad_length
    else if cert_len > len - header_size - code_len then Error Bad_length
    else if header_size + code_len + cert_len <> len then Error Trailing_bytes
    else
      Ok {
        code = String.sub raw header_size code_len;
        cert = String.sub raw (header_size + code_len) cert_len;
      }