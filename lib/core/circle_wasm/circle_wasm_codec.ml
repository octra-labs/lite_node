(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type request_param =
  | Param_null
  | Param_bool of bool
  | Param_int of string
  | Param_string of string

type response =
  | Resp_null
  | Resp_bool of bool
  | Resp_int of string
  | Resp_string of string

let request_magic = "OCWR1"
let response_magic = "OCWS1"

let add_u16 buf value =
  Buffer.add_char buf (Char.chr ((value lsr 8) land 0xff));
  Buffer.add_char buf (Char.chr (value land 0xff))

let add_u32 buf value =
  Buffer.add_char buf (Char.chr ((value lsr 24) land 0xff));
  Buffer.add_char buf (Char.chr ((value lsr 16) land 0xff));
  Buffer.add_char buf (Char.chr ((value lsr 8) land 0xff));
  Buffer.add_char buf (Char.chr (value land 0xff))

let get_u8 raw offset =
  if offset < 0 || offset >= String.length raw then
    Error "out of bounds"
  else
    Ok (Char.code raw.[offset])

let get_u16 raw offset =
  match get_u8 raw offset, get_u8 raw (offset + 1) with
  | Ok a, Ok b ->
    Ok ((a lsl 8) lor b)
  | Error e, _
  | _, Error e ->
    Error e

let get_u32 raw offset =
  match
    get_u8 raw offset,
    get_u8 raw (offset + 1),
    get_u8 raw (offset + 2),
    get_u8 raw (offset + 3)
  with
  | Ok a, Ok b, Ok c, Ok d ->
    Ok
      (((a land 0xff) lsl 24)
       lor ((b land 0xff) lsl 16)
       lor ((c land 0xff) lsl 8)
       lor (d land 0xff))
  | Error e, _, _, _
  | _, Error e, _, _
  | _, _, Error e, _
  | _, _, _, Error e ->
    Error e

let yojson_param = function
  | `Null -> Ok Param_null
  | `Bool b -> Ok (Param_bool b)
  | `String s -> Ok (Param_string s)
  | `Int n -> Ok (Param_int (string_of_int n))
  | `Intlit s ->
    begin
      try
        ignore (Z.of_string s);
        Ok (Param_int s)
      with _ ->
        Error "invalid wasm integer literal"
    end
  | _ ->
    Error "wasm_v1 only supports null, bool, int, and string params"

let encode_request ~method_name params =
  let normalized_params =
    List.map yojson_param params in
  match List.find_opt (function Error _ -> true | Ok _ -> false) normalized_params with
  | Some (Error e) -> Error e
  | _ ->
    let params =
      List.filter_map
        (function
          | Ok value -> Some value
          | Error _ -> None)
        normalized_params in
    let buf = Buffer.create 256 in
    Buffer.add_string buf request_magic;
    add_u16 buf (String.length method_name);
    Buffer.add_string buf method_name;
    add_u16 buf (List.length params);
    List.iter
      (function
        | Param_null ->
          Buffer.add_char buf (Char.chr 0);
          add_u32 buf 0
        | Param_bool false ->
          Buffer.add_char buf (Char.chr 1);
          add_u32 buf 0
        | Param_bool true ->
          Buffer.add_char buf (Char.chr 2);
          add_u32 buf 0
        | Param_int value ->
          Buffer.add_char buf (Char.chr 3);
          add_u32 buf (String.length value);
          Buffer.add_string buf value
        | Param_string value ->
          Buffer.add_char buf (Char.chr 4);
          add_u32 buf (String.length value);
          Buffer.add_string buf value)
      params;
    Ok (Buffer.contents buf)

let decode_response raw =
  let len = String.length raw in
  if len < 10 then
    Error "wasm response too short"
  else if String.sub raw 0 (String.length response_magic) <> response_magic then
    Error "invalid wasm response magic"
  else
    let tag_offset = String.length response_magic in
    match get_u8 raw tag_offset, get_u32 raw (tag_offset + 1) with
    | Ok tag, Ok payload_len ->
      let payload_offset = tag_offset + 5 in
      if payload_offset + payload_len <> len then
        Error "invalid wasm response framing"
      else
        let payload = String.sub raw payload_offset payload_len in
        begin
          match tag with
          | 0 -> Ok Resp_null
          | 1 -> Ok (Resp_bool false)
          | 2 -> Ok (Resp_bool true)
          | 3 -> Ok (Resp_int payload)
          | 4 -> Ok (Resp_string payload)
          | _ ->
            Error "unsupported wasm response tag"
        end
    | Error e, _
    | _, Error e ->
      Error e