(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type code =
  | Ok
  | Cancelled
  | Unknown
  | Invalid_argument
  | Deadline_exceeded
  | Not_found
  | Already_exists
  | Permission_denied
  | Resource_exhausted
  | Failed_precondition
  | Aborted
  | Out_of_range
  | Unimplemented
  | Internal
  | Unavailable
  | Data_loss
  | Unauthenticated

type t = {
  code : code;
  message : string;
  rpc_error : Octra_core.Rpc.rpc_error option;
}

let make code message =
  { code; message; rpc_error = None }

let ok = make Ok ""

let number = function
  | Ok -> 0
  | Cancelled -> 1
  | Unknown -> 2
  | Invalid_argument -> 3
  | Deadline_exceeded -> 4
  | Not_found -> 5
  | Already_exists -> 6
  | Permission_denied -> 7
  | Resource_exhausted -> 8
  | Failed_precondition -> 9
  | Aborted -> 10
  | Out_of_range -> 11
  | Unimplemented -> 12
  | Internal -> 13
  | Unavailable -> 14
  | Data_loss -> 15
  | Unauthenticated -> 16

let code_of_rpc error =
  match error.Octra_core.Rpc.code with
  | -32700 | -32600 | -32602 | 105 | 109 | 114 -> Invalid_argument
  | -32601 -> Unimplemented
  | 101 -> Unauthenticated
  | 104 -> Failed_precondition
  | 107 -> Resource_exhausted
  | 110 | -32005 | -32012 -> Unavailable
  | 100 | 112 -> Not_found
  | _ -> Failed_precondition

let of_rpc error =
  {
    code = code_of_rpc error;
    message = error.Octra_core.Rpc.message;
    rpc_error = Some error;
  }

let hex value =
  "0123456789ABCDEF".[value land 0xf]

let message value =
  let buffer = Buffer.create (String.length value) in
  String.iter
    (fun char ->
      let byte = Char.code char in
      if byte >= 0x20 && byte <= 0x7e && char <> '%' then
        Buffer.add_char buffer char
      else begin
        Buffer.add_char buffer '%';
        Buffer.add_char buffer (hex (byte lsr 4));
        Buffer.add_char buffer (hex byte)
      end)
    value;
  Buffer.contents buffer

let rpc_json error =
  let fields = [
    "code", `Int error.Octra_core.Rpc.code;
    "message", `String error.message;
  ] in
  let fields =
    match error.data with
    | None -> fields
    | Some data -> fields @ ["data", data]
  in
  Yojson.Safe.to_string (`Assoc fields)
  |> Base64.encode_exn

let trailers status =
  let base = ["grpc-status", string_of_int (number status.code)] in
  let base =
    if status.message = "" then base
    else base @ ["grpc-message", message status.message]
  in
  match status.rpc_error with
  | None -> base
  | Some error ->
    base @ [
      "octra-rpc-code", string_of_int error.code;
      "octra-rpc-error-bin", rpc_json error;
    ]