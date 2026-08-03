(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

external run_json_native : string -> int * string
  = "caml_octra_circle_wasm_host_run_json"

type run_error =
  | Rejected of string
  | Unavailable of string

let max_input_bytes = 16_777_216

let error_message = function
  | Rejected message
  | Unavailable message -> message

let run_json_classified input =
  if String.length input > max_input_bytes then
    Error
      (Rejected
         (Printf.sprintf
            "input too large: bytes=%d limit=%d"
            (String.length input)
            max_input_bytes))
  else
    try
      match run_json_native input with
      | 0, output -> Ok output
      | 1, message -> Error (Rejected message)
      | _, message -> Error (Unavailable message)
    with
    | exn -> Error (Unavailable (Printexc.to_string exn))

let run_json input =
  Result.map_error error_message (run_json_classified input)