(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type level =
  | FATAL
  | ERROR
  | WARN
  | INFO
  | TRACE

let level_name = function
  | FATAL -> "FATAL"
  | ERROR -> "ERROR"
  | WARN -> "WARN"
  | INFO -> "INFO"
  | TRACE -> "TRACE"

let timestamp time =
  let whole = floor time in
  let tm = Unix.gmtime whole in
  let millis = int_of_float ((time -. whole) *. 1000.) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%03dZ"
    (tm.tm_year + 1900)
    (tm.tm_mon + 1)
    tm.tm_mday
    tm.tm_hour
    tm.tm_min
    tm.tm_sec
    millis

let clean limit value =
  let source_len = String.length value in
  let output_len = min source_len limit in
  let output = Bytes.create output_len in
  for i = 0 to output_len - 1 do
    let code = Char.code value.[i] in
    Bytes.set output i
      (if code < 32 || code = 127 then ' ' else value.[i])
  done;
  let text = String.trim (Bytes.unsafe_to_string output) in
  if source_len > limit then text ^ " truncated = true" else text

let color_code = function
  | FATAL -> "\027[35m"
  | ERROR -> "\027[31m"
  | WARN -> "\027[33m"
  | INFO -> "\027[32m"
  | TRACE -> "\027[36m"

let render ~color ~timestamp ~level ~component ~message =
  let component = clean 64 component in
  let message = clean 8192 message in
  if color then
    Printf.sprintf "\027[90m%s\027[0m %s%s\027[0m component = \027[34m%s\027[0m%s"
      timestamp
      (color_code level)
      (level_name level)
      component
      (if String.equal message "" then "" else " " ^ message)
  else
    Printf.sprintf "%s %s component = %s%s"
      timestamp
      (level_name level)
      component
      (if String.equal message "" then "" else " " ^ message)