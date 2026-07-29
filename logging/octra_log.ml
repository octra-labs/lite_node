(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type level = Octra_log_format.level =
  | FATAL
  | ERROR
  | WARN
  | INFO
  | TRACE

let min_level = ref INFO

let _set_level l = min_level := l

let level_of_string = function
  | "fatal" -> Some FATAL
  | "error" -> Some ERROR
  | "warn" | "warning" -> Some WARN
  | "info" -> Some INFO
  | "trace" | "debug" -> Some TRACE
  | _ -> None

let init_from_env () =
  match Sys.getenv_opt "OCTRA_LOG_LEVEL" with
  | Some s ->
    let normalized = String.lowercase_ascii (String.trim s) in
    begin
      match level_of_string normalized with
      | Some level -> min_level := level
      | None -> ()
    end
  | None -> ()

let level_ord = function
  | FATAL -> 0
  | ERROR -> 1
  | WARN -> 2
  | INFO -> 3
  | TRACE -> 4

let color_enabled channel =
  Sys.getenv_opt "NO_COLOR" = None
  && match Sys.getenv_opt "OCTRA_LOG_COLOR" with
     | Some "always" -> true
     | Some "never" -> false
     | _ -> Unix.isatty (Unix.descr_of_out_channel channel)

let write channel msg =
  output_string channel msg;
  flush channel

let stdout fmt =
  Printf.ksprintf (write stdout) fmt

let stderr fmt =
  Printf.ksprintf (write stderr) fmt

let log level modul fmt =
  let k msg =
    if level_ord level <= level_ord !min_level then begin
      let line =
        Octra_log_format.render
          ~color:(color_enabled Stdlib.stdout)
          ~timestamp:(Octra_log_format.timestamp (Unix.gettimeofday ()))
          ~level
          ~component:modul
          ~message:msg
      in
      write Stdlib.stdout (line ^ "\n")
    end
  in
  Printf.ksprintf k fmt

let info m fmt = log INFO m fmt

let warn m fmt = log WARN m fmt

let error m fmt = log ERROR m fmt

let fatal m fmt = log FATAL m fmt

let trace m fmt = log TRACE m fmt