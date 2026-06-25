(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


type level =
  | FATAL
  | ERROR
  | WARN
  | INFO
  | TRACE

let level_str = function
  | FATAL -> "FATAL"
  | ERROR -> "ERROR"
  | WARN -> "WARN "
  | INFO -> "INFO "
  | TRACE -> "TRACE"

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

let timestamp () =
  let t = Unix.gettimeofday () in
  let tm = Unix.localtime t in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.tm_year + 1900)
    (tm.tm_mon + 1)
    tm.tm_mday
    tm.tm_hour
    tm.tm_min
    tm.tm_sec

let write channel msg =
  output_string channel msg;
  flush channel

let stdout fmt =
  Printf.ksprintf (write stdout) fmt

let stderr fmt =
  Printf.ksprintf (write stderr) fmt

let log level modul fmt =
  let k msg =
    if level_ord level <= level_ord !min_level then
      stdout "%s %s [%s] %s\n%!" (timestamp ()) (level_str level) modul msg
  in
  Printf.ksprintf k fmt

let info m fmt = log INFO m fmt

let warn m fmt = log WARN m fmt

let error m fmt = log ERROR m fmt

let fatal m fmt = log FATAL m fmt

let trace m fmt = log TRACE m fmt