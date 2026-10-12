(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

external run_json_native : bool -> string -> int * string
  = "caml_octra_circle_wasm_host_run_json"

external hfhe_bridge : unit -> unit = "caml_octra_circle_hfhe_bridge"

let () = Callback.register "octra_circle_wasm_host_hfhe_bridge" hfhe_bridge

type session
type lease
type build

external build_new : unit -> build = "caml_octra_compute_build_new"
external build_begin : build -> string -> int = "caml_octra_compute_build_begin"
external build_step : build -> string -> int * string = "caml_octra_compute_build_step"
external build_close : build -> unit = "caml_octra_compute_build_close"

external lease_new : unit -> lease = "caml_octra_compute_lease_new"
external lease_hold : lease -> string -> int = "caml_octra_compute_lease_hold"
external lease_close : lease -> unit = "caml_octra_compute_lease_close"

external call_new : unit -> session = "caml_octra_circle_call_new"
external call_step : session -> string -> int * string = "caml_octra_circle_call_step"
external call_close : session -> unit = "caml_octra_circle_call_close"

type run_error =
  | Rejected of string
  | Unavailable of string

let max_input_bytes = 16_777_216

let max_call_input_bytes = 67_108_864

let error_message = function
  | Rejected message
  | Unavailable message -> message

let run_call call body =
  let open Lwt.Syntax in
  let session = call_new () in
  let running = ref Lwt.return_unit in
  let rec step body =
    let work = Exec_resource.detach (call_step session) body in
    running := Lwt.catch (fun () -> Lwt.map (fun _ -> ()) work) (fun _ -> Lwt.return_unit);
    let* code, raw = Lwt.no_cancel work in
    match code with
    | 0 -> Lwt.return (Ok raw)
    | 1 -> Lwt.return (Error (Rejected raw))
    | 3 -> let* response = call raw in step response
    | _ -> Lwt.return (Error (Unavailable raw)) in
  Lwt.finalize (fun () -> step body) (fun () ->
    let* () = Lwt.no_cancel !running in
    call_close session;
    Lwt.return_unit)

let run_json_classified ?(optional = false) input =
  if String.length input > max_input_bytes then
    Error
      (Rejected
         (Printf.sprintf
            "input too large: bytes=%d limit=%d"
            (String.length input)
            max_input_bytes))
  else
    try
      match run_json_native optional input with
      | 0, output -> Ok output
      | 1, message -> Error (Rejected message)
      | _, message -> Error (Unavailable message)
    with
    | (Stack_overflow | Out_of_memory) as error -> raise error
    | exn -> Error (Unavailable (Printexc.to_string exn))

let run_json input =
  Result.map_error error_message (run_json_classified input)