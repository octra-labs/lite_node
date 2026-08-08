(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type accelerator =
  | Cpu
  | Cuda
  | Metal
  | Rocm

type limits = {
  accelerator : accelerator;
  lanes : int;
  memory_bytes : int64;
  max_request_bytes : int;
  max_response_bytes : int;
  timeout_seconds : int;
}

type t =
  | Disabled
  | Enabled of limits

let accelerator_name = function
  | Cpu -> "cpu"
  | Cuda -> "cuda"
  | Metal -> "metal"
  | Rocm -> "rocm"

let bool_value = function
  | None -> Ok false
  | Some raw ->
    begin
      match String.lowercase_ascii (String.trim raw) with
      | "1" | "true" | "yes" | "on" -> Ok true
      | "0" | "false" | "no" | "off" -> Ok false
      | _ -> Error "invalid OCTRA_COMPUTE_PROVIDER"
    end

let accelerator_value = function
  | None -> Ok Cpu
  | Some raw ->
    begin
      match String.lowercase_ascii (String.trim raw) with
      | "cpu" -> Ok Cpu
      | "cuda" -> Ok Cuda
      | "metal" -> Ok Metal
      | "rocm" -> Ok Rocm
      | _ -> Error "invalid OCTRA_COMPUTE_ACCELERATOR"
    end

let int_value ~name ~default ~minimum ~maximum = function
  | None -> Ok default
  | Some raw ->
    begin
      match int_of_string_opt (String.trim raw) with
      | Some value when value >= minimum && value <= maximum -> Ok value
      | _ -> Error ("invalid " ^ name)
    end

let int64_value ~name ~default ~minimum ~maximum = function
  | None -> Ok default
  | Some raw ->
    begin
      match Int64.of_string_opt (String.trim raw) with
      | Some value when value >= minimum && value <= maximum -> Ok value
      | _ -> Error ("invalid " ^ name)
    end

let ( let* ) result next =
  match result with
  | Ok value -> next value
  | Error _ as error -> error

let of_inputs ~cli_enabled ~getenv =
  let* env_enabled = bool_value (getenv "OCTRA_COMPUTE_PROVIDER") in
  if not (cli_enabled || env_enabled) then
    Ok Disabled
  else
    let* accelerator = accelerator_value (getenv "OCTRA_COMPUTE_ACCELERATOR") in
    let* lanes =
      int_value
        ~name:"OCTRA_COMPUTE_LANES"
        ~default:1
        ~minimum:1
        ~maximum:4
        (getenv "OCTRA_COMPUTE_LANES") in
    let* memory_bytes =
      int64_value
        ~name:"OCTRA_COMPUTE_MEMORY_BYTES"
        ~default:4_294_967_296L
        ~minimum:1_073_741_824L
        ~maximum:1_099_511_627_776L
        (getenv "OCTRA_COMPUTE_MEMORY_BYTES") in
    let* max_request_bytes =
      int_value
        ~name:"OCTRA_COMPUTE_MAX_REQUEST_BYTES"
        ~default:65_536
        ~minimum:1_024
        ~maximum:1_048_576
        (getenv "OCTRA_COMPUTE_MAX_REQUEST_BYTES") in
    let* max_response_bytes =
      int_value
        ~name:"OCTRA_COMPUTE_MAX_RESPONSE_BYTES"
        ~default:262_144
        ~minimum:1_024
        ~maximum:1_048_576
        (getenv "OCTRA_COMPUTE_MAX_RESPONSE_BYTES") in
    let* timeout_seconds =
      int_value
        ~name:"OCTRA_COMPUTE_TIMEOUT_SEC"
        ~default:300
        ~minimum:1
        ~maximum:1_800
        (getenv "OCTRA_COMPUTE_TIMEOUT_SEC") in
    Ok
      (Enabled {
         accelerator;
         lanes;
         memory_bytes;
         max_request_bytes;
         max_response_bytes;
         timeout_seconds;
       })