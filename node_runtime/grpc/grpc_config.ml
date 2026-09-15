(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  host : string;
  port : int;
  max_request_bytes : int;
  submit_bytes : int option;
  max_response_bytes : int;
  max_streams : int;
  default_deadline_s : float;
  max_deadline_s : float;
}

type setting =
  | Disabled
  | Enabled of t

let ( let* ) = Result.bind

let bool_value ~default name = function
  | None -> Ok default
  | Some raw ->
    match String.lowercase_ascii (String.trim raw) with
    | "1" | "true" | "yes" -> Ok true
    | "0" | "false" | "no" -> Ok false
    | _ -> Error (Printf.sprintf "%s is invalid" name)

let int_value env name value =
  match env name with
  | None -> Ok value
  | Some raw ->
    begin
      match int_of_string_opt (String.trim raw) with
      | Some parsed -> Ok parsed
      | None -> Error (Printf.sprintf "%s is invalid" name)
    end

let in_range name low high value =
  if value < low || value > high then
    Error
      (Printf.sprintf
         "%s is outside range %d..%d"
         name
         low
         high)
  else
    Ok value

let inet host =
  try Ok (Unix.inet_addr_of_string host) with Failure _ ->
    Error "OCTRA_GRPC_HOST must be a numeric address"

let loopback addr =
  let text = Unix.string_of_inet_addr addr in
  text = "::1"
  || String.starts_with ~prefix:"127." text

let enabled env =
  let* host =
    Ok (Option.value ~default:"127.0.0.1" (env "OCTRA_GRPC_HOST"))
  in
  let* addr = inet host in
  let* () =
    if loopback addr then Ok ()
    else Error "OCTRA_GRPC_HOST must be a loopback address"
  in
  let* port = int_value env "OCTRA_GRPC_PORT" 8081 in
  let* port = in_range "OCTRA_GRPC_PORT" 1 65_535 port in
  let* max_request_bytes =
    int_value env "OCTRA_GRPC_MAX_REQUEST_BYTES" 65_536
  in
  let* max_request_bytes =
    in_range
      "OCTRA_GRPC_MAX_REQUEST_BYTES"
      1
      1_048_576
      max_request_bytes
  in
  let* max_response_bytes =
    int_value env "OCTRA_GRPC_MAX_RESPONSE_BYTES" 4_194_304
  in
  let* max_response_bytes =
    in_range
      "OCTRA_GRPC_MAX_RESPONSE_BYTES"
      1
      16_777_216
      max_response_bytes
  in
  let* submit_enabled =
    bool_value ~default:true "OCTRA_GRPC_SUBMIT_ENABLE" (env "OCTRA_GRPC_SUBMIT_ENABLE")
  in
  let* submit_bytes =
    if not submit_enabled then Ok None
    else
      let name = "OCTRA_GRPC_MAX_SUBMIT_BYTES" in
      let limit = Octra_net.P2p_tx_gossip.max_tx_json + 5 in
      let* size = int_value env name limit in
      let* size = in_range name 1 limit size in
      Ok (Some size)
  in
  let* max_streams = int_value env "OCTRA_GRPC_MAX_STREAMS" 32 in
  let* max_streams = in_range "OCTRA_GRPC_MAX_STREAMS" 1 128 max_streams in
  let* default_deadline_ms =
    int_value env "OCTRA_GRPC_DEADLINE_MS" 5_000
  in
  let* max_deadline_ms =
    int_value env "OCTRA_GRPC_MAX_DEADLINE_MS" 10_000
  in
  let* max_deadline_ms =
    in_range "OCTRA_GRPC_MAX_DEADLINE_MS" 1 60_000 max_deadline_ms
  in
  let* default_deadline_ms =
    in_range
      "OCTRA_GRPC_DEADLINE_MS"
      1
      max_deadline_ms
      default_deadline_ms
  in
  Ok
    (Enabled {
       host;
       port;
       max_request_bytes;
       submit_bytes;
       max_response_bytes;
       max_streams;
       default_deadline_s = float_of_int default_deadline_ms /. 1000.0;
       max_deadline_s = float_of_int max_deadline_ms /. 1000.0;
     })

let of_env env =
  match bool_value ~default:false "OCTRA_GRPC_ENABLE" (env "OCTRA_GRPC_ENABLE") with
  | Error _ as error -> error
  | Ok false -> Ok Disabled
  | Ok true -> enabled env