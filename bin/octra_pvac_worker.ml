(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module P = Octra_core.Pvac_verify_protocol
module D = Octra_core.Pvac_verify_direct

let set_oom_priority () =
  if Sys.file_exists "/proc/self/oom_score_adj" then
    try
      let channel = open_out "/proc/self/oom_score_adj" in
      Fun.protect
        ~finally:(fun () -> close_out_noerr channel)
        (fun () -> output_string channel "1000\n")
    with _ ->
      ()

let read_request () =
  let buffer = Bytes.create 65_536 in
  let output = Buffer.create 65_536 in
  let rec loop total =
    if total > P.max_request_bytes then Error "request_too_large"
    else
      let count = input stdin buffer 0 (Bytes.length buffer) in
      if count = 0 then Ok (Buffer.contents output)
      else begin
        Buffer.add_subbytes output buffer 0 count;
        loop (total + count)
      end
  in
  loop 0

let failure hash reason =
  P.{ request_hash = hash; accepted = false; reason }

let request_hash_of_raw raw =
  Digestif.SHA256.digest_string (P.schema ^ "\000" ^ raw)
  |> Digestif.SHA256.to_hex

let run () =
  Pvac_ffi.isolate_worker ();
  set_oom_priority ();
  match read_request () with
  | Error reason ->
    failure (String.make 64 '0') reason
  | Ok raw ->
    begin
      match P.request_of_string raw with
      | Error reason ->
        failure (request_hash_of_raw raw) reason
      | Ok request ->
        D.response request
    end

let () =
  try
    run ()
    |> P.canonical_response
    |> print_endline
  with error ->
    Printf.eprintf
      "event = pvac_worker_failed reason = %s\n%!"
      (Printexc.to_string error);
    exit 1