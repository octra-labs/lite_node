(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module P = Pvac_verify_protocol
module Scheduler = Compute_pool

type priority = Scheduler.priority =
  | Required
  | Speculative

type outcome =
  | Completed of P.response
  | Timed_out
  | Memory_exceeded
  | Busy
  | Unavailable of string
  | Failed of string

type verification_failure =
  | Proof_rejected of string
  | Worker_busy
  | Worker_unavailable of string
  | Worker_timed_out
  | Worker_memory_exceeded
  | Worker_failed of string

type stream = {
  fd : Unix.file_descr;
  data : Buffer.t;
  mutable open_ : bool;
}

let capacity_of_getenv getenv =
  match getenv "OCTRA_PVAC_VERIFY_WORKERS" with
  | None -> 1
  | Some raw ->
    begin
      try
        let value = int_of_string raw in
        if value < 1 || value > 2 then 1 else value
      with _ ->
        1
    end

let capacity = capacity_of_getenv Sys.getenv_opt

let scheduler =
  Scheduler.create
    ~capacity
    ~required_limit:Resource_lanes.preverify_required_queue_limit
    ~speculative_limit:Resource_lanes.preverify_speculative_queue_limit
    ~required_burst:Resource_lanes.preverify_required_burst
    ()

let float_env name default lower upper =
  match Sys.getenv_opt name with
  | None -> default
  | Some raw ->
    begin
      try
        let value = float_of_string raw in
        if value < lower || value > upper then default else value
      with _ ->
        default
    end

let int_env name default lower upper =
  match Sys.getenv_opt name with
  | None -> default
  | Some raw ->
    begin
      try
        let value = int_of_string raw in
        if value < lower || value > upper then default else value
      with _ ->
        default
    end

let timeout_seconds () =
  float_env "OCTRA_PVAC_VERIFY_TIMEOUT_SEC" 600. 1. 1800.

let max_rss_mb () =
  int_env "OCTRA_PVAC_VERIFY_MAX_RSS_MB" 9_216 64 32_768

let executable_candidates () =
  let directory = Filename.dirname Sys.executable_name in
  let extension = if Sys.win32 then ".exe" else "" in
  [
    Filename.concat directory ("octra_pvac_worker" ^ extension);
    Filename.concat directory "octra_pvac_worker.exe";
    Filename.concat directory ("../bin/octra_pvac_worker" ^ extension);
    Filename.concat directory "../bin/octra_pvac_worker.exe";
  ]

let executable path =
  try
    Unix.access path [Unix.X_OK];
    true
  with _ ->
    false

let worker_path () =
  match Sys.getenv_opt "OCTRA_PVAC_VERIFY_WORKER" with
  | Some path when path <> "" && executable path -> Some path
  | Some _ -> None
  | None -> List.find_opt executable (executable_candidates ())

let monotonic_seconds () =
  Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1_000_000_000.

let rss_mb_of_status_line line =
  if not (String.starts_with ~prefix:"VmRSS:" line) then None
  else
    let raw =
      String.trim (String.sub line 6 (String.length line - 6))
    in
    try Some ((Scanf.sscanf raw "%d" Fun.id + 1023) / 1024)
    with _ -> None

let read_rss_mb pid =
  if Sys.os_type <> "Unix" || not (Sys.file_exists "/proc") then None
  else
    let path = Printf.sprintf "/proc/%d/status" pid in
    try
      let channel = open_in path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr channel)
        (fun () ->
          let rec find () =
            match rss_mb_of_status_line (input_line channel) with
            | Some _ as rss -> rss
            | None -> find ()
          in
          try find ()
          with End_of_file -> None)
    with _ ->
      None

let close_noerr fd =
  try Unix.close fd
  with _ -> ()

let terminate pid =
  begin
    try Unix.kill pid Sys.sigkill
    with _ -> ()
  end;
  let rec reap attempts =
    if attempts <= 0 then ()
    else
      try
        match Unix.waitpid [Unix.WNOHANG] pid with
        | 0, _ ->
          Unix.sleepf 0.01;
          reap (attempts - 1)
        | _ ->
          ()
      with _ ->
        ()
  in
  reap 100

let close_stream stream =
  if stream.open_ then begin
    stream.open_ <- false;
    close_noerr stream.fd
  end

let append stream limit bytes count =
  if Buffer.length stream.data + count > limit then Error "output_too_large"
  else begin
    Buffer.add_subbytes stream.data bytes 0 count;
    Ok ()
  end

let rec read_available stream limit bytes =
  if not stream.open_ then Ok ()
  else
    try
      match Unix.read stream.fd bytes 0 (Bytes.length bytes) with
      | 0 ->
        close_stream stream;
        Ok ()
      | count ->
        begin
          match append stream limit bytes count with
          | Error _ as error -> error
          | Ok () -> read_available stream limit bytes
        end
    with
    | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> Ok ()
    | Unix.Unix_error (Unix.EINTR, _, _) -> read_available stream limit bytes
    | error -> Error (Printexc.to_string error)

let write_available fd raw offset =
  if !offset >= String.length raw then Ok true
  else
    try
      let count =
        Unix.write_substring
          fd
          raw
          !offset
          (String.length raw - !offset)
      in
      offset := !offset + count;
      Ok (!offset >= String.length raw)
    with
    | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> Ok false
    | Unix.Unix_error (Unix.EINTR, _, _) -> Ok false
    | Unix.Unix_error (Unix.EPIPE, _, _) -> Error "worker_input_closed"
    | error -> Error (Printexc.to_string error)

let poll_status pid =
  try
    match Unix.waitpid [Unix.WNOHANG] pid with
    | 0, _ -> None
    | _, status -> Some status
  with Unix.Unix_error (Unix.EINTR, _, _) ->
    None

let process_response expected_hash stdout stderr status =
  match status with
  | Unix.WEXITED 0 ->
    begin
      match P.response_of_string stdout with
      | Ok response when response.request_hash = expected_hash ->
        Completed response
      | Ok _ ->
        Failed "response_hash_mismatch"
      | Error error ->
        Failed error
    end
  | Unix.WEXITED code ->
    let reason =
      if String.trim stderr = "" then
        Printf.sprintf "worker_exit_%d" code
      else
        String.trim stderr
    in
    Failed reason
  | Unix.WSIGNALED signal ->
    Failed (Printf.sprintf "worker_signal_%d" signal)
  | Unix.WSTOPPED signal ->
    Failed (Printf.sprintf "worker_stopped_%d" signal)

let run_process worker request =
  let raw = P.canonical_request request in
  let expected_hash = P.request_hash request in
  let input_read, input_write = Unix.pipe () in
  let output_read, output_write = Unix.pipe () in
  let error_read, error_write = Unix.pipe () in
  List.iter
    Unix.set_close_on_exec
    [
      input_read;
      input_write;
      output_read;
      output_write;
      error_read;
      error_write;
    ];
  let close_all () =
    List.iter close_noerr
      [
        input_read;
        input_write;
        output_read;
        output_write;
        error_read;
        error_write;
      ]
  in
  let pid_ref = ref None in
  try
    let pid =
      Unix.create_process_env
        worker
        [|worker|]
        (Unix.environment ())
        input_read
        output_write
        error_write
    in
    pid_ref := Some pid;
    close_noerr input_read;
    close_noerr output_write;
    close_noerr error_write;
    Unix.set_nonblock input_write;
    Unix.set_nonblock output_read;
    Unix.set_nonblock error_read;
    let output = { fd = output_read; data = Buffer.create 4096; open_ = true } in
    let error = { fd = error_read; data = Buffer.create 4096; open_ = true } in
    let input_open = ref true in
    let input_offset = ref 0 in
    let started = monotonic_seconds () in
    let bytes = Bytes.create 65_536 in
    let finish outcome =
      if !input_open then close_noerr input_write;
      input_open := false;
      close_stream output;
      close_stream error;
      outcome
    in
    let fail reason =
      terminate pid;
      finish (Failed reason)
    in
    let rec loop status =
      let elapsed = monotonic_seconds () -. started in
      if elapsed > timeout_seconds () then begin
        terminate pid;
        finish Timed_out
      end else
        match read_rss_mb pid with
        | Some rss when rss > max_rss_mb () ->
          terminate pid;
          finish Memory_exceeded
        | Some _ | None ->
          let status =
            match status with
            | Some _ -> status
            | None -> poll_status pid
          in
          if Option.is_some status && !input_open then begin
            close_noerr input_write;
            input_open := false
          end;
          if
            Option.is_some status
            && not output.open_
            && not error.open_
          then
            match status with
            | Some value ->
              finish
                (process_response
                   expected_hash
                   (Buffer.contents output.data)
                   (Buffer.contents error.data)
                   value)
            | None ->
              fail "worker_status_missing"
          else
            let reads =
              List.filter_map
                (fun stream -> if stream.open_ then Some stream.fd else None)
                [output; error]
            in
            let writes = if !input_open then [input_write] else [] in
            let readable, writable, _ =
              try Unix.select reads writes [] 0.1
              with Unix.Unix_error (Unix.EINTR, _, _) -> [], [], []
            in
            let read_result =
              List.fold_left
                (fun result stream ->
                  match result with
                  | Error _ -> result
                  | Ok () when
                      stream.open_
                      && List.mem stream.fd readable ->
                    read_available stream P.max_response_bytes bytes
                  | Ok () -> Ok ())
                (Ok ())
                [output; error]
            in
            begin
              match read_result with
              | Error reason -> fail reason
              | Ok () ->
                if !input_open && List.mem input_write writable then
                  begin
                    match write_available input_write raw input_offset with
                    | Error reason -> fail reason
                    | Ok true ->
                      close_noerr input_write;
                      input_open := false;
                      loop status
                    | Ok false ->
                      loop status
                  end
                else
                  loop status
            end
    in
    loop None
  with error ->
    Option.iter terminate !pid_ref;
    close_all ();
    Failed (Printexc.to_string error)

let run_unmanaged request =
  match worker_path () with
  | None -> Unavailable "worker_missing"
  | Some worker -> run_process worker request

let run_sync ?(priority = Required) request =
  match
    Scheduler.run_sync scheduler priority (fun () -> run_unmanaged request)
  with
  | Some outcome -> outcome
  | None -> Busy

let try_run_sync ?(priority = Speculative) request =
  match
    Scheduler.try_run_sync scheduler priority (fun () -> run_unmanaged request)
  with
  | Some outcome -> outcome
  | None -> Busy

let run ?(priority = Required) request =
  let open Lwt.Syntax in
  let* outcome =
    Scheduler.run_threaded scheduler priority run_unmanaged request
  in
  match outcome with
  | Some value -> Lwt.return value
  | None -> Lwt.return Busy

let scheduler_stats () =
  Scheduler.stats scheduler

let verification_failure_message = function
  | Proof_rejected reason -> reason
  | Worker_busy -> "proof worker queue is full"
  | Worker_unavailable reason -> "proof worker unavailable: " ^ reason
  | Worker_timed_out -> "proof worker timed out"
  | Worker_memory_exceeded -> "proof worker memory limit exceeded"
  | Worker_failed reason -> "proof worker failed: " ^ reason

let classified_outcome_result = function
  | Completed response when response.accepted -> Ok ()
  | Completed response -> Error (Proof_rejected response.reason)
  | Busy -> Error Worker_busy
  | Unavailable reason -> Error (Worker_unavailable reason)
  | Timed_out -> Error Worker_timed_out
  | Memory_exceeded -> Error Worker_memory_exceeded
  | Failed reason -> Error (Worker_failed reason)

let outcome_result outcome =
  classified_outcome_result outcome
  |> Result.map_error verification_failure_message

let classified_result ?(priority = Required) request =
  let open Lwt.Syntax in
  let* outcome = run ~priority request in
  Lwt.return (classified_outcome_result outcome)

let result ?(priority = Required) request =
  let open Lwt.Syntax in
  let* outcome = run ~priority request in
  Lwt.return (outcome_result outcome)

let result_sync ?(priority = Required) request =
  run_sync ~priority request |> outcome_result

let try_classified_result_sync ?(priority = Speculative) request =
  try_run_sync ~priority request |> classified_outcome_result

let try_result_sync ?(priority = Speculative) request =
  try_classified_result_sync ~priority request
  |> Result.map_error verification_failure_message

let ready_sync () =
  result_sync P.Ping

let ready () =
  result P.Ping

let verify_encrypt_with_priority
    priority
    ~pubkey
    ~cipher
    ~amount
    ~proof
    ~commitment
    ~blinding =
  result ~priority
    (P.Encrypt {
       pubkey;
       cipher;
       amount;
       proof;
       commitment;
       blinding;
     })

let verify_encrypt_classified_with_priority
    priority
    ~pubkey
    ~cipher
    ~amount
    ~proof
    ~commitment
    ~blinding =
  classified_result ~priority
    (P.Encrypt {
       pubkey;
       cipher;
       amount;
       proof;
       commitment;
       blinding;
     })

let verify_encrypt ~pubkey ~cipher ~amount ~proof ~commitment ~blinding =
  verify_encrypt_with_priority
    Required
    ~pubkey
    ~cipher
    ~amount
    ~proof
    ~commitment
    ~blinding

let verify_claim_with_priority priority ~pubkey ~cipher ~proof ~commitment =
  result ~priority (P.Claim { pubkey; cipher; proof; commitment })

let verify_claim ~pubkey ~cipher ~proof ~commitment =
  verify_claim_with_priority Required ~pubkey ~cipher ~proof ~commitment

let verify_claim_classified_with_priority
    priority
    ~pubkey
    ~cipher
    ~proof
    ~commitment =
  classified_result ~priority (P.Claim { pubkey; cipher; proof; commitment })

let verify_claim_classified ~pubkey ~cipher ~proof ~commitment =
  verify_claim_classified_with_priority
    Required
    ~pubkey
    ~cipher
    ~proof
    ~commitment

let verify_key_switch_claim_with_priority
    priority
    ~pubkey
    ~cipher
    ~proof
    ~commitment =
  result ~priority (P.Key_switch_claim { pubkey; cipher; proof; commitment })

let verify_key_switch_claim ~pubkey ~cipher ~proof ~commitment =
  verify_key_switch_claim_with_priority
    Required
    ~pubkey
    ~cipher
    ~proof
    ~commitment

let verify_key_switch_claim_classified_with_priority
    priority
    ~pubkey
    ~cipher
    ~proof
    ~commitment =
  classified_result ~priority
    (P.Key_switch_claim { pubkey; cipher; proof; commitment })

let verify_key_switch_claim_classified ~pubkey ~cipher ~proof ~commitment =
  verify_key_switch_claim_classified_with_priority
    Required
    ~pubkey
    ~cipher
    ~proof
    ~commitment

let verify_range_with_priority priority ~pubkey ~cipher ~proof =
  result ~priority (P.Range { pubkey; cipher; proof })

let verify_range_classified_with_priority priority ~pubkey ~cipher ~proof =
  classified_result ~priority (P.Range { pubkey; cipher; proof })

let verify_range ~pubkey ~cipher ~proof =
  verify_range_with_priority Required ~pubkey ~cipher ~proof

let verify_zero_sync ~pubkey ~cipher ~proof =
  result_sync (P.Zero { pubkey; cipher; proof })

let verify_claim_sync ~pubkey ~cipher ~proof ~commitment =
  result_sync (P.Claim { pubkey; cipher; proof; commitment })

let verify_range_sync ~pubkey ~cipher ~proof =
  result_sync (P.Range { pubkey; cipher; proof })

let verify_range_bound_sync ~pubkey ~cipher ~proof ~commitment =
  result_sync (P.Range_bound { pubkey; cipher; proof; commitment })

let try_verify_zero_sync ~pubkey ~cipher ~proof =
  try_result_sync (P.Zero { pubkey; cipher; proof })

let try_verify_zero_sync_classified ~pubkey ~cipher ~proof =
  try_classified_result_sync (P.Zero { pubkey; cipher; proof })

let try_verify_claim_sync ~pubkey ~cipher ~proof ~commitment =
  try_result_sync (P.Claim { pubkey; cipher; proof; commitment })

let try_verify_claim_sync_classified ~pubkey ~cipher ~proof ~commitment =
  try_classified_result_sync
    (P.Claim { pubkey; cipher; proof; commitment })

let try_verify_range_sync ~pubkey ~cipher ~proof =
  try_result_sync (P.Range { pubkey; cipher; proof })

let try_verify_range_sync_classified ~pubkey ~cipher ~proof =
  try_classified_result_sync (P.Range { pubkey; cipher; proof })

let verify_circle_cell_with_priority
    priority
    ~pubkey
    ~cipher
    ~ciphertext_commitment
    ~proof_kind
    ~proof
    ~amount_commitment =
  result ~priority
    (P.Circle_cell {
       pubkey;
       cipher;
       ciphertext_commitment;
       proof_kind;
       proof;
       amount_commitment;
     })

let verify_circle_cell
    ~pubkey
    ~cipher
    ~ciphertext_commitment
    ~proof_kind
    ~proof
    ~amount_commitment =
  verify_circle_cell_with_priority
    Required
    ~pubkey
    ~cipher
    ~ciphertext_commitment
    ~proof_kind
    ~proof
    ~amount_commitment

let verify_circle_cell_sync
    ~pubkey
    ~cipher
    ~ciphertext_commitment
    ~proof_kind
    ~proof
    ~amount_commitment =
  result_sync
    (P.Circle_cell {
       pubkey;
       cipher;
       ciphertext_commitment;
       proof_kind;
       proof;
       amount_commitment;
     })