(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module P = Pvac_verify_protocol

type outcome =
  | Completed of P.response
  | Timed_out
  | Memory_exceeded
  | Failed of string

type stream = {
  fd : Unix.file_descr;
  data : Buffer.t;
  mutable open_ : bool;
}

let capacity =
  match Sys.getenv_opt "OCTRA_VERIFY_WORKERS" with
  | None -> 1
  | Some raw ->
    begin
      try
        let value = int_of_string raw in
        if value < 1 || value > 2 then 1 else value
      with _ ->
        1
    end

let max_queue =
  match Sys.getenv_opt "OCTRA_VERIFY_QUEUE" with
  | None -> 8
  | Some raw ->
    begin
      try
        let value = int_of_string raw in
        if value < 1 || value > 32 then 8 else value
      with _ ->
        8
    end

let slot_mutex = Mutex.create ()

let slot_ready = Condition.create ()

let active_slots = ref 0

let waiting_slots = ref 0

let acquire_slot () =
  Mutex.lock slot_mutex;
  if !active_slots < capacity then begin
    incr active_slots;
    Mutex.unlock slot_mutex;
    true
  end else if !waiting_slots >= max_queue then begin
    Mutex.unlock slot_mutex;
    false
  end else begin
    incr waiting_slots;
    while !active_slots >= capacity do
      Condition.wait slot_ready slot_mutex
    done;
    decr waiting_slots;
    incr active_slots;
    Mutex.unlock slot_mutex;
    true
  end

let release_slot () =
  Mutex.lock slot_mutex;
  decr active_slots;
  Condition.signal slot_ready;
  Mutex.unlock slot_mutex

let with_slot task =
  if acquire_slot () then
    Fun.protect ~finally:release_slot task
  else
    Failed "worker_queue_full"

let float_env name fallback lower upper =
  match Sys.getenv_opt name with
  | None -> fallback
  | Some raw ->
    begin
      try
        let value = float_of_string raw in
        if value < lower || value > upper then fallback else value
      with _ ->
        fallback
    end

let int_env name fallback lower upper =
  match Sys.getenv_opt name with
  | None -> fallback
  | Some raw ->
    begin
      try
        let value = int_of_string raw in
        if value < lower || value > upper then fallback else value
      with _ ->
        fallback
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

let run_sync request =
  with_slot (fun () ->
    match worker_path () with
    | None -> Failed "worker_missing"
    | Some worker -> run_process worker request)

let run request =
  Lwt_preemptive.detach (fun () -> run_sync request) ()

let outcome_result = function
  | Completed response when response.accepted -> Ok ()
  | Completed response -> Error response.reason
  | Timed_out -> Error "proof worker timed out"
  | Memory_exceeded -> Error "proof worker memory limit exceeded"
  | Failed reason -> Error ("proof worker failed: " ^ reason)

let result request =
  let open Lwt.Syntax in
  let* outcome = run request in
  Lwt.return (outcome_result outcome)

let result_sync request =
  run_sync request |> outcome_result

let ready_sync () =
  result_sync P.Ping

let ready () =
  result P.Ping

let verify_encrypt ~pubkey ~cipher ~amount ~proof ~commitment ~blinding =
  result
    (P.Encrypt {
       pubkey;
       cipher;
       amount;
       proof;
       commitment;
       blinding;
     })

let verify_claim ~pubkey ~cipher ~proof ~commitment =
  result (P.Claim { pubkey; cipher; proof; commitment })

let verify_key_switch_claim ~pubkey ~cipher ~proof ~commitment =
  result (P.Key_switch_claim { pubkey; cipher; proof; commitment })

let verify_range ~pubkey ~cipher ~proof =
  result (P.Range { pubkey; cipher; proof })

let verify_zero_sync ~pubkey ~cipher ~proof =
  result_sync (P.Zero { pubkey; cipher; proof })

let verify_claim_sync ~pubkey ~cipher ~proof ~commitment =
  result_sync (P.Claim { pubkey; cipher; proof; commitment })

let verify_range_sync ~pubkey ~cipher ~proof =
  result_sync (P.Range { pubkey; cipher; proof })

let verify_range_bound_sync ~pubkey ~cipher ~proof ~commitment =
  result_sync (P.Range_bound { pubkey; cipher; proof; commitment })

let verify_circle_cell
    ~pubkey
    ~cipher
    ~ciphertext_commitment
    ~proof_kind
    ~proof
    ~amount_commitment =
  result
    (P.Circle_cell {
       pubkey;
       cipher;
       ciphertext_commitment;
       proof_kind;
       proof;
       amount_commitment;
     })

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