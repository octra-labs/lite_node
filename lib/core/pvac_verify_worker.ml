(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module P = Pvac_verify_protocol
module Channel = Compute_pool

type priority = Channel.priority =
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

type control =
  | Continue
  | Cancel

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

let proof_channel =
  Channel.create
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
        if not (Float.is_finite value) || value < lower || value > upper then default
        else value
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

let executable_paths () =
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
  | None -> List.find_opt executable (executable_paths ())

let monotonic_seconds () =
  Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1_000_000_000.

external wait_io :
  Unix.file_descr -> Unix.file_descr -> Unix.file_descr -> int -> int
  = "octra_io_wait"

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
  let rec reap () =
    try ignore (Unix.waitpid [] pid)
    with
    | Unix.Unix_error (Unix.EINTR, _, _) -> reap ()
    | Unix.Unix_error (Unix.ECHILD, _, _) -> ()
  in
  reap ()

let close_stream ?(close = close_noerr) stream =
  if stream.open_ then begin
    stream.open_ <- false;
    close stream.fd
  end

let append stream limit bytes count =
  if count > limit - Buffer.length stream.data then Error "output_too_large"
  else begin
    Buffer.add_subbytes stream.data bytes 0 count;
    Ok ()
  end

let rec read_available ?(close = close_noerr) stream limit bytes =
  if not stream.open_ then Ok ()
  else
    try
      match Unix.read stream.fd bytes 0 (Bytes.length bytes) with
      | 0 ->
        close_stream ~close stream;
        Ok ()
      | count ->
        begin
          match append stream limit bytes count with
          | Error _ as error -> error
          | Ok () -> Ok ()
        end
    with
    | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> Ok ()
    | Unix.Unix_error (Unix.EINTR, _, _) -> read_available ~close stream limit bytes
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

let exchange ?(control = Atomic.make Continue)
    ?(pipe = Unix.pipe ~cloexec:true) ?(arguments = [])
    ?(limit = P.max_response_bytes) ?(deadline = Float.infinity) worker raw =
  if Atomic.get control = Cancel then Error (Unavailable "worker_cancelled")
  else if Float.is_nan deadline || deadline <= monotonic_seconds () then Error Timed_out
  else if limit < 0 || limit > Sys.max_string_length then Error (Failed "output_limit")
  else
  let descriptors = ref [] in
  let acquire () =
    let read, write = pipe () in
    descriptors := read :: write :: !descriptors;
    read, write
  in
  let close fd =
    if List.mem fd !descriptors then begin
      descriptors := List.filter ((<>) fd) !descriptors;
      close_noerr fd
    end
  in
  let close_all () =
    List.iter close !descriptors
  in
  let pid_ref = ref None in
  let stop () =
    Option.iter terminate !pid_ref;
    pid_ref := None
  in
  try
    let input_read, input_write = acquire () in
    let output_read, output_write = acquire () in
    let error_read, error_write = acquire () in
    let pid =
      Unix.create_process_env
        worker
        (Array.of_list (worker :: arguments))
        (Unix.environment ())
        input_read
        output_write
        error_write
    in
    pid_ref := Some pid;
    close input_read;
    close output_write;
    close error_write;
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
      if !input_open then close input_write;
      input_open := false;
      close_stream ~close output;
      close_stream ~close error;
      outcome
    in
    let fail reason =
      stop ();
      finish (Error (Failed reason))
    in
    let rec loop status =
      let elapsed = monotonic_seconds () -. started in
      if Atomic.get control = Cancel then begin
        stop ();
        finish (Error (Unavailable "worker_cancelled"))
      end else if elapsed > timeout_seconds () || monotonic_seconds () >= deadline then begin
        stop ();
        finish (Error Timed_out)
      end else
        match read_rss_mb pid with
        | Some rss when rss > max_rss_mb () ->
          stop ();
          finish (Error Memory_exceeded)
        | Some _ | None ->
          let status =
            match status with
            | Some _ -> status
            | None ->
              let status = poll_status pid in
              if Option.is_some status then pid_ref := None;
              status
          in
          if Option.is_some status && !input_open then begin
            close input_write;
            input_open := false
          end;
          if
            Option.is_some status
            && not output.open_
            && not error.open_
          then
            match status with
            | Some value ->
              finish (Ok (Buffer.contents output.data, Buffer.contents error.data, value))
            | None ->
              fail "worker_status_missing"
          else
            let mask =
              (if output.open_ then 1 else 0)
              lor (if error.open_ then 2 else 0)
              lor (if !input_open then 4 else 0)
            in
            let ready = wait_io input_write output_read error_read mask in
            let read_result =
              List.fold_left
                (fun result (bit, stream, limit) ->
                  match result with
                  | Error _ -> result
                  | Ok () when
                      stream.open_
                      && ready land bit <> 0 ->
                    read_available ~close stream limit bytes
                  | Ok () -> Ok ())
                (Ok ())
                [1, output, limit; 2, error, P.max_response_bytes]
            in
            begin
              match read_result with
              | Error reason -> fail reason
              | Ok () ->
                if !input_open && ready land 4 <> 0 then
                  begin
                    match write_available input_write raw input_offset with
                    | Error reason -> fail reason
                    | Ok true ->
                      close input_write;
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
    stop ();
    close_all ();
    Error (Failed (Printexc.to_string error))

let run_process ?control ?pipe ?deadline worker request =
  if Option.fold ~none:false ~some:(fun signal -> Atomic.get signal = Cancel) control then
    Unavailable "worker_cancelled"
  else
    let raw = P.request_bytes request in
    let expected_hash = P.request_hash request in
    match exchange ?control ?pipe ?deadline worker raw with
    | Ok (stdout, stderr, status) ->
      begin match process_response expected_hash stdout stderr status with
      | Completed response when not response.accepted
          && List.mem response.reason ["op_invalid"; "schema_invalid"; "math_invalid";
            "request_invalid"; "request_json_invalid"; "request_too_large"] ->
        Unavailable ("worker_protocol_" ^ response.reason)
      | outcome -> outcome
      end
    | Error outcome -> outcome

type session = {
  program : string;
  pid : int;
  input : Unix.file_descr;
  output : Unix.file_descr;
  key : (string * string) option;
}

let close_session session =
  terminate session.pid;
  close_noerr session.input;
  close_noerr session.output

let retire_pid pid =
  begin try Unix.kill pid Sys.sigkill
  with Unix.Unix_error (Unix.ESRCH, _, _) -> () end;
  let rec reap () =
    Lwt.catch
      (fun () -> Lwt.map (fun _ -> ()) (Lwt_unix.waitpid [] pid))
      (function
        | Unix.Unix_error (Unix.EINTR, _, _) -> reap ()
        | Unix.Unix_error (Unix.ECHILD, _, _) -> Lwt.return_unit
        | error -> Lwt.fail error) in
  Lwt.no_cancel (reap ())

let retire_session session =
  let closed = retire_pid session.pid in
  close_noerr session.input;
  close_noerr session.output;
  closed

let open_session ?(on_exit = terminate) program =
  let descriptors = ref [] in
  let pipe () =
    let left, right = Unix.pipe ~cloexec:true () in
    descriptors := left :: right :: !descriptors;
    left, right in
  let pid = ref None in
  try
    let input, write = pipe () in
    let read, output = pipe () in
    let error = Unix.openfile "/dev/null" [Unix.O_WRONLY; Unix.O_CLOEXEC] 0 in
    descriptors := error :: !descriptors;
    Unix.set_nonblock write;
    Unix.set_nonblock read;
    let child = Unix.create_process_env program [|program; "--fhe-session"|]
      (Unix.environment ()) input output error in
    pid := Some child;
    List.iter close_noerr [input; output; error];
    descriptors := [write; read];
    {program; pid = child; input = write; output = read; key = None}
  with error ->
    Option.iter on_exit !pid;
    List.iter close_noerr !descriptors;
    raise error

let session_exchange ~control ~deadline session raw =
  let header = Bytes.create 8 in
  Bytes.set_int64_be header 0 (Int64.of_int (String.length raw));
  let request = Bytes.to_string header ^ raw in
  let sent = ref 0 in
  let received = ref 0 in
  let data = ref (Bytes.create 8) in
  let framed = ref false in
  let rec loop () =
    if Atomic.get control = Cancel then Error (Unavailable "worker_cancelled")
    else if monotonic_seconds () >= deadline then Error Timed_out
    else if Option.fold ~none:false ~some:(fun rss -> rss > max_rss_mb ())
        (read_rss_mb session.pid) then Error Memory_exceeded
    else if !framed && !received = Bytes.length !data && !sent = String.length request then
      Ok (Bytes.unsafe_to_string !data)
    else begin
      let mask = (if !received < Bytes.length !data then 1 else 0)
        lor (if !sent < String.length request then 4 else 0) in
      let ready = wait_io session.input session.output session.output mask in
      if ready land 4 <> 0 then
        sent := !sent + Unix.write_substring session.input request !sent
          (min 65_536 (String.length request - !sent));
      if ready land 1 <> 0 then begin
        let count = Unix.read session.output !data !received (Bytes.length !data - !received) in
        if count = 0 then raise End_of_file;
        received := !received + count;
        if not !framed && !received = 8 then begin
          let length = Bytes.get_int64_be !data 0 in
          if length <= 0L || length > Int64.of_int Fhe_calc.max_bytes then
            failwith "worker frame size";
          data := Bytes.create (Int64.to_int length);
          received := 0;
          framed := true
        end
      end;
      loop ()
    end in
  try loop () with
  | Out_of_memory -> Error Memory_exceeded
  | error -> Error (Failed (Printexc.to_string error))

let run_unmanaged ?control request =
  match worker_path () with
  | None -> Unavailable "worker_missing"
  | Some worker -> run_process ?control worker request

let run_sync ?(priority = Required) request =
  match
    Channel.run_sync proof_channel priority (fun () -> run_unmanaged request)
  with
  | Some outcome -> outcome
  | None -> Busy

let try_run_sync ?(priority = Speculative) request =
  match
    Channel.try_run_sync proof_channel priority (fun () -> run_unmanaged request)
  with
  | Some outcome -> outcome
  | None -> Busy

let run ?(priority = Required) request =
  let control = Atomic.make Continue in
  let pending = Channel.run_threaded proof_channel priority
    (run_unmanaged ~control) request in
  Lwt.on_cancel pending (fun () -> Atomic.set control Cancel);
  let open Lwt.Syntax in
  let* outcome = pending in
  match outcome with
  | Some value -> Lwt.return value
  | None -> Lwt.return Busy

let channel_stats () =
  Channel.stats proof_channel

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

let classified_result ?(math = false) ?(priority = Required) request =
  let open Lwt.Syntax in
  let* outcome = run ~priority (if math then P.Math request else request) in
  Lwt.return (classified_outcome_result outcome)

let result ?(math = false) ?(priority = Required) request =
  let open Lwt.Syntax in
  let* outcome = run ~priority (if math then P.Math request else request) in
  Lwt.return (outcome_result outcome)

let result_sync ?(math = false) ?(priority = Required) request =
  run_sync ~priority (if math then P.Math request else request) |> outcome_result

let classified_result_sync ?(math = false) ?(priority = Required) request =
  run_sync ~priority (if math then P.Math request else request) |> classified_outcome_result

let try_classified_result_sync ?(math = false) ?(priority = Speculative) request =
  try_run_sync ~priority (if math then P.Math request else request) |> classified_outcome_result

let try_result_sync ?(math = false) ?(priority = Speculative) request =
  try_classified_result_sync ~math ~priority request
  |> Result.map_error verification_failure_message

let session_ready () =
  match worker_path () with
  | None -> Error "worker_missing"
  | Some worker ->
    try
      let session = open_session worker in
      Fun.protect ~finally:(fun () -> close_session session) (fun () ->
        let deadline = monotonic_seconds () +. 5. in
        let check index =
          let raw = Fhe_calc.request_bytes
            (Fhe_calc.Read_cipher (true, true, string_of_int index)) in
          match session_exchange ~control:(Atomic.make Continue) ~deadline session raw with
          | Ok reply when Fhe_calc.response_of_bytes (Fhe_calc.hash raw) reply
              = Ok (Error Fhe_calc.Invalid) -> Ok ()
          | _ -> Error "worker_session_unavailable" in
        Result.bind (check 0) (fun () -> check 1))
    with _ -> Error "worker_session_unavailable"

let ready_sync () =
  Result.bind (result_sync P.Ping) session_ready

let ready () =
  result P.Ping

let verify_encrypt_with_priority ~math
    priority
    ~strict
    ~pubkey
    ~cipher
    ~amount
    ~proof
    ~commitment
    ~blinding =
  result ~math ~priority
    (P.Encrypt {
       pubkey;
       cipher;
       amount;
       proof;
       commitment;
       blinding;
       strict;
     })

let verify_encrypt_classified_with_priority ~math
    priority
    ~strict
    ~pubkey
    ~cipher
    ~amount
    ~proof
    ~commitment
    ~blinding =
  classified_result ~math ~priority
    (P.Encrypt {
       pubkey;
       cipher;
       amount;
       proof;
       commitment;
       blinding;
       strict;
     })

let verify_encrypt ~math
    ~strict
    ~pubkey
    ~cipher
    ~amount
    ~proof
    ~commitment
    ~blinding =
  verify_encrypt_with_priority ~math
    Required
    ~strict
    ~pubkey
    ~cipher
    ~amount
    ~proof
    ~commitment
    ~blinding

let verify_claim_with_priority ~math
    priority
    ~strict
    ~pubkey
    ~cipher
    ~proof
    ~commitment =
  result ~math ~priority (P.Claim { pubkey; cipher; proof; commitment; strict })

let verify_claim ~math ~strict ~pubkey ~cipher ~proof ~commitment =
  verify_claim_with_priority ~math
    Required
    ~strict
    ~pubkey
    ~cipher
    ~proof
    ~commitment

let verify_claim_classified_with_priority ~math
    priority
    ~strict
    ~pubkey
    ~cipher
    ~proof
    ~commitment =
  classified_result ~math ~priority
    (P.Claim { pubkey; cipher; proof; commitment; strict })

let verify_claim_classified ~math ~strict ~pubkey ~cipher ~proof ~commitment =
  verify_claim_classified_with_priority ~math
    Required
    ~strict
    ~pubkey
    ~cipher
    ~proof
    ~commitment

let verify_key_switch_claim_with_priority ~math
    priority
    ~strict
    ~pubkey
    ~cipher
    ~proof
    ~commitment =
  result ~math ~priority
    (P.Key_switch_claim { pubkey; cipher; proof; commitment; strict })

let verify_key_switch_claim ~math ~strict ~pubkey ~cipher ~proof ~commitment =
  verify_key_switch_claim_with_priority ~math
    Required
    ~strict
    ~pubkey
    ~cipher
    ~proof
    ~commitment

let verify_key_switch_claim_classified_with_priority ~math
    priority
    ~strict
    ~pubkey
    ~cipher
    ~proof
    ~commitment =
  classified_result ~math ~priority
    (P.Key_switch_claim { pubkey; cipher; proof; commitment; strict })

let verify_key_switch_claim_classified ~math
    ~strict
    ~pubkey
    ~cipher
    ~proof
    ~commitment =
  verify_key_switch_claim_classified_with_priority ~math
    Required
    ~strict
    ~pubkey
    ~cipher
    ~proof
    ~commitment

let verify_historical_migration_claim_with_priority ~math
    priority
    ~strict
    ~pubkey
    ~cipher
    ~proof
    ~commitment =
  result ~math ~priority
    (P.Historical_migration_claim {
       pubkey;
       cipher;
       proof;
       commitment;
       strict;
     })

let verify_historical_migration_claim ~math
    ~strict
    ~pubkey
    ~cipher
    ~proof
    ~commitment =
  verify_historical_migration_claim_with_priority ~math
    Required
    ~strict
    ~pubkey
    ~cipher
    ~proof
    ~commitment

let verify_historical_migration_claim_classified_with_priority ~math
    priority
    ~strict
    ~pubkey
    ~cipher
    ~proof
    ~commitment =
  classified_result ~math ~priority
    (P.Historical_migration_claim {
       pubkey;
       cipher;
       proof;
       commitment;
       strict;
     })

let verify_historical_migration_claim_classified ~math
    ~strict
    ~pubkey
    ~cipher
    ~proof
    ~commitment =
  verify_historical_migration_claim_classified_with_priority ~math
    Required
    ~strict
    ~pubkey
    ~cipher
    ~proof
    ~commitment

let verify_range_with_priority ~math priority ~strict ~pubkey ~cipher ~proof =
  result ~math ~priority (P.Range { pubkey; cipher; proof; strict })

let verify_range_classified_with_priority ~math priority ~strict ~pubkey ~cipher ~proof =
  classified_result ~math ~priority (P.Range { pubkey; cipher; proof; strict })

let verify_range ~math ~strict ~pubkey ~cipher ~proof =
  verify_range_with_priority ~math Required ~strict ~pubkey ~cipher ~proof

let verify_zero_sync ~math ~pubkey ~cipher ~proof =
  result_sync ~math (P.Zero { pubkey; cipher; proof })

let verify_zero_sync_classified ~math ~pubkey ~cipher ~proof =
  classified_result_sync ~math (P.Zero { pubkey; cipher; proof })

let verify_claim_sync ~math ~strict ~pubkey ~cipher ~proof ~commitment =
  result_sync ~math (P.Claim { pubkey; cipher; proof; commitment; strict })

let verify_claim_sync_classified ~math ~strict ~pubkey ~cipher ~proof ~commitment =
  classified_result_sync ~math
    (P.Claim { pubkey; cipher; proof; commitment; strict })

let verify_range_sync ~math ~strict ~pubkey ~cipher ~proof =
  result_sync ~math (P.Range { pubkey; cipher; proof; strict })

let verify_range_bound_sync ~math ~strict ~pubkey ~cipher ~proof ~commitment =
  result_sync ~math (P.Range_bound { pubkey; cipher; proof; commitment; strict })

let verify_range_bound_sync_classified ~math
    ~strict
    ~pubkey
    ~cipher
    ~proof
    ~commitment =
  classified_result_sync ~math
    (P.Range_bound { pubkey; cipher; proof; commitment; strict })

let try_verify_range_bound_sync_classified ~math
    ~strict
    ~pubkey
    ~cipher
    ~proof
    ~commitment =
  try_classified_result_sync ~math
    (P.Range_bound { pubkey; cipher; proof; commitment; strict })

let try_verify_zero_sync ~math ~pubkey ~cipher ~proof =
  try_result_sync ~math (P.Zero { pubkey; cipher; proof })

let try_verify_zero_sync_classified ~math ~pubkey ~cipher ~proof =
  try_classified_result_sync ~math (P.Zero { pubkey; cipher; proof })

let try_verify_claim_sync ~math ~strict ~pubkey ~cipher ~proof ~commitment =
  try_result_sync ~math (P.Claim { pubkey; cipher; proof; commitment; strict })

let try_verify_claim_sync_classified ~math ~strict ~pubkey ~cipher ~proof ~commitment =
  try_classified_result_sync ~math
    (P.Claim { pubkey; cipher; proof; commitment; strict })

let try_verify_range_sync ~math ~strict ~pubkey ~cipher ~proof =
  try_result_sync ~math (P.Range { pubkey; cipher; proof; strict })

let try_verify_range_sync_classified ~math ~strict ~pubkey ~cipher ~proof =
  try_classified_result_sync ~math (P.Range { pubkey; cipher; proof; strict })

let verify_circle_cell_with_priority ~math
    priority
    ~strict
    ~pubkey
    ~cipher
    ~ciphertext_commitment
    ~proof_kind
    ~proof
    ~amount_commitment =
  result ~math ~priority
    (P.Circle_cell {
       pubkey;
       cipher;
       ciphertext_commitment;
       proof_kind;
       proof;
       amount_commitment;
       strict;
     })

let verify_circle_cell ~math
    ~strict
    ~pubkey
    ~cipher
    ~ciphertext_commitment
    ~proof_kind
    ~proof
    ~amount_commitment =
  verify_circle_cell_with_priority ~math
    Required
    ~strict
    ~pubkey
    ~cipher
    ~ciphertext_commitment
    ~proof_kind
    ~proof
    ~amount_commitment

let verify_circle_cell_sync ~math
    ~strict
    ~pubkey
    ~cipher
    ~ciphertext_commitment
    ~proof_kind
    ~proof
    ~amount_commitment =
  result_sync ~math
    (P.Circle_cell {
       pubkey;
       cipher;
       ciphertext_commitment;
       proof_kind;
       proof;
       amount_commitment;
       strict;
     })