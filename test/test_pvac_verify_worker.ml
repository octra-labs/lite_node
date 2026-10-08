(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module P = Octra_core.Pvac_verify_protocol
module W = Octra_core.Pvac_verify_worker
module FB = Octra_core.Crypto.FheBalance
module D = Octra_core.Pvac_verify_direct

let check name value =
  if not value then failwith name

let run name test =
  try test ()
  with error ->
    failwith (name ^ ": " ^ Printexc.to_string error)

let request =
  P.Range {
    pubkey = "invalid";
    cipher = "hfhe_v1|AA==";
    proof = "range_v1|AA==";
    strict = false;
  }

let key_switch_request =
  P.Key_switch_claim {
    pubkey = "invalid";
    cipher = "hfhe_v1|AA==";
    proof = "zero_v1|AA==";
    commitment = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
    strict = false;
  }

let historical_migration_request =
  P.Historical_migration_claim {
    pubkey = "invalid";
    cipher = "hfhe_v1|AA==";
    proof = "zero_v1|AA==";
    commitment = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
    strict = false;
  }

let circle_request =
  P.Circle_cell {
    pubkey = "invalid";
    cipher = "hfhe_v1|AA==";
    ciphertext_commitment = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
    proof_kind = P.Circle_bound_zero;
    proof = "zkzp_v2|AA==";
    amount_commitment = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
    strict = false;
  }

let with_env name value f =
  let previous = Sys.getenv_opt name in
  Unix.putenv name value;
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv name (Option.value ~default:"" previous))
    f

let sample_path () =
  Sys.executable_name

let timeout_config () =
  List.iter (fun raw ->
    with_env "OCTRA_PVAC_VERIFY_TIMEOUT_SEC" raw (fun () ->
      check "invalid worker deadline accepted" (W.timeout_seconds () = 600.)))
    ["nan"; "infinity"; "-infinity"; "0.5"; "1800.1"; "invalid"];
  List.iter (fun (raw, seconds) ->
    with_env "OCTRA_PVAC_VERIFY_TIMEOUT_SEC" raw (fun () ->
      check "worker deadline changed" (W.timeout_seconds () = seconds)))
    ["1", 1.; "1800", 1800.; "600", 600.]

let run_sample mode =
  match mode with
  | "old_session" ->
    if Array.length Sys.argv > 1 then Unix.sleepf 10.
    else begin
      let raw = Octra_core.Fhe_calc.read_request () in
      let request = match P.request_of_string raw with
        | Ok value -> value | Error reason -> failwith reason in
      D.response request |> P.response_bytes |> print_endline
    end
  | "protocol" ->
    let raw = Octra_core.Fhe_calc.read_request () in
    let request = match P.request_of_string raw with
      | Ok value -> value | Error reason -> failwith reason in
    P.response_bytes {request_hash = P.request_hash request; accepted = false; reason = "op_invalid"}
    |> print_endline
  | "sleep" ->
    Unix.sleepf 2.
  | "wait" ->
    let path = Sys.getenv "OCTRA_PVAC_WORKER_PID" in
    let staged = path ^ ".next" in
    let channel = open_out staged in
    Fun.protect ~finally:(fun () -> close_out channel)
      (fun () -> output_string channel (string_of_int (Unix.getpid ())));
    Unix.rename staged path;
    Unix.sleepf 10.
  | "malformed" ->
    print_endline "malformed"
  | "exit" ->
    exit 9
  | "memory" ->
    let bytes = Bytes.make (256 * 1024 * 1024) 'x' in
    for offset = 0 to (Bytes.length bytes / 4096) - 1 do
      Bytes.set bytes (offset * 4096) 'y'
    done;
    Unix.sleepf 5.;
    ignore (Bytes.get bytes (Bytes.length bytes - 1))
  | "flood" ->
    print_string (String.make 131_072 'x');
    flush stdout
  | "wrong_hash" ->
    print_endline
      "{\"schema\":\"octra_pvac_verify\",\"request_hash\":\"0000000000000000000000000000000000000000000000000000000000000000\",\"accepted\":true,\"reason\":\"\"}"
  | _ ->
    print_endline "{}"

let request_roundtrip name request =
  let raw = P.request_bytes request in
  match P.request_of_string raw with
  | Error error -> failwith (name ^ ": " ^ error)
  | Ok parsed ->
    check (name ^ " request hash changed")
      (P.request_hash parsed = P.request_hash request)

let protocol_roundtrip () =
  request_roundtrip "range" request;
  request_roundtrip "key switch" key_switch_request;
  request_roundtrip "historical migration" historical_migration_request;
  request_roundtrip "circle cell" circle_request;
  check "request domains collided"
    (P.request_hash request <> P.request_hash key_switch_request);
  check "circle request domain collided"
    (P.request_hash circle_request <> P.request_hash key_switch_request);
  check "historical migration request domain collided"
    (P.request_hash historical_migration_request <>
     P.request_hash key_switch_request)

let math_protocol () =
  List.iter (fun value ->
    request_roundtrip "math" (P.Math value);
    check "math request hash reused"
      (P.request_hash value <> P.request_hash (P.Math value));
    let json = P.request_bytes value |> Yojson.Safe.from_string in
    match json with
    | `Assoc fields ->
      check "math false changed prior bytes"
        (match P.request_of_json (`Assoc (("math", `Bool false) :: fields)) with
         | Ok parsed -> P.request_bytes parsed = P.request_bytes value
         | Error _ -> false);
      List.iter (fun extra ->
        check "invalid math accepted"
          (Result.is_error (P.request_of_json (`Assoc (extra @ fields)))))
        [["math", `String "true"]; ["math", `Bool true; "math", `Bool false]]
    | _ -> failwith "request object required")
    [P.Ping; request; key_switch_request; circle_request];
  List.iter (fun value ->
    let response = D.response value in
    check "math response hash differs" (response.request_hash = P.request_hash value);
    check "math ping rejected" response.accepted)
    [P.Ping; P.Math P.Ping; P.Ping; P.Math P.Ping]

let rss_status_parser () =
  check "tab separated RSS rejected"
    (W.rss_mb_of_status_line "VmRSS:\t65537 kB" = Some 65);
  check "space separated RSS rejected"
    (W.rss_mb_of_status_line "VmRSS: 1024 kB" = Some 1);
  check "unrelated status accepted"
    (W.rss_mb_of_status_line "VmSize:\t65537 kB" = None);
  check "invalid RSS accepted"
    (W.rss_mb_of_status_line "VmRSS:\tinvalid kB" = None)

let worker_capacity_disjoint () =
  let getenv values name = List.assoc_opt name values in
  check "pvac worker default changed"
    (W.capacity_of_getenv (getenv []) = 1);
  check "pvac worker capacity rejected"
    (W.capacity_of_getenv
       (getenv ["OCTRA_PVAC_VERIFY_WORKERS", "2"])
     = 2);
  check "pvac worker invalid capacity accepted"
    (W.capacity_of_getenv
       (getenv ["OCTRA_PVAC_VERIFY_WORKERS", "8"])
     = 1);
  check "legacy capacity still controls pvac worker"
    (W.capacity_of_getenv
       (getenv ["OCTRA_VERIFY_WORKERS", "2"])
     = 1);
  check "proof pool default changed"
    (Octra_core.Proof_pool.capacity_of_getenv (getenv []) = 2);
  check "proof pool capacity rejected"
    (Octra_core.Proof_pool.capacity_of_getenv
       (getenv ["OCTRA_PROOF_POOL_WORKERS", "8"])
     = 8);
  check "proof pool invalid capacity accepted"
    (Octra_core.Proof_pool.capacity_of_getenv
       (getenv ["OCTRA_PROOF_POOL_WORKERS", "9"])
     = 2);
  check "legacy capacity still controls proof pool"
    (Octra_core.Proof_pool.capacity_of_getenv
       (getenv ["OCTRA_VERIFY_WORKERS", "8"])
     = 2)

let health_succeeds () =
  match W.ready_sync () with
  | Ok () -> ()
  | Error reason -> failwith reason

let session_required () =
  with_env "OCTRA_PVAC_VERIFY_WORKER" (sample_path ()) (fun () ->
    with_env "OCTRA_PVAC_WORKER_SAMPLE_MODE" "old_session" (fun () ->
      check "old proof protocol unavailable" (W.result_sync P.Ping = Ok ());
      let started = W.monotonic_seconds () in
      check "old session passed startup" (W.ready_sync () = Error "worker_session_unavailable");
      check "session startup waited proof deadline" (W.monotonic_seconds () -. started < 7.)))

let actual_worker_rejects () =
  match Lwt_main.run (W.run request) with
  | W.Completed response ->
    check "invalid proof accepted" (not response.P.accepted);
    check "response hash changed"
      (response.request_hash = P.request_hash request)
  | W.Timed_out -> failwith "actual worker timed out"
  | W.Memory_exceeded -> failwith "actual worker exceeded memory"
  | W.Busy -> failwith "actual worker queue is full"
  | W.Unavailable reason -> failwith reason
  | W.Failed reason -> failwith reason

let valid_circle_request () =
  let params = Pvac_ffi.default_params () in
  let pubkey, seckey =
    Pvac_ffi.keygen_from_seed params (Bytes.make 32 '\041')
  in
  let amount = 17L in
  let cipher =
    Pvac_ffi.enc_value_seeded
      pubkey
      seckey
      amount
      (Bytes.make 32 '\042')
  in
  let blinding = Bytes.make 32 '\043' in
  let proof =
    Pvac_ffi.make_zero_proof_bound
      pubkey
      seckey
      cipher
      amount
      blinding
    |> FB.encode_zero_proof
  in
  let amount_commitment =
    Pvac_ffi.pedersen_commit_amount amount blinding
    |> Bytes.to_string
    |> Base64.encode_exn
  in
  P.Circle_cell {
    pubkey =
      Pvac_ffi.serialize_pubkey pubkey
      |> Bytes.to_string;
    cipher = FB.encode_cipher cipher;
    ciphertext_commitment =
      Pvac_ffi.commit_ct pubkey cipher
      |> Bytes.to_string
      |> Base64.encode_exn;
    proof_kind = P.Circle_bound_zero;
    proof;
    amount_commitment;
    strict = true;
  }

let actual_circle_worker_accepts () =
  let request = valid_circle_request () in
  begin
    match D.execute request with
    | Ok () -> ()
    | Error reason -> failwith ("valid direct circle proof rejected: " ^ reason)
  end;
  match Lwt_main.run (W.run request) with
  | W.Completed response ->
    if not response.P.accepted then
      failwith ("valid circle proof rejected: " ^ response.reason);
    check "circle response hash changed"
      (response.request_hash = P.request_hash request)
  | W.Timed_out -> failwith "valid circle worker timed out"
  | W.Memory_exceeded -> failwith "valid circle worker exceeded memory"
  | W.Busy -> failwith "valid circle worker queue is full"
  | W.Unavailable reason -> failwith reason
  | W.Failed reason -> failwith reason

let legacy_circle_proof_rejects () =
  match valid_circle_request () with
  | P.Circle_cell value ->
    let pubkey =
      match FB.load_pubkey_result value.pubkey with
      | Error reason -> failwith reason
      | Ok pubkey -> pubkey
    in
    let request =
      P.Circle_cell {
        value with
        pubkey =
          Pvac_ffi.serialize_pubkey_legacy_v2 pubkey
          |> Bytes.to_string;
      }
    in
    begin
      match D.execute request with
      | Error "pvac pubkey proof circuit lacks alias rejection" -> ()
      | Error reason -> failwith reason
      | Ok () -> failwith "legacy circle proof accepted"
    end
  | _ -> failwith "circle request changed kind"

let malformed_worker_fails () =
  with_env "OCTRA_PVAC_VERIFY_WORKER" (sample_path ()) (fun () ->
    with_env "OCTRA_PVAC_WORKER_SAMPLE_MODE" "malformed" (fun () ->
      match Lwt_main.run (W.run request) with
      | W.Failed _ -> ()
      | W.Completed _ -> failwith "malformed worker accepted"
      | W.Timed_out -> failwith "malformed worker timed out"
      | W.Memory_exceeded -> failwith "malformed worker exceeded memory"
      | W.Busy -> failwith "malformed worker queue is full"
      | W.Unavailable reason -> failwith reason))

let timeout_kills_worker () =
  with_env "OCTRA_PVAC_VERIFY_WORKER" (sample_path ()) (fun () ->
    with_env "OCTRA_PVAC_WORKER_SAMPLE_MODE" "sleep" (fun () ->
      with_env "OCTRA_PVAC_VERIFY_TIMEOUT_SEC" "1" (fun () ->
        match Lwt_main.run (W.run request) with
        | W.Timed_out -> ()
        | W.Completed _ -> failwith "sleeping worker completed"
        | W.Memory_exceeded -> failwith "sleeping worker exceeded memory"
        | W.Busy -> failwith "sleeping worker queue is full"
        | W.Unavailable reason -> failwith reason
        | W.Failed reason -> failwith reason)))

let timeout_keeps_lwt_responsive () =
  with_env "OCTRA_PVAC_VERIFY_WORKER" (sample_path ()) (fun () ->
    with_env "OCTRA_PVAC_WORKER_SAMPLE_MODE" "sleep" (fun () ->
      with_env "OCTRA_PVAC_VERIFY_TIMEOUT_SEC" "1" (fun () ->
        let ticks = ref 0 in
        let verify = W.run request in
        let rec heartbeat () =
          if Lwt.is_sleeping verify then begin
            incr ticks;
            let open Lwt.Syntax in
            let* () = Lwt_unix.sleep 0.05 in
            heartbeat ()
          end else
            Lwt.return_unit
        in
        let outcome, () =
          Lwt_main.run (Lwt.both verify (heartbeat ()))
        in
        begin
          match outcome with
          | W.Timed_out -> ()
          | W.Completed _ -> failwith "sleeping worker completed"
          | W.Memory_exceeded -> failwith "sleeping worker exceeded memory"
          | W.Busy -> failwith "sleeping worker queue is full"
          | W.Unavailable reason -> failwith reason
          | W.Failed reason -> failwith reason
        end;
        check "worker blocked Lwt heartbeat" (!ticks >= 5))))

let cancel_worker () =
  Test_workspace.with_dir "proof-cancel" (fun path ->
    let path = Filename.concat path "pid" in
    with_env "OCTRA_PVAC_VERIFY_WORKER" (sample_path ()) (fun () ->
      with_env "OCTRA_PVAC_WORKER_SAMPLE_MODE" "wait" (fun () ->
        with_env "OCTRA_PVAC_WORKER_PID" path (fun () ->
          with_env "OCTRA_PVAC_VERIFY_TIMEOUT_SEC" "20" (fun () ->
            let pending = W.run request in
            let rec wait () =
              let open Lwt.Syntax in
              if Sys.file_exists path then Lwt.return_unit
              else let* () = Lwt_unix.sleep 0.01 in wait () in
            Lwt_main.run (Lwt_unix.with_timeout 5. wait);
            let channel = open_in path in
            let pid = Fun.protect ~finally:(fun () -> close_in channel)
              (fun () -> int_of_string (input_line channel)) in
            let rec drain () =
              let open Lwt.Syntax in
              if (W.channel_stats ()).active = 0 then Lwt.return_unit
              else let* () = Lwt_unix.sleep 0.01 in drain () in
            Fun.protect ~finally:(fun () ->
              Lwt.cancel pending;
              Lwt_main.run (Lwt_unix.with_timeout 12. drain))
              (fun () ->
                check "worker did not start" ((W.channel_stats ()).active = 1);
                Lwt.cancel pending;
                check "worker cancellation lost"
                  (match Lwt.state pending with Lwt.Fail Lwt.Canceled -> true | _ -> false);
                let stopped = try
                  Lwt_main.run (Lwt_unix.with_timeout 2. drain);
                  true
                with Lwt_unix.Timeout -> false in
                check "cancelled worker retained its slot" stopped;
                let reaped = try Unix.kill pid 0; false with
                  | Unix.Unix_error (Unix.ESRCH, _, _) -> true in
                check "cancelled worker still runs" reaped)))))
  );
  let pipes = ref 0 in
  let pipe () = incr pipes; Unix.pipe ~cloexec:true () in
  let result = W.run_process ~pipe ~control:(Atomic.make W.Cancel) "unused" request in
  check "cancelled request started"
    (result = W.Unavailable "worker_cancelled" && !pipes = 0)

let memory_limit_kills_worker () =
  if Sys.file_exists "/proc" then
    with_env "OCTRA_PVAC_VERIFY_WORKER" (sample_path ()) (fun () ->
      with_env "OCTRA_PVAC_WORKER_SAMPLE_MODE" "memory" (fun () ->
        with_env "OCTRA_PVAC_VERIFY_MAX_RSS_MB" "64" (fun () ->
          with_env "OCTRA_PVAC_VERIFY_TIMEOUT_SEC" "5" (fun () ->
            match Lwt_main.run (W.run request) with
            | W.Memory_exceeded -> ()
            | W.Completed _ -> failwith "memory worker completed"
            | W.Timed_out -> failwith "memory worker timed out"
            | W.Busy -> failwith "memory worker queue is full"
            | W.Unavailable reason -> failwith reason
            | W.Failed reason -> failwith reason))))

let oversized_output_fails () =
  with_env "OCTRA_PVAC_VERIFY_WORKER" (sample_path ()) (fun () ->
    with_env "OCTRA_PVAC_WORKER_SAMPLE_MODE" "flood" (fun () ->
      match Lwt_main.run (W.run request) with
      | W.Failed _ -> ()
      | W.Completed _ -> failwith "oversized output accepted"
      | W.Timed_out -> failwith "oversized output timed out"
      | W.Memory_exceeded -> failwith "oversized output exceeded memory"
      | W.Busy -> failwith "oversized output worker queue is full"
      | W.Unavailable reason -> failwith reason))

let wrong_hash_fails () =
  with_env "OCTRA_PVAC_VERIFY_WORKER" (sample_path ()) (fun () ->
    with_env "OCTRA_PVAC_WORKER_SAMPLE_MODE" "wrong_hash" (fun () ->
      match Lwt_main.run (W.run request) with
      | W.Failed "response_hash_mismatch" -> ()
      | W.Failed reason -> failwith reason
      | W.Completed _ -> failwith "wrong response hash accepted"
      | W.Timed_out -> failwith "wrong response hash timed out"
      | W.Memory_exceeded -> failwith "wrong response hash exceeded memory"
      | W.Busy -> failwith "wrong response hash worker queue is full"
      | W.Unavailable reason -> failwith reason))

let exit_worker_fails () =
  with_env "OCTRA_PVAC_VERIFY_WORKER" (sample_path ()) (fun () ->
    with_env "OCTRA_PVAC_WORKER_SAMPLE_MODE" "exit" (fun () ->
      match Lwt_main.run (W.run request) with
      | W.Failed _ -> ()
      | W.Completed _ -> failwith "exited worker completed"
      | W.Timed_out -> failwith "exited worker timed out"
      | W.Memory_exceeded -> failwith "exited worker exceeded memory"
      | W.Busy -> failwith "exited worker queue is full"
      | W.Unavailable reason -> failwith reason))

let pipe_cleanup () =
  List.iter
    (fun at ->
      let calls = ref 0 in
      let descriptors = ref [] in
      let pipe () =
        incr calls;
        if !calls = at then raise (Unix.Unix_error (Unix.EMFILE, "pipe", ""));
        let read, write = Unix.pipe ~cloexec:true () in
        descriptors := read :: write :: !descriptors;
        read, write
      in
      let result = W.run_process ~pipe "unused" request in
      check "pipe failure accepted" (match result with W.Failed _ -> true | _ -> false);
      List.iter
        (fun fd ->
          let closed =
            try ignore (Unix.fstat fd); false
            with Unix.Unix_error (Unix.EBADF, _, _) -> true
          in
          check "pipe descriptor not released" closed)
        !descriptors)
    [2; 3]

let wide_descriptors () =
  let opened = ref [] in
  Fun.protect
    ~finally:(fun () -> List.iter Unix.close !opened)
    (fun () ->
      for _ = 0 to 1099 do
        opened := Unix.openfile "/dev/null" [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 :: !opened
      done;
      actual_worker_rejects ())

let () =
  match Sys.getenv_opt "OCTRA_PVAC_WORKER_SAMPLE_MODE" with
  | Some mode ->
    run_sample mode
  | None ->
    let worker = match W.worker_path () with
      | Some path -> path | None -> failwith "worker missing" in
    Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" worker;
    with_env "OCTRA_PVAC_WORKER_SAMPLE_MODE" "protocol" (fun () ->
      check "old worker became proof rejection"
        (W.run_process (sample_path ()) P.Ping = W.Unavailable "worker_protocol_op_invalid"));
    run "timeout_config" timeout_config;
    run "protocol_roundtrip" protocol_roundtrip;
    run "math_protocol" math_protocol;
    run "rss_status_parser" rss_status_parser;
    run "worker_capacity_disjoint" worker_capacity_disjoint;
    run "health_succeeds" health_succeeds;
    run "session_required" session_required;
    run "actual_worker_rejects" actual_worker_rejects;
    run "wide_descriptors" wide_descriptors;
    run "pipe_cleanup" pipe_cleanup;
    run "actual_circle_worker_accepts" actual_circle_worker_accepts;
    run "legacy_circle_proof_rejects" legacy_circle_proof_rejects;
    run "malformed_worker_fails" malformed_worker_fails;
    run "timeout_kills_worker" timeout_kills_worker;
    run "timeout_keeps_lwt_responsive" timeout_keeps_lwt_responsive;
    run "cancel_worker" cancel_worker;
    run "memory_limit_kills_worker" memory_limit_kills_worker;
    run "oversized_output_fails" oversized_output_fails;
    run "wrong_hash_fails" wrong_hash_fails;
    run "exit_worker_fails" exit_worker_fails;
    print_endline "status = pass test = pvac_verify_worker"