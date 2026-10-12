(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Native = Octra_core.Circle_wasm_native

type session

external step : Native.build -> string -> int * string = "probe_build_step"
external mode : int -> unit = "probe_input_mode"
external waiting : unit -> bool = "probe_input_wait"
external resume : unit -> unit = "probe_input_resume"
external live : unit -> int = "probe_input_live"
external run_json_native : bool -> string -> int * string = "probe_run_json"
type quota
external hold : int -> quota = "probe_input_hold"
external drop : quota -> unit = "probe_input_drop"
external room : int -> int = "probe_input_room"
external call_new : unit -> session = "probe_call_new"
external call_step : session -> string -> int * string = "probe_call_step"
external call_close : session -> unit = "probe_call_close"
external call_refs : unit -> int = "probe_call_refs"
external probe_bridge : unit -> unit = "probe_hfhe_bridge"
external live_bridge : unit -> unit = "caml_octra_circle_hfhe_bridge"
external hfhe_call : unit -> int = "probe_hfhe_call"
external hfhe_returned : unit -> bool = "probe_hfhe_returned"
external hfhe_signal : unit -> unit = "probe_hfhe_signal"

let fail reason = failwith reason

let run_json = run_json_native false

let input_limit = 128 * 1024 * 1024

let clean ?(sessions = 0) () =
  Gc.full_major ();
  Gc.full_major ();
  if live () <> sessions then fail "native resources retained";
  if room input_limit <> 0 then fail "native input quota retained"

let with_port run =
  let port = Native.build_new () in
  Fun.protect (fun () -> run port)
    ~finally:(fun () -> mode 0; resume (); Native.build_close port; clean ())

let payload = "{\"action\":\"unknown\"}"

let test_input () = with_port (fun port ->
  List.iter (fun input ->
    if step port input <> (1, "compute storage input refused") then
      fail "invalid input result differs";
    clean ()) [""; String.make (16 * 1024 * 1024 + 1) ' '];
  if Native.build_begin port (String.make 64 '8') <> 0 then fail "build begin failed";
  let full = payload ^ String.make (16 * 1024 * 1024 - String.length payload) ' ' in
  List.iter (fun input ->
    if step port input <> (1, "compute storage action refused") then
      fail "valid input result differs";
    clean ()) [payload; full];
  mode 4;
  if step port payload <> (1, "compute memory busy") then fail "quota refusal differs";
  clean ();
  mode 5;
  begin try
    ignore (step port payload);
    fail "allocation failure accepted"
  with Failure reason when reason = "octra_circle_wasm_host: alloc failed" -> () end;
  clean ();
  if step port payload <> (1, "compute storage action refused") then fail "retry failed";
  clean ())

let test_signal () = with_port (fun port ->
  let old = Sys.signal Sys.sigusr1 (Sys.Signal_handle (fun _ -> fail "build signal")) in
  Fun.protect (fun () ->
    mode 1;
    begin try ignore (step port payload); fail "signal was not delivered"
    with Failure reason when reason = "build signal" -> () end;
    clean ())
    ~finally:(fun () -> Sys.set_signal Sys.sigusr1 old))

let test_response () = with_port (fun port ->
  mode 2;
  begin try ignore (step port payload); fail "response allocation accepted"
  with Out_of_memory -> () end;
  clean ())

let test_gc_close () = with_port (fun port ->
  mode 3;
  let result = ref None in
  let worker = Thread.create (fun () ->
    result := Some (try Ok (step port payload) with error -> Error error)) () in
  Fun.protect (fun () ->
    let deadline = Unix.gettimeofday () +. 3. in
    while not (waiting ()) do
      if Unix.gettimeofday () > deadline then fail "build pause not reached";
      Thread.delay 0.001
    done;
    Gc.compact ();
    Native.build_close port;
    resume ();
    Thread.join worker;
    begin match !result with
    | Some (Ok (1, "compute storage port closed")) -> ()
    | _ -> fail "closed build result differs"
    end;
    clean ())
    ~finally:(fun () -> resume (); Thread.join worker))

let bytes values = String.of_seq (List.to_seq (List.map Char.chr values))

let rec uint value =
  if value < 128 then bytes [value]
  else bytes [128 lor (value land 127)] ^ uint (value lsr 7)

let text value = uint (String.length value) ^ value

let guest ?(hfhe = false) invokes =
  let section tag value = bytes [tag] ^ text value in
  let export name kind index = text name ^ bytes [kind; index] in
  let request = if hfhe then "OCWR1\000\016fhe_encrypt_zero\000\001\004\000\000\000\000"
    else "OCWR1\000\010call_value\000\000" in
  let import = if hfhe then "host_hfhe_invoke_len" else "host_circle_invoke_len" in
  let update = bytes (if invokes then
    [0; 0x41; 0; 0x41; String.length request; 0x10; 0; 0x1a; 0x41; 0; 0x0b]
    else [0; 0x41; 0; 0x0b]) in
  let code = "\000asm\001\000\000\000"
    ^ section 1 (bytes [2; 0x60; 2; 0x7f; 0x7f; 1; 0x7f;
      0x60; 1; 0x7f; 1; 0x7f])
    ^ section 2 (bytes [1] ^ text "octra" ^ text import ^ bytes [0; 0])
    ^ section 3 (bytes [2; 1; 0])
    ^ section 5 (bytes [1; 1; 1; 1])
    ^ section 7 (bytes [3] ^ export "memory" 2 0
      ^ export "octra_alloc" 0 1 ^ export "octra_update" 0 2)
    ^ section 10 (bytes [2] ^ text (bytes [0; 0x41; 0x80; 0x20; 0x0b]) ^ text update)
    ^ section 11 (bytes [1; 0; 0x41; 0; 0x0b] ^ text request) in
  Yojson.Safe.to_string (`Assoc [
    "action", `String "execute"; "code_b64", `String (Base64.encode_exn code);
    "export_name", `String "octra_update"; "request_b64", `String "";
    "storage_pairs", `List []; "hfhe_strict", `Bool false; "is_view", `Bool false;
    "update_policy", `Bool true; "execution_profile", `String "standard";
    "hfhe_caps", `List (if hfhe then [`String "fhe_encrypt"] else []);
    "hfhe_active_key", `Assoc ["key_id", `String "test";
      "pubkey_b64", `String ""; "seckey_b64", `String ""];
    "fuel_limit", `Int (if hfhe then 10_000_000 else 2_000_000)])

let with_session run =
  let session = call_new () in
  Fun.protect (fun () -> run session)
    ~finally:(fun () -> mode 0; resume (); call_close session; clean ())

let reply raw =
  let id = Yojson.Safe.Util.member "id" (Yojson.Safe.from_string raw) in
  Yojson.Safe.to_string (`Assoc ["id", id;
    "response_b64", `String (Base64.encode_exn "OCWS1\000\000\000\000\000");
    "effort", `Int 10; "storage_pairs", `List []])

let test_run_controls () =
  let good = "{\"action\":\"compute_self_test\"}" in
  let local = match Yojson.Safe.from_string (guest false) with
    | `Assoc fields -> Yojson.Safe.to_string (`Assoc (
        ("is_view", `Bool true) :: ("inspect_policy", `Bool true) ::
        List.remove_assoc "is_view" fields))
    | _ -> fail "view payload invalid" in
  let first = run_json good in
  if fst first <> 0 then fail "native run control failed";
  List.iter (fun input ->
    let expected = run_json input in
    clean ();
    mode 2;
    begin try ignore (run_json input); fail "native response allocation accepted"
    with Out_of_memory -> () end;
    clean ();
    if run_json input <> expected then fail "native run retry differs";
    clean ()) [good; local; payload; ""];
  if run_json good <> first then fail "native run result changed";
  mode 6;
  begin try ignore (run_json payload); fail "native input allocation accepted"
  with Failure reason when reason = "octra_circle_wasm_host: alloc failed" -> () end;
  clean ()

let test_call_controls () =
  List.iter (fun (input, expected_code) -> with_session (fun session ->
    let code, raw = call_step session input in
    if code <> expected_code then fail ("session control failed: " ^ raw);
    clean ~sessions:1 ();
    let next, _ = call_step session (if input = "" then guest false else payload) in
    if next <> (if input = "" then 0 else 2) then fail "session state changed";
    clean ~sessions:1 ())) [guest false, 0; payload, 1; "", 2];
  with_session (fun session ->
    let code, raw = call_step session (guest true) in
    if code <> 3 then fail ("session did not suspend: " ^ raw);
    clean ~sessions:1 ();
    let code, raw = call_step session (reply raw) in
    if code <> 0 || Yojson.Safe.Util.member "success" (Yojson.Safe.from_string raw) <> `Bool true
    then fail "resumed session failed";
    clean ~sessions:1 ());
  List.iter (fun input -> with_session (fun session ->
    mode 2;
    begin try ignore (call_step session input); fail "session response allocation accepted"
    with Out_of_memory -> () end;
    clean ~sessions:1 ())) [guest false; guest true; payload];
  with_session (fun session ->
    call_close session;
    call_close session;
    if fst (call_step session payload) <> 2 then fail "closed session accepted";
    clean ())

let test_call_size () =
  List.iter (fun size -> with_session (fun session ->
    let input = guest false in
    let input = input ^ String.make (size - String.length input) ' ' in
    if fst (call_step session input) <> 0 then fail "session input limit reduced";
    clean ~sessions:1 ())) [16_777_217; 67_108_863; 67_108_864]

let test_call_preflight () =
  let oversized = String.make 67_108_865 ' ' in
  let refuse ?(sessions = 1) session input =
    mode 6;
    let result = Fun.protect (fun () -> call_step session input)
      ~finally:(fun () -> mode 0) in
    if result <> (2, "circle session input invalid") then fail "session input result changed";
    clean ~sessions () in
  with_session (fun session ->
    List.iter (refuse session) [""; oversized];
    if fst (call_step session (guest false)) <> 0 then fail "input refusal closed session";
    List.iter (refuse session) [""; oversized];
    if call_step session payload <> (2, "circle session closed") then
      fail "input refusal reopened session");
  with_session (fun session ->
    let code, raw = call_step session (guest true) in
    if code <> 3 then fail "session did not suspend";
    List.iter (refuse session) [""; oversized];
    let input = reply raw in
    let input = input ^ String.make (67_108_864 - String.length input) ' ' in
    if fst (call_step session input) <> 0 then fail "input refusal changed suspended session";
    clean ~sessions:1 ());
  with_session (fun session ->
    call_close session;
    List.iter (refuse ~sessions:0 session) [""; payload; oversized]);
  with_session (fun session ->
    mode 6;
    begin try ignore (call_step session (guest false)); fail "session allocation accepted"
    with Failure reason when reason = "octra_circle_wasm_host: alloc failed" -> () end;
    clean ~sessions:1 ();
    if fst (call_step session (guest false)) <> 0 then fail "allocation failure changed session";
    clean ~sessions:1 ())

let test_native_signal () =
  let old = Sys.signal Sys.sigusr1 (Sys.Signal_handle (fun _ -> fail "native signal")) in
  let check sessions run =
    mode 1;
    begin try ignore (run ()); fail "native signal was not delivered"
    with Failure reason when reason = "native signal" -> () end;
    clean ~sessions () in
  Fun.protect (fun () ->
    List.iter (fun input -> check 0 (fun () -> run_json input)) [payload; ""];
    with_session (fun session ->
      check 1 (fun () -> call_step session payload);
      if fst (call_step session (guest false)) <> 0 then fail "signal changed session";
      clean ~sessions:1 ()))
    ~finally:(fun () -> Sys.set_signal Sys.sigusr1 old)

let paused run inspect =
  mode 3;
  let result = ref None in
  let worker = Thread.create (fun () ->
    result := Some (try Ok (run ()) with error -> Error error)) () in
  Fun.protect (fun () ->
    let deadline = Unix.gettimeofday () +. 3. in
    while not (waiting ()) do
      if Unix.gettimeofday () > deadline then fail "native pause not reached";
      Thread.delay 0.001
    done;
    Gc.compact ();
    inspect ();
    resume ();
    Thread.join worker;
    match !result with
    | Some (Ok result) -> result
    | _ -> fail "paused native call failed")
    ~finally:(fun () -> resume (); Thread.join worker)

let with_quota size run =
  let quota = hold size in
  Fun.protect run ~finally:(fun () -> drop quota; clean ())

let test_input_queue () =
  let module Host = Octra_core.Circle_wasm_host in
  let body = String.make (1024 * 1024) 'q' in
  let payload = `Assoc ["action", `String "unknown"; "body", `String body] in
  let expected = Lwt_main.run (Host.read_process_json ~optional:true payload) in
  let bad = `Float nan in
  let error = try ignore (Yojson.Safe.to_string bad); fail "invalid JSON was encoded"
    with Yojson.Json_error reason -> reason in
  List.iter (fun call ->
    try ignore (Host.read_process_json ?call bad); fail "required JSON error was delayed"
    with Yojson.Json_error reason when reason = error -> ())
    [None; Some (fun _ -> Lwt.return `Null)];
  let limits = Lwt_preemptive.get_bounds () in
  Lwt_preemptive.set_bounds (0, 1);
  let started = Atomic.make false in
  let stopped = Atomic.make false in
  let blocker = Lwt_preemptive.detach (fun () ->
    Atomic.set started true;
    while not (Atomic.get stopped) do Thread.delay 0.001 done) () in
  let deadline = Unix.gettimeofday () +. 3. in
  let rec await () =
    if Atomic.get started then Lwt.return_unit
    else if Unix.gettimeofday () > deadline then Lwt.fail_with "queue pause not reached"
    else Lwt.bind (Lwt_unix.sleep 0.001) await in
  let queued = ref [] in
  let invalid = ref None in
  Fun.protect (fun () ->
    Lwt_main.run (await ());
    Gc.full_major ();
    let before = Gc.allocated_bytes () in
    queued := List.init 8 (fun _ -> Host.read_process_json ~optional:true payload);
    let allocated = Gc.allocated_bytes () -. before in
    Printf.printf "event = input_queue requests = 8 allocated = %.0f limit = %d\n%!"
      allocated (String.length body / 2);
    if allocated >= float_of_int (String.length body / 2) then
      fail "queued input was encoded";
    invalid := Some (Host.read_process_json ~optional:true bad);
    let canceled = List.hd !queued in
    Lwt.cancel canceled;
    begin match Lwt.state canceled with
    | Lwt.Fail Lwt.Canceled -> ()
    | _ -> fail "waiting input was not canceled"
    end;
    Atomic.set stopped true;
    Lwt_main.run blocker;
    List.iter (fun work ->
      if Lwt_main.run work <> expected then fail "queued input result changed")
      (List.tl !queued);
    begin try
      ignore (Lwt_main.run (Option.get !invalid));
      fail "queued JSON error was ignored"
    with Yojson.Json_error reason when reason = error -> () end)
    ~finally:(fun () ->
      Atomic.set stopped true;
      Lwt_main.run blocker;
      List.iter (fun work ->
        Lwt_main.run (Lwt.catch (fun () -> Lwt.map ignore work)
          (function Lwt.Canceled -> Lwt.return_unit | error -> Lwt.fail error))) !queued;
      Option.iter (fun work ->
        Lwt_main.run (Lwt.catch (fun () -> Lwt.map ignore work)
          (function Yojson.Json_error _ -> Lwt.return_unit | error -> Lwt.fail error))) !invalid;
      Lwt_preemptive.set_bounds limits;
      clean ())

let test_optional_input () =
  let size = String.length payload in
  let expected = run_json payload in
  mode 10;
  Fun.protect (fun () ->
    if run_json_native true payload <> expected then fail "optional input result changed")
    ~finally:(fun () -> mode 0; clean ());
  with_quota (input_limit - size) (fun () ->
    if run_json_native true payload <> expected then fail "exact input quota refused";
    if room size <> 0 then fail "completed input retained quota");
  with_quota (input_limit - size + 1) (fun () ->
    mode 6;
    let refused = Fun.protect (fun () -> run_json_native true payload)
      ~finally:(fun () -> mode 0) in
    if refused <> (2, "native input busy") then fail "input pressure class changed");
  with_quota input_limit (fun () ->
    if Native.run_json_classified ~optional:true payload <>
       Error (Native.Unavailable "native input busy") then
      fail "optional pressure became rejection";
    if run_json payload <> expected then fail "optional pressure blocked required input";
    if run_json_native true "" <> (2, "native input busy") then
      fail "empty input skipped reservation");
  List.iter (fun selected ->
    mode selected;
    begin try ignore (run_json_native true payload); fail "input failure accepted"
    with Out_of_memory when selected = 2 -> ()
       | Failure reason when selected = 6 && reason = "octra_circle_wasm_host: alloc failed" -> ()
    end;
    mode 0;
    clean ()) [2; 6];
  let old = Sys.signal Sys.sigusr1 (Sys.Signal_handle (fun _ -> fail "input signal")) in
  Fun.protect (fun () ->
    mode 1;
    begin try ignore (run_json_native true payload); fail "input signal absent"
    with Failure reason when reason = "input signal" -> () end;
    clean ())
    ~finally:(fun () -> Sys.set_signal Sys.sigusr1 old);
  let actual = paused (fun () -> run_json_native true payload) (fun () ->
    if room (input_limit - size) <> 0 || room (input_limit - size + 1) <> 2 then
      fail "active input quota differs") in
  if actual <> expected then fail "paused input result changed";
  clean ()

let test_optional_routes () =
  let module Host = Octra_core.Circle_wasm_host in
  let input = Yojson.Safe.from_string (guest false) in
  let code_b64 = Yojson.Safe.Util.(input |> member "code_b64" |> to_string) in
  let unavailable = function
    | Error (Host.Unavailable "native input busy") -> ()
    | _ -> fail "optional route skipped input quota" in
  let execute is_view = Host.execute
    ~inspect_mode:Octra_core.Rule_graph.Prior ~proof_mode:Octra_core.Rule_graph.Prior
    ~call:None ~code_b64 ~export_name:"octra_update" ~request_bytes:""
    ~storage_tbl:(Hashtbl.create 0) ~storage_cache_key:None ~caller:"" ~address:""
    ~tx_hash:"" ~current_epoch:0 ~hfhe_caps:[] ~hfhe_pubkeys:[] ~hfhe_active_key:None
    ~hfhe_strict:false ~math:false ~float_mode:Octra_core.Rule_graph.Prior
    ~hfhe_mode:Octra_core.Circle_hfhe_transcript.Direct ~public_reads:[]
    ~fuel_limit:2_000_000 ~is_view ~update_policy:true in
  let controls = [
    (fun () -> Host.compute_storage_status (String.make 64 '0'));
    (fun () -> Host.compute_storage_drop (String.make 64 '0'));
    (fun () -> Host.compute_self_test ());
  ] in
  let expected = List.map (fun run -> Lwt_main.run (run ())) controls in
  with_quota input_limit (fun () ->
    unavailable (Lwt_main.run (Host.describe ~optional:true code_b64));
    unavailable (Lwt_main.run (Host.validate ~optional:true code_b64));
    List.iter2 (fun run expected ->
      if Lwt_main.run (run ()) <> expected then fail "input pressure blocked control work")
      controls expected;
    unavailable (Lwt_main.run (execute true));
    if Result.is_error (Lwt_main.run (Host.validate code_b64)) then
      fail "optional pressure blocked required validation";
    if Result.is_error (Lwt_main.run (Host.describe code_b64)) then
      fail "optional pressure blocked required description";
    if Result.is_error (Lwt_main.run (execute false)) then
      fail "optional pressure blocked required execution");
  if Result.is_error (Lwt_main.run (execute true)) then
    fail "optional execution did not recover"

let test_optional_cancel () =
  mode 3;
  let work = Lwt_preemptive.detach (run_json_native true) payload in
  let deadline = Unix.gettimeofday () +. 3. in
  let rec await () =
    if waiting () then Lwt.return_unit
    else if Unix.gettimeofday () > deadline then Lwt.fail_with "input pause not reached"
    else Lwt.bind (Lwt_unix.sleep 0.001) await in
  Fun.protect (fun () -> Lwt_main.run (Lwt.bind (await ()) (fun () ->
    let abandoned = Lwt.protected work in
    Lwt.cancel abandoned;
    if room (input_limit - String.length payload + 1) <> 2 then
      fail "cancel released active input";
    resume ();
    Lwt.map (fun result ->
      if result <> run_json payload then fail "cancel changed native completion") work)))
    ~finally:(fun () -> resume (); mode 0; ignore (Lwt_main.run work); clean ())

let test_native_gc () =
  let expected = run_json payload in
  let actual = paused (fun () -> run_json payload) (fun () -> ()) in
  if actual <> expected then fail "native GC result changed";
  clean ();
  with_session (fun session ->
    let code, _ = paused (fun () -> call_step session (guest false)) (fun () ->
      if call_refs () <> 2 then fail "active session not retained";
      call_close session;
      if call_refs () <> 1 then fail "active session released early") in
    if code <> 0 then fail "retained session failed";
    clean ())

let test_call_wrapper () =
  let success () =
    match Lwt_main.run (Native.run_call (fun raw -> Lwt.return (reply raw)) (guest true)) with
    | Ok raw when Yojson.Safe.Util.member "success" (Yojson.Safe.from_string raw) = `Bool true -> ()
    | _ -> fail "call wrapper control failed" in
  success ();
  begin try
    ignore (Lwt_main.run (Native.run_call (fun _ -> Lwt.fail (Failure "caller failed"))
      (guest true)));
    fail "caller failure accepted"
  with Failure reason when reason = "caller failed" -> () end;
  let reached, wake = Lwt.wait () in
  let blocked, _ = Lwt.task () in
  let work = Native.run_call (fun _ -> Lwt.wakeup_later wake (); blocked) (guest true) in
  Lwt_main.run (Lwt.bind reached (fun () ->
    Lwt.cancel work;
    Lwt.catch (fun () -> Lwt.bind work (fun _ -> Lwt.fail_with "call was not cancelled"))
      (function Lwt.Canceled -> Lwt.return_unit | error -> Lwt.fail error)));
  success ()

let with_backend bridge handler run =
  let module Backend = Octra_core.Circle_wasm_hfhe_backend in
  Backend.ensure_registered ();
  Callback.register "octra_circle_wasm_host_hfhe_bridge" bridge;
  Callback.register "octra_circle_wasm_host_hfhe_call_json" handler;
  Fun.protect run ~finally:(fun () ->
    Callback.register "octra_circle_wasm_host_hfhe_bridge" live_bridge;
    Callback.register "octra_circle_wasm_host_hfhe_call_json" Backend.call_json;
    mode 0)

let test_hfhe_bridge () =
  let answer = "{\"ok\":true,\"value\":\"test\"}" in
  with_backend probe_bridge (fun _ -> answer) (fun () ->
    if hfhe_call () <> 0 then fail "HFHE bridge control failed";
    clean ();
    List.iter (fun selected ->
      mode selected;
      if hfhe_call () <> 1 || not (hfhe_returned ()) then fail "HFHE exception crossed call";
      clean ()) [7; 8];
    List.iter (fun selected ->
      let nested = ref false in
      Callback.register "octra_circle_wasm_host_hfhe_call_json" (fun _ ->
        if not !nested then begin
          nested := true;
          mode selected;
          if hfhe_call () <> (if selected = 0 then 0 else 1) then
            fail "nested HFHE bridge failed"
        end;
        answer);
      if hfhe_call () <> 0 then fail "HFHE context was not restored";
      clean ()) [0; 7]);
  with_backend probe_bridge (fun _ -> raise Out_of_memory) (fun () ->
    if hfhe_call () <> 1 || not (hfhe_returned ()) then fail "HFHE backend exception escaped";
    clean ());
  with_backend probe_bridge (fun _ -> answer) (fun () ->
    let old = Sys.signal Sys.sigusr1 (Sys.Signal_handle (fun _ -> fail "HFHE signal")) in
    Fun.protect (fun () ->
      mode 9;
      begin try ignore (hfhe_call ()); Gc.minor (); fail "HFHE signal was not delivered"
      with Failure reason when reason = "HFHE signal" -> () end;
      if not (hfhe_returned ()) then fail "HFHE signal crossed call";
      clean ())
      ~finally:(fun () -> Sys.set_signal Sys.sigusr1 old))

let test_hfhe_reply () =
  let answer = "{\"ok\":true,\"value\":\"test\"}" in
  let bridge () = probe_bridge (); hfhe_signal (); Gc.minor () in
  with_backend bridge (fun _ -> answer) (fun () ->
    let old = Sys.signal Sys.sigusr1 (Sys.Signal_handle (fun _ -> fail "HFHE signal")) in
    Fun.protect (fun () ->
      if hfhe_call () <> 1 || not (hfhe_returned ()) then
        fail "HFHE reply exception escaped";
      clean ())
      ~finally:(fun () -> Sys.set_signal Sys.sigusr1 old))

let test_hfhe_session () =
  let answer = "{\"ok\":true,\"value\":\"test\"}" in
  let check accepted bridge = with_backend bridge (fun _ -> answer) (fun () ->
    let check_reply raw =
      let response = Yojson.Safe.from_string raw in
      let error = if accepted then `Null else
        `String "octra_circle_wasm_host: hfhe callback exception" in
      if Yojson.Safe.Util.member "success" response <> `Bool accepted ||
         Yojson.Safe.Util.member "unavailable" response <> error then
        fail "HFHE response class differs" in
    let code, raw = run_json (guest ~hfhe:true true) in
    if code <> 0 then fail "HFHE ordinary result differs";
    check_reply raw;
    clean ();
    with_session (fun session ->
      let code, raw = call_step session (guest ~hfhe:true true) in
      if code <> 0 then fail ("HFHE session returned no receipt: " ^ raw);
      check_reply raw;
      if call_step session payload <> (2, "circle session closed") then
        fail "HFHE session retained its lock";
      clean ~sessions:1 ())) in
  check true live_bridge;
  check false (fun () -> raise Out_of_memory)

let () =
  test_input_queue ();
  test_optional_input ();
  test_optional_routes ();
  test_optional_cancel ();
  test_run_controls ();
  test_call_controls ();
  test_native_signal ();
  test_native_gc ();
  test_call_size ();
  test_call_preflight ();
  test_call_wrapper ();
  test_hfhe_bridge ();
  test_hfhe_reply ();
  test_hfhe_session ();
  test_input ();
  test_signal ();
  test_response ();
  test_gc_close ();
  print_endline "status = pass test = build_input"