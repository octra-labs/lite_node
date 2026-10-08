(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module VM = Octra_vm.Contract_vm
module Q = Octra_vm.Fhe_queue
module T = Octra_vm.Fhe_task
module P = Pvac_ffi
module Calc = Octra_core.Fhe_calc
module Image = Octra_core.Fhe_image
module Wire = Octra_core.Pvac_verify_protocol

let with_env name value run =
  let previous = Sys.getenv_opt name in
  Unix.putenv name value;
  Fun.protect ~finally:(fun () -> Unix.putenv name (Option.value ~default:"" previous)) run

let expect reason ok = if not ok then failwith reason
let ticket index = Octra_vm.Proof_wait.{request = string_of_int index; generation = "test"}
let entry index deadline = Q.{ticket = ticket index; deadline; abandoned = false; urgent = true}

let wire_checks () =
  let pk, left, right = "key\000\255", "left\000", "right\255" in
  let key = Image.{data = pk; sampling = P.{rows = 64; columns = 32;
    weight = 3; noise = 7; branches = 5}; words = Some 1; size = Some 4096} in
  let cipher = Image.{data = left; words = Some 17;
    shape = Some P.{slots = 2; layers = 3; edges = 5; c0 = 2; base_layers = 1}} in
  List.iter (fun size ->
    let key = {key with size = Some size} in
    let cost = Option.get (T.key_effort (T.Add (key, cipher, cipher))) in
    let volume = size + String.length pk in
    expect "key transfer price differs"
      (volume <= 16 * cost && 16 * cost < volume + 16))
    [0; 1; 15; 16; 17; 4096; 33_554_432];
  expect "negative key size accepted"
    (T.key_effort (T.Commit ({key with size = Some (-1)}, cipher)) = None);
  let requests = Calc.[Read_key (true, pk); Read_key (false, pk);
    Read_cipher (true, false, left); Read_cipher (false, true, left);
    Add (pk, left, right); Sub (pk, left, right);
    Mul (true, 32, pk, left, right, String.make 32 '\001');
    Scale (false, pk, left, Int64.min_int); Divide (pk, left, Int64.max_int);
    Add_int (true, pk, left, -1L); Sub_int (false, pk, left, 0L); Commit (pk, left)] in
  List.iter (fun request ->
    let raw = Calc.request_bytes request in
    expect "native request changed" (Calc.request_of_bytes raw = Ok request);
    for size = 0 to String.length raw - 1 do
      expect "partial native frame accepted"
        (Result.is_error (Calc.request_of_bytes (String.sub raw 0 size)))
    done;
    expect "native extra bytes accepted" (Result.is_error (Calc.request_of_bytes (raw ^ "\000")));
    let bytes = Bytes.of_string raw in
    List.iter (fun length ->
      Bytes.set_int64_be bytes (String.length Calc.magic + 1) length;
      expect "native false length accepted"
        (Result.is_error (Calc.request_of_bytes (Bytes.to_string bytes))))
      [-1L; Int64.max_int; Int64.of_int Calc.max_bytes];
    let id = Calc.hash raw in
    List.iter (fun result ->
      let bytes = Calc.response_bytes id result in
      expect "native response changed" (Calc.response_of_bytes id bytes = Ok result);
      expect "other native request accepted"
        (Result.is_error (Calc.response_of_bytes (String.make 64 '0') bytes)))
      Calc.[Ok (Key key); Ok (Cipher cipher); Ok (Digest right);
        Error Invalid; Error Memory; Error Stack]) requests;
  let key_meta = Image.key_meta key in
  let cipher_meta = Image.cipher_meta cipher in
  for size = 0 to String.length key_meta - 1 do
    expect "partial key metadata accepted"
      (Result.is_error (Image.key_of_meta pk (String.sub key_meta 0 size)));
    expect "partial cipher metadata accepted"
      (Result.is_error (Image.cipher_of_meta left (String.sub cipher_meta 0 size)))
  done;
  expect "key metadata extra byte accepted"
    (Result.is_error (Image.key_of_meta pk (key_meta ^ "\000")));
  expect "cipher metadata extra byte accepted"
    (Result.is_error (Image.cipher_of_meta left (cipher_meta ^ "\000")));
  List.iter (fun index ->
    List.iter (fun value ->
      let bytes = Bytes.of_string key_meta in
      Bytes.set_int64_be bytes (index * 8) value;
      expect "key metadata overflow accepted"
        (Result.is_error (Image.key_of_meta pk (Bytes.to_string bytes)));
      let bytes = Bytes.of_string cipher_meta in
      Bytes.set_int64_be bytes (index * 8) value;
      expect "cipher metadata overflow accepted"
        (Result.is_error (Image.cipher_of_meta left (Bytes.to_string bytes))))
      [Int64.min_int; Int64.max_int]) (List.init 7 Fun.id);
  List.iter (fun fields ->
    expect "invalid cipher metadata accepted"
      (Result.is_error (Image.cipher_of_meta left (Image.numbers fields))))
    [[-2; 1; 2; 3; 5; 2; 1]; [17; 2; 2; 3; 5; 2; 1];
     [17; 0; 2; 3; 5; 2; 1]; [17; 1; 2; 3; 5; 2; 4];
     [17; 1; -1; 3; 5; 2; 1]];
  let missing = Image.{cipher with shape = None; words = None} in
  expect "absent cipher metadata changed"
    (Image.cipher_of_meta left (Image.cipher_meta missing) = Ok missing);
  let missing = Image.{key with words = None; size = None} in
  expect "absent key metadata changed"
    (Image.key_of_meta pk (Image.key_meta missing) = Ok missing);
  List.iter (fun fields ->
    expect "invalid native fields accepted"
      (Result.is_error (Calc.request_of_bytes (Calc.encode fields))))
    [["read_key"; "2"; pk]; ["read_cipher"; "1"; "2"; left];
     ["add"; pk; left]; ["scale"; "2"; pk; left; "0"];
     ["mul"; "1"; "-1"; pk; left; right; "seed"];
     ["divide"; pk; left; "9223372036854775808"]]

let child_sample () =
  P.isolate_worker ();
  let session = Array.length Sys.argv = 2 && Sys.argv.(1) = "--fhe-session" in
  let raw = if session then Calc.read_frame () else Calc.read_request () in
  let write bytes =
    if session then Calc.write_frame bytes
    else begin output_string stdout bytes; flush stdout end in
  let proof = Array.length Sys.argv = 1 in
  match Sys.getenv "OCTRA_FHE_TEST" with
  | "frame" | "size" as mode ->
    let header = Bytes.create 8 in
    Bytes.set_int64_be header 0 (if mode = "frame" then 0L else Int64.max_int);
    output_bytes stdout header;
    flush stdout
  | "session" ->
    let digest = Printf.sprintf "%032d" (Unix.getpid ()) in
    let rec reply raw =
      write (Calc.response_bytes (Calc.hash raw) (Ok (Calc.Digest digest)));
      reply (Calc.read_frame ()) in
    (try reply raw with End_of_file -> ())
  | "crash" -> Unix.kill (Unix.getpid ()) Sys.sigkill
  | "wait" ->
    let path = Sys.getenv "OCTRA_FHE_PID" in
    let channel = open_out (path ^ ".next") in
    Fun.protect ~finally:(fun () -> close_out channel)
      (fun () -> output_string channel (string_of_int (Unix.getpid ())));
    Unix.rename (path ^ ".next") path;
    Unix.sleepf 15.
  | "hash" ->
    let hash = String.make 64 '0' in
    let reply = if proof then Wire.response_bytes
        {request_hash = hash; accepted = true; reason = ""}
      else Calc.response_bytes hash (Ok (Calc.Digest (String.make 32 'd'))) in
    write reply
  | "kind" when proof ->
    output_string stdout "{}";
    flush stdout
  | "kind" ->
    let cipher = match Calc.request_of_bytes raw with
      | Ok (Calc.Commit (_, cipher)) -> cipher
      | _ -> failwith "test request" in
    let image = P.deserialize_cipher (Bytes.of_string cipher) |> Image.of_cipher in
    write (Calc.response_bytes (Calc.hash raw) (Ok (Calc.Cipher image)))
  | _ -> failwith "test mode"

let queue_checks () =
  let view = {(entry 90 100L) with urgent = false} in
  let active, _ = Q.delta Q.empty (0L, Q.Submit view) in
  let next, effects = Q.delta active (1L, Q.Submit (entry 91 100L)) in
  expect "view was not interrupted" (effects = [Q.Fail (view.ticket, Q.Cancelled)]);
  expect "interruption released live worker"
    (Option.map (fun (work : Q.entry) -> work.abandoned) next.active = Some true);
  let _, effects = Q.delta next (2L, Q.Complete view.ticket) in
  expect "consensus did not resume after view"
    (effects = [Q.Retire view.ticket; Q.Start (ticket 91)]);
  let first = entry 0 100L in
  let active, effects = Q.delta Q.empty (0L, Q.Submit first) in
  expect "queue did not start" (effects = [Q.Start first.ticket]);
  let waiting, _ = Q.delta active (0L, Q.Submit (entry 1 200L)) in
  List.iter (fun (now, message) ->
    let held, effects = Q.delta waiting (now, message) in
    expect "cancel released running work" (Option.is_some held.active);
    expect "cancel started overlapping work"
      (not (List.exists (function Q.Start _ -> true | _ -> false) effects));
    let resumed, effects = Q.delta held (now, Q.Complete first.ticket) in
    match message with
    | Q.Stop -> expect "stopped queue restarted" (resumed.closed && resumed.active = None)
    | _ -> expect "completed work did not release slot" (effects = [Q.Retire first.ticket; Q.Start (ticket 1)]))
    [1L, Q.Cancel first.ticket; 100L, Q.Tick; 1L, Q.Stop];
  let duplicate, effects = Q.delta active (0L, Q.Submit first) in
  expect "duplicate accepted" (duplicate = active && effects = [Q.Refuse Q.Duplicate]);
  let other = {first.ticket with generation = "previous"} in
  let unchanged, effects = Q.delta active (0L, Q.Complete other) in
  expect "other generation released worker" (unchanged = active && effects = []);
  let full = List.fold_left (fun state id ->
    fst (Q.delta state (0L, Q.Submit (entry id 100L)))) active
    (List.init Q.capacity (fun index -> index + 1)) in
  let refused, effects = Q.delta full (0L, Q.Submit (entry 99 100L)) in
  expect "queue overflow accepted" (refused = full && effects = [Q.Refuse Q.Busy]);
  let views = List.fold_left (fun state id ->
    fst (Q.delta state (0L, Q.Submit {(entry id 100L) with urgent = false}))) active [1; 2] in
  let held, effects = Q.delta views (0L, Q.Submit {(entry 3 100L) with urgent = false}) in
  expect "views consumed reserve" (held = views && effects = [Q.Refuse Q.Busy]);
  let required, effects = Q.delta views (0L, Q.Submit (entry 4 100L)) in
  expect "required work refused" (effects = []);
  let _, effects = Q.delta required (0L, Q.Complete first.ticket) in
  expect "view delayed required work" (effects = [Q.Retire first.ticket; Q.Start (ticket 4)]);
  let random = Random.State.make [|712; 28|] in
  let rec trace count now state =
    if count = 0 then ()
    else
      let index = Random.State.int random 12 in
      let message = match Random.State.int random 6 with
        | 0 | 1 -> Q.Submit {(entry index (Int64.add now 5L)) with urgent = index mod 2 = 0}
        | 2 -> Q.Complete (ticket index)
        | 3 -> Q.Cancel (ticket index)
        | 4 -> Q.Tick
        | _ -> Q.Stop in
      let next, effects = Q.delta state (now, message) in
      let entries = Option.to_list next.active @ next.pending in
      let ids = List.map (fun (entry : Q.entry) -> entry.ticket) entries in
      expect "queue exceeded capacity" (List.length next.pending <= Q.capacity);
      expect "views exceeded capacity"
        (List.length (List.filter (fun (entry : Q.entry) -> not entry.urgent) next.pending) <= Q.view_limit);
      expect "queue duplicated identity" (List.length ids = List.length (List.sort_uniq compare ids));
      expect "closed queue has waiting work" (not next.closed || next.pending = []);
      expect "queue started twice" (List.length (List.filter (function Q.Start _ -> true | _ -> false) effects) <= 1);
      let state = if next.closed && next.active = None then Q.empty else next in
      trace (count - 1) (Int64.succ now) state
  in
  trace 30_000 0L Q.empty

let bytes = function
  | VM.VPubKey key -> key.data
  | VM.VCipher cipher -> cipher.data
  | VM.VString value | VM.VBytes value -> value
  | value -> VM.to_string value

let image_checks worker key secret =
  let read request =
    let raw = Calc.request_bytes request in
    let deadline = Mtime_clock.elapsed_ns () |> Int64.to_float |> fun now -> now /. 1e9 +. 20. in
    match Octra_core.Pvac_verify_worker.exchange ~deadline ~arguments:["--fhe"]
        ~limit:Calc.max_bytes worker raw with
    | Ok (reply, _, Unix.WEXITED 0) ->
      begin match Calc.response_of_bytes (Calc.hash raw) reply with
      | Ok value -> value
      | Error error -> failwith error
      end
    | _ -> failwith "native image worker failed" in
  let check_key registry raw =
    let native = P.deserialize_pubkey (Bytes.of_string raw) in
    match read (Calc.Read_key (registry, raw)) with
    | Ok (Calc.Key image) ->
      expect "native key bytes differ"
        (image.data = (P.serialize_pubkey native |> Bytes.to_string));
      expect "native key sampling differs" (image.sampling = P.pubkey_sampling native);
      expect "native key words differ" (image.words = Some (P.pubkey_bit_words native));
      expect "native key size differs" (image.size = Some (P.pubkey_image_size native))
    | _ -> failwith "native key image refused" in
  List.iter (fun raw -> List.iter (fun registry -> check_key registry raw) [false; true])
    [P.serialize_pubkey key |> Bytes.to_string;
      P.serialize_pubkey_legacy_v2 key |> Bytes.to_string];
  List.iter (fun slots ->
    let values = Array.init slots (fun index -> Int64.of_int (index + 1)) in
    let cipher = P.enc_values_seeded key secret values (Bytes.make 32 '\027') in
    let raw = P.serialize_cipher cipher |> Bytes.to_string in
    List.iter (fun (strict, cap) ->
      let native = P.deserialize_cipher ~strict ~cap (Bytes.of_string raw) in
      match read (Calc.Read_cipher (strict, cap, raw)) with
      | Ok (Calc.Cipher image) ->
        expect "native cipher bytes differ"
          (image.data = (P.serialize_cipher native |> Bytes.to_string));
        expect "native cipher shape differs" (image.shape = Some (P.cipher_shape native));
        expect "native cipher words differ" (image.words = Some (P.cipher_bit_words native))
      | _ -> failwith "native cipher image refused")
      [false, false; false, true; true, true]) [1; 2; 8];
  List.iter (fun request ->
    expect "native invalid image accepted" (read request = Error Calc.Invalid))
    Calc.[Read_key (false, ""); Read_key (true, "invalid");
      Read_cipher (false, false, "invalid"); Read_cipher (false, true, "invalid");
      Read_cipher (true, false, "invalid"); Read_cipher (true, true, "invalid")]

let session_checks worker key cipher =
  let module W = Octra_core.Pvac_verify_worker in
  let pk = P.serialize_pubkey key |> Bytes.to_string in
  let ct = P.serialize_cipher cipher |> Bytes.to_string in
  let request = Calc.Add (pk, ct, ct) in
  let expected = Calc.eval request in
  let opened = ref [] in
  Fun.protect ~finally:(fun () -> List.iter Unix.close !opened) (fun () ->
    for _ = 0 to 1099 do
      opened := Unix.openfile "/dev/null" [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 :: !opened
    done;
  let process = W.open_session worker in
  Fun.protect ~finally:(fun () -> W.close_session process) (fun () ->
    List.iter (fun wire ->
      let raw = Calc.request_bytes wire in
      let control = Atomic.make W.Continue in
      let deadline = W.monotonic_seconds () +. 20. in
      match W.session_exchange ~control ~deadline process raw with
      | Ok raw -> expect "session changed arithmetic"
          (Calc.response_of_bytes (Calc.hash (Calc.request_bytes wire)) raw = Ok expected)
      | _ -> failwith "session refused arithmetic")
      [request; Calc.map_key Calc.key_ref request; request;
       Calc.map_key Calc.key_ref request];
    List.iter (fun registry ->
      let raw = Calc.request_bytes (Calc.Read_key (registry, Calc.key_ref pk)) in
      match W.session_exchange ~control:(Atomic.make W.Continue)
          ~deadline:(W.monotonic_seconds () +. 20.) process raw with
      | Ok reply -> expect "session reference accepted as public key"
          (Calc.response_of_bytes (Calc.hash raw) reply = Ok (Error Calc.Invalid))
      | _ -> failwith "key reference did not return input refusal") [false; true]);
  let process = W.open_session worker in
  Fun.protect ~finally:(fun () -> W.close_session process) (fun () ->
    let wire = Calc.map_key Calc.key_ref request in
    let raw = Calc.request_bytes wire in
    match W.session_exchange ~control:(Atomic.make W.Continue)
        ~deadline:(W.monotonic_seconds () +. 20.) process raw with
    | Ok reply -> expect "new session accepted missing key"
        (Calc.response_of_bytes (Calc.hash raw) reply = Ok (Error Calc.Invalid))
    | _ -> failwith "session did not refuse missing key"))

let session_priority key cipher =
  let actor = T.create () in
  let request = T.Commit (Image.of_key key, Image.of_cipher cipher) in
  let run urgent =
    let deadline = Int64.add (Mtime_clock.elapsed_ns ()) 20_000_000_000L in
    Lwt_main.run (T.run ~actor ~urgent ~ticket:(ticket 80) ~deadline request) in
  with_env "OCTRA_PVAC_VERIFY_WORKER" Sys.executable_name (fun () ->
    with_env "OCTRA_FHE_TEST" "session" (fun () ->
      Fun.protect ~finally:(fun () -> T.stop actor) (fun () ->
        let first = run true in
        expect "required session refused" (Result.is_ok first);
        Thread.delay 0.05;
        expect "session died after operation" (run true = first);
        let view = run false in
        expect "view session refused" (Result.is_ok view);
        expect "view took required process" (view <> first);
        expect "view replaced required cache" (run true = first);
        expect "required work replaced view cache" (run false = view);
        let pid = match view with
          | Ok (T.Text raw) -> Base64.decode_exn raw |> int_of_string
          | _ -> failwith "view process missing" in
        with_env "OCTRA_PVAC_VERIFY_WORKER" "runtime_data/absent_worker" (fun () ->
          expect "missing worker ignored" (run true = Error (T.Resource Host)));
        Lwt_main.run (Lwt_unix.sleep 11.);
        expect "launch failure kept idle process"
          (try Unix.kill pid 0; false with Unix.Unix_error (Unix.ESRCH, _, _) -> true))))

let session_recover key cipher =
  let actor = T.create () in
  let key = Image.of_key key and cipher = Image.of_cipher cipher in
  let run request =
    let deadline = Int64.add (Mtime_clock.elapsed_ns ()) 20_000_000_000L in
    Lwt_main.run (T.run ~actor ~ticket:(ticket 84) ~deadline request) in
  Fun.protect ~finally:(fun () -> T.stop actor) (fun () ->
    with_env "OCTRA_PVAC_VERIFY_WORKER" "runtime_data/absent_worker" (fun () ->
      for _ = 1 to 3 do
        expect "absent worker was accepted"
          (run (T.Add (key, cipher, cipher)) = Error (T.Resource Host))
      done);
    expect "invalid session input accepted"
      (run (T.Add (key, {cipher with data = "invalid"}, cipher)) = Error T.Invalid);
    let request = T.Add (key, cipher, cipher) in
    let expected = T.eval request in
    expect "valid session input refused" (Result.is_ok expected);
    expect "invalid input changed session key" (run request = expected);
    expect "recovered session changed bytes" (run request = expected))

let session_cost key cipher =
  let actor = T.create () in
  let request = T.Add (Image.of_key key, Image.of_cipher cipher, Image.of_cipher cipher) in
  let run () =
    let deadline = Int64.add (Mtime_clock.elapsed_ns ()) 20_000_000_000L in
    Lwt_main.run (T.run ~actor ~ticket:(ticket 81) ~deadline request) in
  Fun.protect ~finally:(fun () -> T.stop actor) (fun () ->
    let started = Mtime_clock.elapsed_ns () in
    let expected = run () in
    expect "cold arithmetic refused" (Result.is_ok expected);
    let cold = Int64.sub (Mtime_clock.elapsed_ns ()) started in
    let started = Mtime_clock.elapsed_ns () in
    for _ = 1 to 64 do
      expect "warm arithmetic changed" (run () = expected)
    done;
    let warm = Int64.sub (Mtime_clock.elapsed_ns ()) started in
    Printf.printf "event = fhe_session cold_ms = %.3f warm_ms = %.3f count = 64\n%!"
      (Int64.to_float cold /. 1e6) (Int64.to_float warm /. 64e6))

let session_retire ~cancel key cipher =
  let released, release = Lwt.wait () in
  let count = ref 0 in
  let reap process =
    incr count;
    let open Lwt.Syntax in
    let* () = Octra_core.Pvac_verify_worker.retire_session process in
    released in
  let steps = ref 0 in
  let clock () =
    incr steps;
    expect "session close repeated wake" (!steps < 512);
    Mtime_clock.elapsed_ns () in
  let actor = T.create ~clock ~reap () in
  let request = T.Commit (Image.of_key key, Image.of_cipher cipher) in
  let run () = T.run ~actor ~ticket:(ticket 83)
    ~deadline:(Int64.add (Mtime_clock.elapsed_ns ()) 20_000_000_000L) request in
  let worker = Result.get_ok (Option.to_result ~none:"worker missing"
    (Octra_core.Pvac_verify_worker.worker_path ())) in
  Fun.protect ~finally:(fun () ->
    if Lwt.is_sleeping released then Lwt.wakeup_later release ();
    T.stop actor;
    Lwt_main.run (Lwt_unix.sleep 0.02)) (fun () ->
    with_env "OCTRA_PVAC_VERIFY_WORKER" Sys.executable_name (fun () ->
      with_env "OCTRA_FHE_TEST" "session" (fun () ->
        expect "close setup refused" (Result.is_ok (Lwt_main.run (run ())))));
    with_env "OCTRA_PVAC_VERIFY_WORKER" worker (fun () ->
      let pending = run () in
      expect "closing session started work" (Lwt.is_sleeping pending);
      let queued = List.init 32 (fun _ -> run ()) in
      expect "closing session exceeded queue" (T.stats ~actor () = (true, Q.capacity));
      let pulse = ref false in
      Lwt_main.run (let open Lwt.Syntax in
        let* () = Lwt_unix.sleep 0.02 in
        pulse := true;
        Lwt.return_unit);
      expect "session close blocked owner" (!pulse && !count = 1 && Lwt.is_sleeping released);
      expect "closing session lost waiter" (Lwt.is_sleeping pending);
      if cancel then begin
        Lwt.cancel pending;
        expect "closing cancel released capacity" (T.stats ~actor () = (true, Q.capacity))
      end;
      expect "session close repeated" (!count = 1);
      Lwt.wakeup_later release ();
      if cancel then
        expect "closing session lost cancellation" (Lwt.state pending = Lwt.Fail Lwt.Canceled)
      else expect "reaped session retained capacity"
        (Result.is_ok (Lwt_main.run (Lwt_unix.with_timeout 5. (fun () -> pending))));
      let replies = Lwt_main.run (Lwt_unix.with_timeout 5. (fun () -> Lwt.all queued)) in
      expect "closing session changed queue capacity"
        (List.length (List.filter Result.is_ok replies) = Q.capacity);
      expect "closing session changed overflow result"
        (List.for_all (fun value -> Result.is_ok value || value = Error (T.Resource Host)) replies)))

let key_cost key secret cipher =
  let other, other_secret = P.keygen_from_seed (P.default_params ()) (Bytes.make 32 '\031') in
  let other_cipher = P.enc_value_seeded other other_secret 9L (Bytes.make 32 '\030') in
  let inputs = [|key, secret, cipher; other, other_secret, other_cipher|] in
  let requests = Array.map (fun (key, _, cipher) ->
    T.Add (Image.of_key key, Image.of_cipher cipher, Image.of_cipher cipher)) inputs in
  let expected = Array.map (fun request -> T.direct (T.local ()) request) requests in
  let charge = Array.map (fun request -> Option.get (T.key_effort request)) requests in
  Array.iteri (fun index (key, secret, _) ->
    match expected.(index) with
    | Ok (T.Cipher value) ->
      let value = P.deserialize_cipher ~strict:true ~cap:true (Bytes.of_string value.data) in
      expect "key cost changed amount" (P.dec_value key secret value = 18L)
    | _ -> failwith "key cost arithmetic failed") inputs;
  let sample mode count choose =
    let actor = ref (T.create ()) in
    let run request = T.run ~actor:!actor ~ticket:(ticket 84)
      ~deadline:(Int64.add (Mtime_clock.elapsed_ns ()) 20_000_000_000L) request
      |> Lwt_main.run in
    Fun.protect ~finally:(fun () -> T.stop !actor;
      Lwt_main.run (Lwt_unix.sleep 0.02)) (fun () ->
      if mode <> "cold" then
        expect "key cost setup changed bytes" (run requests.(0) = expected.(0));
      let samples = Array.init count (fun index ->
        let slot = choose index in
        if mode = "cold" then begin
          T.stop !actor;
          Lwt_main.run (Lwt_unix.sleep 0.02);
          actor := T.create ()
        end;
        let start = Mtime_clock.elapsed_ns () in
        let value = run requests.(slot) in
        let duration = Int64.to_float (Int64.sub (Mtime_clock.elapsed_ns ()) start) /. 1e6 in
        expect "key cost changed bytes" (value = expected.(slot));
        duration) in
      Array.sort Float.compare samples;
      let mean = Array.fold_left (+.) 0. samples /. float_of_int count in
      let p95 = samples.((count * 95 + 99) / 100 - 1) in
      Printf.printf "event = key_cost mode = %s count = %d mean_ms = %.3f p95_ms = %.3f max_ms = %.3f effort = %d\n%!"
        mode count mean p95 samples.(count - 1) charge.(0)) in
  let image = Image.of_key key in
  let size = Option.get image.size in
  Printf.printf "event = key_size encoded = %d decoded = %d effort = %d\n%!"
    (String.length image.data) size charge.(0);
  sample "cold" 8 (fun _ -> 0);
  sample "warm" 32 (fun _ -> 0);
  sample "alternate" 32 (fun index -> index mod 2);
  let actor = T.create () in
  Fun.protect ~finally:(fun () -> T.stop actor; Lwt_main.run (Lwt_unix.sleep 0.02)) (fun () ->
    let samples = Array.init 16 (fun index ->
      let key, _, _ = inputs.(index mod 2) in
      let image = Image.of_key key in
      let start = Mtime_clock.elapsed_ns () in
      let result = T.run ~actor ~ticket:(ticket 85)
        ~deadline:(Int64.add start 20_000_000_000L) (T.Load_key image.data) |> Lwt_main.run in
      let duration = Int64.to_float (Int64.sub (Mtime_clock.elapsed_ns ()) start) /. 1e6 in
      expect "key load changed bytes" (result = Ok (T.Key image));
      duration) in
    Array.sort Float.compare samples;
    Printf.printf "event = key_cost mode = load count = 16 mean_ms = %.3f p95_ms = %.3f max_ms = %.3f effort = %d\n%!"
      (Array.fold_left (+.) 0. samples /. 16.) samples.(15) samples.(15)
      (Option.get (Octra_vm.Fhe_memory.key_read_effort ~active:true image.data)))

let state ?(math = true) ?(work = true) async key raw cipher =
  let ctx = {VM.default_ctx with async_exec = async; math; point_ops = true;
    proof_exec = Octra_core.Rule_graph.Active;
    tx_hash = "fhe-task"; tree_hash = "root"; current_epoch = 1_663_000;
    fhe_work = (if work then Octra_core.Rule_graph.Active else Prior);
    get_fhe_pubkey = (fun _ -> Some (VM.Key_bytes raw))} in
  let state = VM.create_state ~ctx ~limit:100_000_000 ~caller:"sender"
    ~origin:"sender" ~address:"program" ~value:Z.zero ~storage:(Hashtbl.create 1) () in
  state.regs.(0) <- VM.VString "sender";
  state.regs.(1) <- VM.VPubKey (Image.of_key key);
  state.regs.(2) <- VM.VCipher (Image.of_cipher cipher);
  state.regs.(3) <- VM.VInt (Z.of_int 3);
  state.regs.(4) <- VM.VString (Base64.encode_exn raw);
  state.regs.(6) <- VM.VString (Base64.encode_exn (P.serialize_cipher cipher |> Bytes.to_string));
  state

let identity_checks key raw cipher =
  let open Lwt.Syntax in
  Lwt_list.iter_s (fun async ->
    let st = state async key raw cipher in
    let run code = if async then VM.run_async st code else Lwt.return (VM.run st code) in
    let* ok = run VM.[|MOV (7, 1); EQ (8, 7, 1); MSTORE (0, 2);
      MLOAD (9, 0); EQ (10, 9, 2); NEQ (11, 9, 2);
      FHE_LOAD_PK (12, 0); FHE_LOAD_PK (13, 0); EQ (14, 12, 13);
      NEQ (15, 12, 13); FHE_DESER_PK (16, 4); EQ (17, 12, 16);
      FHE_DESER (18, 6); FHE_DESER (19, 6); EQ (20, 18, 19);
      NEQ (21, 18, 19); FHE_SCALE (22, 1, 2, 3); EQ (23, 22, 2); STOP|] in
    expect "identity execution refused" ok;
    List.iter (fun (index, value) ->
      expect "native object identity changed" (st.regs.(index) = VM.VBool value))
      [8, true; 10, true; 11, false; 14, false; 15, true;
       17, false; 20, false; 21, true; 23, false];
    expect "independent key bytes differ" (bytes st.regs.(12) = bytes st.regs.(13));
    expect "independent cipher bytes differ" (bytes st.regs.(18) = bytes st.regs.(19));
    let key = Image.of_key key in
    let ctx = {st.ctx with fhe_memory = None;
      get_fhe_pubkey = (fun _ -> Some (VM.Key_value key))} in
    let st = VM.create_state ~ctx ~limit:10_000_000 ~caller:"sender"
      ~origin:"sender" ~address:"program" ~value:Z.zero ~storage:(Hashtbl.create 1) () in
    st.regs.(0) <- VM.VString "sender";
    let code = VM.[|FHE_LOAD_PK (1, 0); FHE_LOAD_PK (2, 0); EQ (3, 1, 2); STOP|] in
    let* ok = if async then VM.run_async st code else Lwt.return (VM.run st code) in
    expect "resident key identity changed" (ok && st.regs.(3) = VM.VBool true);
    Lwt.return_unit) [false; true]

let native_checks key raw cipher =
  let open Lwt.Syntax in
  let ops = VM.[FHE_LOAD_PK (5, 0); FHE_SER_PK (5, 1); FHE_DESER_PK (5, 4);
    FHE_SER (5, 2); FHE_DESER (5, 6); FHE_ADD (5, 1, 2, 2); FHE_SUB (5, 1, 2, 2);
    FHE_MUL (5, 1, 2, 2); FHE_SCALE (5, 1, 2, 3); FHE_DIV_CONST (5, 1, 2, 3);
    FHE_ADD_CONST (5, 1, 2, 3); FHE_SUB_CONST (5, 1, 2, 3); FHE_COMMIT (5, 1, 2)] in
  let pulse = ref 0 in
  let rec heartbeat () =
    let* () = Lwt_unix.sleep 0.001 in
    incr pulse;
    heartbeat () in
  let timer = heartbeat () in
  Lwt.finalize (fun () ->
    Lwt_list.iter_s (fun op ->
      let sync = state false key raw cipher in
      let async = state true key raw cipher in
      let program = [|op; VM.STOP|] in
      expect "synchronous fhe refused" (VM.run sync program);
      let before = !pulse in
      let* ok = VM.run_async async program in
      expect "asynchronous fhe refused" ok;
      expect "fhe bytes changed" (bytes sync.regs.(5) = bytes async.regs.(5));
      expect "fhe effort changed" (sync.effort_used = async.effort_used);
      expect "fhe reservation changed"
        (Option.map Octra_vm.Fhe_memory.used sync.ctx.fhe_memory
         = Option.map Octra_vm.Fhe_memory.used async.ctx.fhe_memory);
      let prior = state true key raw cipher in
      let prior = {prior with ctx = {prior.ctx with proof_exec = Octra_core.Rule_graph.Prior}} in
      expect "prior native operation refused" (VM.run prior program);
      expect "prior native bytes changed" (bytes prior.regs.(5) = bytes async.regs.(5));
      expect "prior native reservation changed"
        (Option.map Octra_vm.Fhe_memory.used prior.ctx.fhe_memory
         = Option.map Octra_vm.Fhe_memory.used async.ctx.fhe_memory);
      if op = VM.FHE_LOAD_PK (5, 0) then
        expect "key load blocked event loop" (!pulse > before);
      Lwt.return_unit) ops)
    (fun () -> Lwt.cancel timer; Lwt.return_unit)

let prior_checks key raw cipher =
  let program = VM.[|FHE_LOAD_PK (1, 0); FHE_ADD (5, 1, 2, 2);
    FHE_ADD (5, 1, 2, 5); FHE_SER (6, 5); STOP|] in
  let run mode =
    let base = state true key raw cipher in
    let st = {base with ctx = {base.ctx with proof_exec = mode}} in
    st, VM.run_async st program in
  with_env "OCTRA_PVAC_VERIFY_WORKER" "runtime_data/absent_worker" (fun () ->
    let st, result = run Octra_core.Rule_graph.Prior in
    expect "prior arithmetic used worker" (Lwt.state result = Lwt.Return true);
    let expected = P.ct_add key cipher (P.ct_add key cipher cipher)
      |> P.serialize_cipher |> Bytes.to_string |> Base64.encode_exn in
    expect "prior arithmetic changed" (st.regs.(6) = VM.VString expected);
    let _, result = run Octra_core.Rule_graph.Active in
    expect "active arithmetic ignored missing worker"
      (try ignore (Lwt_main.run result); false
       with Octra_core.Exec_resource.Unavailable Host -> true));
  let rec ready () =
    let open Lwt.Syntax in
    let deadline = Int64.add (Mtime_clock.elapsed_ns ()) 1_000_000_000L in
    let* result = T.run ~ticket:(ticket 85) ~deadline (T.Verify (true, Wire.Ping)) in
    if result = Ok (T.Verified true) then Lwt.return_unit
    else let* () = Lwt_unix.sleep 0.005 in ready () in
  Lwt_main.run (Lwt_unix.with_timeout 2. ready);
  let other, secret = P.keygen_from_seed (P.default_params ()) (Bytes.make 32 '\075') in
  let other_cipher = P.enc_value_seeded other secret 3L (Bytes.make 32 '\076') in
  let program = VM.[|FHE_ADD (5, 1, 2, 2); FHE_ADD (6, 8, 9, 9);
    FHE_ADD (5, 1, 2, 2); FHE_ADD (6, 8, 9, 9); STOP|] in
  let run ?(view = false) mode =
    let base = state true key raw cipher in
    let st = {base with is_view = view; ctx = {base.ctx with proof_exec = mode}} in
    st.regs.(8) <- VM.VPubKey (Image.of_key other);
    st.regs.(9) <- VM.VCipher (Image.of_cipher other_cipher);
    expect "alternating keys refused" (Lwt_main.run (VM.run_async st program));
    st in
  let prior = run Octra_core.Rule_graph.Prior in
  let active = run Octra_core.Rule_graph.Active in
  let key_cost key = T.key_effort
    (T.Add (Image.of_key key, Image.of_cipher cipher, Image.of_cipher cipher))
    |> Option.get in
  expect "key transfer work was not charged"
    (active.effort_used - prior.effort_used = 2 * (key_cost key + key_cost other));
  expect "key cache changed arithmetic"
    (bytes active.regs.(5) = bytes prior.regs.(5)
      && bytes active.regs.(6) = bytes prior.regs.(6));
  let repeated = run Octra_core.Rule_graph.Active in
  expect "warm key cache changed effort" (repeated.effort_used = active.effort_used);
  List.iter (fun mode ->
    let view = run ~view:true mode in
    expect "view key transfer escaped effort" (view.effort_used = active.effort_used))
    [Octra_core.Rule_graph.Prior; Octra_core.Rule_graph.Active];
  let local = T.local () in
  let key = Image.of_key key and cipher = Image.of_cipher cipher in
  let kept = List.init 128 (fun _ ->
    match T.direct local (T.Scale (true, key, cipher, 1L)) with
    | Ok (T.Cipher value) -> value
    | _ -> failwith "repeated cipher refused") in
  List.iter (fun cipher ->
    expect "native cache eviction changed bytes"
      (T.direct local (T.Add (key, cipher, cipher)) = T.direct (T.local ()) (T.Add (key, cipher, cipher)))) kept

let math_checks key raw cipher =
  let open Lwt.Syntax in
  Lwt_list.iter_s (fun (math, work, scalar) ->
    Lwt_list.iter_s (fun op ->
      let sync = state ~math ~work false key raw cipher in
      let async = state ~math ~work true key raw cipher in
      sync.regs.(3) <- VM.VInt (Z.of_int64 scalar);
      async.regs.(3) <- VM.VInt (Z.of_int64 scalar);
      let program = [|op; VM.STOP|] in
      let expected = VM.run sync program in
      let* actual = VM.run_async async program in
      expect "native math verdict changed" (expected = actual);
      expect "native math bytes changed" (bytes sync.regs.(5) = bytes async.regs.(5));
      expect "native math effort changed" (sync.effort_used = async.effort_used);
      expect "native math memory changed"
        (Option.map Octra_vm.Fhe_memory.used sync.ctx.fhe_memory
         = Option.map Octra_vm.Fhe_memory.used async.ctx.fhe_memory);
      Lwt.return_unit)
      VM.[FHE_MUL (5, 1, 2, 2); FHE_SCALE (5, 1, 2, 3);
        FHE_ADD_CONST (5, 1, 2, 3); FHE_SUB_CONST (5, 1, 2, 3)])
    [false, false, -3L; true, false, Int64.min_int; true, true, -1L]

let process_checks request =
  let actor = T.create () in
  let run () =
    let deadline = Int64.add (Mtime_clock.elapsed_ns ()) 10_000_000_000L in
    Lwt_main.run (T.run ~actor ~ticket:(ticket 20) ~deadline request) in
  with_env "OCTRA_PVAC_VERIFY_WORKER" (Sys.executable_name) (fun () ->
    List.iter (fun mode -> with_env "OCTRA_FHE_TEST" mode (fun () ->
      expect ("native process fault returned a verdict: " ^ mode)
        (run () = Error (T.Resource Host)))) ["crash"; "hash"; "kind"; "frame"; "size"]);
  let expected = T.eval request in
  expect "native process did not recover" (run () = expected);
  expect "native slot not retired" (T.stats ~actor () = (false, 0))

let process_cancel ~expire request =
  Test_workspace.with_dir "fhe-process" (fun directory ->
    let path = Filename.concat directory "pid" in
    with_env "OCTRA_PVAC_VERIFY_WORKER" Sys.executable_name (fun () ->
      with_env "OCTRA_FHE_TEST" "wait" (fun () ->
        with_env "OCTRA_FHE_PID" path (fun () ->
          let actor = T.create () in
          let span = if expire then 2_000_000_000L else 10_000_000_000L in
          let deadline = Int64.add (Mtime_clock.elapsed_ns ()) span in
          let pending = T.run ~actor ~ticket:(ticket 40) ~deadline request in
          let rec ready () =
            let open Lwt.Syntax in
            if Sys.file_exists path then Lwt.return_unit
            else let* () = Lwt_unix.sleep 0.01 in ready () in
          let rec drained () =
            let open Lwt.Syntax in
            if not (fst (T.stats ~actor ())) then Lwt.return_unit
            else let* () = Lwt_unix.sleep 0.01 in drained () in
          Fun.protect ~finally:(fun () ->
            Lwt.cancel pending;
            Lwt_main.run (Lwt_unix.with_timeout 12. drained)) (fun () ->
            Lwt_main.run (Lwt_unix.with_timeout 5. ready);
            let channel = open_in path in
            let pid = Fun.protect ~finally:(fun () -> close_in channel)
              (fun () -> int_of_string (input_line channel)) in
            if expire then
              expect "expired native proof returned verdict"
                (Lwt_main.run pending = Error (T.Resource Host))
            else begin
              Lwt.cancel pending;
              expect "native cancel freed running slot" (T.stats ~actor () = (true, 0))
            end;
            let retired = try
              Lwt_main.run (Lwt_unix.with_timeout 2. drained);
              true
            with Lwt_unix.Timeout -> false in
            expect "cancelled native child retained slot" retired;
            if not expire then
              expect "native cancel lost" (Lwt.state pending = Lwt.Fail Lwt.Canceled);
            let stopped = try Unix.kill pid 0; false with
              Unix.Unix_error (Unix.ESRCH, _, _) -> true in
            expect "native child still runs" stopped)))))

let process_priority worker =
  Test_workspace.with_dir "fhe-priority" (fun directory ->
    let path = Filename.concat directory "pid" in
    with_env "OCTRA_PVAC_VERIFY_WORKER" Sys.executable_name (fun () ->
      with_env "OCTRA_FHE_TEST" "wait" (fun () ->
        with_env "OCTRA_FHE_PID" path (fun () ->
          let actor = T.create () in
          let deadline = Int64.add (Mtime_clock.elapsed_ns ()) 10_000_000_000L in
          let request = T.Verify (false, Wire.Ping) in
          let pending = T.run ~actor ~urgent:false ~ticket:(ticket 82) ~deadline request in
          Fun.protect ~finally:(fun () -> T.stop actor) (fun () ->
            let rec ready () =
              if Sys.file_exists path then Lwt.return_unit
              else Lwt.bind (Lwt_unix.sleep 0.005) ready in
            Lwt_main.run (Lwt_unix.with_timeout 5. ready);
            Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" worker;
            let required = T.run ~actor ~ticket:(ticket 83) ~deadline request in
            expect "priority released live view" (T.stats ~actor () = (true, 1));
            expect "priority did not cancel view"
              (Lwt_main.run pending = Error (T.Resource Host));
            expect "view delayed required proof"
              (Lwt_main.run (Lwt_unix.with_timeout 2. (fun () -> required))
                = Ok (T.Verified true));
            let channel = open_in path in
            let pid = Fun.protect ~finally:(fun () -> close_in channel)
              (fun () -> int_of_string (input_line channel)) in
            let reaped = try ignore (Unix.waitpid [Unix.WNOHANG] pid); false with
              | Unix.Unix_error (Unix.ECHILD, _, _) -> true in
            expect "required proof overlapped view process" reaped)))))

let owner_checks () =
  let actor = T.create () in
  let deadline = Int64.add (Mtime_clock.elapsed_ns ()) 10_000_000_000L in
  for index = 0 to 999 do
    let reply = T.run ~actor ~ticket:(ticket index) ~deadline (T.Write_secret "bytes") in
    expect "serialization entered worker queue"
      (Lwt.state reply = Lwt.Return (Ok (T.Text "Ynl0ZXM=")))
  done;
  expect "serialization retained work" (T.stats ~actor () = (false, 0));
  let request = T.Verify (true, Wire.Ping) in
  let deadline () = Int64.add (Mtime_clock.elapsed_ns ()) 10_000_000_000L in
  List.iter (fun (error, resource) ->
    let actor = T.create ~clock:(fun () -> raise error) () in
    let reply = T.run ~actor ~ticket:(ticket 60) ~deadline:(deadline ()) request in
    expect "owner fault escaped typed result"
      (Lwt_main.run reply = Error (T.Resource resource));
    expect "owner fault retained unstarted work" (T.stats ~actor () = (false, 0));
    T.stop actor;
    T.stop actor)
    [Failure "clock", Octra_core.Exec_resource.Host;
      Out_of_memory, Memory; Stack_overflow, Stack];
  let armed = ref false in
  let actor = T.create ~clock:(fun () ->
    if !armed then failwith "clock" else Mtime_clock.elapsed_ns ()) () in
  let first = T.run ~actor ~ticket:(ticket 61) ~deadline:(deadline ()) request in
  let second = T.run ~actor ~ticket:(ticket 62) ~deadline:(deadline ()) request in
  armed := true;
  let replies = try
    Lwt_main.run (Lwt_unix.with_timeout 5. (fun () -> Lwt.all [first; second]))
  with Lwt_unix.Timeout -> failwith "owner completion fault stranded replies" in
  expect "owner completion fault returned verdict"
    (List.for_all ((=) (Error (T.Resource Host))) replies);
  expect "owner completion fault retained slot" (T.stats ~actor () = (false, 0));
  expect "owner fault restarted queue"
    (Lwt_main.run (T.run ~actor ~ticket:(ticket 63) ~deadline:(deadline ()) request)
      = Error (T.Resource Host));
  let ready = ref false in
  let retry = T.create ~clock:(fun () ->
    if !ready then Mtime_clock.elapsed_ns () else failwith "clock") () in
  expect "owner first fault returned data"
    (Lwt_main.run (T.run ~actor:retry ~ticket:(ticket 65) ~deadline:(deadline ()) request)
      = Error (T.Resource Host));
  ready := true;
  expect "owner did not recover after drain"
    (Lwt_main.run (T.run ~actor:retry ~ticket:(ticket 66) ~deadline:(deadline ()) request)
      = Ok (T.Verified true));
  T.stop retry;
  let actor = T.create () in
  expect "owner fault affected another actor"
    (Lwt_main.run (T.run ~actor ~ticket:(ticket 64) ~deadline:(deadline ()) request)
      = Ok (T.Verified true))

let owner_cancel mode =
  Test_workspace.with_dir "fhe-owner" (fun directory ->
    let path = Filename.concat directory "pid" in
    with_env "OCTRA_PVAC_VERIFY_WORKER" Sys.executable_name (fun () ->
      with_env "OCTRA_FHE_TEST" "wait" (fun () ->
        with_env "OCTRA_FHE_PID" path (fun () ->
          let armed = ref false in
          let actor = T.create ~clock:(fun () ->
            if !armed then failwith "clock" else Mtime_clock.elapsed_ns ()) () in
          let span = if mode = "expiry" then 2_000_000_000L else 10_000_000_000L in
          let deadline = Int64.add (Mtime_clock.elapsed_ns ()) span in
          let request = T.Verify (true, Wire.Ping) in
          let first = T.run ~actor ~ticket:(ticket 70) ~deadline request in
          let second = T.run ~actor ~ticket:(ticket 71) ~deadline request in
          let rec ready () =
            let open Lwt.Syntax in
            if Sys.file_exists path then Lwt.return_unit
            else let* () = Lwt_unix.sleep 0.01 in ready () in
          let rec drained () =
            let open Lwt.Syntax in
            if T.stats ~actor () = (false, 0) then Lwt.return_unit
            else let* () = Lwt_unix.sleep 0.01 in drained () in
          Fun.protect ~finally:(fun () ->
            T.stop actor;
            Lwt_main.run (Lwt_unix.with_timeout 5. drained)) (fun () ->
            Lwt_main.run (Lwt_unix.with_timeout 5. ready);
            let channel = open_in path in
            let pid = Fun.protect ~finally:(fun () -> close_in channel)
              (fun () -> int_of_string (input_line channel)) in
            armed := true;
            if mode = "cancel" then Lwt.cancel first
            else if mode = "submit" then
              expect "owner submit fault returned verdict"
                (Lwt_main.run (T.run ~actor ~ticket:(ticket 72) ~deadline request)
                  = Error (T.Resource Host));
            expect "owner fault stranded waiting request"
              (Lwt_main.run (Lwt_unix.with_timeout 5. (fun () -> second))
                = Error (T.Resource Host));
            if mode = "cancel" then
              expect "owner fault lost cancellation" (Lwt.state first = Lwt.Fail Lwt.Canceled)
            else expect "owner fault returned running verdict"
              (Lwt_main.run first = Error (T.Resource Host));
            Lwt_main.run (Lwt_unix.with_timeout 5. drained);
            let reaped = try Unix.kill pid 0; false with
              Unix.Unix_error (Unix.ESRCH, _, _) -> true in
            expect "owner fault left child running" reaped)))))

let program_failure key raw cipher =
  let module V = Octra_vm in
  let module S = Octra_core.Store_irmin in
  let module J = V.Program_journal in
  let caller = "oct" ^ String.make 44 '1' in
  let source = {|
program NativeMath {
  state {
    count: int
    result: string
  }
  constructor() {
    self.count = 0
    self.result = ""
  }
  fn add(key: address, data: bytes): int {
    self.count = self.count + 1
    let pk = fhe_load_pk(key)
    let ct = fhe_deser(data)
    self.result = fhe_ser(fhe_add(pk, ct, ct))
    return self.count
  }
}
|} in
  let package = match V.Program_package.compile_with ~compiler:Preview
      ~point_ops:true ~main:"main.aml" ~sources:[{path = "main.aml"; body = source}] with
    | Ok value -> value
    | Error error -> failwith (V.Program_package.error_message error) in
  let checked = match V.Program_package.admit_base64 ~compiler:Preview
      ~point_ops:true (Base64.encode_exn package.package) with
    | Ok value -> value.program
    | Error error -> failwith (V.Program_package.error_message error) in
  let ctx = {(state true key raw cipher).ctx with proof_exec = Octra_core.Rule_graph.Active} in
  Test_workspace.with_dir "fhe-program" (fun path ->
    let store = Lwt_main.run (S.open_store ~fresh:true path) in
    Fun.protect ~finally:(fun () -> Lwt_main.run (S.close store)) (fun () ->
      let journal = J.create () in
      let address, result = V.Contract.deploy ~journal ~admitted:checked ~ctx store
        caller "CUSTOM" [||] package.envelope 0 in
      expect "native program creation refused" result.success;
      V.Program_store.stage store journal;
      J.discard journal;
      let commit = Lwt_main.run (S.get_commit_hash store) in
      let params = [`String caller;
        `String (Base64.encode_exn (P.serialize_cipher cipher |> Bytes.to_string))] in
      let run () =
        let ctx = {ctx with fhe_memory = Some (V.Fhe_memory.create ())} in
        V.Contract.execute_call_async ~journal ~ctx
          ~limit:40_000_000 store address "add" params caller Z.zero in
      let values () = J.storage_entries journal |> List.map (fun (address, table) ->
        address, List.sort compare (Hashtbl.fold (fun key value rows -> (key, value) :: rows) table []))
        |> List.sort compare in
      let initial = Lwt_main.run (run ()) in
      expect "native program control refused" (initial.success && initial.return_value = Some (VM.VInt Z.one));
      let saved = values () in
      with_env "OCTRA_PVAC_VERIFY_WORKER" Sys.executable_name (fun () ->
        with_env "OCTRA_FHE_TEST" "crash" (fun () ->
          let module Private = Octra_core.Private_ledger in
          let resource = try
            Private.worker_retry (fun () -> Lwt.map ignore (run ()))
            |> Lwt_main.run;
            false
          with Private.Worker_stopped reason -> reason = "host unavailable" in
          expect "native failure became a receipt" resource));
      expect "native failure changed journal" (values () = saved);
      expect "native failure changed store" (Lwt_main.run (S.get_commit_hash store) = commit);
      let retry = Lwt_main.run (run ()) in
      expect "native program retry refused"
        (retry.success && retry.return_value = Some (VM.VInt (Z.of_int 2)))))

let cancel_checks request =
  let open Lwt.Syntax in
  let actor = T.create () in
  let deadline = Int64.add (Mtime_clock.elapsed_ns ()) 10_000_000_000L in
  let pending = T.run ~actor ~ticket:(ticket 0) ~deadline request in
  expect "native worker not started" (T.stats ~actor () = (true, 0));
  Lwt.cancel pending;
  expect "cancel released native slot" (T.stats ~actor () = (true, 0));
  let queued = List.init Q.capacity (fun id ->
    T.run ~actor ~ticket:(ticket (id + 1)) ~deadline request) in
  expect "native queue differs" (T.stats ~actor () = (true, Q.capacity));
  let* overflow = T.run ~actor ~ticket:(ticket 99) ~deadline request in
  expect "native overflow accepted" (overflow = Error (T.Resource Host));
  T.stop actor;
  let* replies = Lwt.all queued in
  expect "stopped native work returned data" (List.for_all ((=) (Error (T.Resource Host))) replies);
  let rec drain () =
    if fst (T.stats ~actor ()) then
      let* () = Lwt_unix.sleep 0.01 in drain ()
    else Lwt.return_unit in
  let* () = Lwt_unix.with_timeout 10. drain in
  let* refused = T.run ~actor ~ticket:(ticket 88) ~deadline request in
  expect "stopped actor restarted" (refused = Error (T.Resource Host));
  expect "cancel changed to success" (Lwt.state pending = Lwt.Fail Lwt.Canceled);
  Lwt.return_unit

let vm_cancel_check key raw cipher =
  let open Lwt.Syntax in
  let program = VM.[|FHE_LOAD_PK (5, 0); LDI (7, VInt Z.one); SSTORE ("done", 7); STOP|] in
  let cancelled = state true key raw cipher in
  let pending = VM.run_async cancelled program in
  expect "vm did not suspend on key load" (cancelled.pc = 1 && Lwt.is_sleeping pending);
  Lwt.cancel pending;
  let fresh = state true key raw cipher in
  let* ok = VM.run_async fresh program in
  expect "new generation refused" ok;
  expect "cancelled vm wrote storage" (Hashtbl.length cancelled.storage = 0);
  expect "cancelled vm accepted result" (cancelled.regs.(5) = VM.VInt Z.zero && cancelled.reverted);
  expect "cancelled vm returned success" (Lwt.state pending = Lwt.Fail Lwt.Canceled);
  expect "new generation lost write" (Hashtbl.find_opt fresh.storage "done" = Some "1");
  Lwt.return_unit

let () =
  let child = (Array.length Sys.argv = 2
      && List.mem Sys.argv.(1) ["--fhe"; "--fhe-session"])
    || (Array.length Sys.argv = 1 && Option.fold ~none:false
      ~some:((<>) "") (Sys.getenv_opt "OCTRA_FHE_TEST")) in
  if child then begin
    child_sample ();
    exit 0
  end;
  let worker = match Octra_core.Pvac_verify_worker.worker_path () with
    | Some path -> path
    | None -> failwith "native worker missing" in
  Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" worker;
  owner_checks ();
  List.iter owner_cancel ["submit"; "cancel"; "expiry"];
  wire_checks ();
  queue_checks ();
  let key, secret = P.keygen_from_seed (P.default_params ()) (Bytes.make 32 '\029') in
  image_checks worker key secret;
  let raw = P.serialize_pubkey key |> Bytes.to_string in
  let cipher = P.enc_value_seeded key secret 9L (Bytes.make 32 '\030') in
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--cost" then begin
    key_cost key secret cipher;
    exit 0
  end;
  session_checks worker key cipher;
  session_priority key cipher;
  session_recover key cipher;
  session_cost key cipher;
  List.iter (fun cancel -> session_retire ~cancel key cipher) [false; true];
  let original = P.serialize_cipher cipher in
  Lwt_main.run (identity_checks key raw cipher);
  Lwt_main.run (native_checks key raw cipher);
  prior_checks key raw cipher;
  Lwt_main.run (math_checks key raw cipher);
  let request = T.Commit (Image.of_key key, Image.of_cipher cipher) in
  let proof = T.Verify (true, Wire.Ping) in
  process_priority worker;
  List.iter (fun request ->
    process_checks request;
    process_cancel ~expire:false request;
    process_cancel ~expire:true request) [request; proof];
  List.iter (fun math ->
    expect "proof owner rejected ping" (T.eval (T.Verify (math, Wire.Ping)) = Ok (T.Verified true));
    let request = Wire.Range {pubkey = "invalid"; cipher = "hfhe_v1|AA==";
      proof = "range_v1|AA=="; strict = true} in
    expect "proof owner accepted invalid proof"
      (T.eval (T.Verify (math, request)) = Ok (T.Verified false))) [false; true];
  program_failure key raw cipher;
  List.iter (fun request -> Lwt_main.run (cancel_checks request)) [T.Load_key raw; proof];
  Lwt_main.run (vm_cancel_check key raw cipher);
  expect "worker changed key" (P.serialize_pubkey key |> Bytes.to_string = raw);
  expect "worker changed cipher" (P.serialize_cipher cipher = original);
  print_endline "event = fhe_task status = passed"