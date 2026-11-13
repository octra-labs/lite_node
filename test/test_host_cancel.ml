(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Core = Octra_core
module Native = Core.Circle_wasm_native
module Worker = Core.Circle_wasm_worker
module Work = Core.Exec_resource

let need yes reason = if not yes then failwith reason
let run = Lwt_main.run
let seconds () = Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e9

let code ?request ?(writes = 0) instructions =
  let byte value = String.make 1 (Char.chr value) in
  let rec size value = if value < 128 then byte value
    else byte ((value land 127) lor 128) ^ size (value lsr 7) in
  let string value = size (String.length value) ^ value in
  let vector values = size (List.length values) ^ String.concat "" values in
  let section tag value = byte tag ^ string value in
  let export name kind index = string name ^ byte kind ^ size index in
  let body value = string ("\000" ^ value ^ "\011") in
  let imported = if Option.is_some request || writes > 0 then 1 else 0 in
  let data = if writes = 0 then Option.value request ~default:"" else
    List.init writes (fun i -> Printf.sprintf "w%02d\000" i) |> String.concat "" in
  let rec integer value = if value < 64 then byte value
    else byte ((value land 127) lor 128) ^ integer (value lsr 7) in
  let const value = "\065" ^ integer value in
  let invoke = match request with None -> String.concat "" (List.init writes (fun i ->
      const (i * 4) ^ const 3 ^ const 2_097_152 ^ const 4_000_000 ^ "\016\000\026"))
    | Some raw ->
    const 0 ^ const (String.length raw) ^ "\016\000\026" in
  String.concat "" ["\000asm\001\000\000\000";
    section 1 (vector ["\096\001\127\001\127"; "\096\002\127\127\001\127";
      "\096\004\127\127\127\127\001\127"]);
    (if imported = 0 then "" else section 2
      (vector [string "octra" ^ (if writes = 0 then string "host_circle_invoke_len" ^ "\000\001"
        else string "host_kv_put" ^ "\000\002")]));
    section 3 (vector [size 0; size 1]);
    section 5 (vector ["\000" ^ size (if writes = 0 then 1 else 160)]);
    section 7 (vector [export "memory" 2 0; export "octra_alloc" 0 imported; export "octra_update" 0 (imported + 1)]);
    section 10 (vector [body (const (String.length data)); body (invoke ^ instructions)]);
    (if imported = 0 then "" else section 11 (vector ["\000\065\000\011" ^ string data]))]

let payload ?request instructions = Yojson.Safe.to_string (`Assoc [
  "action", `String "execute";
  "code_b64", `String (Base64.encode_exn (code ?request instructions));
  "export_name", `String "octra_update";
  "request_b64", `String "";
  "storage_pairs", `List [];
  "fuel_limit", `Int 20_000_000;
  "execution_profile", `String "standard";
  "hfhe_strict", `Bool false;
  "update_policy", `Bool true;
  "is_view", `Bool false])

let unused _ = failwith "unexpected host callback"

let no_children () =
  need (try ignore (Unix.waitpid [Unix.WNOHANG] (-1)); false
    with Unix.Unix_error (Unix.ECHILD, _, _) -> true) "host child not reaped"

let test_equal () =
  List.iter (fun instructions ->
    let body = payload instructions in
    let expected = run (Native.run_call unused body) in
    let actual = run (Worker.run ~call:unused body) in
    need (actual = expected) "native host output differs";
    (match actual with Ok _ -> () | Error error -> failwith (Native.error_message error));
    no_children ()) ["\065\000"; "\000\065\000"]

let test_cancel () =
  let request = Core.Circle_wasm_codec.encode_request ~method_name:"call_value" [] |> Result.get_ok in
  let input = payload ~request "\065\000" in
  let before = Core.Compute_memory.stats Core.Compute_memory.shared in
  List.iter (fun limited ->
    let entered = ref None in
    let clock () = Option.fold ~none:0. ~some:(fun started -> seconds () -. started) !entered in
    let call _ = entered := Some (seconds ()); fst (Lwt.task ()) in
    let started = Unix.gettimeofday () in
    let result = run (Lwt_unix.with_timeout 1. (fun () ->
      Work.select ~clock ?limit:(if limited then Some 0.02 else None)
      ~running:(fun () -> limited || clock () < 0.02) (fun () ->
        Lwt.catch (fun () -> Work.run ~hash:"heavy" (fun () ->
          Lwt.map (fun _ -> false) (Worker.run ~call input)))
          (function Work.Exhausted ("heavy", Work.Time) -> Lwt.return_true | error -> Lwt.fail error)))) in
    need (result && Option.is_some !entered) "native work ignored cancellation";
    let elapsed = Unix.gettimeofday () -. started in
    need (elapsed < 1.) "host cancellation latency exceeded";
    Printf.printf "event = host_cancel limited = %b elapsed_ms = %.3f\n%!" limited (elapsed *. 1000.);
    need (Core.Compute_memory.stats Core.Compute_memory.shared = before) "host memory not released";
    no_children ()) [true; false]

let test_closed () =
  let before = Core.Compute_memory.stats Core.Compute_memory.shared in
  let refused = run (Work.select ~running:(fun () -> false) (fun () ->
    Lwt.catch (fun () -> Lwt.map (fun _ -> false) (Worker.run (payload "\065\000")))
      (function Work.Unavailable Work.Time -> Lwt.return_true | error -> Lwt.fail error))) in
  need refused "closed host accepted";
  need (Core.Compute_memory.stats Core.Compute_memory.shared = before) "closed host leaked memory";
  no_children ()

let test_busy () =
  let lane = Core.Circle_wasm_hfhe_backend.verifier_lane in
  Mutex.lock lane;
  Fun.protect ~finally:(fun () -> Mutex.unlock lane) (fun () ->
    let result = run (Worker.run ~call:unused (payload "\065\000")) in
    need (Result.is_ok result) "standard execution depends on hfhe lane";
    no_children ())

let nested bytes () =
  let request = Core.Circle_wasm_codec.encode_request ~method_name:"call_value" [] |> Result.get_ok in
  let raw = payload ~request "\065\000" in
  let input = raw ^ String.make (max 0 (bytes - String.length raw)) ' ' in
  let leaf = payload "\065\000" in
  let leaf = leaf ^ String.make (max 0 (bytes - String.length leaf)) ' ' in
  let count = ref 0 in
  let before = Core.Compute_memory.stats Core.Compute_memory.shared in
  let rec invoke depth =
    incr count;
    if depth = 0 then Worker.run ~call:unused leaf else
    Worker.run ~call:(fun raw ->
      need ((Worker.memory_stats ()).jobs = !count + 1)
        "nested input not accounted";
      need ((Worker.memory_stats ()).bytes >= !count * String.length input
        + Core.Pvac_verify_protocol.max_request_bytes)
        "nested bytes not accounted";
      let id = Yojson.Safe.Util.member "id" (Yojson.Safe.from_string raw) in
      let open Lwt.Syntax in
      let* result = invoke (depth - 1) in
      need (Result.is_ok result) "nested execution refused";
      Lwt.return (Yojson.Safe.to_string (`Assoc ["id", id; "response_b64", `String "";
        "storage_pairs", `List []; "effort", `Int 0]))) input in
  need (Result.is_ok (run (invoke 8)) && !count = 9) "depth eight execution differs";
  need (Core.Compute_memory.stats Core.Compute_memory.shared = before) "nested memory leaked";
  need ((Worker.memory_stats ()).jobs = 0 && (Worker.memory_stats ()).bytes = 0)
    "required memory leaked";
  no_children ()

let test_nested () = nested 0 ()
let test_nested_large () = nested 60_000_000 ()

let test_roots () =
  let request = Core.Circle_wasm_codec.encode_request ~method_name:"call_value" [] |> Result.get_ok in
  let pad raw = raw ^ String.make (40 * 1_048_576 - String.length raw) ' ' in
  let input = pad (payload ~request "\065\000") in
  let leaf = pad (payload "\065\000") in
  List.iter (fun cancel ->
    let before = Core.Compute_memory.stats Core.Compute_memory.shared in
    let entered, wake = Lwt.wait () and ready, finish = Lwt.task () in
    let release () = if Lwt.is_sleeping ready then Lwt.wakeup_later finish () in
    let joined work = Lwt.catch (fun () -> Lwt.map ignore work) (fun _ -> Lwt.return_unit) in
    let waiting = ref Lwt.return_unit in
    let held = Worker.run ~call:(fun raw ->
      let id = Yojson.Safe.Util.member "id" (Yojson.Safe.from_string raw) in
      Lwt.wakeup_later wake ();
      Lwt.map (fun () -> Yojson.Safe.to_string (`Assoc ["id", id;
        "response_b64", `String ""; "storage_pairs", `List []; "effort", `Int 0])) ready) input in
    run (Lwt_unix.with_timeout 20. (fun () -> Lwt.finalize (fun () ->
      let open Lwt.Syntax in
      let* () = entered in
      let second = Worker.run ~call:unused leaf in
      waiting := joined second;
      let* () = Lwt_unix.sleep 4. in
      need (Lwt.is_sleeping second) "large root exhausted waiting";
      let queued = Worker.inbox_stats () in
      need (queued.bytes = String.length leaf && queued.jobs = 1) "large root queue accounting differs";
      let frames = Worker.memory_stats () in
      need (frames.bytes = String.length input + Core.Pvac_verify_protocol.max_request_bytes
        && frames.jobs = 2) "large root running accounting differs";
      let* second = if cancel <> `Waiting then Lwt.return second else begin
        Lwt.cancel second;
        let* () = !waiting in
        let queued = Worker.inbox_stats () in
        need (queued.bytes = 0 && queued.jobs = 0 && Lwt.is_sleeping held)
          "large root cancellation lost owner";
        let next = Worker.run ~call:unused leaf in
        waiting := joined next;
        Lwt.return next
      end in
      let* () = match cancel with
        | `Active ->
          Lwt.cancel held;
          Lwt.catch (fun () -> let* _ = held in Lwt.fail_with "active root was not cancelled")
            (function Lwt.Canceled -> Lwt.return_unit | error -> Lwt.fail error)
        | `None | `Waiting ->
          release ();
          let* first = held in
          need (Result.is_ok first) "large root result differs";
          Lwt.return_unit in
      let* second = second in
      need (Result.is_ok second) "large root result differs";
      Lwt.return_unit) (fun () ->
        release ();
        Lwt.bind (joined held) (fun () -> !waiting))));
    need (Core.Compute_memory.stats Core.Compute_memory.shared = before) "large roots retained shared memory";
    List.iter (fun usage -> need (usage.Core.Compute_memory.bytes = 0 && usage.jobs = 0)
      "large roots retained input") [Worker.inbox_stats (); Worker.memory_stats ()];
    no_children ()) [`None; `Waiting; `Active]

let test_slot_wait () =
  let request = Core.Circle_wasm_codec.encode_request ~method_name:"call_value" [] |> Result.get_ok in
  let entered, wake = Lwt.wait () in
  let held = Worker.run ~call:(fun _ ->
    Lwt.wakeup_later wake (); fst (Lwt.task ())) (payload ~request "\065\000") in
  let now = ref 0. in
  let result = run (Lwt.finalize (fun () ->
    let open Lwt.Syntax in
    let* () = entered in
    let pending = Work.select ~clock:(fun () -> !now) ~limit:0.5 ~running:(fun () -> true)
      (fun () -> Work.run ~hash:"waiting" (fun () -> Work.interrupt (fun () ->
        Worker.run ~call:unused (payload "\065\000")))) in
    let* () = Lwt.pause () in
    now := 100.;
    let* () = Lwt_unix.sleep 0.03 in
    need (Lwt.is_sleeping pending) "slot wait charged execution";
    Lwt.cancel held;
    pending) (fun () -> Lwt.cancel held;
      Lwt.catch (fun () -> Lwt.map ignore held) (function
        Lwt.Canceled -> Lwt.return_unit | error -> Lwt.fail error))) in
  need (Result.is_ok result) "waiting host refused";
  no_children ()

let test_slot_cancel () =
  let request = Core.Circle_wasm_codec.encode_request ~method_name:"call_value" [] |> Result.get_ok in
  let entered, wake = Lwt.wait () in
  let held = Worker.run ~call:(fun _ ->
    Lwt.wakeup_later wake (); fst (Lwt.task ())) (payload ~request "\065\000") in
  let current = ref true in
  run (Lwt.finalize (fun () ->
    let open Lwt.Syntax in
    let* () = entered in
    let pending = Work.select ~running:(fun () -> !current) (fun () ->
      Work.run ~hash:"waiting-cancel" (fun () -> Work.interrupt (fun () ->
        Worker.run ~call:unused (payload "\065\000")))) in
    let* () = Lwt.pause () in
    current := false;
    let* stopped = Lwt_unix.with_timeout 1. (fun () ->
      Lwt.catch (fun () -> Lwt.map (fun _ -> false) pending)
        (function Work.Exhausted ("waiting-cancel", Work.Time) -> Lwt.return_true
          | error -> Lwt.fail error)) in
    need stopped "queued host ignored context change";
    need (Lwt.is_sleeping held) "queued cancellation stopped owner";
    Lwt.return_unit) (fun () -> Lwt.cancel held;
      Lwt.catch (fun () -> Lwt.map ignore held) (function
        Lwt.Canceled -> Lwt.return_unit | error -> Lwt.fail error)));
  need (Result.is_ok (run (Worker.run ~call:unused (payload "\065\000"))))
    "queued cancellation retained slot";
  no_children ()

let test_proof_wait () =
  let module W = Core.Pvac_verify_worker in
  let held, release = Lwt.task () in
  let busy = Core.Compute_pool.run_async W.proof_channel Required (fun () -> held) in
  let now = ref 0. in
  let pending = Work.select ~clock:(fun () -> !now) ~limit:0.5 ~running:(fun () -> true)
    (fun () -> Work.run ~hash:"proof-wait" (fun () -> Work.interrupt (fun () ->
      Work.interrupt (fun () -> W.run_joined
        (W.P.Zero {pubkey = "x"; cipher = "x"; proof = "x"}))))) in
  let result = run (Lwt.finalize (fun () ->
    let open Lwt.Syntax in
    let* () = Lwt.pause () in
    now := 100.;
    let* () = Lwt_unix.sleep 0.03 in
    need (Lwt.is_sleeping pending) "proof queue charged execution";
    Lwt.wakeup_later release ();
    pending) (fun () -> Lwt.cancel held; Lwt.cancel pending;
      Lwt.catch (fun () -> Lwt.map ignore busy)
        (function Lwt.Canceled -> Lwt.return_unit | error -> Lwt.fail error))) in
  need (match W.classified_outcome_result result with
    | Error (W.Proof_rejected _) -> true | _ -> false) "proof wait lost verification";
  need ((W.channel_stats ()).active = 0) "proof slot leaked";
  no_children ()

let test_memory_wait () =
  let memory = Core.Compute_memory.shared in
  let before = Core.Compute_memory.stats memory in
  let ticket = Core.Compute_memory.reserve memory ~speculative:false
    (Core.Compute_memory.limit_of_getenv Sys.getenv_opt) |> Option.get in
  let pending = Worker.run ~call:unused (payload "\065\000") in
  let result = run (Lwt.finalize (fun () ->
    Lwt.catch (fun () -> Lwt_unix.with_timeout 10. (fun () -> pending))
      (function Work.Unavailable Work.Memory ->
        Lwt.fail_with "required root depends on shared memory" | error -> Lwt.fail error)) (fun () ->
      Core.Compute_memory.release ticket; Lwt.cancel pending; Lwt.return_unit)) in
  need (Result.is_ok result) "required memory retry failed";
  need (Core.Compute_memory.stats memory = before) "required memory retry leaked";
  no_children ()

let test_local_failure () =
  let previous = Sys.getenv "OCTRA_PVAC_VERIFY_WORKER" in
  Fun.protect ~finally:(fun () -> Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" previous) (fun () ->
    Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" "/no-worker";
    need (Result.is_error (run (Worker.check ()))) "missing worker passed startup check";
    let refused = run (Lwt.catch (fun () -> Lwt.map (fun _ -> false)
      (Worker.run (payload "\065\000")))
      (function Work.Unavailable Work.Host -> Lwt.return_true | error -> Lwt.fail error)) in
    need refused "local process failure became native result";
    let fields = Yojson.Safe.Util.to_assoc (Yojson.Safe.from_string (payload "\065\000")) in
    List.iter (fun stable ->
      let input = `Assoc (("stable_errors", `Bool stable) :: fields) in
      let isolated = run (Lwt.catch (fun () ->
        Lwt.map (fun result -> need (Result.is_ok result) "prior host failed"; false)
          (Core.Circle_wasm_host.read_process_json input))
        (function Work.Unavailable Work.Host -> Lwt.return_true | error -> Lwt.fail error)) in
      need (isolated = stable) "host profile route differs") [false; true])

let test_input_size () =
  let memory = Core.Compute_memory.shared in
  let before = Core.Compute_memory.stats memory in
  let input = String.make (Native.max_call_input_bytes + 1) ' ' in
  need (match run (Native.run_call unused input) with
    | Error (Native.Unavailable _) -> true | _ -> false) "native size outcome differs";
  let ticket = Core.Compute_memory.reserve memory ~speculative:false
    (Core.Compute_memory.limit_of_getenv Sys.getenv_opt) |> Option.get in
  let rejected = run (Lwt.finalize (fun () ->
    Lwt_unix.with_timeout 1. (fun () ->
      Lwt.catch (fun () -> Lwt.map (fun _ -> false) (Worker.run ~call:unused input))
        (function Work.Unavailable Work.Host -> Lwt.return_true | error -> Lwt.fail error)))
    (fun () -> Core.Compute_memory.release ticket; Lwt.return_unit)) in
  need rejected "oversize input entered queue";
  need (Core.Compute_memory.stats memory = before) "oversize input leaked memory";
  no_children ()

let test_large_output () =
  let fields = Yojson.Safe.Util.to_assoc (Yojson.Safe.from_string (payload "\065\000")) in
  let storage = List.init 12 (fun i -> `Assoc [
    "key_b64", `String (Base64.encode_exn (Printf.sprintf "s%03d" i));
    "value_b64", `String (Base64.encode_exn (String.make 3_200_000 'a'))]) in
  let fields = List.remove_assoc "code_b64" fields |> List.remove_assoc "storage_pairs" in
  let input = Yojson.Safe.to_string (`Assoc (("code_b64", `String (Base64.encode_exn
    (code ~writes:4 "\065\000"))) :: ("storage_pairs", `List storage) :: fields)) in
  need (String.length input < Native.max_call_input_bytes) "large input exceeds protocol";
  let expected = run (Native.run_call unused input) |> Result.get_ok in
  need (String.length expected > Native.max_call_input_bytes) "large output case too small";
  let actual = run (Lwt_unix.with_timeout 30. (fun () -> Worker.run ~call:unused input))
    |> Result.get_ok in
  need (actual = expected) "large output differs";
  no_children ()

let test_capacity () =
  let request = Core.Circle_wasm_codec.encode_request ~method_name:"call_value" [] |> Result.get_ok in
  let input = payload ~request "\065\000" in
  let entered, wake = Lwt.wait () in
  let held = Worker.run ~optional:true ~call:(fun _ ->
    Lwt.wakeup_later wake (); fst (Lwt.task ())) input in
  let result = run (Lwt.finalize (fun () ->
    let open Lwt.Syntax in
    let* () = entered in
    let* rejected = Lwt.catch (fun () -> Lwt.map (fun _ -> false)
      (Worker.run ~optional:true input))
      (function Work.Unavailable Work.Host -> Lwt.return_true | error -> Lwt.fail error) in
    need rejected "busy view was queued";
    Lwt_unix.with_timeout 1. (fun () -> Worker.run ~call:unused (payload "\065\000")))
    (fun () -> Lwt.cancel held;
      Lwt.catch (fun () -> Lwt.map ignore held) (function Lwt.Canceled -> Lwt.return_unit | e -> Lwt.fail e))) in
  need (Result.is_ok result) "view blocked required execution";
  need (Result.is_ok (run (Worker.check ()))) "worker startup check failed";
  no_children ()

let test_direct_cancel () =
  let before = Core.Compute_memory.stats Core.Compute_memory.shared in
  let request = Core.Circle_wasm_codec.encode_request ~method_name:"call_value" [] |> Result.get_ok in
  let entered, wake = Lwt.wait () in
  let pending = Worker.run ~call:(fun _ -> Lwt.wakeup_later wake (); fst (Lwt.task ()))
    (payload ~request "\065\000") in
  let timer = Lwt.bind entered (fun () ->
    Lwt.cancel pending; Lwt.return_unit) in
  let cancelled = run (Lwt_unix.with_timeout 1. (fun () -> Lwt.catch (fun () -> Lwt.map (fun _ -> false) pending)
    (function Lwt.Canceled -> Lwt.return_true | error -> Lwt.fail error))) in
  Lwt.cancel timer;
  need cancelled "explicit host cancel ignored";
  need (Core.Compute_memory.stats Core.Compute_memory.shared = before) "explicit cancel leaked memory";
  no_children ()

let test_host_calls () =
  let request = Core.Circle_wasm_codec.encode_request ~method_name:"call_value" [] |> Result.get_ok in
  let input = payload ~request "\065\000" in
  let queries = ref [] in
  let call raw =
    queries := raw :: !queries;
    let id = Yojson.Safe.Util.member "id" (Yojson.Safe.from_string raw) in
    let open Lwt.Syntax in
    let* nested = Worker.run ~call:unused (payload "\065\000") in
    need (Result.is_ok nested) "nested host not available";
    Lwt.return (Yojson.Safe.to_string (`Assoc ["id", id; "response_b64", `String "";
      "storage_pairs", `List []; "effort", `Int 0])) in
  let expected = run (Native.run_call call input) in
  let first = !queries in
  queries := [];
  let actual = run (Worker.run ~call input) in
  need (Result.is_ok actual && actual = expected && !queries = first && List.length first = 1)
    "host resume differs";
  no_children ();
  let entered = ref false in
  let current = ref true in
  let call _ = entered := true; current := false; fst (Lwt.task ()) in
  let refused = run (Work.select ~running:(fun () -> !current) (fun () ->
    Lwt.catch (fun () -> Work.run ~hash:"callback" (fun () ->
      Lwt.map (fun _ -> false) (Worker.run ~call input)))
      (function Work.Exhausted ("callback", Work.Time) -> Lwt.return_true | error -> Lwt.fail error))) in
  need (refused && !entered) "host callback not cancelled";
  no_children ()

let test_batches () =
  let request = Core.Circle_wasm_codec.encode_request ~method_name:"call_value" [] |> Result.get_ok in
  let fields = Yojson.Safe.Util.to_assoc (Yojson.Safe.from_string
    (payload ~request (String.make 2000 '\001' ^ "\065\000"))) in
  List.iter (fun bytes ->
    let storage = if bytes = 0 then [] else [`Assoc [
      "key_b64", `String (Base64.encode_exn "amount");
      "value_b64", `String (Base64.encode_exn (String.make bytes 'a'))]] in
    let input = Yojson.Safe.to_string (`Assoc (("stable_errors", `Bool true) ::
      ("storage_pairs", `List storage) :: List.remove_assoc "storage_pairs" fields)) in
    let replies = ref [] in
    let call raw =
      replies := raw :: !replies;
      let id = Yojson.Safe.Util.member "id" (Yojson.Safe.from_string raw) in
      Lwt.return (Yojson.Safe.to_string (`Assoc ["id", id; "response_b64", `String "";
        "storage_pairs", `List storage; "effort", `Int 0])) in
    let expected = run (Native.run_call call input) in
    need (Result.is_ok expected && List.length !replies = 1) "batch reference incomplete";
    let output = Yojson.Safe.from_string (Result.get_ok expected) in
    need (Yojson.Safe.Util.member "success" output = `Bool true) "batch reference refused";
    let query = List.hd !replies in
    List.iter (fun count ->
      List.iter (fun (mode, execute) ->
        replies := [];
        let before = Core.Compute_memory.stats Core.Compute_memory.shared in
        let started = seconds () in
        List.iter (fun _ ->
          let actual = run (execute input) in
          need (actual = expected) "batch output differs";
          need (Core.Compute_memory.stats Core.Compute_memory.shared = before) "batch memory leaked";
          no_children ()) (List.init count Fun.id);
        need (List.length !replies = count && List.for_all (( = ) query) !replies)
          "batch callback count differs";
        Printf.printf "event = host_batch mode = %s bytes = %d calls = %d elapsed_ms = %.3f\n%!"
          mode bytes count ((seconds () -. started) *. 1000.))
        ["native", Native.run_call call; "isolated", Worker.run ~optional:false ~call]) [1; 4; 16])
    [0; 131_072; 1_048_576]

let child_worker () =
  ignore (Unix.setsid ());
  let input = Core.Fhe_calc.read_frame () in
  if input = "Cframe-large" || input = "Cframe-limit" then begin
    Core.Fhe_calc.write_frame "3ready";
    need (Core.Fhe_calc.read_frame () = "ready") "proof frame handshake differs";
    if input = "Cframe-large" then begin
      let header = Bytes.create 8 in
      Bytes.set_int64_be header 0 (Int64.of_int (Core.Pvac_verify_protocol.max_request_bytes + 2));
      output_bytes stdout header;
      output_char stdout '4';
      flush stdout;
      Unix.sleep 30; Unix._exit 0
    end;
    let request = Core.Pvac_verify_protocol.request_bytes Core.Pvac_verify_protocol.Ping in
    Core.Fhe_calc.write_frame ("4" ^ request ^ String.make
      (Core.Pvac_verify_protocol.max_request_bytes - String.length request) ' ');
    need (Core.Fhe_calc.read_frame () = "0") "proof frame result differs";
    Core.Fhe_calc.write_frame "0proof-frame";
    Unix._exit 0
  end;
  if input = "Ccredit" then begin
    Core.Fhe_calc.write_frame "3ready";
    need (Core.Fhe_calc.read_frame () = "ready") "proof pressure handshake differs";
    for _ = 1 to 3 do
      Core.Fhe_calc.write_frame ("4" ^ Core.Pvac_verify_protocol.request_bytes
        (Core.Pvac_verify_protocol.Zero {pubkey = "x"; cipher = "x"; proof = "x"}));
      need (Core.Fhe_calc.read_frame () = "1") "proof pressure verdict differs"
    done;
    Core.Fhe_calc.write_frame "0proof-rejected";
    Unix._exit 0
  end;
  if input = "Cproof" then begin
    Core.Fhe_calc.write_frame ("4" ^ Core.Pvac_verify_protocol.request_bytes
      (Core.Pvac_verify_protocol.Zero {pubkey = "x"; cipher = "x"; proof = "x"}));
    Unix.sleep 5; Unix._exit 0
  end;
  if input = "Cblocked" then begin Unix.sleep 30; Unix._exit 0 end;
  let child = Unix.fork () in
  if child = 0 then begin Unix.sleep 30; Unix._exit 0 end;
  Core.Fhe_calc.write_frame ("3" ^ string_of_int child);
  Unix.sleep 30

let test_proof_frame () =
  List.iter (fun (body, accepted) ->
    let previous = Sys.getenv "OCTRA_PVAC_VERIFY_WORKER" in
    let before = Core.Compute_memory.stats Core.Compute_memory.shared in
    let frames = Worker.memory_stats () in
    Fun.protect ~finally:(fun () -> Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" previous) (fun () ->
      Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" Sys.executable_name;
      let call raw =
        need (raw = "ready") "proof frame callback differs";
        Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" previous;
        Lwt.return "ready" in
      let result = run (Lwt.catch (fun () ->
        Lwt.map Option.some (Work.select ~limit:(if accepted then 30. else 2.)
          ~running:(fun () -> true) (fun () ->
            Work.run ~hash:"proof-frame" (fun () -> Worker.run ~call body)))) (function
        | Work.Exhausted ("proof-frame", Work.Host) -> Lwt.return_none
        | Work.Exhausted ("proof-frame", Work.Time) ->
          Lwt.return_some (Error (Native.Unavailable "frame timeout"))
        | error -> Lwt.fail error)) in
      need (Core.Compute_memory.stats Core.Compute_memory.shared = before
        && Worker.memory_stats () = frames) "proof frame retained memory";
      no_children ();
      if accepted then need (result = Some (Ok "proof-frame")) "maximum proof frame refused"
      else need (result = None) "oversize proof frame read body"))
    ["frame-large", false; "frame-limit", true]

let test_proof_capacity () =
  let module M = Core.Compute_memory in
  let module Pool = Core.Compute_pool in
  let module W = Core.Pvac_verify_worker in
  List.iter (fun (optional, kind) ->
    let previous = Sys.getenv "OCTRA_PVAC_VERIFY_WORKER" in
    let before = M.stats M.shared in
    let frames = Worker.memory_stats () in
    let held = ref [] and jobs = ref [] in
    let timer = ref None in
    let ready, release = Lwt.wait () in
    let release_jobs () = if Lwt.is_sleeping ready then Lwt.wakeup_later release () in
    let call raw =
      need (raw = "ready") "proof pressure callback differs";
      Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" previous;
      begin match kind with
      | "bytes" ->
        held := [M.reserve M.shared ~speculative:false
          (M.limit_of_getenv Sys.getenv_opt - (M.stats M.shared).bytes) |> Option.get]
      | "slots" ->
        let rec fill () = match M.reserve M.shared ~speculative:false 0 with
          | None -> () | Some ticket -> held := ticket :: !held; fill () in
        fill ()
      | "queue" ->
        for _ = 1 to W.capacity + Core.Resource_lanes.preverify_required_queue_limit + 1 do
          let job = Pool.run_async W.proof_channel Pool.Required (fun () -> Lwt.protected ready) in
          match Lwt.state job with
          | Lwt.Sleep -> jobs := job :: !jobs
          | Lwt.Return None -> ()
          | _ -> failwith "proof queue setup differs"
        done;
        let count = (W.channel_stats ()).required_waiting in
        need (count > 0) "proof queue was not filled";
        let rec wait () =
          let current = (W.channel_stats ()).required_waiting in
          if current > count then begin
            need (current <= Core.Resource_lanes.preverify_required_queue_limit)
              "required queue limit exceeded";
            Lwt.return_unit
          end else Lwt.bind (Lwt_unix.sleep 0.001) wait in
        timer := Some (Lwt.finalize wait (fun () -> release_jobs (); Lwt.return_unit))
      | _ -> failwith "proof pressure mode differs"
      end;
      Lwt.return "ready" in
    Fun.protect ~finally:(fun () -> Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" previous) (fun () ->
      Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" Sys.executable_name;
      let result = run (Lwt_unix.with_timeout 3. (fun () -> Lwt.finalize (fun () ->
        Lwt.catch (fun () ->
          let open Lwt.Syntax in
          let* result = Worker.run ~optional ~call "credit" in
          let* () = Option.value !timer ~default:Lwt.return_unit in
          Lwt.return_some result)
          (function Work.Unavailable Work.Memory -> Lwt.return_none | error -> Lwt.fail error))
        (fun () ->
          Option.iter Lwt.cancel !timer;
          release_jobs ();
          List.iter M.release !held;
          Lwt.map ignore (Lwt.all !jobs)))) in
      need (M.stats M.shared = before && Worker.memory_stats () = frames)
        "proof pressure retained memory";
      no_children ();
      if optional then need (result = None) "view used required credit"
      else need (result = Some (Ok "proof-rejected")) ("required child lost " ^ kind)))
    [false, "bytes"; false, "slots"; false, "queue"; true, "bytes"]

let test_proof_cancel () =
  Test_workspace.with_dir "host-proof-cancel" (fun path ->
    let previous = Sys.getenv "OCTRA_PVAC_VERIFY_WORKER" in
    let marker = Filename.concat path "started" in
    let before = Core.Compute_memory.stats Core.Compute_memory.shared in
    Lwt_preemptive.simple_init ();
    let pool = Lwt_preemptive.get_bounds () in
    Lwt_preemptive.set_bounds (0, 1);
    let lock = Mutex.create () in
    let condition = Condition.create () in
    let entered = Atomic.make false in
    let released = ref false in
    let release () =
      Mutex.lock lock;
      released := true;
      Condition.broadcast condition;
      Mutex.unlock lock in
    let busy = Lwt_preemptive.detach (fun () ->
      Mutex.lock lock;
      Atomic.set entered true;
      while not !released do Condition.wait condition lock done;
      Mutex.unlock lock) () in
    Fun.protect ~finally:(fun () ->
      release ();
      run busy;
      Lwt_preemptive.set_bounds pool;
      Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" previous;
      Unix.putenv "OCTRA_HOST_PROOF_TEST" "") (fun () ->
      let rec ready () = if Atomic.get entered then Lwt.return_unit else
        Lwt.bind (Lwt_unix.sleep 0.001) ready in
      run (ready ());
      let timer = Lwt.bind (Lwt_unix.sleep 0.5) (fun () -> release (); Lwt.return_unit) in
      Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" Sys.executable_name;
      Unix.putenv "OCTRA_HOST_PROOF_TEST" marker;
      let stopped = run (Lwt_unix.with_timeout 1. (fun () ->
        Work.select ~running:(fun () -> not (Sys.file_exists marker)) (fun () ->
          Lwt.catch (fun () -> Work.run ~hash:"proof" (fun () ->
            Lwt.map (fun _ -> false) (Worker.run ~call:unused "proof")))
            (function Work.Exhausted ("proof", Work.Time) -> Lwt.return_true | error -> Lwt.fail error)))) in
      need (stopped && Sys.file_exists marker) "parent proof did not cancel";
      need (not !released) "proof cancellation waited for thread pool";
      Lwt.cancel timer;
      need ((Core.Pvac_verify_worker.channel_stats ()).active = 0) "parent proof slot survived";
      need (Core.Compute_memory.stats Core.Compute_memory.shared = before) "parent proof memory survived";
      no_children ()))

let test_blocked_worker () =
  let previous = Sys.getenv "OCTRA_PVAC_VERIFY_WORKER" in
  Fun.protect ~finally:(fun () -> Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" previous) (fun () ->
    Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" Sys.executable_name;
    let stopped = try run (Lwt_unix.with_timeout 1. (fun () ->
      Work.select ~limit:0.02 ~running:(fun () -> true) (fun () ->
        Lwt.catch (fun () -> Work.run ~hash:"blocked" (fun () ->
          Lwt.map (fun _ -> false) (Worker.run ~call:unused "blocked")))
          (function Work.Exhausted ("blocked", Work.Time) -> Lwt.return_true | error -> Lwt.fail error))))
      with Lwt_unix.Timeout -> failwith "host worker did not stop" in
    need stopped "blocked host accepted";
    no_children ())

let test_thread_pool () =
  Lwt_preemptive.simple_init ();
  let previous = Lwt_preemptive.get_bounds () in
  let program = Sys.getenv "OCTRA_PVAC_VERIFY_WORKER" in
  let mutex = Mutex.create () in
  let condition = Condition.create () in
  let started = Atomic.make false in
  let released = ref false in
  let release () =
    Mutex.lock mutex;
    released := true;
    Condition.broadcast condition;
    Mutex.unlock mutex in
  Fun.protect ~finally:(fun () ->
    release ();
    Lwt_preemptive.set_bounds previous;
    Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" program) (fun () ->
    Lwt_preemptive.set_bounds (0, 1);
    Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" Sys.executable_name;
    let busy = Lwt_preemptive.detach (fun () ->
      Mutex.lock mutex;
      Atomic.set started true;
      while not !released do Condition.wait condition mutex done;
      Mutex.unlock mutex) () in
    let open Lwt.Syntax in
    let rec ready () = if Atomic.get started then Lwt.return_unit
      else let* () = Lwt_unix.sleep 0.001 in ready () in
    let result, elapsed = run (Lwt.finalize (fun () ->
      let* () = ready () in
      let timer = let* () = Lwt_unix.sleep 0.3 in release (); Lwt.return_unit in
      Lwt.finalize (fun () ->
        let before = Unix.gettimeofday () in
        let* result = Lwt.catch (fun () -> Lwt_unix.with_timeout 1. (fun () ->
          Work.select ~limit:0.02 ~running:(fun () -> true) (fun () ->
            Lwt.catch (fun () -> Work.run ~hash:"pool" (fun () ->
              Lwt.map (fun _ -> false) (Worker.run ~call:unused "blocked")))
              (function Work.Exhausted ("pool", Work.Time) -> Lwt.return_true | error -> Lwt.fail error))))
          (function Lwt_unix.Timeout -> Lwt.fail_with "host worker did not stop" | error -> Lwt.fail error) in
        Lwt.return (result, Unix.gettimeofday () -. before))
        (fun () -> Lwt.cancel timer; Lwt.return_unit))
      (fun () -> release (); Lwt.no_cancel busy)) in
    need result "pool host not cancelled";
    need (elapsed < 0.15) "host cancellation waited for compute pool";
    Printf.printf "event = host_pool elapsed_ms = %.3f\n%!" (elapsed *. 1000.);
    no_children ())

let test_async_charge () =
  let now = ref 0. in
  let stopped = run (Work.select ~clock:(fun () -> !now) ~limit:0.06 ~running:(fun () -> true) (fun () ->
    Lwt.catch (fun () -> Work.run ~hash:"async" (fun () ->
      let action () = Work.interrupt (fun () ->
        let open Lwt.Syntax in
        let* () = Lwt.pause () in
        now := !now +. 0.04;
        Lwt.return_unit) in
      Lwt.bind (action ()) (fun () -> Lwt.map (fun () -> false) (action ()))))
      (function Work.Exhausted ("async", Work.Time) -> Lwt.return_true | error -> Lwt.fail error))) in
  need stopped "async host time not charged"

let test_descendant () =
  let previous = Sys.getenv "OCTRA_PVAC_VERIFY_WORKER" in
  let child = ref None in
  let current = ref true in
  let cleanup () =
    Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" previous;
    Option.iter (fun pid -> try Unix.kill pid Sys.sigkill
      with Unix.Unix_error (Unix.ESRCH, _, _) -> ()) !child in
  Fun.protect ~finally:cleanup (fun () ->
    Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" Sys.executable_name;
    let call raw = child := Some (int_of_string raw); current := false; fst (Lwt.task ()) in
    let refused = run (Work.select ~running:(fun () -> !current) (fun () ->
      Lwt.catch (fun () -> Lwt.map (fun _ -> false) (Worker.run ~call "owned"))
        (function Work.Unavailable Work.Time -> Lwt.return_true | error -> Lwt.fail error))) in
    need (refused && Option.is_some !child) "descendant case did not start";
    no_children ();
    let pid = Option.get !child in
    let rec gone count =
      try Unix.kill pid 0;
        if count = 0 then false else begin Unix.sleepf 0.01; gone (count - 1) end
      with Unix.Unix_error (Unix.ESRCH, _, _) -> true in
    need (gone 100) "host descendant survived cancellation";
    child := None)

let test_callback_cancel () =
  let cleaned = ref false in
  let pending, _ = Lwt.task () in
  let result = run (Work.select ~running:(fun () -> false) (fun () ->
    Lwt.catch (fun () -> Work.interrupt (fun () ->
      Lwt.finalize (fun () -> pending) (fun () -> cleaned := true; Lwt.return_unit)))
      (function Work.Unavailable Work.Time -> Lwt.return_unit | error -> Lwt.fail error))) in
  need (result = () && not !cleaned) "closed action started";
  let current = ref true in
  let pending, _ = Lwt.task () in
  let timer = Lwt.bind (Lwt_unix.sleep 0.01) (fun () -> current := false; Lwt.return_unit) in
  run (Work.select ~running:(fun () -> !current) (fun () ->
    Lwt.catch (fun () -> Work.interrupt (fun () ->
      Lwt.finalize (fun () -> pending) (fun () ->
        Lwt.bind (Lwt_unix.sleep 0.02) (fun () -> cleaned := true; Lwt.return_unit))))
      (function Work.Unavailable Work.Time -> Lwt.return_unit | error -> Lwt.fail error)));
  Lwt.cancel timer;
  need !cleaned "cancel returned before cleanup"

let () =
  (match Sys.getenv_opt "OCTRA_HOST_PROOF_TEST" with
  | Some marker when marker <> "" && Array.length Sys.argv = 1 ->
    let output = open_out marker in
    output_string output (string_of_int (Unix.getpid ()));
    close_out output;
    Unix.sleep 5;
    exit 0
  | _ -> ());
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--circle-session" then begin
    child_worker (); exit 0
  end;
  let cases =
    ["roots", test_roots; "proof-frame", test_proof_frame; "proof-capacity", test_proof_capacity;
     "pool", test_thread_pool; "charge", test_async_charge;
     "slot-wait", test_slot_wait; "slot-cancel", test_slot_cancel; "proof-wait", test_proof_wait;
     "nested-large", test_nested_large;
     "equal", test_equal; "busy", test_busy; "nested", test_nested;
     "memory", test_memory_wait; "proof", test_proof_cancel;
     "local", test_local_failure; "input", test_input_size;
     "large", test_large_output; "capacity", test_capacity;
     "cancel", test_cancel; "blocked", test_blocked_worker; "closed", test_closed;
     "cleanup", test_callback_cancel; "direct", test_direct_cancel;
     "calls", test_host_calls; "descendant", test_descendant; "batches", test_batches] in
  let cases = if Array.length Sys.argv = 2 then
      [Sys.argv.(1), List.assoc Sys.argv.(1) cases] else cases in
  List.iter (fun (name, test) ->
    Printf.printf "event = host_case name = %s status = begin\n%!" name;
    test ();
    Printf.printf "event = host_case name = %s status = passed\n%!" name)
    cases;
  print_endline "event = host_cancel status = passed"