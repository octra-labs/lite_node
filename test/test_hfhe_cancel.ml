(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C = Octra_core
module P = C.Pvac_verify_protocol
module F = C.Proof_frame
module K = C.Proof_keeper
module Worker = C.Pvac_verify_worker
open Yojson.Safe.Util

let need reason value = if not value then failwith reason

let ready owner =
  let deadline = F.now () +. 120. in
  let rec wait () = match K.phase owner with
    | C.Proof_warm.Ready _ -> ()
    | C.Proof_warm.Closed -> failwith "proof session closed"
    | _ when F.now () >= deadline -> failwith "proof session startup timed out"
    | _ -> Thread.delay 0.01; wait () in
  wait ()

let large path destination =
  let raw = In_channel.with_open_bin path In_channel.input_all in
  need "large proof digest differs" (Digestif.SHA256.(to_hex (digest_string raw))
    = "57fa6b9809d67f78c3414ac4f24807616951ef0e6b6b506ec9573a20485d35de");
  let parts = In_channel.with_open_bin path (fun input ->
    let rows = List.init 5 (fun _ ->
      let size = input_binary_int input in
      need "large proof field exceeds limit" (size > 0 && size <= P.max_request_bytes);
      really_input_string input size) in
    need "large proof has extra bytes" (match input_char input with _ -> false
      | exception End_of_file -> true);
    rows) in
  let value = match parts with
    | [pubkey; cipher; proof; point; digest] -> P.{pubkey;
      cipher = C.Crypto.FheBalance.prefix ^ Base64.encode_exn cipher;
      ciphertext_commitment = Base64.encode_exn digest; proof_kind = Circle_range;
      proof = C.Crypto.FheBalance.range_proof_prefix ^ Base64.encode_exn proof;
      amount_commitment = Base64.encode_exn point; strict = true}
    | _ -> failwith "large proof fields missing" in
  let request = P.Proof_key (P.Math (P.Circle_cell value)) in
  let owner = K.create ~memory:K.min_memory_mb (Sys.getenv "OCTRA_PVAC_VERIFY_WORKER") in
  Fun.protect ~finally:(fun () -> K.stop owner) (fun () ->
    ready owner;
    let parent = (K.stats owner).parent |> Option.get in
    let resident = F.rss parent in
    need "large parent exceeds reserve" (resident > 0 && resident < K.reserve_mb);
    let peak = Atomic.make resident in
    let finished = Atomic.make false in
    let sampler = Thread.create (fun () ->
      while not (Atomic.get finished) do
        List.iter (fun child -> Option.iter (fun memory ->
          Atomic.set peak (max (Atomic.get peak) memory)) (F.total_rss parent child)) (F.children parent);
        Thread.delay 0.01
      done) () in
    let started = F.now () in
    Fun.protect ~finally:(fun () -> Atomic.set finished true; Thread.join sampler) (fun () ->
      (match K.run owner ~control:(Atomic.make C.Proof_link.Continue)
        ~deadline:(F.now () +. 120.) request with
      | K.Accepted reply -> need "large proof refused" (reply.accepted && reply.request_hash = P.request_hash request)
      | _ -> failwith "large proof check unavailable");
      need "large proof skipped prepared table" ((K.stats owner).prepared = 1));
    let elapsed = F.now () -. started in
    need "large proof retained parent bytes" (F.rss parent <= resident + 8);
    need "large proof retained child" (F.children parent = []);
    (match K.run owner ~control:(Atomic.make C.Proof_link.Continue)
      ~deadline:(F.now () +. 2.) request with
    | K.Accepted reply -> need "large replay refused" reply.accepted
    | _ -> failwith "large replay unavailable");
    need "large replay repeated work" ((K.stats owner).prepared = 1 && (K.stats owner).reused = 1);
    let altered = P.Proof_key (P.Math (P.Circle_cell {value with
      ciphertext_commitment = Base64.encode_exn (String.make 32 '\000')})) in
    (match K.run owner ~control:(Atomic.make C.Proof_link.Continue)
      ~deadline:(F.now () +. 10.) altered with
    | K.Recheck (Some token) ->
      need "changed cipher commitment accepted"
        (Result.is_error (C.Pvac_verify_direct.execute ~keyed:true altered));
      K.settle owner token C.Proof_warm.Agreed;
      ready owner
    | _ -> failwith "changed large request reused");
    need "changed request polluted cache" ((K.stats owner).reused = 1 && (K.stats owner).accepted = 2);
    let result = `Assoc ["status", `String "passed";
      "input_sha256", `String Digestif.SHA256.(to_hex (digest_string raw));
      "parent_mb", `Int resident; "parent_after_mb", `Int (F.rss parent);
      "combined_peak_mb", `Int (Atomic.get peak); "check_ms", `Float (elapsed *. 1000.);
      "prepared", `Int (K.stats owner).prepared; "reused", `Int (K.stats owner).reused] in
    Out_channel.with_open_gen [Open_wronly; Open_creat; Open_excl; Open_binary] 0o600 destination
      (fun stream -> output_string stream (Yojson.Safe.pretty_to_string result ^ "\n"));
    print_endline "event = hfhe_large status = passed")

let saved () =
  need "input and output required" (Array.length Sys.argv = 3);
  let path = Sys.argv.(1) in
  let samples = Yojson.Safe.from_file path |> member "samples" |> to_list in
  need "proof samples missing" (List.length samples = 16);
  let positives = List.filter (fun row -> member "accepted" row |> to_bool) samples in
  let read sample name =
    let path = Filename.concat (Filename.dirname path) (member name sample |> to_string) in
    let raw = In_channel.with_open_bin path In_channel.input_all in
    need "proof input digest differs"
      (Digestif.SHA256.(to_hex (digest_string raw)) = (member (name ^ "_sha256") sample |> to_string));
    raw in
  let request sample =
    let key = read sample "key" and cipher = read sample "cipher" in
    let proof = read sample "proof" and point = read sample "point" in
    P.Proof_key (P.Math (P.Claim {
    pubkey = key; cipher = C.Crypto.FheBalance.prefix ^ Base64.encode_exn cipher;
    proof = C.Crypto.FheBalance.zero_proof_prefix ^ Base64.encode_exn proof;
    commitment = Base64.encode_exn point; strict = true})) in
  need "proof worker not ready" (Worker.ready_sync () = Ok ());
  let owner = K.current (Sys.getenv "OCTRA_PVAC_VERIFY_WORKER") |> Option.get in
  ready owner;
  let parent = (K.stats owner).parent |> Option.get in
  let resident = F.rss parent in
  let accepted = (K.stats owner).prepared in
  let peak = ref resident in
  let run request =
    let pending = Worker.run_joined request in
    let rec monitor () =
      if not (Lwt.is_sleeping pending) then Lwt.return_unit else
      let open Lwt.Syntax in
      List.iter (fun child -> Option.iter (fun memory -> peak := max !peak memory)
        (F.total_rss parent child)) (F.children parent);
      let* () = Lwt_unix.sleep 0.01 in
      monitor () in
    Lwt_main.run (let open Lwt.Syntax in
      let* result, () = Lwt.both pending (monitor ()) in Lwt.return result) in
  let requests = List.init 3 (fun index -> request (List.nth positives index)) in
  List.iter (fun request -> match run request with
    | Worker.Completed response -> need "saved proof refused"
      (response.accepted && response.request_hash = P.request_hash request)
    | _ -> failwith "saved proof unavailable") requests;
  need "prepared verification unused" ((K.stats owner).prepared = accepted + 3);
  List.iter (fun request -> match run request with
    | Worker.Completed response -> need "repeated proof refused"
      (response.accepted && response.request_hash = P.request_hash request)
    | _ -> failwith "repeated proof unavailable") requests;
  need "repeated proof not reused" ((K.stats owner).prepared = accepted + 3
    && (K.stats owner).reused = 3);
  List.iter (fun request ->
    need "repeated proof ignored cancellation"
      (K.run owner ~control:(Atomic.make C.Proof_link.Cancel)
        ~deadline:(F.now () +. 10.) request = K.Cancelled);
    need "repeated proof ignored deadline"
      (K.run owner ~control:(Atomic.make C.Proof_link.Continue)
        ~deadline:(F.now () -. 1.) request = K.Timed_out)) requests;
  let invalid = List.find (fun row -> not (member "accepted" row |> to_bool)) samples |> request in
  List.iter (fun index ->
    ready owner;
    let before = (K.stats owner).rechecked in
    (match run invalid with
    | Worker.Completed response -> need "invalid proof reused"
      (not response.accepted && response.request_hash = P.request_hash invalid)
    | _ -> failwith "invalid proof unavailable");
    ready owner;
    let after = K.stats owner in
    need "negative proof skipped prepared check" (after.rechecked = before + 1);
    need "negative proof reuse differs" (after.reused = 3);
    Printf.printf "event = proof_refused index = %d checks = %d reused = %d\n%!"
      index after.rechecked after.reused) [1; 2];
  need "negative result retained" ((K.stats owner).reused = 3
    && (K.stats owner).rechecked = 2);
  need "parent retained request memory" (F.rss parent <= resident + 8);
  need "request memory missing" (!peak > resident);
  let before = C.Compute_memory.stats C.Compute_memory.shared in
  let request = request (List.nth positives 3) in
  let pending = Worker.run_joined request in
  let child = ref None in
  let elapsed = ref 0. in
  let cancelled = Lwt_main.run (let open Lwt.Syntax in
    let deadline = F.now () +. 10. in
    let rec wait () =
      need "proof child not started" (F.now () < deadline && Lwt.is_sleeping pending);
      match F.children parent with
      | [pid] -> child := Some pid; Lwt.return_unit
      | [] -> let* () = Lwt_unix.sleep 0.01 in wait ()
      | _ -> Lwt.fail_with "parallel proof children" in
    let* () = wait () in
    let* () = Lwt_unix.sleep 0.2 in
    need "proof ended before cancellation" (Lwt.is_sleeping pending);
    let started = F.now () in
    Lwt.cancel pending;
    let* cancelled = Lwt.catch (fun () -> Lwt.map (fun _ -> false) pending)
      (function Lwt.Canceled -> Lwt.return_true | error -> Lwt.fail error) in
    elapsed := F.now () -. started;
    Lwt.return cancelled) in
  need "proof cancellation lost" cancelled;
  need "proof cancellation too slow" (!elapsed < 3.);
  need "proof cancellation retained bytes" (C.Compute_memory.stats C.Compute_memory.shared = before);
  need "proof cancellation retained slot" ((Worker.channel_stats ()).active = 0);
  need "cancelled proof child survived" (try Unix.kill (Option.get !child) 0; false
    with Unix.Unix_error (Unix.ESRCH, _, _) -> true);
  ready owner;
  need "loaded cancellation replaced parent" ((K.stats owner).parent = Some parent);
  (match run request with Worker.Completed response -> need "proof after cancellation refused" response.accepted
    | _ -> failwith "proof after cancellation unavailable");
  need "post-cancel child retained" (F.children parent = []);
  let result = `Assoc ["status", `String "passed"; "parent_mb", `Int resident;
    "parent_after_mb", `Int (F.rss parent); "combined_peak_mb", `Int !peak;
    "negative_checks", `Int (K.stats owner).rechecked; "reused_checks", `Int (K.stats owner).reused;
    "cancel_ms", `Float (!elapsed *. 1000.)] in
  Out_channel.with_open_gen [Open_wronly; Open_creat; Open_excl; Open_binary] 0o600 Sys.argv.(2)
    (fun stream -> output_string stream (Yojson.Safe.pretty_to_string result ^ "\n"));
  print_endline "event = hfhe_cancel status = passed"

let () =
  if Array.length Sys.argv = 4 && Sys.argv.(1) = "--large" then large Sys.argv.(2) Sys.argv.(3)
  else if Array.length Sys.argv = 3 && Sys.argv.(1) = "--case" then
    Test_workspace.with_dir "proof_large" (fun dir -> large Sys.argv.(2) (Filename.concat dir "result.json"))
  else saved ()