(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module P = Octra_core.Pvac_verify_protocol
module W = Octra_core.Pvac_verify_worker

let check name value = if not value then failwith name

let reference raw =
  Digestif.SHA256.(to_hex (digest_string (P.schema ^ "\000" ^ raw)))

let request size = P.Range {
  pubkey = String.make size 'k';
  cipher = "cipher";
  proof = String.make (size * 2) 'p';
  strict = true;
}

let allocated run =
  Gc.full_major ();
  let before = Gc.allocated_bytes () in
  let value = run () in
  value, Gc.allocated_bytes () -. before

let cost () =
  let request = request (8 * 1024 * 1024) in
  let (size, expected), encoded = allocated (fun () ->
    let raw = P.request_bytes request in
    String.length raw, raw) in
  let expected = reference expected in
  let hash, hashed = allocated (fun () -> P.request_hash request) in
  check "large request hash differs" (hash = expected);
  let attempts = ref 0 in
  let outcome, sent = allocated (fun () ->
    W.run_process ~pipe:(fun () -> incr attempts; failwith "cost_pipe")
      "unused" request) in
  check "pipe refusal differs" (outcome = W.Failed "Failure(\"cost_pipe\")" && !attempts = 1);
  let allowance = encoded +. 1048576. in
  Printf.printf "event = worker_cost bytes = %d encode = %.0f hash = %.0f send = %.0f limit = %.0f\n%!"
    size encoded hashed sent allowance;
  check "request serialized twice" (sent <= allowance);
  check "request hash copies packet" (hashed <= allowance)

let response () =
  let raw = In_channel.input_all stdin in
  let hash = reference raw in
  let hash = if Sys.getenv_opt "OCTRA_WORKER_COST" = Some "wrong" then
      String.make 64 '0' else hash in
  P.response_bytes {request_hash = hash; accepted = false; reason = "proof_invalid"}
  |> print_endline

let protocol () =
  let base = request 4096 in
  let requests = [base; P.Math base; P.Proof_key base; P.Math (P.Proof_key base)] in
  let hashes = List.map (fun request ->
    let raw = P.request_bytes request in
    let expected = reference raw in
    check "request hash differs" (P.request_hash request = expected);
    let parsed = match P.request_of_string raw with
      | Ok parsed -> parsed | Error reason -> failwith reason in
    check "request roundtrip differs" (P.request_bytes parsed = raw);
    Unix.putenv "OCTRA_WORKER_COST" "reply";
    check "response refused" (W.run_process Sys.executable_name request =
      W.Completed {request_hash = expected; accepted = false; reason = "proof_invalid"});
    Unix.putenv "OCTRA_WORKER_COST" "wrong";
    check "response hash accepted" (W.run_process Sys.executable_name request =
      W.Failed "response_hash_mismatch");
    expected) requests in
  check "request domains collided" (List.length (List.sort_uniq String.compare hashes) = 4);
  let called = ref false in
  let cancelled, bytes = allocated (fun () ->
    W.run_process ~control:(Atomic.make W.Cancel)
      ~pipe:(fun () -> called := true; Unix.pipe ()) "unused" (List.hd requests)) in
  check "cancelled request executed" (cancelled = W.Unavailable "worker_cancelled" && not !called);
  check "cancelled request encoded" (bytes < 1048576.);
  Unix.putenv "OCTRA_WORKER_COST" ""

let () =
  match Sys.getenv_opt "OCTRA_WORKER_COST" with
  | Some ("reply" | "wrong") -> response ()
  | _ -> protocol (); cost (); print_endline "event = worker_cost status = passed"