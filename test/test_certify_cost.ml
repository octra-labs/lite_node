(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module P = Pvac_ffi
module C = Octra_core
module V = Octra_vm
module T = V.Fhe_task
module Wire = C.Pvac_verify_protocol
module Image = C.Fhe_image

open Yojson.Safe.Util

let need reason value = if not value then failwith reason
let read path = In_channel.with_open_bin path In_channel.input_all
let save path value =
  Out_channel.with_open_gen [Open_wronly; Open_creat; Open_excl; Open_binary] 0o600 path
    (fun channel -> output_string channel (Yojson.Safe.pretty_to_string value ^ "\n"))
let hash data = Digestif.SHA256.(to_hex (digest_string data))
let deadline () = Int64.add (Mtime_clock.elapsed_ns ()) 120_000_000_000L
let ticket name = V.Proof_wait.{request = name; generation = "certify_cost"}

let timed label run =
  let start = Mtime_clock.elapsed_ns () in
  let result = run () in
  let ms = Int64.to_float (Int64.sub (Mtime_clock.elapsed_ns ()) start) /. 1e6 in
  Printf.printf "event = certify_cost phase = %s ms = %.3f\n%!" label ms;
  result, (label, `Float ms)

let input folder sample name =
  let file = member name sample |> to_string in
  let raw = read (Filename.concat folder file) in
  let expected = member (name ^ "_sha256") sample |> to_string in
  need "input hash differs" (hash raw = expected);
  raw

let proof_request key cipher proof point = Wire.Claim {
  pubkey = key; cipher = C.Crypto.FheBalance.prefix ^ Base64.encode_exn cipher;
  proof = C.Crypto.FheBalance.zero_proof_prefix ^ Base64.encode_exn proof;
  commitment = Base64.encode_exn point; strict = true;
}

let components folder sample =
  let raw = input folder sample "key" in
  let ct = input folder sample "cipher" in
  let proof = input folder sample "proof" in
  let point = input folder sample "point" in
  let key, parse = timed "parse_key" (fun () -> P.deserialize_pubkey (Bytes.of_string raw)) in
  let valid, check = timed "check_key" (fun () -> C.Pvac_registry.check_key ~keyed:true key) in
  need "key check failed" (valid = Ok ());
  let encoded, encode = timed "write_key" (fun () -> P.serialize_pubkey key |> Bytes.to_string) in
  need "key encoding differs" (encoded = raw);
  let image, metadata = timed "key_metadata" (fun () -> Image.{data = encoded;
    sampling = P.pubkey_sampling key; words = Some (P.pubkey_bit_words key);
    size = Some (P.pubkey_image_size key)}) in
  let cipher, decode = timed "parse_cipher" (fun () ->
    P.deserialize_cipher ~strict:true ~cap:true (Bytes.of_string ct)) in
  let cipher_image, serialize = timed "write_cipher" (fun () -> Image.of_cipher cipher) in
  need "cipher encoding differs" (cipher_image.data = ct);
  let decoded_proof, parse_proof = timed "parse_proof" (fun () ->
    P.deserialize_zero_proof (Bytes.of_string proof)) in
  let accepted, verify = timed "verify_proof" (fun () ->
    P.verify_zero_bound ~keyed:true ~math:true key cipher decoded_proof (Bytes.of_string point)) in
  need "native proof refused" accepted;
  let request = proof_request raw ct proof point in
  let direct, total = timed "direct_request" (fun () -> C.Pvac_verify_direct.execute ~keyed:true ~math:true request) in
  need "direct request refused" (direct = Ok ());
  let worker, isolated = timed "proof_worker" (fun () -> T.eval (T.Verify (true, Wire.Proof_key request))) in
  need "proof worker refused" (worker = Ok (T.Verified true));
  let actor = T.create () in
  Fun.protect ~finally:(fun () -> T.stop actor; Lwt_main.run (Lwt_unix.sleep 0.05)) (fun () ->
    let run name request = timed name (fun () ->
      Lwt_main.run (T.run ~actor ~ticket:(ticket name) ~deadline:(deadline ()) request)) in
    let first, cold = run "key_worker_cold" (T.Read_key raw) in
    need "cold key image differs" (first = Ok (T.Key image));
    let second, warm = run "key_worker_repeat" (T.Read_key raw) in
    need "repeat key image differs" (second = first);
    let last, cipher_worker = run "cipher_worker" (T.Read_cipher (true, true, ct)) in
    need "worker cipher image differs" (last = Ok (T.Cipher cipher_image));
    `Assoc ["phases_ms", `Assoc [parse; check; encode; metadata; decode; serialize; parse_proof; verify;
        total; isolated; cold; warm; cipher_worker];
      "key_bytes", `Int (String.length raw); "key_image_bytes", `Int (Option.get image.size);
      "cipher_bytes", `Int (String.length ct); "proof_bytes", `Int (String.length proof);
      "passed", `Bool true])

let vm folder sample =
  let module VM = V.Contract_vm in
  let raw = input folder sample "key" in
  let ct = input folder sample "cipher" in
  let proof = input folder sample "proof" in
  let point = input folder sample "point" in
  let ctx = {VM.default_ctx with async_exec = true; math = true; point_ops = true;
    scalar_work = true; int_work = V.Int_work.Active;
    proof_exec = C.Rule_graph.Active; proof_key = C.Rule_graph.Active;
    fhe_work = C.Rule_graph.Active; fhe_memory = Some (V.Fhe_memory.create ());
    current_epoch = 2_000_010; tx_hash = "certify_cost"; tree_hash = "measured"} in
  let state = VM.create_state ~ctx ~limit:10_000_000 ~caller:"sender" ~origin:"sender"
    ~address:"program" ~value:Z.zero ~storage:(Hashtbl.create 1) () in
  let encoded = Base64.encode_exn raw in
  state.regs.(0) <- VM.VString encoded;
  state.regs.(1) <- VM.VString (Base64.encode_exn ct);
  state.regs.(2) <- VM.VString (Base64.encode_exn proof);
  state.regs.(3) <- VM.VString (Base64.encode_exn point);
  let result, duration = timed "vm_operations" (fun () -> Lwt_main.run (VM.run_async state
    VM.[|FHE_DESER_PK (4, 0); FHE_SER_PK (5, 4); FHE_DESER (6, 1);
      FHE_VERIFY_BOUND (7, 4, 6, 2, 3); STOP|])) in
  need "VM operations failed" (result && not state.reverted);
  need "VM serialized key differs" (state.regs.(5) = VM.VString encoded);
  need "VM proof refused" (state.regs.(7) = VM.VBool true);
  `Assoc ["phases_ms", `Assoc [duration]; "effort", `Int state.effort_used;
    "passed", `Bool true; "whole_certify", `Bool false]

let verify folder sample =
  let key = input folder sample "key" in
  let cipher = input folder sample "cipher" in
  let proof = input folder sample "proof" in
  let point = input folder sample "point" in
  let expected = member "accepted" sample |> to_bool in
  let request = proof_request key cipher proof point in
  let result, duration = timed "proof_worker" (fun () -> T.eval (T.Verify (true, Wire.Proof_key request))) in
  need "proof verdict differs" (result = Ok (T.Verified expected));
  `Assoc ["phases_ms", `Assoc [duration]; "accepted", `Bool expected;
    "request_hash", `String (Wire.request_hash (Wire.Math (Wire.Proof_key request)));
    "passed", `Bool true]

let prepared folder sample =
  let key = input folder sample "key" in
  let cipher = input folder sample "cipher" in
  let proof = input folder sample "proof" in
  let point = input folder sample "point" in
  let expected = member "accepted" sample |> to_bool in
  let request = proof_request key cipher proof point in
  let before = C.Proof_ready.sums () in
  let refused = C.Proof_ready.rejected () in
  let result, duration = timed "prepared_request" (fun () ->
    C.Pvac_verify_direct.execute ~keyed:true ~math:true request) in
  need "prepared verdict differs" ((result = Ok ()) = expected);
  let sums = C.Proof_ready.sums () - before in
  let rejected = C.Proof_ready.rejected () - refused in
  need "prepared FFI image unused" (not expected || sums > 0);
  need "accepted equation recorded as refused" (not expected || rejected = 0);
  `Assoc ["phases_ms", `Assoc [duration]; "accepted", `Bool expected;
    "sums", `Int sums; "rejected", `Int rejected; "whole_circle_call", `Bool false; "passed", `Bool true]

let () =
  Circle_host.worker ();
  need "input output mode and count required" (Array.length Sys.argv = 5);
  let path = Sys.argv.(1) and output = Sys.argv.(2) and mode = Sys.argv.(3) in
  let count = int_of_string Sys.argv.(4) in
  need "sample count invalid" (count > 0 && count <= 24);
  let data = Yojson.Safe.from_file path in
  let samples = member "samples" data |> to_list in
  need "proof samples missing" (List.length samples = 16);
  let all = List.mem mode ["verify"; "circle"; "prepared"; "session"; "late"] in
  let samples = if all then samples
    else List.filter (fun row -> member "accepted" row |> to_bool) samples in
  need "positive samples missing" (all || List.length samples = 8);
  if mode = "prepared" then begin
    ignore (timed "prepare_points" C.Proof_ready.prepare)
  end;
  let owner = if mode = "session" || mode = "late" then begin
    need "proof worker not ready" (C.Pvac_verify_worker.ready_sync () = Ok ());
    let owner = C.Proof_keeper.current (Sys.getenv "OCTRA_PVAC_VERIFY_WORKER") |> Option.get in
    let deadline = C.Proof_frame.now () +. 120. in
    let rec wait () = match C.Proof_keeper.phase owner with
      | C.Proof_warm.Ready _ -> ()
      | C.Proof_warm.Closed -> failwith "proof session closed"
      | _ when C.Proof_frame.now () >= deadline -> failwith "proof session startup timed out"
      | _ -> Thread.delay 0.01; wait () in
    ignore (timed "session_start" wait);
    Some owner
  end else None in
  let circle folder sample index =
    let key = input folder sample "key" in
    let cipher = input folder sample "cipher" in
    let proof = input folder sample "proof" in
    let point = input folder sample "point" in
    let request = C.Circle_wasm_codec.encode_request ~method_name:"fhe_verify_bound"
      [`String (Base64.encode_exn key);
       `String (C.Crypto.FheBalance.prefix ^ Base64.encode_exn cipher);
       `String (C.Crypto.FheBalance.zero_proof_prefix ^ Base64.encode_exn proof);
       `String (Base64.encode_exn point)] |> Circle_proposal.unwrap in
    let dir = Filename.concat output ("state_" ^ string_of_int index) in
    Unix.mkdir dir 0o700;
    let settings = [C.Validator_policy.env_name, "100";
      "OCTRA_BFT_RELEASE_PROFILE", "devnet_full_v1";
      "OCTRA_EMISSION_PROFILE", ""; "OCTRA_EMISSION_ACTIVATION_EPOCH", ""] in
    let previous = List.map (fun (name, _) -> name, Option.value (Sys.getenv_opt name) ~default:"") settings in
    let head = !(Recovery_case.HM.cached) in
    Fun.protect ~finally:(fun () ->
      List.iter (fun (name, value) -> Unix.putenv name value) previous;
      Recovery_case.HM.cached := head) (fun () ->
        List.iter (fun (name, value) -> Unix.putenv name value) settings;
        Circle_host.cost ~request ~accepted:(member "accepted" sample |> to_bool) dir) in
  let execute folder sample index = match mode with
    | "components" -> components folder sample | "vm" -> vm folder sample
    | "verify" -> verify folder sample | "circle" -> circle folder sample index
    | "prepared" -> prepared folder sample
    | "session" | "late" ->
      let owner = Option.get owner in
      let before = C.Proof_keeper.stats owner in
      let result = circle folder sample index in
      let after = C.Proof_keeper.stats owner in
      if mode = "late" then begin
        need "late input must be refused" (not (member "accepted" sample |> to_bool));
        need "late equation was rechecked" (after.rechecked = before.rechecked);
        need "late refusal entered positive cache" (after.reused = before.reused);
        need "late equation witness unused" (after.refused > before.refused);
        let request = proof_request (input folder sample "key") (input folder sample "cipher")
          (input folder sample "proof") (input folder sample "point") in
        let request = Wire.Math (Wire.Proof_key request) in
        List.iter (fun _ ->
          let result = C.Proof_keeper.run owner ~control:(Atomic.make C.Proof_link.Continue)
            ~deadline:(C.Proof_frame.now () +. 30.) request in
          need "late refusal became positive" (match result with C.Proof_keeper.Accepted response ->
            not response.accepted && response.request_hash = Wire.request_hash request | _ -> false)) [1; 2];
        let last = C.Proof_keeper.stats owner in
        need "late refusal reused result" (last.refused = after.refused + 2 && last.reused = after.reused)
      end else need "circle session unused" (if member "accepted" sample |> to_bool then
        after.prepared + after.reused > before.prepared + before.reused
        else after.rechecked > before.rechecked);
      result
    | _ -> failwith "mode invalid" in
  for index = 0 to count - 1 do
    let sample = List.nth samples (index mod List.length samples) in
    let result = execute (Filename.dirname path) sample index in
    save (Filename.concat output ("sample_" ^ string_of_int index ^ ".json"))
      (`Assoc ["name", member "name" sample; "measurement", result])
  done;
  save (Filename.concat output "result.json") (`Assoc ["status", `String "passed";
    "mode", `String mode; "samples", `Int count; "proofs_generated", `Int 0;
    "deployment_ready", `Bool false]);
  print_endline "event = certify_cost status = passed"