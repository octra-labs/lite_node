(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Core = Octra_core
module VM = Octra_vm.Contract_vm
module Host = Core.Circle_wasm_host
module Wire = Core.Circle_wasm_codec
module Receipt = Core.Circle_hfhe_transcript
module Rule = Core.Rule_graph

let expect reason value = if not value then failwith reason
let checks = ref 0
let b64 value = Base64.encode_exn value

let read path =
  let input = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in input) (fun () ->
    let field () = really_input_string input (input_binary_int input) in
    let keys = Array.init 3 (fun _ -> field ()) in
    let cipher = field () in
    let point = field () in
    let proofs = Array.init 4 (fun _ -> Array.init 3 (fun _ -> field ())) in
    expect "unexpected sample bytes" (pos_in input = in_channel_length input);
    keys, cipher, point, proofs)

let vm ~async ~view ~keyed kind key cipher point proof =
  let ctx = {VM.default_ctx with math = true; async_exec = async;
    proof_key = (if keyed then Rule.Active else Rule.Prior);
    proof_exec = Rule.Active; fhe_work = Rule.Active} in
  let state = VM.create_state ~ctx ~limit:40_000_000
    ~caller:"caller" ~origin:"caller" ~address:"program"
    ~value:Z.zero ~storage:(Hashtbl.create 1) () in
  state.is_view <- view;
  List.iteri (fun index value -> state.regs.(index + 1) <- VM.VBytes (b64 value))
    [key; cipher; proof; point];
  let opcode = match kind with
    | 0 -> VM.FHE_VERIFY_ZERO (0, 5, 6, 3)
    | 1 -> VM.FHE_VERIFY_BOUND (0, 5, 6, 3, 4)
    | _ -> VM.FHE_VERIFY_RANGE (0, 5, 6, 3) in
  let program = [|VM.FHE_DESER_PK (5, 1); VM.FHE_DESER (6, 2); opcode; VM.STOP|] in
  let success = if async then Lwt_main.run (VM.run_async
      ~view_seconds:(Core.Pvac_verify_worker.timeout_seconds ()) state program)
    else VM.run state program in
  expect "VM execution failed" success;
  match state.regs.(0) with
  | VM.VBool value -> value
  | _ -> failwith "VM verdict missing"

let guest request =
  let byte value = String.make 1 (Char.chr value) in
  let rec number limit value =
    if value < limit then byte value
    else byte ((value land 127) lor 128) ^ number limit (value lsr 7) in
  let size = number 128 in
  let string value = size (String.length value) ^ value in
  let vector values = size (List.length values) ^ String.concat "" values in
  let section tag value = byte tag ^ string value in
  let const value = "\065" ^ number 64 value in
  let export name kind index = string name ^ byte kind ^ size index in
  let body instructions = string ("\000" ^ instructions ^ "\011") in
  let length = String.length request in
  String.concat "" [
    "\000asm\001\000\000\000";
    section 1 (vector ["\096\002\127\127\001\127"; "\096\001\127\001\127"]);
    section 2 (vector [string "octra" ^ string "host_hfhe_invoke_len" ^ "\000\000"]);
    section 3 (vector [size 1; size 0]);
    section 5 (vector ["\000" ^ size ((length + 65_535 + 4096) / 65_536)]);
    section 7 (vector [export "memory" 2 0; export "octra_alloc" 0 1;
      export "octra_update" 0 2]);
    section 10 (vector [body (const length);
      body (const 0 ^ const length ^ "\016\000\026" ^ const 0)]);
    section 11 (vector ["\000" ^ const 0 ^ "\011" ^ string request]);
  ] |> b64

let unwrap = function Ok value -> value | Error reason -> failwith reason

let wasm ~keyed ~mode kind key cipher point proof =
  let method_name, prefix = match kind with
    | 0 -> "fhe_verify_zero", Core.Crypto.FheBalance.zero_proof_prefix
    | 1 -> "fhe_verify_bound", Core.Crypto.FheBalance.zero_proof_prefix
    | _ -> "fhe_verify_range", Core.Crypto.FheBalance.range_proof_prefix in
  let cipher = Pvac_ffi.deserialize_cipher (Bytes.of_string cipher)
    |> Core.Crypto.FheBalance.encode_cipher in
  let params = [`String (b64 key); `String cipher; `String (prefix ^ b64 proof)] in
  let params = if kind = 0 then params else params @ [`String (b64 point)] in
  let request = Wire.encode_request ~method_name params |> unwrap in
  let request_bytes = Wire.encode_request ~method_name:"check" [] |> unwrap in
  let current_epoch = (Option.get (Rule.proof_key_activation_for_chain
    "octra-devnet-9871-cluster")).activation_epoch in
  Lwt_main.run (Host.execute ~table_mode:Rule.Active
    ~inspect_mode:Rule.Active ~proof_mode:Rule.Active
    ~call:None ~code_b64:(guest request) ~export_name:"octra_update" ~request_bytes
    ~storage_tbl:(Hashtbl.create 1) ~storage_cache_key:None
    ~caller:"caller" ~address:"circle" ~tx_hash:"proof-key" ~current_epoch
    ~hfhe_caps:[method_name] ~hfhe_pubkeys:[] ~hfhe_active_key:None
    ~hfhe_strict:true ~keyed ~math:true ~float_mode:Rule.Active ~hfhe_mode:mode
    ~public_reads:[] ~fuel_limit:40_000_000 ~is_view:false ~update_policy:true)

let verdict = function
  | Ok value when value.Host.success ->
    begin match value.hfhe_transcript with
    | [{Receipt.result = Some result; _}] -> result, value.hfhe_transcript
    | _ -> failwith "WASM verdict missing"
    end
  | Ok value -> failwith (Option.value ~default:"WASM failed" value.Host.error)
  | Error reason -> failwith (Host.error_message reason)

let refused = function
  | Error _ -> true
  | Ok value -> not value.Host.success

let run path only =
  let keys, cipher, point, proofs = read path in
  Array.iteri (fun series proofs ->
    let keyed = series >= 2 in
    let source = series mod 2 in
    Array.iteri (fun kind proof ->
      if only = None || only = Some (series, kind) then begin
      let cases = [keyed, source, not (keyed && source = 1);
        keyed, 1 - source, not keyed;
        not keyed, source, false; true, 2, false] in
      List.iter (fun (policy, key, expected) ->
        List.iter (fun (async, view) ->
          let started = Mtime_clock.elapsed_ns () in
          let actual = try vm ~async ~view ~keyed:policy kind keys.(key) cipher point proof
            with error ->
              let trace = Printexc.get_raw_backtrace () in
              let elapsed = Int64.sub (Mtime_clock.elapsed_ns ()) started
                |> Int64.to_float |> fun value -> value /. 1e9 in
              Printf.eprintf "event = vm_error series = %d kind = %d key = %d policy = %b async = %b view = %b seconds = %.3f\n%!"
                series kind key policy async view elapsed;
              Printexc.raise_with_backtrace error trace in
          incr checks;
          expect "VM key policy differs" (actual = expected))
          [false, false; false, true; true, false; true, true];
        let actual, _ = wasm ~keyed:policy ~mode:Receipt.Capture
          kind keys.(key) cipher point proof |> verdict in
        incr checks;
        expect "WASM key policy differs" (actual = expected)) cases;
      if keyed && source = 0 then begin
        let actual, entries = wasm ~keyed ~mode:Receipt.Capture
          kind keys.(source) cipher point proof |> verdict in
        expect "WASM honest proof refused" actual;
        let same = wasm ~keyed ~mode:(Receipt.Consume entries)
          kind keys.(source) cipher point proof in
        expect "WASM receipt reuse failed" (not (refused same));
        let other = wasm ~keyed ~mode:(Receipt.Consume entries)
          kind keys.(1 - source) cipher point proof in
        expect "WASM receipt reused with another key" (refused other);
        let other = wasm ~keyed:false ~mode:(Receipt.Consume entries)
          kind keys.(source) cipher point proof in
        expect "WASM receipt reused with another policy" (refused other);
        checks := !checks + 3
      end;
      Printf.printf "status = pass test = proof_key series = %d kind = %d\n%!" series kind
      end) proofs) proofs;
  Printf.printf "status = pass test = proof_key checks = %d\n%!" !checks

let () =
  let argc = Array.length Sys.argv in
  expect "expected sample path and optional group" (argc = 2 || argc = 4);
  let only = if argc = 2 then None else begin
    let series, kind = int_of_string Sys.argv.(2), int_of_string Sys.argv.(3) in
    expect "invalid proof group" (series >= 0 && series < 4 && kind >= 0 && kind < 3);
    Some (series, kind)
  end in
  run Sys.argv.(1) only