(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_vm

let require condition reason = if not condition then failwith reason

let check_admission () =
  let code = [|Contract_vm.LOAD_INT8_FP (0, 1, 2, 3, 4); Contract_vm.STOP|] in
  let raw = Bytecode.encode code in
  List.iter (fun point_ops ->
    List.iter (fun result ->
      match result with
      | Error (Admission.Unsafe_error message) ->
        require (message = "consensus unsafe opcode LOAD_INT8_FP at pc 0") "wrong policy refusal"
      | _ -> failwith "float decode reached consensus")
      [Admission.of_code ~point_ops code; Admission.of_program ~point_ops code;
       Admission.decode ~point_ops raw; Admission.decode_deploy ~point_ops raw];
    require (Contract.decode_loaded ~point_ops raw = None) "loaded float decode")
    [false; true]

let check_fixed_input () =
  let raw = String.init 32 Char.chr in
  let encoded = Base64.encode_exn raw in
  require (Program_input.parse [Program_type_flow.Bytes32] [`String encoded] =
    Ok [Contract_vm.VBytes32 raw]) "program input bytes";
  require (Contract_vm.worker_commitment encoded = Some encoded) "commitment bytes";
  let input = String.make 1_000_000 'A' in
  let before = Gc.allocated_bytes () in
  let program = Program_input.parse [Program_type_flow.Bytes32] [`String input] in
  let commitment = Contract_vm.worker_commitment input in
  let allocated = Gc.allocated_bytes () -. before in
  require (Result.is_error program && commitment = None && allocated < 100_000.)
    "fixed input allocated payload"

let loaded ?(point_ops = true) raw =
  match Admission.decode ~point_ops raw with
  | Ok value -> Admission.code value
  | Error error -> failwith (Admission.error_message error)

let integer code =
  match code.(0) with
  | Contract_vm.LDI (_, Contract_vm.VInt value) -> value
  | _ -> failwith "integer code differs"

let number size tag =
  Bytecode.encode [|Contract_vm.LDI (0, Contract_vm.VInt
    (Z.of_string ("0x" ^ String.make size 'a' ^ string_of_int tag)));
    Contract_vm.STOP|]

let check_decode_cache () =
  let raw = number 60_000 1 in
  let before = Gc.allocated_bytes () in
  let first = loaded raw in
  let cold = Gc.allocated_bytes () -. before in
  let copy = String.sub raw 0 (String.length raw) in
  let before = Gc.allocated_bytes () in
  let second = loaded copy in
  let warm = Gc.allocated_bytes () -. before in
  require (integer first == integer second) "program code decoded again";
  require (warm < cold /. 4.) "cached decode allocation differs";
  require (first != second) "instruction array shared";
  first.(0) <- Contract_vm.REVERT;
  require ((loaded raw).(0) = second.(0)) "caller modified cached code";
  let changed = number 60_000 2 in
  require (not (Z.equal (integer (loaded changed)) (integer second)))
    "changed program reused prior code";
  require (integer (loaded raw) == integer second) "read lost cached program";
  Printf.printf "decode_cache = pass cold_bytes = %.0f warm_bytes = %.0f\n" cold warm

let check_cache_modes () =
  let raw = Bytecode.encode [|Contract_vm.FHE_PEDERSEN_IDENTITY 0; Contract_vm.STOP|] in
  ignore (loaded raw);
  let expected = match Bytecode.decode ~active:false raw with
    | Error reason -> Admission.Decode_error reason
    | Ok _ -> failwith "historical decoder admitted point opcode" in
  require (Admission.decode ~point_ops:false raw = Error expected)
    "cache changed historical refusal";
  ignore (loaded raw);
  let raw = Bytecode.encode [|Contract_vm.LDI (0, Contract_vm.VInt (Z.of_int 77));
    Contract_vm.STOP|] in
  for size = 0 to String.length raw do
    let input = String.sub raw 0 size in
    List.iter (fun point_ops ->
      let expected = match Bytecode.decode ~active:point_ops input with
        | Error reason -> Error (Admission.Decode_error reason)
        | Ok code -> Admission.of_code ~point_ops code in
      let observe = Result.map (fun value -> Admission.code value, Admission.profile value) in
      for _ = 1 to 2 do
        require (observe (Admission.decode ~point_ops input) = observe expected)
          "cached decode changed result"
      done) [true; false]
  done

let check_cache_capacity () =
  let raw = number 100 100 in
  let first = integer (loaded raw) in
  require (integer (loaded raw) == first) "entry not cached before eviction";
  for index = 1 to 64 do
    ignore (loaded (number 100 index));
    require (integer (loaded raw) == first) "recent entry evicted"
  done;
  for index = 1 to 65 do ignore (loaded (number 100 (1000 + index))) done;
  require (integer (loaded raw) != first) "cache entry limit ignored";
  let large = number 300_000 101 in
  let prior = (loaded large).(0) in
  require ((loaded large).(0) == prior) "large code not cached before eviction";
  for index = 1 to 32 do
    ignore (loaded (Bytecode.encode [|Contract_vm.LDI (0,
      Contract_vm.VString (String.make 600_000 (Char.chr index))); Contract_vm.STOP|]))
  done;
  require ((loaded large).(0) != prior) "cache byte limit ignored"

let check_large_cache () =
  let large = number 300_000 1 in
  let first = integer (loaded large) in
  require (Z.equal first (integer (loaded large))) "large decode value changed";
  require (integer (loaded large) == first) "large integer decoded again";
  let source = Bytecode.encode [|Contract_vm.LDI (0, Contract_vm.VInt (Z.of_int 7));
    Contract_vm.STOP|] in
  let buffer = Buffer.create (16 * 1024 * 1024) in
  Buffer.add_substring buffer source 0 13;
  Bytecode.put_u32le buffer (16 * 1024 * 1024 - 64);
  Buffer.add_string buffer (String.make (16 * 1024 * 1024 - 64) '7');
  Buffer.add_substring buffer source 18 (String.length source - 18);
  let raw = Buffer.contents buffer in
  require (Result.is_ok (Program_envelope.encode ~code:raw ~cert:"{}"))
    "integer exceeds deploy size";
  let first = integer (loaded raw) in
  require (integer (loaded raw) == first) "deploy size integer decoded again";
  let raw = Bytecode.encode [|Contract_vm.LDI (0,
    Contract_vm.VString (String.make (15 * 1024 * 1024) 'q'));
    Contract_vm.STOP|] in
  require (Result.is_ok (Program_envelope.encode ~code:raw ~cert:"{}"))
    "program exceeds deploy size";
  let text code = match code.(0) with
    | Contract_vm.LDI (_, Contract_vm.VString value) -> value
    | _ -> failwith "string code differs" in
  let first = text (loaded raw) in
  require (text (loaded raw) == first) "large string decoded again";
  let raw = Bytecode.encode [|Contract_vm.LDI (0,
    Contract_vm.VString (String.make (16 * 1024 * 1024) 'r'));
    Contract_vm.STOP|] in
  let first = text (loaded raw) in
  require (text (loaded raw) = first) "large string changed";
  require (text (loaded raw) != first) "cache retained more than byte limit"

let check_large_mix () =
  let value = Z.of_string ("0x" ^ String.make 300_000 'b') in
  let numbers = Array.init 65 (fun index -> Contract_vm.LDI (3, Contract_vm.VInt
    (Z.of_string ("0x" ^ String.make 4096 'f' ^ string_of_int index)))) in
  let code = Array.concat [
    [|Contract_vm.LDI (0, Contract_vm.VInt value);
      Contract_vm.STOP;
      Contract_vm.LDI (1, Contract_vm.VString (String.make (15 * 1024 * 1024) 'm'))|]
    ; numbers; Array.make 60_000 (Contract_vm.ADD (2, 0, 0))] in
  let raw = Bytecode.encode code in
  require (Result.is_ok (Program_envelope.encode ~code:raw ~cert:"{}"))
    "mixed program exceeds deploy size";
  List.iter (fun point_ops ->
    let first = loaded ~point_ops raw in
    let second = loaded ~point_ops raw in
    require (Z.equal (integer first) value) "mixed integer differs";
    require (integer first == integer second) "mixed integer decoded again")
    [false; true]

let check_integer_cache () =
  let read = Bytecode.Integers.read in
  let raw = "0x" ^ String.make 65536 'c' in
  let first = read raw in
  require (read raw == first) "integer not cached before eviction";
  for index = 1 to 512 do
    ignore (read ("0x" ^ String.make 65536 'd' ^ string_of_int index));
    require (read raw == first) "recent integer evicted"
  done;
  for index = 1 to 513 do ignore (read (raw ^ string_of_int index)) done;
  require (read raw != first) "integer memory eviction ignored";
  let large digit = "0x" ^ String.make (12 * 1024 * 1024) digit in
  let source = large 'a' in
  let value = read source in
  require (read source == value) "large integer not cached before eviction";
  ignore (read (large 'b'));
  require (read source != value) "integer byte limit ignored";
  let bad = String.make 65536 '!' in
  for _ = 1 to 2 do
    let expected = try ignore (Z.of_string bad); None with exn -> Some exn in
    let actual = try ignore (read bad); None with exn -> Some exn in
    require (actual = expected) "integer refusal changed";
    require (Z.equal (read raw) first) "integer failure retained lock"
  done

let check_cache_trust () =
  let private_key = String.make 32 '\043' in
  let key = match Mirage_crypto_ec.Ed25519.priv_of_octets private_key with
    | Ok key -> key | Error _ -> failwith "signing key refused" in
  let public_key = Mirage_crypto_ec.Ed25519.(pub_to_octets (pub_of_priv key)) in
  let good = Program_attestation.{id = "decode-key"; public_key} in
  let wrong = Program_attestation.{id = "decode-key"; public_key = String.make 32 'x'} in
  let compiled = Oct_compile.compile_program
    "program Number { public view fn read(): int { return 7 } }"
    |> Oct_compile.attest_program ~key_id:"decode-key" ~private_key in
  let raw = match compiled.program_envelope with
    | Some raw when compiled.error = None -> raw
    | _ -> failwith "signed compile failed" in
  require (Result.is_ok (Admission.decode_program ~trusted:[good] raw)) "signed decode refused";
  List.iter (fun trusted ->
    require (Result.is_error (Admission.decode_program ~trusted raw))
      "cached code bypassed compiler trust") [[]; [wrong]; [wrong; good]];
  require (Result.is_ok (Admission.decode_program ~trusted:[good; wrong] raw))
    "compiler key order changed";
  let compiled = Oct_compile.compile_program
    "program SourceOnly { public view fn read(): int { return 8 } }" in
  let raw = Option.get compiled.program_envelope in
  require (Result.is_ok (Admission.decode_program_source raw)) "source decode refused";
  require (Result.is_error (Admission.decode_program ~trusted:[good] raw))
    "source cache bypassed attestation"

let check_cache_threads () =
  let inputs = Array.init 8 (fun index -> number 100 (3000 + index)) in
  let errors = Array.make 8 None in
  let jobs = Array.init 8 (fun index -> Thread.create (fun () ->
    try
      for turn = 0 to 127 do
        let raw = inputs.((turn + index) mod Array.length inputs) in
        let expected = match Bytecode.decode raw with
          | Ok code -> integer code | Error reason -> failwith reason in
        let code = loaded raw in
        require (Z.equal (integer code) expected) "concurrent code differs";
        code.(0) <- Contract_vm.REVERT;
        require (Z.equal (integer (loaded raw)) expected) "concurrent array shared"
      done
    with error -> errors.(index) <- Some error) ()) in
  Array.iter Thread.join jobs;
  Array.iter (function None -> () | Some error -> raise error) errors

let () =
  check_admission ();
  check_fixed_input ();
  check_decode_cache ();
  check_large_cache ();
  check_large_mix ();
  check_integer_cache ();
  check_cache_modes ();
  check_cache_capacity ();
  check_cache_trust ();
  check_cache_threads ();
  Printf.printf "decode_policy = pass\n"