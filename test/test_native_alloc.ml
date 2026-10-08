(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

external arm : int -> unit = "octra_alloc_fault"
external probe : unit -> bool = "octra_alloc_probe"

let expect label value = if not value then failwith label

let refused label count run =
  expect "invalid case name"
    (String.for_all (function 'a'..'z' | '0'..'9' | '_' -> true | _ -> false) label);
  Printf.printf "event = allocation case = %s\n%!" label;
  let result = Fun.protect ~finally:(fun () -> arm (-1)) (fun () ->
    arm count;
    try ignore (run ()); false with Out_of_memory -> true | _ -> false) in
  expect ("native allocation error lost: " ^ label) result

let () =
  expect "allocation injector cannot unwind" (probe ());
  let args = Array.to_list Sys.argv |> List.tl in
  if args = ["--fhe"] || args = ["--fhe-session"] then begin
    Fun.protect ~finally:(fun () -> arm (-1)) (fun () ->
      arm 0;
      if args = ["--fhe-session"] then Octra_core.Fhe_calc.serve_session ()
      else Octra_core.Fhe_calc.serve ());
    exit 0
  end;
  let pk, sk = Pvac_ffi.keygen_from_seed (Pvac_ffi.default_params ()) (Bytes.make 32 '\005') in
  let cipher = Pvac_ffi.enc_value_seeded pk sk 7L (Bytes.make 32 '\006') in
  let encoded = Pvac_ffi.serialize_cipher cipher in
  let key = Pvac_ffi.serialize_pubkey pk in
  if Array.length Sys.argv > 1 then begin
    if Array.to_list Sys.argv |> List.tl <> ["--preview"] then failwith "unknown allocation test option";
    let zero = Pvac_ffi.enc_zero_seeded pk sk (Bytes.make 32 '\007') in
    let proof = Pvac_ffi.make_zero_proof pk sk zero
      |> Octra_core.Crypto.FheBalance.encode_zero_proof in
    Preview_alloc.run arm key (Pvac_ffi.serialize_cipher zero) proof;
    exit 0
  end;
  let public = Octra_core.Crypto.FheBalance.encode_cipher cipher in
  refused "cipher_decode" 0 (fun () -> Octra_core.Crypto.FheBalance.decode_cipher public);
  refused "cipher_parse" 0 (fun () -> Pvac_ffi.deserialize_cipher encoded);
  refused "key_parse" 0 (fun () -> Pvac_ffi.deserialize_pubkey key);
  refused "cipher_add" 1 (fun () -> Pvac_ffi.ct_add pk cipher cipher);
  refused "cipher_serialize" 0 (fun () -> Pvac_ffi.serialize_cipher cipher);
  refused "key_serialize" 0 (fun () -> Pvac_ffi.serialize_pubkey pk);
  List.iter (fun count ->
    refused "cipher_parse_alloc" count (fun () -> Pvac_ffi.deserialize_cipher encoded);
    refused "key_parse_alloc" count (fun () -> Pvac_ffi.deserialize_pubkey key);
    refused "cipher_add_alloc" count (fun () -> Pvac_ffi.ct_add pk cipher cipher)) [1; 2; 3];
  let seed = Bytes.make 32 '\007' in
  List.iter (fun (label, run) -> refused label 0 run)
    ["sub", (fun () -> Pvac_ffi.ct_sub pk cipher cipher);
     "mul", (fun () -> Pvac_ffi.ct_mul_seeded pk cipher cipher seed);
     "scale", (fun () -> Pvac_ffi.ct_scale pk cipher 2L);
     "div", (fun () -> Pvac_ffi.ct_div_const pk cipher 2L 0L);
     "add_const", (fun () -> Pvac_ffi.ct_add_const pk cipher 2L 0L);
     "sub_const", (fun () -> Pvac_ffi.ct_sub_const pk cipher 2L);
     "square", (fun () -> Pvac_ffi.ct_square_seeded pk cipher seed)];
  refused "commit" 0 (fun () -> Pvac_ffi.commit_ct pk cipher);
  refused "public_cipher" 0 (fun () -> Pvac_ffi.serialize_cipher_public cipher);
  refused "legacy_key" 0 (fun () -> Pvac_ffi.serialize_pubkey_legacy_v2 pk);
  let module VM = Octra_vm.Contract_vm in
  List.iter (fun (label, op) ->
    let state = VM.create_state ~limit:10_000_000 ~ctx:VM.default_ctx ~is_view:true
      ~caller:"sender" ~origin:"sender" ~address:"program" ~value:Z.zero
      ~storage:(Hashtbl.create 1) () in
    state.regs.(0) <- VM.VPubKey (Octra_core.Fhe_image.of_key pk);
    state.regs.(1) <- VM.VCipher (Octra_core.Fhe_image.of_cipher cipher);
    state.regs.(2) <- VM.VString (Bytes.to_string encoded |> Base64.encode_exn);
    state.regs.(3) <- VM.VString (Bytes.to_string key |> Base64.encode_exn);
    let result = Fun.protect ~finally:(fun () -> arm (-1)) (fun () ->
      arm 0;
      VM.run state [|op; VM.STOP|]) in
    expect ("vm used parent native allocation: " ^ label) (result && not state.reverted);
    expect "isolated operation lost output" (state.regs.(4) <> VM.VInt Z.zero))
    ["vm_add", VM.FHE_ADD (4, 0, 1, 1);
     "vm_cipher_parse", VM.FHE_DESER (4, 2);
     "vm_key_parse", VM.FHE_DESER_PK (4, 3);
     "vm_cipher_serialize", VM.FHE_SER (4, 1);
     "vm_key_serialize", VM.FHE_SER_PK (4, 0)];
  let malformed = Bytes.of_string "invalid" in
  let commitment = Pvac_ffi.pedersen_commit_amount 0L seed in
  List.iter (fun (label, run) ->
    expect (label ^ " accepted invalid bytes") (not (run ()));
    refused label 0 run)
    ["range_any", (fun () -> Pvac_ffi.verify_range_any pk cipher malformed true);
     "range_commitment", (fun () -> Pvac_ffi.verify_range_bound pk cipher malformed commitment);
     "range_prior", (fun () -> Pvac_ffi.verify_range_amount_prior pk cipher malformed commitment)];
  Gc.full_major ();
  let zero = Pvac_ffi.enc_zero_seeded pk sk seed in
  let proof = Pvac_ffi.make_zero_proof pk sk zero in
  expect "zero proof control failed" (Pvac_ffi.verify_zero pk zero proof);
  refused "zero_verify" 0 (fun () -> Pvac_ffi.verify_zero pk zero proof);
  let proof_bytes = Pvac_ffi.serialize_zero_proof proof in
  refused "zero_parse" 0 (fun () -> Pvac_ffi.deserialize_zero_proof proof_bytes);
  refused "zero_serialize" 0 (fun () -> Pvac_ffi.serialize_zero_proof proof);
  refused "encrypt" 0 (fun () -> Pvac_ffi.enc_value_seeded pk sk 7L seed);
  refused "encrypt_vector" 0 (fun () -> Pvac_ffi.enc_values_seeded pk sk [|7L; 8L|] seed);
  refused "encrypt_zero" 0 (fun () -> Pvac_ffi.enc_zero_seeded pk sk seed);
  refused "decrypt" 0 (fun () -> Pvac_ffi.dec_value pk sk cipher);
  refused "decrypt_vector" 0 (fun () -> Pvac_ffi.dec_values pk sk cipher);
  refused "zero_prove" 0 (fun () -> Pvac_ffi.make_zero_proof pk sk zero);
  refused "zero_commitment_prove" 0 (fun () -> Pvac_ffi.make_zero_proof_bound pk sk zero 0L seed);
  refused "range_prove" 0 (fun () -> Pvac_ffi.make_range_proof pk sk cipher 7L);
  refused "range_aggregate_prove" 0 (fun () -> Pvac_ffi.make_aggregated_range_proof pk sk cipher 7L);
  refused "evalkey" 0 (fun () -> Pvac_ffi.make_evalkey pk sk 1 1);
  let params = Pvac_ffi.default_params () in
  refused "parameters" 0 Pvac_ffi.default_params;
  refused "keygen_seeded" 0 (fun () -> Pvac_ffi.keygen_from_seed params seed);
  refused "keygen_random" 0 (fun () -> Pvac_ffi.keygen params);
  let secret = Pvac_ffi.serialize_seckey sk in
  refused "secret_parse" 0 (fun () -> Pvac_ffi.deserialize_seckey secret);
  refused "secret_serialize" 0 (fun () -> Pvac_ffi.serialize_seckey sk);
  let module FB = Octra_core.Crypto.FheBalance in
  let request = Octra_core.Pvac_verify_protocol.Zero {
    pubkey = Bytes.to_string key;
    cipher = FB.encode_cipher zero;
    proof = FB.encode_zero_proof proof;
  } in
  expect "worker control failed" (Octra_core.Pvac_verify_direct.response request).accepted;
  refused "worker_request" 0 (fun () -> Octra_core.Pvac_verify_direct.response request);
  expect "cipher input changed" (encoded = Pvac_ffi.serialize_cipher cipher);
  expect "key input changed" (key = Pvac_ffi.serialize_pubkey pk);
  expect "arithmetic changed" (Pvac_ffi.dec_value pk sk (Pvac_ffi.ct_add pk cipher cipher) = 14L);
  expect "invalid bytes reported as memory error"
    (try ignore (Pvac_ffi.deserialize_cipher (Bytes.of_string "invalid")); false
     with Failure _ -> true | _ -> false);
  Preview_alloc.run arm key (Pvac_ffi.serialize_cipher zero)
    (Octra_core.Crypto.FheBalance.encode_zero_proof proof);
  print_endline "status = pass test = native_alloc"