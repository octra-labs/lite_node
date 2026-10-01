(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let mk_vk ~n_public =
  let g1 = 64 in
  let g2 = 128 in
  let total = 6 + 4 + g1 + 3 * g2 + (n_public + 1) * g1 in
  let b = Bytes.make total '\x00' in
  Bytes.set b 0 'O'; Bytes.set b 1 'G'; Bytes.set b 2 '1';
  Bytes.set b 3 '6'; Bytes.set b 4 'V'; Bytes.set b 5 '1';
  Bytes.set b 6 (Char.chr ((n_public lsr 24) land 0xff));
  Bytes.set b 7 (Char.chr ((n_public lsr 16) land 0xff));
  Bytes.set b 8 (Char.chr ((n_public lsr 8) land 0xff));
  Bytes.set b 9 (Char.chr (n_public land 0xff));
  b

let mk_proof () =
  let b = Bytes.make 262 '\x00' in
  Bytes.set b 0 'O'; Bytes.set b 1 'G'; Bytes.set b 2 '1';
  Bytes.set b 3 '6'; Bytes.set b 4 'P'; Bytes.set b 5 '1';
  b

let mk_inputs ~n = Bytes.make (n * 32) '\x00'

let must_be label expected actual =
  if expected = actual then
    Printf.printf "case = %s status = pass result = %b\n%!" label actual
  else begin
    Printf.eprintf "case = %s status = fail actual = %b expected = %b\n%!" label actual expected;
    exit 1
  end

let () =
  let n = 2 in
  let vk_ok = mk_vk ~n_public:n in
  let pf_ok = mk_proof () in
  let in_ok = mk_inputs ~n in

  must_be "garbage"
    false (Zk_ffi.groth16_verify_bn254
      (Bytes.of_string "garbage") (Bytes.of_string "x") (Bytes.of_string "y"));

  must_be "empty"
    false (Zk_ffi.groth16_verify_bn254 Bytes.empty Bytes.empty Bytes.empty);

  must_be "infinity_points"
    false (Zk_ffi.groth16_verify_bn254 vk_ok pf_ok in_ok);

  let vk_bad_magic = Bytes.copy vk_ok in
  Bytes.set vk_bad_magic 0 'X';
  must_be "vk_magic"
    false (Zk_ffi.groth16_verify_bn254 vk_bad_magic pf_ok in_ok);

  let pf_bad_magic = Bytes.copy pf_ok in
  Bytes.set pf_bad_magic 0 'X';
  must_be "proof_magic"
    false (Zk_ffi.groth16_verify_bn254 vk_ok pf_bad_magic in_ok);

  let vk_truncated = Bytes.sub vk_ok 0 (Bytes.length vk_ok - 1) in
  must_be "vk_length"
    false (Zk_ffi.groth16_verify_bn254 vk_truncated pf_ok in_ok);

  let in_off = Bytes.sub in_ok 0 (Bytes.length in_ok - 1) in
  must_be "input_width"
    false (Zk_ffi.groth16_verify_bn254 vk_ok pf_ok in_off);

  let in_count_mismatch = mk_inputs ~n:(n + 1) in
  must_be "input_count"
    false (Zk_ffi.groth16_verify_bn254 vk_ok pf_ok in_count_mismatch);

  let vk_overflow = mk_vk ~n_public:0 in
  let bnp = Bytes.copy vk_overflow in
  Bytes.set bnp 6 '\xff'; Bytes.set bnp 7 '\xff';
  Bytes.set bnp 8 '\xff'; Bytes.set bnp 9 '\xff';
  must_be "vk_count_overflow"
    false (Zk_ffi.groth16_verify_bn254 bnp pf_ok in_ok);

  let n_large = 33 in
  let vk_large = mk_vk ~n_public:n_large in
  let in_large = mk_inputs ~n:n_large in
  must_be "vk_budget"
    false (Zk_ffi.groth16_verify_bn254 vk_large pf_ok in_large);

  let pf_short = Bytes.sub pf_ok 0 (Bytes.length pf_ok - 1) in
  must_be "proof_short"
    false (Zk_ffi.groth16_verify_bn254 vk_ok pf_short in_ok);

  let pf_long = Bytes.cat pf_ok (Bytes.make 1 '\x00') in
  must_be "proof_long"
    false (Zk_ffi.groth16_verify_bn254 vk_ok pf_long in_ok);

  let n0 = 0 in
  let vk0 = mk_vk ~n_public:n0 in
  let in0 = mk_inputs ~n:n0 in
  must_be "zero_inputs"
    false (Zk_ffi.groth16_verify_bn254 vk0 pf_ok in0);

  let vk_garbage_alpha = Bytes.copy vk_ok in
  for i = 10 to 73 do
    Bytes.set vk_garbage_alpha i (Char.chr ((i * 17) land 0xff))
  done;
  must_be "vk_curve"
    false (Zk_ffi.groth16_verify_bn254 vk_garbage_alpha pf_ok in_ok);

  let vk_alpha_overflow = Bytes.copy vk_ok in
  for i = 10 to 41 do
    Bytes.set vk_alpha_overflow i '\xff'
  done;
  must_be "vk_field"
    false (Zk_ffi.groth16_verify_bn254 vk_alpha_overflow pf_ok in_ok);

  let determinism_a = Zk_ffi.groth16_verify_bn254 vk_ok pf_ok in_ok in
  let determinism_b = Zk_ffi.groth16_verify_bn254 vk_ok pf_ok in_ok in
  let determinism_c = Zk_ffi.groth16_verify_bn254 vk_ok pf_ok in_ok in
  if determinism_a = determinism_b && determinism_b = determinism_c then
    Printf.printf "case = determinism status = pass calls = 3 result = %b\n%!" determinism_a
  else begin
    Printf.eprintf "case = determinism status = fail reason = result_mismatch\n%!";
    exit 1
  end;

  Printf.printf "status = pass test = zk_ffi cases = 16\n%!"