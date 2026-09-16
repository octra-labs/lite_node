(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let check name actual expected =
  if Z.equal actual expected then Printf.printf "%s = 1\n%!" name
  else begin
    Printf.printf "%s = 0\n%!" name;
    exit 1
  end

let check_none name value =
  match value with
  | None -> Printf.printf "%s = 1\n%!" name
  | Some _ ->
    Printf.printf "%s = 0\n%!" name;
    exit 1

let make_state () =
  Octra_vm.Contract_vm.create_state
    ~caller:"oct1111111111111111111111111111111111111111111"
    ~origin:"oct1111111111111111111111111111111111111111111"
    ~address:"oct2222222222222222222222222222222222222222222"
    ~value:Z.zero
    ~storage:(Hashtbl.create 4)
    ~limit:100_000
    ()

let set_mem st addr value =
  Hashtbl.replace st.Octra_vm.Contract_vm.memory.data addr
    (Octra_vm.Contract_vm.VInt (Z.of_int value))

let set_mem_z st addr value =
  Hashtbl.replace st.Octra_vm.Contract_vm.memory.data addr
    (Octra_vm.Contract_vm.VInt value)

let get_mem st addr =
  match Hashtbl.find_opt st.Octra_vm.Contract_vm.memory.data addr with
  | Some (Octra_vm.Contract_vm.VInt value) -> Z.to_int value
  | _ -> 0

let set_mem_array st base values =
  Array.iteri (fun i value -> set_mem st (base + i) value) values

let get_mem_array st base n =
  Array.init n (fun i -> get_mem st (base + i))

let set_reg st reg value =
  st.Octra_vm.Contract_vm.regs.(reg) <-
    Octra_vm.Contract_vm.VInt (Z.of_int value)

let test_runtime () =
  let st = make_state () in
  let q = 65536 in
  set_mem st 10 q;
  set_mem st 11 (2 * q);
  set_mem st 20 (3 * q);
  set_mem st 21 (4 * q);
  set_reg st 0 30;
  set_reg st 1 10;
  set_reg st 2 20;
  set_reg st 3 1;
  set_reg st 4 2;
  set_reg st 5 1;
  let _ = Octra_vm.Contract_vm.exec_one st
    (Octra_vm.Contract_vm.MATMUL_Q16 (0, 1, 2, 3, 4, 5)) in
  check "q16_matmul_runtime" (Z.of_int (get_mem st 30)) (Z.of_int (11 * q));
  set_mem st 40 32768;
  set_mem st 41 (-32768);
  set_reg st 0 40;
  set_reg st 1 2;
  set_reg st 2 16;
  let _ = Octra_vm.Contract_vm.exec_one st
    (Octra_vm.Contract_vm.SHIFT_ROUND_INPLACE (0, 1, 2)) in
  check "q16_shift_runtime_positive" (Z.of_int (get_mem st 40)) Z.one;
  check "q16_shift_runtime_negative" (Z.of_int (get_mem st 41)) (Z.neg Z.one);
  set_mem st 100 (1 * q);
  set_mem st 101 (2 * q);
  set_mem st 102 (3 * q);
  set_mem st 103 (4 * q);
  set_reg st 0 100;
  set_reg st 1 4;
  let _ = Octra_vm.Contract_vm.exec_one st
    (Octra_vm.Contract_vm.SOFTMAX_Q16_INPLACE (0, 1)) in
  let total = get_mem st 100 + get_mem st 101 + get_mem st 102 + get_mem st 103 in
  check "q16_softmax_sum" (Z.of_int total) (Z.of_int q);
  if get_mem st 100 < get_mem st 101 && get_mem st 101 < get_mem st 102
     && get_mem st 102 < get_mem st 103 then
    Printf.printf "q16_softmax_order = 1\n%!"
  else begin
    Printf.printf "q16_softmax_order = 0\n%!";
    exit 1
  end;
  let bad_softmax = make_state () in
  set_mem_z bad_softmax 700 (Z.shift_left Z.one 64);
  set_reg bad_softmax 0 700;
  set_reg bad_softmax 1 1;
  ignore (Octra_vm.Contract_vm.exec_one bad_softmax
    (Octra_vm.Contract_vm.SOFTMAX_Q16_INPLACE (0, 1)));
  check "q16_softmax_range_reject"
    (Z.of_int (if bad_softmax.Octra_vm.Contract_vm.reverted then 1 else 0)) Z.one;
  let bad_matmul = make_state () in
  bad_matmul.Octra_vm.Contract_vm.regs.(0) <-
    Octra_vm.Contract_vm.VInt (Z.shift_left Z.one 200);
  set_reg bad_matmul 1 0;
  set_reg bad_matmul 2 10;
  set_reg bad_matmul 3 1;
  set_reg bad_matmul 4 1;
  set_reg bad_matmul 5 1;
  ignore (Octra_vm.Contract_vm.exec_one bad_matmul
    (Octra_vm.Contract_vm.MATMUL_Q16 (0, 1, 2, 3, 4, 5)));
  check "q16_matmul_address_reject"
    (Z.of_int (if bad_matmul.Octra_vm.Contract_vm.reverted then 1 else 0)) Z.one;
  let bad_matmul_value = make_state () in
  set_mem_z bad_matmul_value 10 (Z.shift_left Z.one 64);
  set_mem bad_matmul_value 20 q;
  set_reg bad_matmul_value 0 30;
  set_reg bad_matmul_value 1 10;
  set_reg bad_matmul_value 2 20;
  set_reg bad_matmul_value 3 1;
  set_reg bad_matmul_value 4 1;
  set_reg bad_matmul_value 5 1;
  ignore (Octra_vm.Contract_vm.exec_one bad_matmul_value
    (Octra_vm.Contract_vm.MATMUL_Q16 (0, 1, 2, 3, 4, 5)));
  check "q16_matmul_range_reject"
    (Z.of_int (if bad_matmul_value.Octra_vm.Contract_vm.reverted then 1 else 0)) Z.one

let test_normalization () =
  let open Octra_vm in
  let st = make_state () in
  let q = 65536 in
  set_reg st 0 100;
  set_reg st 1 2;
  set_reg st 2 200;
  set_reg st 3 300;
  set_mem_array st 100 [|q; 2 * q|];
  set_mem_array st 200 [|q; q|];
  set_mem_array st 300 [|0; 0|];
  ignore (Octra_vm.Contract_vm.exec_one st
    (Octra_vm.Contract_vm.RMSNORM_Q16_INPLACE (0, 1, 2)));
  let rms = get_mem_array st 100 2 in
  check "q16_rms_positive" (Z.of_int (if rms.(0) > 0 then 1 else 0)) Z.one;
  check "q16_rms_order" (Z.of_int (if rms.(1) > rms.(0) then 1 else 0)) Z.one;
  set_mem_array st 100 [|q; 2 * q|];
  set_reg st 0 100;
  set_reg st 1 2;
  set_reg st 2 200;
  set_reg st 3 300;
  ignore (Octra_vm.Contract_vm.exec_one st
    (Octra_vm.Contract_vm.LAYERNORM_Q16_INPLACE (0, 1, 2, 3)));
  let layer = get_mem_array st 100 2 in
  check "q16_layer_sum" (Z.of_int (layer.(0) + layer.(1))) Z.zero;
  check "q16_layer_order" (Z.of_int (if layer.(1) > layer.(0) then 1 else 0)) Z.one;
  let huge = Z.shift_left Z.one 64 in
  check_none "q16_layer_range"
    (Fixed_q16.layer [|huge|] [|Z.of_int q|] [|Z.zero|]);
  set_mem st 400 1;
  set_mem st 401 2;
  set_mem st 402 3;
  set_reg st 0 400;
  set_reg st 1 2;
  set_reg st 2 402;
  Hashtbl.replace st.Octra_vm.Contract_vm.memory.data 402
    (Octra_vm.Contract_vm.VString "bad");
  ignore (Octra_vm.Contract_vm.exec_one st
    (Octra_vm.Contract_vm.RMSNORM_Q16_INPLACE (0, 1, 2)));
  check "q16_memory_type_reject"
    (Z.of_int (if st.Octra_vm.Contract_vm.reverted then 1 else 0)) Z.one

let test_profile () =
  let open Octra_vm in
  let code = [|
    Contract_vm.LDI (1, Contract_vm.VInt Z.zero);
    Contract_vm.EXP_Q16 (0, 1);
    Contract_vm.LDI (2, Contract_vm.VInt Z.zero);
    Contract_vm.LDI (3, Contract_vm.VInt (Z.of_int 4));
    Contract_vm.SOFTMAX_Q16_INPLACE (2, 3);
    Contract_vm.LDI (4, Contract_vm.VInt Z.zero);
    Contract_vm.LDI (5, Contract_vm.VInt (Z.of_int 2));
    Contract_vm.LDI (6, Contract_vm.VInt Z.zero);
    Contract_vm.LDI (7, Contract_vm.VInt Z.zero);
    Contract_vm.LAYERNORM_Q16_INPLACE (4, 5, 6, 7);
    Contract_vm.LDI (8, Contract_vm.VInt Z.zero);
    Contract_vm.LDI (9, Contract_vm.VInt (Z.of_int 2));
    Contract_vm.LDI (10, Contract_vm.VInt Z.zero);
    Contract_vm.RMSNORM_Q16_INPLACE (8, 9, 10);
  |] in
  (match Admission.of_code code with
   | Error _ -> Printf.printf "q16_legacy_reject = 1\n%!"
   | Ok _ ->
     Printf.printf "q16_legacy_reject = 0\n%!";
     exit 1);
  (match Admission.of_program code with
   | Ok _ -> Printf.printf "q16_program_accept = 1\n%!"
   | Error error ->
     Printf.printf "q16_program_accept = 0\n%!";
     failwith (Admission.error_message error));
  let raw = Bytecode.encode [|
    Contract_vm.EXP_Q16 (0, 1);
    Contract_vm.SOFTMAX_Q16_INPLACE (2, 3);
    Contract_vm.LAYERNORM_Q16_INPLACE (4, 5, 6, 7);
    Contract_vm.RMSNORM_Q16_INPLACE (8, 9, 10);
  |] in
  (match Bytecode.decode raw with
  | Ok [|Contract_vm.EXP_Q16 (0, 1); Contract_vm.SOFTMAX_Q16_INPLACE (2, 3);
         Contract_vm.LAYERNORM_Q16_INPLACE (4, 5, 6, 7);
         Contract_vm.RMSNORM_Q16_INPLACE (8, 9, 10)|] ->
    Printf.printf "q16_wire_roundtrip = 1\n%!"
  | Ok _ ->
    Printf.printf "q16_wire_roundtrip = 0\n%!";
    exit 1
  | Error error ->
    Printf.printf "q16_wire_roundtrip = 0\n%!";
    failwith error);
  let program = Octra_vm.Oct_compile.compile_program
    "program Q16 { fn run(): bool { return rmsnorm_q16(0, 2, 4) } }" in
  (match program.Octra_vm.Oct_compile.error, program.Octra_vm.Oct_compile.program_envelope with
   | None, Some _ -> Printf.printf "q16_compiler_program = 1\n%!"
   | _ ->
     Printf.printf "q16_compiler_program = 0 error = %s\n%!"
       (Option.value ~default:"none" program.Octra_vm.Oct_compile.error);
     exit 1);
  let legacy = Octra_vm.Oct_compile.compile
    "contract Q16 { fn run(): bool { return layernorm_q16(0, 2, 4, 6) } }" in
  match legacy.Octra_vm.Oct_compile.error with
  | Some _ -> Printf.printf "q16_compiler_legacy_reject = 1\n%!"
  | None ->
    Printf.printf "q16_compiler_legacy_reject = 0\n%!";
    exit 1

let test_strict_runtime () =
  let st = Octra_vm.Contract_vm.create_state
    ~strict_values:true
    ~caller:"oct1111111111111111111111111111111111111111111"
    ~origin:"oct1111111111111111111111111111111111111111111"
    ~address:"oct2222222222222222222222222222222222222222222"
    ~value:Z.zero
    ~storage:(Hashtbl.create 4)
    () in
  st.Octra_vm.Contract_vm.regs.(1) <- Octra_vm.Contract_vm.VString "1";
  ignore (Octra_vm.Contract_vm.exec_one st (Octra_vm.Contract_vm.EXP_Q16 (0, 1)));
  check "q16_exp_strict_type" (Z.of_int (if st.Octra_vm.Contract_vm.reverted then 1 else 0)) Z.one

let test_output_range () =
  let open Octra_vm in
  let values = [|Z.zero; Z.of_int 65536|] in
  let limit = Z.pred (Z.shift_left Z.one 63) in
  let gains = Array.make 2 limit in
  let bias = Array.make 2 limit in
  check_none "layer_output" (Fixed_q16.layer ~math:true values gains bias);
  check_none "rms_output" (Fixed_q16.rms ~math:true values gains);
  assert (Option.is_some (Fixed_q16.layer ~math:false values gains bias));
  assert (Option.is_some (Fixed_q16.rms ~math:false values gains));
  List.iter (fun math ->
    List.iter (fun instr ->
      let st = Contract_vm.create_state
          ~caller:"oct1111111111111111111111111111111111111111111"
          ~origin:"oct1111111111111111111111111111111111111111111"
          ~address:"oct2222222222222222222222222222222222222222222"
          ~value:Z.zero ~storage:(Hashtbl.create 4) ~limit:100_000
          ~ctx:{Contract_vm.default_ctx with math} () in
      Array.iteri (fun i value -> set_mem_z st (100 + i) value) values;
      Array.iteri (fun i value -> set_mem_z st (200 + i) value) gains;
      Array.iteri (fun i value -> set_mem_z st (300 + i) value) bias;
      set_reg st 0 100;
      set_reg st 1 2;
      set_reg st 2 200;
      set_reg st 3 300;
      ignore (Contract_vm.exec_one st instr);
      assert (st.reverted = math);
      if math then Array.iteri (fun i value ->
        assert (Hashtbl.find st.memory.data (100 + i) = Contract_vm.VInt value)) values
    ) [Contract_vm.LAYERNORM_Q16_INPLACE (0, 1, 2, 3);
       Contract_vm.RMSNORM_Q16_INPLACE (0, 1, 2)]
  ) [false; true]

let () =
  test_output_range ();
  let open Octra_vm in
  let one = Z.of_int 65536 in
  check "q16_round_positive" (Fixed_q16.round16 (Z.of_int 32768)) Z.one;
  check "q16_round_negative" (Fixed_q16.round16 (Z.of_int (-32768))) (Z.neg Z.one);
  check "q16_round_exact" (Fixed_q16.round16 (Z.mul (Z.of_int 3) one)) (Z.of_int 3);
  check "q16_exp_zero" (Fixed_q16.exp Z.zero) (Z.of_int 65536);
  check "q16_exp_negative_bound" (Fixed_q16.exp (Z.of_int (-10000000))) Z.zero;
  let exp_positive = Fixed_q16.exp (Z.of_int 10000000) in
  check "q16_exp_positive_bound"
    (Z.of_int (if Z.compare (Z.abs (Z.sub exp_positive (Z.of_int 195360063)))
                  (Z.of_int 4096) < 0 then 1 else 0)) Z.one;
  check "q16_inverse_pow_range"
    (Z.of_int (if Option.is_some (Fixed_q16.inverse_pow (Z.mul (Z.of_int 10000) one) one) then 1 else 0)) Z.one;
  if Z.compare (Fixed_q16.exp (Z.of_int 32768)) (Fixed_q16.exp Z.zero) > 0 then
    Printf.printf "q16_exp_monotone = 1\n%!"
  else begin
    Printf.printf "q16_exp_monotone = 0\n%!";
    exit 1
  end;
  check "q16_trunc_mul" (Fixed_q16.trunc_mul (Z.mul (Z.of_int 3) one) (Z.of_int 2))
    (Z.of_int 6);
  check_none "q16_invalid_shift_zero" (Fixed_q16.make_round 0);
  check_none "q16_invalid_shift_large" (Fixed_q16.make_round 63);
  match Fixed_q16.make_round 16 with
  | None ->
    Printf.printf "q16_rounder = 0\n%!";
    exit 1
  | Some round ->
    check "q16_rounder_positive" (round (Z.of_int 32768)) Z.one;
    check "q16_rounder_negative" (round (Z.of_int (-32768))) (Z.neg Z.one);
    test_runtime ();
    test_normalization ();
    test_profile ();
    test_strict_runtime ()