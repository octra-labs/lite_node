(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_vm
open Contract_vm

let require condition reason = if not condition then failwith reason

let budget () =
  Byte_work.create (Option.get (Byte_work.limits ~key_bytes:64 ~value_bytes:1_000_000
    ~copy_bytes:1_000_000 ~write_bytes:1_000_000 ~alloc_bytes:8_000_000 ~unit_bytes:32))

let state ?(strict = false) ?(active = true) ?(int_work = Int_work.Prior) () =
  let work = if active then Some (budget ()) else None in
  let st = create_state ~strict_values:strict ~limit:1_000_000
    ~ctx:{default_ctx with byte_work = work; int_work} ~caller:"" ~origin:"" ~address:""
    ~value:Z.zero ~storage:(Hashtbl.create 1) () in
  for index = 0 to 7 do st.regs.(index) <- VInt (Z.of_int 2) done;
  st.regs.(9) <- VString "ab";
  st.regs.(10) <- VInt Z.one;
  for index = 0 to 15 do
    Hashtbl.add st.memory.data index (VInt (Z.of_int (index + 1)))
  done;
  st.memory.size <- 16;
  Hashtbl.add st.storage "saved" "value";
  st

let cases = [
  VECDOT (8, 0, 1, 2), [0; 1];
  VECDOT_Q16 (8, 0, 1, 2), [0; 1];
  VECDOT_FP (8, 0, 1, 2), [0; 1];
  MATMUL (0, 1, 2, 3, 4, 5), [0; 1; 2];
  MATMUL_Q16 (0, 1, 2, 3, 4, 5), [0; 1; 2];
  MATMUL_FP (0, 1, 2, 3, 4, 5), [0; 1; 2];
  SOFTMAX_INPLACE (0, 1), [0];
  SOFTMAX_Q16_INPLACE (0, 1), [0];
  LAYERNORM_INPLACE (0, 1, 2, 3), [0; 2; 3];
  LAYERNORM_Q16_INPLACE (0, 1, 2, 3), [0; 2; 3];
  RMSNORM_INPLACE (0, 1, 2), [0; 2];
  RMSNORM_Q16_INPLACE (0, 1, 2), [0; 2];
  RMSNORM_FP (0, 1, 2), [0; 2];
  RELU_INPLACE (0, 1), [0];
  SILU_INPLACE (0, 1), [0];
  SILU_Q16_INPLACE (0, 1), [0];
  SILU_FP (0, 1), [0];
  ELEMWISE_MUL_INPLACE (0, 1, 2), [0; 1];
  ELEMWISE_MUL_Q16 (0, 1, 2), [0; 1];
  ELEMWISE_MUL_FP (0, 1, 2), [0; 1];
  RESIDUAL_ADD (0, 1, 2), [0; 1];
  RESIDUAL_ADD_Q16 (0, 1, 2), [0; 1];
  RESIDUAL_ADD_FP (0, 1, 2), [0; 1];
  SHIFT_ROUND_INPLACE (0, 1, 2), [0];
  ROPE_APPLY (0, 1, 2, 10), [0];
  ROPE_APPLY_Q16 (0, 1, 2, 10), [0];
  ROPE_APPLY_FP (0, 1, 2, 10), [0];
  ARGMAX_FP (8, 0, 1), [0];
  ARGMAX_Q16 (8, 0, 1), [0];
  APPEND_VEC_FP (0, 1, 2, 3), [0; 2];
  APPEND_VEC_Q16 (0, 1, 2, 3), [0; 2];
  ATTENTION_KV_FP (0, 1, 2, 3, 4, 5, 6, 7), [0; 1; 2; 3];
  ATTENTION_KV_Q16 (0, 1, 2, 3, 4, 5, 6, 7), [0; 1; 2; 3];
  SLOADN (0, 1, 2), [0; 1]]

let bindings table =
  Hashtbl.to_seq table |> List.of_seq |> List.sort compare

let check_refusal st op =
  let registers = Array.copy st.regs in
  let memory = bindings st.memory.data in
  let size = st.memory.size in
  let storage = bindings st.storage in
  let undo = st.undo_stack in
  let before = Gc.allocated_bytes () in
  let result = try exec_one st op
    with Invalid_argument _ -> failwith "address exception escaped vm" in
  require (not result && st.reverted) "invalid memory span accepted";
  require (Gc.allocated_bytes () -. before < 100_000.) "span checked after allocation";
  require (st.regs = registers) "span refusal changed registers";
  require (bindings st.memory.data = memory && st.memory.size = size)
    "span refusal changed memory";
  require (bindings st.storage = storage && st.undo_stack = undo)
    "span refusal changed storage"

let check_immediate () =
  List.iter (fun index ->
    let st = state () in
    check_refusal st (MSTORE (index, 0))) [-1; min_int; max_int];
  List.iter (fun index ->
    let st = state () in
    Hashtbl.replace st.memory.data index (VInt Z.one);
    check_refusal st (MLOAD (8, index))) [-1; min_int]

let check_addresses () =
  List.iter (fun strict ->
    List.iter (fun (op, addresses) ->
      List.iter (fun address ->
        List.iter (fun value ->
          let st = state ~strict () in
          (match op with
           | APPEND_VEC_FP _ | APPEND_VEC_Q16 _ -> st.regs.(1) <- VInt Z.zero
           | _ -> ());
          st.regs.(address) <- VInt (Z.of_int value);
          check_refusal st op)
          [-1; min_int; max_int]) addresses) cases)
    [false; true]

let check_raw () =
  List.iter (fun (dst, off, count) ->
    let st = state () in
    List.iter (fun (index, value) -> st.regs.(index) <- VInt (Z.of_int value))
      [0, dst; 1, off; 2, count];
    check_refusal st (LOAD_INT8_BYTES_TO_MEM (0, 9, 1, 2, 10)))
    [max_int, 0, 2; -1, 0, 2; 0, max_int, 2; 0, max_int - 1, 3]

let compare_state prior active =
  require (prior.reverted = active.reverted && prior.regs = active.regs)
    "valid memory result changed";
  require (prior.memory.size = active.memory.size &&
    bindings prior.memory.data = bindings active.memory.data) "valid memory writes changed";
  require (bindings prior.storage = bindings active.storage) "valid storage changed"

let check_values () =
  List.iter (fun (strict, int_work, text) ->
    List.iter (fun (op, addresses) ->
      List.iter (fun layout ->
        let prepare active =
          let st = state ~strict ~active ~int_work () in
          List.iteri (fun index reg ->
            st.regs.(reg) <- VInt (Z.of_int (layout index))) addresses;
          if text then Array.iteri (fun index -> function
            | VInt value -> st.regs.(index) <- VString (Z.to_string value)
            | _ -> ()) st.regs;
          st in
        let prior = prepare false in
        let active = prepare true in
        let expected = exec_one prior op in
        require (exec_one active op = expected) "valid memory acceptance changed";
        compare_state prior active)
        [(fun _ -> 0); (fun index -> index); (fun index -> 8 * index)]) cases)
    [false, Int_work.Prior, false; false, Int_work.Active, false;
     true, Int_work.Prior, false; true, Int_work.Active, false;
     false, Int_work.Prior, true; false, Int_work.Active, true]

let check_edges () =
  List.iter (fun active ->
    let st = state ~active () in
    require (exec_one st (MSTORE (max_int - 1, 0))) "last cell refused";
    require (st.memory.size = max_int) "last cell size";
    require (exec_one st (MLOAD (8, max_int - 1))) "last cell read refused";
    require (st.regs.(8) = st.regs.(0)) "last cell value";
    Hashtbl.replace st.memory.data max_int (VInt Z.one);
    require (exec_one st (MLOAD (8, max_int))) "last readable cell refused") [false; true];
  List.iter (fun op ->
    List.iter (fun address ->
      let prepare active =
        let st = state ~active () in
        st.regs.(0) <- VInt (Z.of_int address);
        st.regs.(1) <- VInt (Z.of_int 2);
        st in
      let prior = prepare false in
      let active = prepare true in
      require (exec_one prior op && exec_one active op) "valid high address refused";
      compare_state prior active)
      [0; max_int - 1])
    [SILU_FP (0, 1); SOFTMAX_INPLACE (0, 1); RELU_INPLACE (0, 1)]

let check_arithmetic () =
  let values = [min_int; -1; 0; 1; 2; 131072; 1_048_576; max_int - 1; max_int] in
  let within value = Z.sign value >= 0 && Z.leq value (Z.of_int max_int) in
  List.iter (fun base ->
    List.iter (fun count ->
      let sized = base >= 0 && count >= 0 && within Z.(add (of_int base) (of_int count)) in
      let valid = base >= 0 && count >= 0 && (count = 0 ||
        within Z.(add (of_int base) (sub (of_int count) one))) in
      require (Mem_span.valid base count = valid) "span arithmetic";
      require (Mem_span.sized base count = sized) "size arithmetic";
      List.iter (fun index ->
        let shift = Z.mul (Z.of_int index) (Z.of_int count) in
        let target = Z.add (Z.of_int base) shift in
        let last = Z.add target (Z.sub (Z.of_int count) Z.one) in
        let accepted = index >= 0 && count > 0 && within shift && within target && within last in
        let expected = if accepted then Some (Z.to_int target) else None in
        require (Mem_span.offset base index count = expected) "offset arithmetic") values) values) values

let check_empty_load () =
  List.iter (fun count ->
    let prepare active =
      let st = state ~active () in
      st.regs.(0) <- VInt (Z.of_int min_int);
      st.regs.(1) <- VInt (Z.of_int max_int);
      st.regs.(2) <- VInt (Z.of_int count);
      st in
    let prior = prepare false in
    let active = prepare true in
    require (exec_one prior (SLOADN (0, 1, 2)) && exec_one active (SLOADN (0, 1, 2)))
      "empty load changed";
    compare_state prior active) [min_int; -1; 0]

let check_offsets () =
  List.iter (fun op ->
    List.iter (fun dst ->
      let prepare active =
        let st = state ~active () in
        st.regs.(0) <- VInt (Z.of_int dst);
        st in
      let prior = prepare false in
      let active = prepare true in
      require (exec_one prior op && exec_one active op) "valid offset refused";
      compare_state prior active) [-1; 0; 1])
    [APPEND_VEC_FP (0, 1, 2, 3); APPEND_VEC_Q16 (0, 1, 2, 3)];
  List.iter (fun dst ->
    let st = state () in
    st.regs.(0) <- VInt (Z.of_int dst);
    check_refusal st (APPEND_VEC_FP (0, 1, 2, 3))) [max_int - 4; max_int]

let check_prior () =
  let st = state ~active:false () in
  require (exec_one st (MSTORE (max_int, 0))) "prior immediate changed";
  require (st.memory.size = min_int) "prior size changed";
  require (exec_one st (MLOAD (8, max_int))) "prior read changed";
  let st = state ~active:false () in
  st.regs.(0) <- VInt (Z.of_int max_int);
  require (exec_one st (APPEND_VEC_FP (0, 1, 2, 3))) "prior append changed";
  require (Hashtbl.mem st.memory.data (min_int + 3)) "prior append address changed";
  let st = state ~active:false () in
  st.regs.(0) <- VInt Z.zero;
  st.regs.(1) <- VInt (Z.of_int max_int);
  let raised = try ignore (exec_one st (LOAD_INT8_BYTES_TO_MEM (0, 9, 1, 2, 10))); false
    with Invalid_argument _ -> true in
  require raised "prior raw range changed"

let check_large () =
  let prepare active =
    let st = state ~active () in
    st.regs.(0) <- VInt (Z.of_int 16_777_217);
    st.regs.(1) <- VInt (Z.of_int 131073);
    st in
  let prior = prepare false in
  let active = prepare true in
  require (exec_one prior (RELU_INPLACE (0, 1)) && exec_one active (RELU_INPLACE (0, 1)))
    "existing span limit reduced";
  compare_state prior active

let () =
  check_arithmetic ();
  check_immediate ();
  check_addresses ();
  check_raw ();
  check_values ();
  check_edges ();
  check_empty_load ();
  check_offsets ();
  check_prior ();
  check_large ();
  Printf.printf "mem_span = pass\n"