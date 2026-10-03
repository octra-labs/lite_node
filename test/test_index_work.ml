(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_vm
open Contract_vm

let require condition reason = if not condition then failwith reason

let budget ?(alloc = 1_000_000) () =
  Byte_work.create (Option.get (Byte_work.limits ~key_bytes:64 ~value_bytes:1_000_000
    ~copy_bytes:1_000_000 ~write_bytes:1_000_000 ~alloc_bytes:alloc ~unit_bytes:32))

let state ?(strict = false) ?(limit = 1_000_000) ?work () =
  create_state ~strict_values:strict ~limit ~ctx:{default_ctx with byte_work = work}
    ~caller:"" ~origin:"" ~address:"" ~value:Z.zero ~storage:(Hashtbl.create 1) ()

let cases = [
  MLOADR (8, 0), [0];
  MSTORER (0, 8), [0];
  PARSE_INTS (8, 9, 0), [0];
  OBJECT_MEMBER_REF_AT (8, 9, 0), [0];
  SUBSTR (8, 9, 0, 1), [0; 1];
  BITSHL (8, 9, 0), [0];
  BITSHR (8, 9, 0), [0];
  SKEYS (8, 9, 0), [0];
  SKEYS_PAGE (8, 10, 9, 11, 0), [0];
  SLOADN (0, 1, 2), [0; 1; 2];
  SSTOREN (0, 1, 2), [0; 1; 2];
  MATMUL (0, 1, 2, 3, 4, 5), [0; 1; 2; 3; 4; 5];
  MATMUL_Q16 (0, 1, 2, 3, 4, 5), [0; 1; 2; 3; 4; 5];
  MATMUL_FP (0, 1, 2, 3, 4, 5), [0; 1; 2; 3; 4; 5];
  VECDOT (8, 0, 1, 2), [0; 1; 2];
  VECDOT_Q16 (8, 0, 1, 2), [0; 1; 2];
  VECDOT_FP (8, 0, 1, 2), [0; 1; 2];
  SOFTMAX_INPLACE (0, 1), [0; 1];
  SOFTMAX_Q16_INPLACE (0, 1), [0; 1];
  LAYERNORM_INPLACE (0, 1, 2, 3), [0; 1; 2; 3];
  LAYERNORM_Q16_INPLACE (0, 1, 2, 3), [0; 1; 2; 3];
  RELU_INPLACE (0, 1), [0; 1];
  RMSNORM_INPLACE (0, 1, 2), [0; 1; 2];
  RMSNORM_Q16_INPLACE (0, 1, 2), [0; 1; 2];
  RMSNORM_FP (0, 1, 2), [0; 1; 2];
  SILU_INPLACE (0, 1), [0; 1];
  SILU_Q16_INPLACE (0, 1), [0; 1];
  SILU_FP (0, 1), [0; 1];
  ELEMWISE_MUL_INPLACE (0, 1, 2), [0; 1; 2];
  ELEMWISE_MUL_Q16 (0, 1, 2), [0; 1; 2];
  ELEMWISE_MUL_FP (0, 1, 2), [0; 1; 2];
  RESIDUAL_ADD (0, 1, 2), [0; 1; 2];
  RESIDUAL_ADD_Q16 (0, 1, 2), [0; 1; 2];
  RESIDUAL_ADD_FP (0, 1, 2), [0; 1; 2];
  ROPE_APPLY (0, 1, 2, 9), [0; 1; 2];
  ROPE_APPLY_Q16 (0, 1, 2, 9), [0; 1; 2];
  ROPE_APPLY_FP (0, 1, 2, 9), [0; 1; 2];
  LOAD_INT8_BYTES_TO_MEM (0, 9, 1, 2, 10), [0; 1; 2];
  LOAD_INT8_B64_TO_MEM (0, 9, 1, 2, 10), [0; 1; 2];
  LOAD_INT8_Q16 (0, 9, 1, 2, 10), [0; 1; 2];
  LOAD_INT8_FP (0, 9, 1, 2, 10), [0; 1; 2];
  APPEND_VEC_FP (0, 1, 2, 3), [0; 1; 2; 3];
  APPEND_VEC_Q16 (0, 1, 2, 3), [0; 1; 2; 3];
  ARGMAX_FP (8, 0, 1), [0; 1];
  ARGMAX_Q16 (8, 0, 1), [0; 1];
  ATTENTION_KV_FP (0, 1, 2, 3, 4, 5, 6, 7), [0; 1; 2; 3; 4; 5; 6; 7];
  ATTENTION_KV_Q16 (0, 1, 2, 3, 4, 5, 6, 7), [0; 1; 2; 3; 4; 5; 6; 7];
  SHIFT_ROUND_INPLACE (0, 1, 2), [0; 1; 2]]

let check_quota () =
  let input = VString (String.make 500_000 '9') in
  List.iter (fun (op, indices) ->
    List.iter (fun index ->
      List.iter (fun (alloc, limit) ->
        let work = budget ~alloc () in
        let st = state ~work ~limit () in
        st.regs.(index) <- input;
        let original = Array.copy st.regs in
        let before = Gc.allocated_bytes () in
        require (not (reserve_numbers st op)) "index admission ignored quota";
        require (Gc.allocated_bytes () -. before < 100_000.) "index parsed before admission";
        require (Byte_work.available work = alloc && st.effort_used = 0)
          "index refusal spent partial quote";
        require (st.regs = original) "index admission changed registers")
        [0, 1_000_000; 8_000_000, 0]) indices) cases

let check_overflow () =
  let huge = Z.shift_left Z.one 100 in
  let values = [VInt huge; VInt (Z.neg huge); VU64 huge; VU128 huge; VU256 huge;
    VString (Z.to_string huge); VString (Z.to_string (Z.neg huge))] in
  List.iter (fun strict ->
    List.iter (fun (op, indices) ->
      List.iter (fun index ->
        List.iter (fun value ->
          let st = state ~strict ~work:(budget ()) () in
          st.regs.(index) <- value;
          Hashtbl.add st.memory.data 0 (VInt (Z.of_int 19));
          st.memory.size <- 1;
          Hashtbl.add st.storage "a" "saved";
          let original = Array.copy st.regs in
          let accepted =
            try exec_one st op
            with Z.Overflow -> failwith "index conversion escaped vm" in
          require (not accepted && st.reverted) "oversized index accepted";
          require (st.regs = original) "index refusal changed registers";
          require (Hashtbl.length st.memory.data = 1 && st.memory.size = 1 &&
            Hashtbl.find st.memory.data 0 = VInt (Z.of_int 19))
            "index refusal changed memory";
          require (Hashtbl.length st.storage = 1 && Hashtbl.find st.storage "a" = "saved"
            && st.undo_stack = []) "index refusal changed storage") values) indices) cases)
    [false; true]

let check_roles () =
  let huge = VInt (Z.shift_left Z.one 100) in
  List.iter (fun (op, indices) ->
    let st = state ~work:(budget ~alloc:0 ()) () in
    Array.fill st.regs 0 (Array.length st.regs) huge;
    List.iter (fun index -> st.regs.(index) <- VInt Z.one) indices;
    require (reserve_numbers st op) "non-index operand narrowed";
    require (st.effort_used = 0) "typed index charged parsing";
    let prior = state () in
    Array.fill prior.regs 0 (Array.length prior.regs) huge;
    require (reserve_numbers prior op && prior.effort_used = 0)
      "inactive number policy changed") cases

let bindings table =
  Hashtbl.to_seq table |> List.of_seq |> List.sort compare

let check_values () =
  let prepare strict work indices convert =
    let st = state ~strict ?work () in
    for index = 0 to 7 do st.regs.(index) <- VInt Z.one done;
    st.regs.(9) <- VString "YWJj";
    st.regs.(10) <- VInt Z.one;
    st.regs.(11) <- VString "";
    List.iter (fun index -> st.regs.(index) <- convert Z.one) indices;
    for index = 0 to 9 do
      Hashtbl.add st.memory.data index (VInt (Z.of_int (index + 1)))
    done;
    st.memory.size <- 10;
    Hashtbl.add st.storage "a" "saved";
    st in
  List.iter (fun strict ->
    List.iter (fun (op, indices) ->
      List.iter (fun convert ->
        let prior = prepare strict None indices convert in
        let active = prepare strict (Some (budget ())) indices convert in
        let before = exec_one prior op in
        let after = exec_one active op in
        require (before = after && prior.reverted = active.reverted) "index acceptance changed";
        require (prior.regs = active.regs) "indexed output changed";
        require (prior.memory.size = active.memory.size &&
          bindings prior.memory.data = bindings active.memory.data) "indexed memory changed";
        require (bindings prior.storage = bindings active.storage) "indexed storage changed")
        [(fun number -> VInt number); (fun number -> VString (Z.to_string number));
         (fun _ -> VString "0x1"); (fun _ -> VString "01")]) cases)
    [false; true]

let check_quotes () =
  List.iter (fun (op, source, passes) ->
    let memory = passes * 98 in
    let parsing = passes * 7 in
    let check alloc limit =
      let work = budget ~alloc () in
      let st = state ~work ~limit () in
      st.regs.(source) <- VString "1";
      let original = Array.copy st.regs in
      let accepted = reserve_numbers st op in
      require (accepted = (alloc >= memory && limit >= parsing + effort_cost op))
        "index quote threshold";
      require (st.regs = original) "index quote changed input";
      require (Byte_work.available work = (if accepted then alloc - memory else alloc))
        "index allocation quote";
      require (st.effort_used = (if accepted then parsing else 0)) "index effort quote" in
    for alloc = 0 to memory + 1 do check alloc 1000 done;
    for limit = 0 to parsing + effort_cost op + 1 do check 1000 limit done)
    [MLOADR (8, 0), 0, 2; PARSE_INTS (8, 9, 0), 0, 2;
     SUBSTR (8, 9, 0, 1), 0, 3; SSTOREN (0, 1, 2), 2, 3;
     LOAD_INT8_FP (0, 9, 1, 2, 10), 1, 3];
  let st = state ~work:(budget ~alloc:300 ()) () in
  st.regs.(0) <- VString "1";
  st.regs.(1) <- VString "2";
  let before = st.effort_used in
  require (not (reserve_numbers st (VECDOT_FP (8, 0, 1, 2)))) "partial index quote accepted";
  require (st.effort_used = before &&
    Byte_work.available (Option.get st.ctx.byte_work) = 300) "partial index quote spent"

let check_scalars () =
  let ops = [
    EXP_LUT (8, 0), 0; EXP_Q16 (8, 0), 0; TRANSFER (8, 9, 0), 0;
    FHE_SCALE (8, 9, 10, 0), 0; FHE_DIV_CONST (8, 9, 10, 0), 0;
    FHE_ADD_CONST (8, 9, 10, 0), 0; FHE_SUB_CONST (8, 9, 10, 0), 0;
    FHE_PEDERSEN (8, 0, 9), 0; LOAD_INT8_BYTES_TO_MEM (1, 9, 2, 3, 0), 0;
    LOAD_INT8_B64_TO_MEM (1, 9, 2, 3, 0), 0; LOAD_INT8_Q16 (1, 9, 2, 3, 0), 0;
    LOAD_INT8_FP (1, 9, 2, 3, 0), 0; ROPE_APPLY (1, 2, 3, 0), 0;
    ROPE_APPLY_Q16 (1, 2, 3, 0), 0; ROPE_APPLY_FP (1, 2, 3, 0), 0] in
  List.iter (fun (op, source) ->
    let st = state ~work:(budget ~alloc:0 ()) () in
    st.regs.(source) <- VString (String.make 500_000 '9');
    let before = Gc.allocated_bytes () in
    require (not (reserve_numbers st op)) "numeric parameter ignored quota";
    require (Gc.allocated_bytes () -. before < 100_000.) "numeric parameter parsed early";
    st.regs.(source) <- VInt (Z.shift_left Z.one 100);
    require (reserve_numbers st op) "numeric parameter narrowed to index") ops

let check_machine_edges () =
  let lo = Z.of_int min_int in
  let hi = Z.of_int max_int in
  let values = [Z.pred lo, false; lo, true; Z.succ lo, true;
    Z.pred hi, true; hi, true; Z.succ hi, false] in
  List.iter (fun (op, indices) ->
    List.iter (fun index ->
      List.iter (fun (number, accepted) ->
        List.iter (fun value ->
          let st = state ~work:(budget ()) () in
          st.regs.(index) <- value;
          require (reserve_numbers st op = accepted) "machine edge admission")
          [VInt number; VString (Z.to_string number)]) values) indices) cases

let check_aliases () =
  List.iter (fun (op, passes) ->
    let alloc = passes * 98 in
    List.iter (fun space ->
      let work = budget ~alloc:space () in
      let st = state ~work () in
      st.regs.(0) <- VString "1";
      let accepted = reserve_numbers st op in
      require (accepted = (space >= alloc)) "aliased index admission";
      require (Byte_work.available work = (if accepted then space - alloc else space))
        "aliased index allocation";
      require (st.effort_used = (if accepted then passes * 7 else 0))
        "aliased index effort") [alloc - 1; alloc; alloc + 1])
    [SSTOREN (0, 0, 0), 9; MATMUL_FP (0, 0, 0, 0, 0, 0), 12;
     LOAD_INT8_BYTES_TO_MEM (0, 9, 0, 0, 0), 7];
  List.iter (fun op ->
    let prepare work =
      let st = state ?work () in
      st.regs.(0) <- VString "1";
      st.regs.(1) <- VString "abcd";
      Hashtbl.add st.memory.data 1 (VInt (Z.of_int 9));
      st.memory.size <- 2;
      st in
    let prior = prepare None in
    let active = prepare (Some (budget ())) in
    require (exec_one prior op && exec_one active op) "aliased index refused";
    require (prior.regs = active.regs && prior.memory.size = active.memory.size &&
      bindings prior.memory.data = bindings active.memory.data) "aliased index result")
    [MLOADR (0, 0); MSTORER (0, 0); SUBSTR (0, 1, 0, 0)]

let () =
  check_quota ();
  check_overflow ();
  check_roles ();
  check_values ();
  check_quotes ();
  check_scalars ();
  check_machine_edges ();
  check_aliases ();
  Printf.printf "index_work = pass\n"