(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_vm
open Contract_vm

let require condition reason = if not condition then failwith reason

let budget alloc =
  Byte_work.create (Option.get (Byte_work.limits ~key_bytes:64 ~value_bytes:1_000_000
    ~copy_bytes:1_000_000 ~write_bytes:1_000_000 ~alloc_bytes:alloc ~unit_bytes:32))

let state ?(alloc = 8_000_000) ?(active = true) ?(strict = false) ?(math = false)
    ?(addr = 2) ?(int_work = Int_work.Active) () =
  let work = if active then Some (budget alloc) else None in
  let st = create_state ~strict_values:strict ~limit:1_000_000
    ~ctx:{default_ctx with byte_work = work; int_work; math}
    ~caller:"" ~origin:"" ~address:"" ~value:Z.zero ~storage:(Hashtbl.create 1) () in
  for index = 0 to 7 do st.regs.(index) <- VInt (Z.of_int 2) done;
  st.regs.(0) <- VInt (Z.of_int addr);
  st.regs.(9) <- VInt (Z.of_int 655360000);
  for index = 0 to 15 do
    Hashtbl.add st.memory.data index (VInt (Z.of_int (index + 1)))
  done;
  st.memory.size <- 16;
  Hashtbl.add st.storage "saved" "value";
  st

let cases = [
  MATMUL (0, 1, 2, 3, 4, 5), 72;
  MATMUL_Q16 (0, 1, 2, 3, 4, 5), 200;
  MATMUL_FP (0, 1, 2, 3, 4, 5), 120;
  VECDOT (8, 0, 1, 2), 24;
  VECDOT_Q16 (8, 0, 1, 2), 120;
  VECDOT_FP (8, 0, 1, 2), 48;
  SOFTMAX_INPLACE (0, 1), 48;
  SOFTMAX_Q16_INPLACE (0, 1), 120;
  LAYERNORM_INPLACE (0, 1, 2, 3), 24;
  LAYERNORM_Q16_INPLACE (0, 1, 2, 3), 168;
  RMSNORM_INPLACE (0, 1, 2), 24;
  RMSNORM_Q16_INPLACE (0, 1, 2), 120;
  RMSNORM_FP (0, 1, 2), 24;
  SILU_Q16_INPLACE (0, 1), 96;
  ELEMWISE_MUL_INPLACE (0, 1, 2), 24;
  ELEMWISE_MUL_Q16 (0, 1, 2), 120;
  RESIDUAL_ADD (0, 1, 2), 24;
  RESIDUAL_ADD_Q16 (0, 1, 2), 120;
  APPEND_VEC_Q16 (0, 1, 2, 3), 48;
  ARGMAX_Q16 (8, 0, 1), 48;
  ROPE_APPLY_Q16 (0, 1, 2, 9), 88;
  ATTENTION_KV_FP (0, 1, 2, 3, 4, 5, 6, 7), 24;
  ATTENTION_KV_Q16 (0, 1, 2, 3, 4, 5, 6, 7), 888]

let bindings table =
  Hashtbl.to_seq table |> List.of_seq |> List.sort compare

let refused st op =
    let registers = Array.copy st.regs in
    let memory = bindings st.memory.data in
    let size = st.memory.size in
    let storage = bindings st.storage in
    require (not (exec_one st op) && st.reverted) "vector ignored allocation budget";
    require (st.regs = registers && bindings st.memory.data = memory &&
      st.memory.size = size) "vector refusal changed output";
    require (bindings st.storage = storage && st.undo_stack = [])
      "vector refusal changed storage";
    ()

let check_size () =
  List.iter (fun (op, size) ->
    List.iter (fun alloc ->
      let st = state ~alloc () in
      refused st op;
      require (Byte_work.available (Option.get st.ctx.byte_work) = alloc)
        "refusal spent allocation budget") [0; size - 1];
    let st = state ~alloc:size () in
    require (exec_one st op && not st.reverted) "exact vector budget refused";
    require (Byte_work.available (Option.get st.ctx.byte_work) = 0)
      "vector quote changed") cases

let check_values () =
  List.iter (fun (strict, int_work, math, addr) ->
    List.iter (fun (op, _) ->
      let prior = state ~active:false ~strict ~int_work ~math ~addr () in
      let active = state ~strict ~int_work ~math ~addr () in
      let expected = exec_one prior op in
      require (exec_one active op = expected) "vector acceptance changed";
      require (active.reverted = prior.reverted && active.regs = prior.regs)
        "vector result changed";
      require (active.memory.size = prior.memory.size &&
        bindings active.memory.data = bindings prior.memory.data)
        "vector memory changed";
      require (bindings active.storage = bindings prior.storage)
        "vector storage changed") cases)
    (List.concat_map (fun strict ->
      List.concat_map (fun int_work ->
        List.concat_map (fun math ->
          List.map (fun addr -> strict, int_work, math, addr) [0; 2; 8])
          [false; true]) [Int_work.Prior; Int_work.Active]) [false; true])

let check_plan () =
  let open Vector_space in
  List.iter (fun (parts, expected) ->
    require (bytes parts = Some expected) "array plan changed")
    [arrays 2 0, 0; arrays 2 1, 32; matmul 2 3 5, 272;
     q16_matmul 2 3 5, 456; rope 6, 200; attention 1 1 1 1, 256];
  for tokens = 1 to 4 do
    for heads = 1 to 4 do
      for keys = 1 to 4 do
        for width = 1 to 4 do
          let slots = 2 * heads * tokens * width + 4 * heads * tokens +
            4 * heads * width + 4 * tokens * keys * width + 2 * heads in
          let headers = heads * (tokens + width + 5) + 9 in
          require (bytes (attention tokens heads keys width) =
            Some (8 * (slots + headers))) "attention arrays differ"
        done
      done
    done
  done;
  List.iter (fun parts ->
    require (bytes parts = None) "invalid array plan accepted")
    [arrays (-1) 1; arrays 1 (-1); [Array [max_int; 2]];
     [Array [Sys.max_array_length + 1]]; arrays max_int 1;
     [Repeat (max_int, [Repeat (max_int, [Array [1]])])]]

let check_shared () =
  let op = VECDOT_FP (8, 0, 1, 2) in
  let st = state ~alloc:96 () in
  require (exec_one st op && exec_one st op) "shared vector budget refused";
  refused st op;
  require (Byte_work.available (Option.get st.ctx.byte_work) = 0)
    "shared vector budget restored";
  let prior = state ~active:false () in
  require (exec_one prior op) "prior dot refused";
  let short = { (state ()) with effort_limit = prior.effort_used + 1 } in
  refused short op;
  require (Byte_work.available (Option.get short.ctx.byte_work) = 8_000_000)
    "effort refusal spent allocation budget";
  let seen = ref 0 in
  let no_cache = state ~alloc:0 () in
  require (prepare_int_work no_cache Int_work.Mul 2
    (fun _ -> incr seen; Z.zero, Z.zero) = None && !seen = 0)
    "integer operands read before admission"

let check_early () =
  let measure n =
    let st = state ~alloc:0 () in
    st.regs.(2) <- VInt (Z.of_int n);
    let before = Gc.allocated_bytes () in
    let accepted = exec_one st (VECDOT_FP (8, 0, 1, 2)) in
    let allocated = Gc.allocated_bytes () -. before in
    require (not accepted && st.reverted) "large vector accepted";
    allocated in
  let small = measure 1 in
  let large = measure 1_048_576 in
  require (large < 65_536.0 && large -. small < 32_768.0)
    "vector allocated before refusal"

let check_attention () =
  let op = ATTENTION_KV_Q16 (0, 1, 2, 3, 4, 5, 6, 7) in
  List.iter (fun (tokens, heads, keys, width, size) ->
    let prepare alloc active =
      let st = state ~alloc ~active () in
      List.iteri (fun index value -> st.regs.(index) <- VInt (Z.of_int value))
        [0; 100; 200; 300; tokens; heads; keys; width];
      List.iter (fun (base, count) ->
        for index = 0 to count - 1 do
          Hashtbl.replace st.memory.data (base + index)
            (VInt (Z.of_int (65536 * (index mod 5 - 2))))
        done)
        [0, heads * width; 100, tokens * keys * width; 200, tokens * keys * width];
      st in
    let short = prepare (size - 1) true in
    refused short op;
    require (Byte_work.available (Option.get short.ctx.byte_work) = size - 1)
      "attention refusal spent budget";
    let prior = prepare size false in
    let active = prepare size true in
    require (exec_one prior op && exec_one active op) "grouped attention refused";
    require (bindings prior.memory.data = bindings active.memory.data &&
      prior.memory.size = active.memory.size) "grouped attention output changed";
    require (Byte_work.available (Option.get active.ctx.byte_work) = 0)
      "grouped attention quote changed";
    let denied = prepare 0 true in
    let before = Gc.allocated_bytes () in
    let accepted = exec_one denied op in
    let allocated = Gc.allocated_bytes () -. before in
    require (not accepted && denied.reverted && allocated < 65_536.0)
      "attention allocated before refusal")
    [3, 4, 2, 3, 2408; 2, 18, 2, 1, 3944]

let check_prior () =
  let st = state ~active:false () in
  require (reserve_vectors st [Vector_space.Array [max_int; 2]] &&
    st.effort_used = 0) "prior array plan applied"

let () =
  check_plan ();
  check_size ();
  check_values ();
  check_shared ();
  check_early ();
  check_attention ();
  check_prior ();
  Printf.printf "vector_space = pass\n"