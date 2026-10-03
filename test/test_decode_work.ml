(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_vm
open Contract_vm

let require condition reason = if not condition then failwith reason

let budget ?(alloc = 1_000_000) () =
  Byte_work.create (Option.get (Byte_work.limits ~key_bytes:64 ~value_bytes:1_000_000
    ~copy_bytes:1_000_000 ~write_bytes:1_000_000 ~alloc_bytes:alloc ~unit_bytes:32))

let state ?(limit = 1_000_000) ?work () =
  create_state ~limit ~ctx:{default_ctx with byte_work = work}
    ~caller:"" ~origin:"" ~address:"" ~value:Z.zero ~storage:(Hashtbl.create 4) ()

let load st opcode ~dst ~input ~off ~count ~scale =
  st.regs.(0) <- VInt (Z.of_int dst);
  st.regs.(1) <- VString input;
  st.regs.(2) <- VInt (Z.of_int off);
  st.regs.(3) <- VInt (Z.of_int count);
  st.regs.(4) <- VInt scale;
  exec_one st opcode

let fp = LOAD_INT8_FP (0, 1, 2, 3, 4)
let integer = LOAD_INT8_B64_TO_MEM (0, 1, 2, 3, 4)
let q16 = LOAD_INT8_Q16 (0, 1, 2, 3, 4)

let check_cache_identity () =
  let first = Base64.encode_exn (String.make 32 '\001' ^ String.make 16 '\002') in
  let second = Base64.encode_exn (String.make 32 '\001' ^ String.make 16 '\003') in
  require (String.length first = String.length second &&
    String.sub first 0 32 = String.sub second 0 32) "cache inputs";
  List.iter (fun work ->
    let st = state ?work () in
    require (load st fp ~dst:10 ~input:first ~off:40 ~count:1 ~scale:(fp64_to_z 1.))
      "first decode refused";
    require (load st fp ~dst:20 ~input:second ~off:40 ~count:1 ~scale:(fp64_to_z 1.))
      "second decode refused";
    require (mem_get_fp64 st.memory.data 10 = 2. && mem_get_fp64 st.memory.data 20 = 3.)
      "cache reused another input") [None; Some (budget ())]

let check_cache_invalid () =
  let input = Base64.encode_exn (String.make 48 '\002') in
  let broken = String.sub input 0 (String.length input - 4) ^ "!!!!" in
  let st = state () in
  require (load st fp ~dst:10 ~input ~off:40 ~count:1 ~scale:(fp64_to_z 1.))
    "valid decode refused";
  require (not (load st fp ~dst:20 ~input:broken ~off:40 ~count:1 ~scale:(fp64_to_z 1.)))
    "cache accepted invalid input";
  require (not (Hashtbl.mem st.memory.data 20)) "invalid decode wrote memory"

let check_decode_quota () =
  let input = Base64.encode_exn (String.make 500_000 'a') in
  List.iter (fun opcode ->
    let st = state ~work:(budget ~alloc:64 ()) () in
    let before = Gc.allocated_bytes () in
    let accepted = load st opcode ~dst:10 ~input ~off:0 ~count:1 ~scale:Z.one in
    let allocated = Gc.allocated_bytes () -. before in
    require (not accepted) "decode quota ignored";
    require (allocated < 100_000.) "decode allocated before admission";
    require (Hashtbl.length st.memory.data = 0) "refused decode wrote memory")
    [integer; q16; fp]

let check_decode_effort () =
  let input = Base64.encode_exn (String.make 500_000 'a') in
  List.iter (fun opcode ->
    let st = state ~limit:100 ~work:(budget ~alloc:2_000_000 ()) () in
    let before = Gc.allocated_bytes () in
    let accepted = load st opcode ~dst:10 ~input ~off:0 ~count:1 ~scale:Z.one in
    let allocated = Gc.allocated_bytes () -. before in
    require (not accepted && allocated < 100_000.) "decode effort checked after allocation";
    require (Hashtbl.length st.memory.data = 0) "effort refusal wrote memory")
    [integer; q16; fp]

let check_fixed_text () =
  List.iter (fun size ->
    let raw = String.init size (fun index -> Char.chr (index land 255)) in
    let encoded = Base64.encode_exn raw in
    require (decode_raw_or_b64_len size raw = Some raw) "raw fixed bytes";
    require (decode_raw_or_b64_len size encoded = Some raw) "encoded fixed bytes";
    let input = String.make 1_000_000 'A' in
    let before = Gc.allocated_bytes () in
    let result = decode_raw_or_b64_len size input in
    let allocated = Gc.allocated_bytes () -. before in
    require (result = None && allocated < 100_000.) "fixed decode allocated whole input")
    [32; 64]

let check_spawn_quota () =
  let input = Base64.encode_exn ("OCTB12345678" ^ String.make 500_000 'a') in
  List.iter (fun opcode ->
    let work = budget ~alloc:64 () in
    let called = ref false in
    let ctx = {default_ctx with byte_work = Some work;
      deploy_contract = (fun _ _ _ _ _ ->
        called := true;
        Ok {spawned_addr = "program"; effort_used = 0; events = []})} in
    let st = create_state ~limit:1_000_000 ~ctx ~caller:"" ~origin:"" ~address:""
      ~value:Z.zero ~storage:(Hashtbl.create 1) () in
    st.regs.(0) <- VString input;
    let before = Gc.allocated_bytes () in
    let accepted = exec_one st opcode in
    let allocated = Gc.allocated_bytes () -. before in
    require (not accepted && not !called) "spawn decode quota ignored";
    require (allocated < 100_000.) "spawn decoded before admission";
    require (Hashtbl.length st.storage = 0) "spawn refusal changed nonce")
    [SPAWN (1, 0); SPAWN2 (1, 0, 2, 0)]

let check_plan () =
  for length = 0 to 512 do
    List.iter (fun cells ->
      List.iter (fun bits ->
        List.iter (fun cached ->
          let plan = Option.get (Byte_work.decoding ~length ~cells ~bits ~cached) in
          let capacity = ((length + 3) / 4) * 3 in
          let memory = cells * (80 + (bits + 7) / 8) in
          let allocated = memory + if cached then 0 else 2 * capacity in
          let requests = if cached then [Byte_work.Allocate allocated]
            else [Byte_work.Scan length; Byte_work.Allocate allocated] in
          require (plan.max_bytes = capacity && plan.requests = requests) "decode size plan")
          [false; true]) [0; 1; 63; 64; 1024]) [0; 1; 3; 100]
  done;
  List.iter (fun (length, cells, bits) ->
    require (Byte_work.decoding ~length ~cells ~bits ~cached:false = None)
      "invalid decode size accepted")
    [-1, 1, 1; 1, -1, 1; 1, 1, -1; max_int, 1, 1; 1, max_int, max_int];
  for size = 0 to 512 do
    require (Byte_work.encoded_size size = Some (((size + 2) / 3) * 4)) "encoded size"
  done;
  require (Byte_work.encoded_size (-1) = None && Byte_work.encoded_size max_int = None)
    "encoded size overflow"

let check_vector_values () =
  let raw = String.init 256 Char.chr in
  let input = Base64.encode_exn raw in
  List.iter (fun opcode ->
    List.iter (fun scale ->
      let prior = state () in
      let active = state ~work:(budget ()) () in
      require (load prior opcode ~dst:10 ~input ~off:0 ~count:256 ~scale)
        "prior vector refused";
      require (load active opcode ~dst:10 ~input ~off:0 ~count:256 ~scale)
        "metered vector refused";
      for index = 10 to 265 do
        require (Hashtbl.find prior.memory.data index = Hashtbl.find active.memory.data index)
          "vector value changed"
      done) [Z.zero; Z.one; Z.of_int (-8192)]) [integer; q16];
  let work = budget ~alloc:16 () in
  let st = state ~work () in
  require (not (load st integer ~dst:0 ~input ~off:0 ~count:256 ~scale:Z.one))
    "vector cells not reserved";
  require (Hashtbl.length st.memory.data = 0) "vector partial output"

let check_invalid_ranges () =
  let input = Base64.encode_exn "abcdef" in
  List.iter (fun opcode ->
    List.iter (fun (dst, off, count) ->
      let st = state ~work:(budget ()) () in
      require (not (load st opcode ~dst ~input ~off ~count ~scale:Z.one))
        "invalid vector range accepted";
      require (Hashtbl.length st.memory.data = 0) "invalid range wrote memory")
      [-1, 0, 1; max_int, 0, 1; 16_777_216, 0, 1; 0, -1, 1;
       0, max_int, 2; 0, 6, 1; 0, 0, 0; 0, 0, 1_048_577])
    [integer; q16; fp];
  let st = state ~work:(budget ()) () in
  st.regs.(0) <- VInt (Z.shift_left Z.one 100);
  st.regs.(1) <- VString input;
  st.regs.(2) <- VInt Z.zero;
  st.regs.(3) <- VInt Z.one;
  st.regs.(4) <- VInt Z.one;
  require (not (exec_one st integer)) "oversized destination accepted"

let check_cache_price () =
  let input = Base64.encode_exn (String.make 48 '\002') in
  let st = state () in
  require (load st fp ~dst:10 ~input ~off:0 ~count:1 ~scale:(fp64_to_z 1.))
    "cache load";
  require (st.effort_used = effort_cost fp + 1 + 48 / 4) "uncached price";
  let before = st.effort_used in
  let copy = String.sub input 0 (String.length input) in
  require (load st fp ~dst:20 ~input:copy ~off:0 ~count:1 ~scale:(fp64_to_z 1.))
    "cache repeat";
  require (st.effort_used - before = effort_cost fp) "cached price";
  require (Hashtbl.length st.decoded_chunk_cache = 1) "identical input duplicated";
  require (Hashtbl.length (state ()).decoded_chunk_cache = 0) "cache crossed invocation";
  let work = budget () in
  let st = state ~work () in
  require (load st fp ~dst:10 ~input ~off:0 ~count:1 ~scale:(fp64_to_z 1.)) "metered cache";
  let before = Byte_work.available work in
  require (exec_one st CHECKPOINT && exec_one st ROLLBACK) "cache rollback";
  require (Byte_work.available work = before) "cache rollback refunded";
  require (load st fp ~dst:20 ~input ~off:0 ~count:1 ~scale:(fp64_to_z 1.)) "metered repeat";
  require (before - Byte_work.available work = 32 + 88) "cached payload allocated again"

let check_fixed_compat () =
  let old expected text =
    if String.length text = expected then Some text
    else match Base64.decode text with
      | Ok raw when String.length raw = expected -> Some raw
      | _ -> None in
  let equal expected text =
    require (decode_raw_or_b64_len expected text = old expected text) "fixed input changed" in
  List.iter (fun expected ->
    let raw = String.init expected (fun index -> Char.chr (index land 255)) in
    let encoded = Base64.encode_exn raw in
    for index = 0 to String.length encoded - 1 do
      for byte = 0 to 255 do
        let changed = Bytes.of_string encoded in
        Bytes.set changed index (Char.chr byte);
        equal expected (Bytes.to_string changed)
      done
    done;
    for count = 0 to 64 do
      equal expected (encoded ^ String.make count '=');
      equal expected (String.make count '=');
      equal expected (String.make count 'A')
    done) [32; 64];
  for size = 0 to 256 do
    let raw = String.make size '\255' in
    let text = Base64.encode_exn raw in
    List.iter (fun expected ->
      equal expected raw;
      equal expected text;
      equal expected (Base64.encode_exn ~pad:false raw)) [0; 3; 31; 32; 33; 63; 64; 65]
  done

let check_spawn_values () =
  let raw = "OCTB12345678" in
  List.iter (fun input ->
    List.iter (fun opcode ->
      let received = ref None in
      let ctx = {default_ctx with byte_work = Some (budget ());
        deploy_contract = (fun _ code _ _ _ ->
          received := Some code;
          Ok {spawned_addr = "program"; effort_used = 0; events = []})} in
      let st = create_state ~limit:20_000 ~ctx ~caller:"" ~origin:"" ~address:""
        ~value:Z.zero ~storage:(Hashtbl.create 1) () in
      st.regs.(0) <- VString input;
      require (exec_one st opcode && !received = Some raw) "spawn bytes changed")
      [SPAWN (1, 0); SPAWN2 (1, 0, 2, 0)]) [raw; Base64.encode_exn raw]

let () =
  check_plan ();
  check_cache_identity ();
  check_cache_invalid ();
  check_decode_quota ();
  check_decode_effort ();
  check_fixed_text ();
  check_spawn_quota ();
  check_vector_values ();
  check_invalid_ranges ();
  check_cache_price ();
  check_fixed_compat ();
  check_spawn_values ();
  Printf.printf "decode_work = pass\n"