(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_vm
open Contract_vm

let require condition reason = if not condition then failwith reason

let budget ?(alloc = 1_000_000) () =
  Byte_work.create (Option.get (Byte_work.limits ~key_bytes:64 ~value_bytes:1_000_000
    ~copy_bytes:1_000_000 ~write_bytes:1_000_000 ~alloc_bytes:alloc ~unit_bytes:32))

let state ?(limit = 1_000_000) ?(strict = false) ?(mode = Int_work.Prior) ?work () =
  create_state ~limit ~strict_values:strict
    ~ctx:{default_ctx with byte_work = work; int_work = mode}
    ~caller:"" ~origin:"" ~address:"" ~value:Z.zero ~storage:(Hashtbl.create 1) ()

let parse st input base =
  st.regs.(0) <- VString input;
  st.regs.(1) <- base;
  st.regs.(2) <- VInt (Z.of_int (-1));
  exec_one st (PARSE_INTS (2, 0, 1))

let check_quota () =
  let input = String.make 500_000 '9' in
  let st = state ~work:(budget ~alloc:64 ()) () in
  let before = Gc.allocated_bytes () in
  let accepted = parse st input (VInt Z.zero) in
  let allocated = Gc.allocated_bytes () -. before in
  require (not accepted) "integer parse ignored quota";
  require (allocated < 100_000.) "integer parse allocated before admission";
  require (Hashtbl.length st.memory.data = 0) "refused parse wrote memory"

let check_effort () =
  let input = String.make 500_000 '9' in
  let st = state ~limit:12 ~work:(budget ~alloc:2_000_000 ()) () in
  let before = Gc.allocated_bytes () in
  let accepted = parse st input (VInt Z.zero) in
  require (not accepted && Gc.allocated_bytes () -. before < 100_000.)
    "integer effort checked after allocation";
  require (Hashtbl.length st.memory.data = 0) "effort refusal wrote memory"

let check_refusal () =
  List.iter (fun (input, base, strict, limit) ->
    let st = state ~limit ~strict ~work:(budget ()) () in
    Hashtbl.replace st.memory.data 10 (VInt (Z.of_int 91));
    st.memory.size <- 11;
    require (not (parse st input (VInt (Z.of_int base)))) "invalid parse accepted";
    require (Hashtbl.length st.memory.data = 1 &&
      Hashtbl.find st.memory.data 10 = VInt (Z.of_int 91) && st.memory.size = 11)
      "parse refusal published partial output";
    require (st.regs.(2) = VInt (Z.of_int (-1))) "parse refusal changed count")
    ["1,2,invalid", 10, true, 1000; "1,2", 16_777_216, false, 1000;
     "1,2,3", 10, false, 15]

let check_empty () =
  let input = String.make 500_000 ',' in
  let st = state ~work:(budget ~alloc:0 ()) () in
  let before = Gc.allocated_bytes () in
  require (parse st input (VInt Z.zero)) "empty fields refused";
  require (Gc.allocated_bytes () -. before < 100_000.) "empty fields copied";
  require (st.regs.(2) = VInt Z.zero && Hashtbl.length st.memory.data = 0)
    "empty fields created cells"

let check_base () =
  let base = VString (String.make 500_000 '0' ^ "1") in
  let st = state ~work:(budget ~alloc:128 ()) () in
  let before = Gc.allocated_bytes () in
  require (not (parse st "1" base)) "base conversion ignored quota";
  require (Gc.allocated_bytes () -. before < 100_000.) "base converted before admission";
  require (Hashtbl.length st.memory.data = 0) "invalid base wrote memory"

let check_values () =
  let inputs = [""; ",,,"; "1,2,-3"; " 1 ,\t-2\r,\n0x10\012";
    "0b101,0o77,0xFF,1_000,+1,-0,00001"; "_,0x,not_int,1e9";
    "1\000,2\011,3\255"; "4294967296,18446744073709551615"] in
  List.iter (fun input ->
    List.iter (fun strict ->
      let prior = state ~strict () in
      let active = state ~strict ~work:(budget ()) () in
      let old_ok = parse prior input (VInt (Z.of_int 10)) in
      let new_ok = parse active input (VInt (Z.of_int 10)) in
      require (old_ok = new_ok) "parse acceptance changed";
      if old_ok then begin
        require (prior.regs.(2) = active.regs.(2) &&
          prior.memory.size = active.memory.size &&
          Hashtbl.length prior.memory.data = Hashtbl.length active.memory.data)
          "parse shape changed";
        Hashtbl.iter (fun key value ->
          require (Hashtbl.find_opt active.memory.data key = Some value)
            "parsed integer changed") prior.memory.data
      end) [false; true]) inputs;
  let st = state () in
  require (parse st "1,2,,3" (VInt Z.zero)) "prior parse refused";
  require (st.effort_used = effort_cost (PARSE_INTS (2, 0, 1)) + 6) "prior effort changed"

let check_plan () =
  let reference text =
    String.split_on_char ',' text |> List.map String.trim
    |> List.filter (fun part -> part <> "") in
  let read strict fields =
    let rec loop values = function
      | [] -> Some (Array.of_list (List.rev values))
      | field :: rest ->
        let value =
          try Some (Z.of_string field)
          with Invalid_argument _ | Failure _ -> if strict then None else Some Z.zero in
        Option.bind value (fun value -> loop (value :: values) rest)
    in
    loop [] fields in
  let check text =
    let fields = reference text in
    let count = List.length fields in
    let chars = List.fold_left (fun total part -> total + String.length part) 0 fields in
    let fits ~count:_ ~chars:_ = true in
    let plan = match Int_text.plan ~capacity:count ~fits text with
      | Some plan -> plan
      | None -> failwith "parse fields changed" in
    require (plan.count = count && plan.chars = chars) "parse fields changed";
    List.iter (fun strict ->
      require (Int_text.read ~strict plan = read strict fields) "integer text changed")
      [false; true];
    if count > 0 then
      require (Int_text.plan ~capacity:(count - 1) ~fits text = None)
        "parse capacity ignored";
    require (Int_text.plan ~capacity:count ~fits:(fun ~count:_ ~chars:_ -> false) text = None)
      "parse plan ignored refusal"
  in
  for byte = 0 to 255 do
    let byte = String.make 1 (Char.chr byte) in
    List.iter check [byte; byte ^ "1" ^ byte; "1," ^ byte ^ "2,"; "1" ^ byte ^ "2"]
  done;
  let random = Random.State.make [|721; 905|] in
  let alphabet = " ,0123456789-+xboABCDEF_\t\r\n\012\011\000" in
  for _ = 1 to 2048 do
    check (String.init (Random.State.int random 96) (fun _ ->
      alphabet.[Random.State.int random (String.length alphabet)]))
  done;
  List.iter check ["0b101,0o77,0xFF,1_000,-0x10"; "  ,,\012\t\n,, ";
    String.make 2048 '9'; "1," ^ String.make 2048 '0' ^ "2"];
  require (Int_text.plan ~capacity:(-1) ~fits:(fun ~count:_ ~chars:_ -> true) "" = None)
    "negative parse capacity";
  let calls = ref 0 in
  let result = Int_text.plan ~capacity:1
    ~fits:(fun ~count:_ ~chars:_ -> incr calls; true) ("1,2," ^ String.make 500_000 '9') in
  require (result = None && !calls = 2) "parse did not stop at capacity"

let check_quote () =
  for length = 0 to 128 do
    for cells = 0 to 128 do
      require (Byte_work.parsing ~length ~cells =
        Some (2 * cells, [Byte_work.Scan length;
          Byte_work.Allocate (96 * cells + 2 * length)])) "integer quote changed"
    done
  done;
  List.iter (fun (length, cells) ->
    require (Byte_work.parsing ~length ~cells = None) "invalid integer quote")
    [-1, 0; 0, -1; max_int, 0; 0, max_int; max_int, max_int]

let check_thresholds () =
  let input = "1,22,333" in
  let needed = 300 in
  let effort = 29 in
  for alloc = 0 to needed + 1 do
    let work = budget ~alloc () in
    let st = state ~work () in
    let accepted = parse st input (VInt (Z.of_int 10)) in
    require (accepted = (alloc >= needed)) "integer allocation threshold";
    if accepted then
      require (Byte_work.available work = alloc - needed && st.effort_used = effort)
        "integer debit differs"
    else
      require (Hashtbl.length st.memory.data = 0 && Byte_work.available work = alloc)
        "refused plan published output"
  done;
  for limit = 0 to effort + 1 do
    let st = state ~limit ~work:(budget ()) () in
    let accepted = parse st input (VInt (Z.of_int 10)) in
    require (accepted = (limit >= effort)) "integer effort threshold";
    if not accepted then require (Hashtbl.length st.memory.data = 0)
      "effort threshold wrote memory"
  done

let check_shared_budget () =
  let work = budget ~alloc:300 () in
  let first = state ~work () in
  require (parse first "1,2" (VInt Z.zero)) "first shared parse";
  let remaining = Byte_work.available work in
  require (exec_one first CHECKPOINT && exec_one first ROLLBACK) "parse rollback";
  require (Byte_work.available work = remaining) "parse work refunded";
  let second = state ~work () in
  require (parse second "3" (VInt Z.zero)) "second shared parse";
  let third = state ~work () in
  require (not (parse third "4" (VInt Z.zero)) && Hashtbl.length third.memory.data = 0)
    "parse owner allowance reset"

let check_sources () =
  List.iter (fun base ->
    let st = state ~work:(budget ()) () in
    require (not (parse st "1" base)) "invalid integer base";
    require (Hashtbl.length st.memory.data = 0) "invalid base published output")
    [VInt (Z.of_int (-1)); VInt (Z.of_int 16_777_217); VInt (Z.shift_left Z.one 100)];
  List.iter (fun base ->
    let prior = state () in
    let active = state ~work:(budget ()) () in
    require (parse prior "1" base && parse active "1" base) "compatible base refused";
    require (prior.memory.size = active.memory.size) "base conversion changed")
    [VString "1"; VString "0001"; VString "not_number"; VInt Z.zero; VBool true];
  let st = state ~work:(budget ()) () in
  require (parse st "1" (VInt (Z.of_int 16_777_216))) "last cell refused";
  require (st.memory.size = 16_777_217) "last cell size";
  let st = state ~work:(budget ~alloc:64 ()) () in
  st.regs.(0) <- VInt (Z.shift_left Z.one 100_000);
  st.regs.(1) <- VInt Z.zero;
  let before = Gc.allocated_bytes () in
  require (not (exec_one st (PARSE_INTS (2, 0, 1)))) "numeric text ignored quota";
  require (Gc.allocated_bytes () -. before < 100_000.) "numeric text converted early"

let number_ops =
  [ADD (2, 0, 1); SUB (2, 0, 1); MUL (2, 0, 1);
   DIV (2, 0, 1); MOD (2, 0, 1); NEG (2, 0); ABS (2, 0);
   LT (2, 0, 1); GT (2, 0, 1); BITAND (2, 0, 1);
   BITOR (2, 0, 1); BITXOR (2, 0, 1); BITSHL (2, 0, 1); BITSHR (2, 0, 1)]

let check_number_quota () =
  let input = VString (String.make 500_000 '9') in
  let check strict mode op source alloc limit =
    let work = budget ~alloc () in
    let st = state ~limit ~strict ~mode ~work () in
    st.regs.(0) <- VInt Z.zero;
    st.regs.(1) <- VInt Z.zero;
    st.regs.(source) <- input;
    st.regs.(2) <- VInt (Z.of_int 19);
    let before = Gc.allocated_bytes () in
    let accepted = exec_one st op in
    require (Gc.allocated_bytes () -. before < 100_000.)
      "number parsed before admission";
    require (not accepted) "number ignored quota";
    require (st.regs.(2) = VInt (Z.of_int 19)) "number refusal changed output";
    require (Byte_work.available work = alloc) "refused number spent allocation";
    require (Hashtbl.length st.memory.data = 0) "number refusal wrote memory" in
  List.iter (fun mode ->
    List.iter (fun op ->
      let sources = match op with NEG _ | ABS _ -> [0] | _ -> [0; 1] in
      List.iter (fun source ->
        check false mode op source 0 1_000_000;
        check false mode op source 8_000_000 0) sources) number_ops;
    List.iter (fun source ->
      check true mode (ADD (2, 0, 1)) source 0 1_000_000;
      check true mode (ADD (2, 0, 1)) source 8_000_000 0) [0; 1])
    [Int_work.Prior; Int_work.Active]

let check_number_values () =
  let values = [VString "17"; VString "-17"; VString "+0x7f"; VString "0b101";
    VString "0o77"; VString "1_000"; VString ""; VString "not_int";
    VString "1\000"; VString " 17 "; VString (String.make 50 '9');
    VInt (Z.of_int 17); VU64 (Z.of_int 17); VU128 (Z.of_int 17);
    VU256 (Z.of_int 17); VBool true] in
  List.iter (fun mode ->
    List.iter (fun strict ->
      List.iter (fun op ->
        List.iter (fun left ->
          List.iter (fun right ->
            let prior = state ~strict ~mode () in
            let active = state ~strict ~mode ~work:(budget ()) () in
            List.iter (fun st ->
              st.regs.(0) <- left;
              st.regs.(1) <- right;
              st.regs.(2) <- VInt (Z.of_int 19)) [prior; active];
            let old_ok = exec_one prior op in
            let new_ok = exec_one active op in
            require (old_ok = new_ok && prior.regs = active.regs)
              "number result changed";
            require (prior.reverted = active.reverted) "number status changed")
            [VInt Z.zero; VInt (Z.of_int 3); VString "2"]) values)
        number_ops) [false; true]) [Int_work.Prior; Int_work.Active]

let check_number_price () =
  let text = VString "7" in
  let cases = [
    ADD (2, 0, 1), false, text, VInt Z.zero, 2;
    ADD (2, 0, 1), false, text, text, 4;
    ADD (2, 0, 0), false, text, VInt Z.zero, 4;
    ADD (0, 0, 1), false, text, VInt Z.zero, 2;
    ADD (2, 0, 1), true, text, VInt Z.zero, 3;
    ADD (2, 0, 1), true, VInt Z.zero, text, 3;
    NEG (2, 0), false, text, VInt Z.zero, 2;
    LT (2, 0, 1), false, text, VInt Z.zero, 1] in
  List.iter (fun (op, strict, left, right, passes) ->
    let memory = passes * 98 in
    let cost = passes * 7 + effort_cost op in
    let check alloc limit =
      let work = budget ~alloc () in
      let st = state ~strict ~limit ~work () in
      st.regs.(0) <- left;
      st.regs.(1) <- right;
      let original = Array.copy st.regs in
      let accepted = exec_one st op in
      require (accepted = (alloc >= memory && limit >= cost)) "number threshold";
      if accepted then begin
        require (st.effort_used = cost) "number work price";
        require (Byte_work.available work = alloc - memory) "number allocation price"
      end else begin
        require (st.regs = original) "refused number changed register";
        require (Byte_work.available work = alloc) "partial number allocation";
        require (st.effort_used = 0) "partial number effort"
      end in
    for alloc = 0 to memory + 1 do check alloc 1000 done;
    for limit = 0 to cost + 1 do check 1000 limit done) cases;
  let work = budget ~alloc:392 () in
  let st = state ~work () in
  st.regs.(0) <- text;
  st.regs.(1) <- VInt Z.zero;
  require (exec_one st (ADD (2, 0, 1))) "shared number first";
  require (exec_one st CHECKPOINT && exec_one st ROLLBACK) "shared number rollback";
  require (Byte_work.available work = 196) "number work refunded";
  require (exec_one st (ADD (2, 0, 1))) "shared number second";
  require (Byte_work.available work = 0) "shared number accounting";
  require (not (exec_one st (ADD (2, 0, 1)))) "shared number allowance reset"

let () =
  check_number_quota ();
  check_number_values ();
  check_number_price ();
  check_plan ();
  check_quote ();
  check_thresholds ();
  check_shared_budget ();
  check_sources ();
  check_refusal ();
  check_quota ();
  check_effort ();
  check_empty ();
  check_base ();
  check_values ();
  Printf.printf "int_text = pass\n"