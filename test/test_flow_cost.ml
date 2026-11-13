(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module V = Octra_vm.Contract_vm
module F = Octra_vm.Program_type_flow

let need reason value = if not value then failwith reason
let verdict = function Ok () -> "ok" | Error error -> F.error_message error

let joins () =
  let numbers = F.[Int; U64; U128; U256] in
  let text = F.[String; Bytes; Bytes32] in
  let scalars = numbers @ text @ F.[Bool; Addr] in
  let kinds = scalars @ F.[Cipher; PubKey; Unknown] in
  let operations = V.[MOV (2, 1), kinds; NEG (2, 1), numbers;
    STRLEN (2, 1), text; ASSERT 1, F.[Bool];
    SSTORE ("out", 1), scalars; MOV (1, 1), kinds] in
  let choices = None :: List.map Option.some kinds in
  List.iter (fun left -> List.iter (fun right ->
    let facts = {F.empty_facts with root = List.filter_map (fun (slot, kind) ->
      Option.map (fun kind -> slot, kind) kind) [100, left; 101, right]} in
    let input slot label = if List.mem_assoc slot facts.root then V.MLOAD (1, slot) else V.JDEST label in
    List.iter (fun (op, allowed) ->
      let expected = match left, right with
        | Some a, Some b -> List.mem a allowed && List.mem b allowed
          && (a = b || List.mem a numbers && List.mem b numbers)
        | _ -> false in
      let code = V.[|LDI (0, VBool true); JIF (0, 10); input 100 20; JMP 11;
        JDEST 10; input 101 21; JDEST 11; op; STOP|] in
      need "flow type join differs" (Result.is_ok (F.check ~facts code) = expected)) operations)
    choices) choices;
  let values = V.[VString "get"; VString "set"; VString (String.make 4096 'a');
    VInt Z.zero; VInt Z.one; VBool false] in
  List.iter (fun left -> List.iter (fun right ->
    let facts = {F.empty_facts with xcalls = [F.{pc = 9; method_name = "get";
      inputs = []; output = Int; capabilities = []}]} in
    let code = V.[|LDI (0, VBool true); JIF (0, 10); LDI (1, left); JMP 11;
      JDEST 10; LDI (1, right); JDEST 11; MOV (5, 1); LDI (2, VString "target");
      XCALL (3, 2, 5, 4, 0); STOP|] in
    need "flow constant join differs" (Result.is_ok (F.check ~facts code) =
      (left = V.VString "get" && right = V.VString "get"))) values) values

let edges () =
  let check ?(facts = F.empty_facts) max_steps code expected =
    need "flow worklist differs" (verdict (F.check ~max_steps ~facts code) = expected) in
  check 0 [||] "instruction at pc 0 is outside the typed Program profile";
  check 0 V.[|STOP|] "Program type flow exceeds 0 steps";
  check (-1) V.[|STOP|] "Program type flow exceeds -1 steps";
  check 1 V.[|STOP|] "ok";
  check 1 V.[|JDEST 0; JMP 0|] "Program type flow exceeds 1 steps";
  check 2 V.[|JDEST 0; JMP 0|] "ok";
  check 10 V.[|LDI (0, VBool true); JIF (0, 10); NEG (1, 63); STOP;
    JDEST 10; STRLEN (2, 62); STOP|] "uninitialized register r63 at pc 2";
  check 10 V.[|LDI (0, VBool true); JIF (0, 10); LDI (1, VInt Z.zero);
    JMP 10; JDEST 10; MOV (2, 1); STOP|] "uninitialized register r1 at pc 5";
  List.iter (fun reg -> List.iter (fun dest ->
    let code = V.[|LDI (0, VBool true); LDI (2, VString "text"); JIF (0, 10);
      LDI (reg, VInt Z.zero); JMP 11; JDEST 10; LDI (reg, VInt Z.one);
      JDEST 11; ADD (dest, reg, 2); STOP|] in
    check 11 code "Program type flow exceeds 11 steps";
    check 12 code "expected int in r2 at pc 8, got string") [3; reg]) [1; 63];
  let facts = {F.empty_facts with xcalls = [F.{pc = 8; method_name = "get";
    inputs = []; output = Int; capabilities = []}]} in
  List.iter (fun dest -> check ~facts 12 V.[|LDI (0, VBool true); LDI (2, VString "target");
    JIF (0, 10); LDI (1, VString "get"); JMP 11; JDEST 10; LDI (1, VString "set");
    JDEST 11; XCALL (dest, 2, 1, 4, 0); STOP|]
    "external call ABI mismatch at pc 8: method is not constant") [3; 1];
  List.iter (fun (reg, other) ->
    let facts = {F.empty_facts with root = [100, F.Int; 101, F.String; 102, F.Bool; 103, F.Bool]} in
    check ~facts 32 V.[|LDI (32, VBool true); JIF (32, 10); MLOAD (reg, 100);
      MLOAD (other, 102); JMP 11; JDEST 10; MLOAD (reg, 101); MLOAD (other, 103);
      JDEST 11; MOV (31, reg); MOV (30, other); STOP|]
      (Printf.sprintf "uninitialized register r%d at pc 9" reg)) [0, 63; 63, 0]

let fanout arms width =
  let open V in
  List.init arms (fun reg -> LDI (reg, VString "left"))
  @ [LDI (63, VBool true)]
  @ List.init arms (fun reg -> JIF (63, 1000 + reg))
  @ [JMP 2000]
  @ List.concat_map (fun reg -> [JDEST (1000 + reg); LDI (reg, VString "right"); JMP 2000])
      (List.init arms Fun.id)
  @ [JDEST 2000] @ List.init width (fun _ -> NOP) @ [STOP]
  |> Array.of_list

let cost arms width =
  let code = fanout arms width in
  let saved = Array.copy code in
  let steps = (arms + 1) * width + 7 * arms + 4 in
  need "flow step limit changed" (verdict (F.check ~max_steps:(steps - 1) code) =
    Printf.sprintf "Program type flow exceeds %d steps" (steps - 1));
  need "flow exact step count changed" (verdict (F.check ~max_steps:steps code) = "ok");
  for index = 0 to 2 do
    Gc.full_major ();
    let bytes = Gc.allocated_bytes () and counter = Mtime_clock.counter () in
    let result = F.check code in
    let allocated = Gc.allocated_bytes () -. bytes in
    let elapsed = Mtime.Span.to_float_ns (Mtime_clock.count counter) /. 1e6 in
    need "flow generated program refused" (verdict result = "ok");
    need "flow mutated input" (code = saved);
    Printf.printf "event = flow arms = %d width = %d steps = %d index = %d allocated = %.0f ms = %.3f\n%!"
      arms width steps index allocated elapsed;
    need "flow repeated array allocation"
      (allocated < float_of_int (steps * 2304 + Array.length code * 4096))
  done

let () =
  joins ();
  edges ();
  List.iter (fun (arms, width) -> cost arms width) [1, 0; 3, 17; 63, 5700];
  Printf.printf "event = flow status = passed\n%!"