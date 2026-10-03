(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_vm
open Contract_vm

let require condition reason = if not condition then failwith reason

let check_concat () =
  let inputs = [VString "x"; VBytes "x"; VBytes32 "x"; VInt Z.one;
                VBool true; VAddr "x"] in
  List.iter (fun byte_result ->
    List.iter (fun strict_values ->
      List.iter (fun left -> List.iter (fun right ->
        let state = create_state ~byte_result ~strict_values
          ~caller:"" ~origin:"" ~address:"" ~value:Z.zero
          ~storage:(Hashtbl.create 0) () in
        state.regs.(0) <- left;
        state.regs.(1) <- right;
        let bytes = match left, right with VBytes _, VBytes _ -> true | _ -> false in
        let expected = not strict_values || byte_result = String_bytes || bytes in
        require (exec_one state (CONCAT (2, 0, 1)) = expected) "concat admission differs";
        if expected then begin
          let text = to_string left ^ to_string right in
          let value = if byte_result = Typed_bytes && bytes then VBytes text else VString text in
          require (state.regs.(2) = value) "concat result differs";
          require (state.effort_used = 3) "concat effort differs"
        end) inputs) inputs) [false; true]) [String_bytes; Typed_bytes]

let check_substr () =
  List.iter (fun byte_result ->
    List.iter (fun source ->
      let state = create_state ~byte_result ~strict_values:true
        ~caller:"" ~origin:"" ~address:"" ~value:Z.zero
        ~storage:(Hashtbl.create 0) () in
      state.regs.(0) <- source;
      state.regs.(1) <- VInt Z.zero;
      state.regs.(2) <- VInt Z.one;
      require (exec_one state (SUBSTR (3, 0, 1, 2))) "substr refused";
      let expected = match byte_result, source with
        | Typed_bytes, VBytes _ -> VBytes "a" | _ -> VString "a" in
      require (state.regs.(3) = expected) "substr result differs")
      [VString "ab"; VBytes "ab"; VBytes32 "ab"]) [String_bytes; Typed_bytes]

let check_decoder () =
  let raw = Bytecode.encode [|FHE_PEDERSEN_IDENTITY 0; STOP|] in
  require (Result.is_ok (Bytecode.decode ~active:true raw)) "active decoder refused";
  require (Result.is_error (Bytecode.decode ~active:false raw)) "historical decoder accepted";
  let raw = Bytecode.encode [|LDI (0, VInt Z.one); STOP|] in
  require (Bytecode.decode ~active:true raw = Bytecode.decode ~active:false raw)
    "historical instruction differs"

let pool_image consts ops =
  let output = Buffer.create 128 in
  Buffer.add_string output Bytecode.magic;
  Bytecode.put_u16le output Bytecode.version;
  Bytecode.put_u16le output (List.length consts);
  Bytecode.put_u32le output (Array.length ops);
  List.iter (fun value ->
    let data = Bytecode.const_data value in
    Bytecode.put_u8 output (Bytecode.const_tag value);
    Bytecode.put_u32le output (String.length data);
    Buffer.add_string output data) consts;
  Array.iter (Buffer.add_string output) ops;
  Buffer.contents output

let check_pool_share () =
  let ldi = "\x0b\x00\x00\x00" in
  let check active text ops =
    let raw = pool_image [Bytecode.CInt text] ops in
    let code = match Bytecode.decode ~active raw with
      | Ok code -> code
      | Error reason -> failwith reason in
    let number = function
      | LDI (_, VInt value) | CAP_CHECK (value, _) | CAP_CLOSE (value, _) -> value
      | _ -> failwith "constant instruction differs" in
    let first = number code.(0) in
    require (Z.equal first (Z.of_string text)) "constant value differs";
    Array.iter (fun op ->
      require (number op == first) "integer pool is copied per reference") code;
    if String.length text >= 65536 then
      match Bytecode.decode ~active raw with
      | Ok copy ->
        require (number copy.(0) == first) "integer pool decoded again";
        require (copy != code) "code array shared"
      | Error reason -> failwith reason in
  List.iter (fun active ->
    List.iter (fun text ->
      check active text [|ldi|];
      check active text (Array.make 16 ldi))
      ["123456789012345678901234567890";
       "-123456789012345678901234567890";
       "0x123456789abcdef123456789abcdef"])
    [false; true];
  check true "123456789012345678901234567890"
    [|ldi; "\x89\x00\x00\x01"; "\x8a\x00\x00\x01"; ldi|];
  List.iter (fun active ->
    check active ("0x" ^ String.make (1024 * 1024) 'a') (Array.make 100_000 ldi))
    [false; true]

let check_pool_errors () =
  List.iter (fun active ->
    let raw = pool_image [Bytecode.CInt "invalid"] [|"\x17"|] in
    require (Bytecode.decode ~active raw = Ok [|STOP|]) "unused integer was parsed";
    let raw = pool_image [Bytecode.CInt "invalid"] [|"\xff"|] in
    let expected = if active then "unknown opcode 0xff at pc 0"
      else "unknown opcode 0xff at 24" in
    require (Bytecode.decode ~active raw = Error expected) "unused integer changed error order";
    let raw = pool_image [Bytecode.CInt "1"] [|"\x0b\x00\x01\x00"|] in
    let expected = if active then "constant reference 1 at pc 0"
      else "Invalid_argument(\"index out of bounds\")" in
    require (Bytecode.decode ~active raw = Error expected) "constant index error differs";
    let raw = pool_image [Bytecode.CStr "name"; Bytecode.CBool true]
      [|"\x0b\x00\x00\x00"; "\x0b\x01\x01\x00"; "\x17"|] in
    require (Bytecode.decode ~active raw = Ok [|LDI (0, VString "name"); LDI (1, VBool true); STOP|])
      "constant type differs";
    List.iter (fun text ->
      let consts = [Bytecode.CInt text; Bytecode.CInt "987654321098765432109876543210"] in
      let raw = pool_image consts
        [|"\x0b\x00\x00\x00"; "\x0b\x01\x01\x00"; "\x0b\x02\x00\x00"; "\x17"|] in
      match Bytecode.decode_image ~active raw with
      | Error reason -> failwith reason
      | Ok image ->
        require (Array.to_list (Array.map (fun cell -> cell.Bytecode.value) image.consts) = consts)
          "constant image differs";
        require (image.code = [|LDI (0, VInt (Z.of_string text));
          LDI (1, VInt (Z.of_string "987654321098765432109876543210"));
          LDI (2, VInt (Z.of_string text)); STOP|]) "constant index value differs")
      ["123456789012345678901234567890"; "-123456789012345678901234567890"])
    [false; true]

let check_object_charge () =
  let run object_cost =
    let storage = Hashtbl.create 2 in
    Hashtbl.add storage "a" "0";
    Hashtbl.add storage "b" "1";
    let state = create_state ~ctx:{default_ctx with object_cost}
      ~caller:"" ~origin:"" ~address:"" ~value:Z.zero ~storage () in
    let op = OBJECT_TRANSITION_APPLY (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10) in
    require (not (exec_one state op) && state.reverted) "invalid object transition accepted";
    require (Hashtbl.length storage = 2) "refused object changed storage";
    state.effort_used in
  require (run true - run false = 10) "object scan charge differs"

let check_reg_spans () =
  List.iter (fun (base, count, accepted) ->
    List.iter (fun op ->
      match Verifier.verify [|op; STOP|] with
      | Ok () -> require accepted "invalid register span accepted"
      | Error (Verifier.InvalidRegSpan (0, start, size)) ->
        require (not accepted && start = base && size = count)
          "register span refusal differs"
      | Error _ -> failwith "register span error differs")
      [XCALL (0, 1, 2, base, count); SPAWN2 (0, 1, base, count)])
    [0, 0, true; 0, 64, true; 63, 0, true; 63, 1, true;
     64, 0, false; 63, 2, false; -1, 0, false; 0, -1, false;
     max_int, 1, false; 1, max_int, false]

let () =
  check_concat ();
  check_substr ();
  check_decoder ();
  check_pool_share ();
  check_pool_errors ();
  check_object_charge ();
  check_reg_spans ();
  Printf.printf "byte_modes = pass pairs = 144 slices = 6\n"