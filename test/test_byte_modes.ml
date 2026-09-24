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
  check_object_charge ();
  check_reg_spans ();
  Printf.printf "byte_modes = pass pairs = 144 slices = 6\n"