(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  code : Contract_vm.instr array;
  digest : string;
}

type outcome = {
  value : Contract_vm.v;
  effort : int;
}

type kind =
  | Int
  | Bool
  | Bytes
  | Data

type error =
  | Decode of string
  | Verify of string
  | Opcode of int * int
  | Literal of int
  | Mark of int * int
  | Empty_reg of int * int
  | Wrong_kind of int * int * kind * kind
  | Opaque_reg of int * int
  | Cycle of int * int
  | Fallthrough of int
  | Dead_code of int
  | Refused

let verifier_text = function
  | Contract_vm.Verifier.InvalidReg (pc, reg) ->
    Printf.sprintf "register is invalid pc = %d reg = %d" pc reg
  | Contract_vm.Verifier.InvalidRegSpan (pc, base, count) ->
    Printf.sprintf "register span is invalid pc = %d base = %d count = %d"
      pc base count
  | Contract_vm.Verifier.InvalidJumpDest target ->
    Printf.sprintf "jump target is absent target = %d" target
  | Contract_vm.Verifier.DuplicateJDest target ->
    Printf.sprintf "jump target is repeated target = %d" target
  | Contract_vm.Verifier.CodeTooLarge count ->
    Printf.sprintf "instruction count exceeds capacity count = %d" count
  | Contract_vm.Verifier.EmptyCode -> "instruction stream is empty"
  | Contract_vm.Verifier.ReservedKey (pc, _) ->
    Printf.sprintf "storage key is reserved pc = %d" pc
  | Contract_vm.Verifier.CapabilityLiteral pc ->
    Printf.sprintf "capability literal is forbidden pc = %d" pc
  | Contract_vm.Verifier.CapabilityKind (pc, kind) ->
    Printf.sprintf "capability kind is invalid pc = %d kind = %s"
      pc (Z.to_string kind)

let data_literal value =
  let size = String.length value in
  let rec loop at =
    if at = size then true
    else if value.[at] = '0' || value.[at] = '1' then loop (at + 1)
    else false
  in
  size > 4 && String.sub value 0 4 = "AR1\n" && loop 4

let literal_kind = function
  | Contract_vm.VInt _ -> Some Int
  | Contract_vm.VBool _ -> Some Bool
  | Contract_vm.VString value when data_literal value -> Some Data
  | Contract_vm.VBytes _ -> Some Bytes
  | Contract_vm.VString _
  | Contract_vm.VBytes32 _
  | Contract_vm.VU64 _
  | Contract_vm.VU128 _
  | Contract_vm.VU256 _
  | Contract_vm.VAddr _
  | Contract_vm.VCap _
  | Contract_vm.VCipher _
  | Contract_vm.VPubKey _ -> None

let allowed_literal value =
  Option.is_some (literal_kind value)

let check_op pc = function
  | Contract_vm.LDI (_, value) when not (allowed_literal value) -> Error (Literal pc)
  | Contract_vm.JDEST target when target <> pc -> Error (Mark (pc, target))
  | Contract_vm.LDI _
  | Contract_vm.MOV _
  | Contract_vm.ADD _
  | Contract_vm.SUB _
  | Contract_vm.MUL _
  | Contract_vm.DIV _
  | Contract_vm.MOD _
  | Contract_vm.NEG _
  | Contract_vm.ABS _
  | Contract_vm.EQ _
  | Contract_vm.CONCAT _
  | Contract_vm.STRLEN _
  | Contract_vm.SUBSTR _
  | Contract_vm.JMP _
  | Contract_vm.JIF _
  | Contract_vm.JDEST _
  | Contract_vm.NOP
  | Contract_vm.STOP -> Ok ()
  | op -> Error (Opcode (pc, Bytecode.op_tag op))

let check code =
  let rec loop pc =
    if pc = Array.length code then Ok ()
    else
      match check_op pc code.(pc) with
      | Ok () -> loop (pc + 1)
      | Error error -> Error error
  in
  loop 0

let kind_text = function
  | Int -> "int"
  | Bool -> "bool"
  | Bytes -> "bytes"
  | Data -> "data"

let read pc regs reg =
  match regs.(reg) with
  | Some kind -> Ok kind
  | None -> Error (Empty_reg (pc, reg))

let expect pc regs reg expected =
  match read pc regs reg with
  | Ok actual when actual = expected -> Ok ()
  | Ok actual -> Error (Wrong_kind (pc, reg, expected, actual))
  | Error error -> Error error

let expect_same pc regs left right =
  match read pc regs left, read pc regs right with
  | Ok Data, Ok Data -> Error (Opaque_reg (pc, left))
  | Ok left_kind, Ok right_kind when left_kind = right_kind -> Ok ()
  | Ok left_kind, Ok right_kind ->
    Error (Wrong_kind (pc, right, left_kind, right_kind))
  | Error error, _
  | _, Error error -> Error error

let write regs reg kind =
  regs.(reg) <- Some kind;
  Ok regs

let int_binary pc regs dst left right =
  match expect pc regs left Int, expect pc regs right Int with
  | Ok (), Ok () -> write regs dst Int
  | Error error, _
  | _, Error error -> Error error

let step pc regs = function
  | Contract_vm.LDI (dst, value) ->
    begin
      match literal_kind value with
      | Some kind -> write regs dst kind
      | None -> Error (Literal pc)
    end
  | Contract_vm.MOV (dst, src) ->
    begin
      match read pc regs src with
      | Ok kind -> write regs dst kind
      | Error error -> Error error
    end
  | Contract_vm.ADD (dst, left, right)
  | Contract_vm.SUB (dst, left, right)
  | Contract_vm.MUL (dst, left, right)
  | Contract_vm.DIV (dst, left, right)
  | Contract_vm.MOD (dst, left, right) ->
    int_binary pc regs dst left right
  | Contract_vm.NEG (dst, src)
  | Contract_vm.ABS (dst, src) ->
    begin
      match expect pc regs src Int with
      | Ok () -> write regs dst Int
      | Error error -> Error error
    end
  | Contract_vm.EQ (dst, left, right) ->
    begin
      match expect_same pc regs left right with
      | Ok () -> write regs dst Bool
      | Error error -> Error error
    end
  | Contract_vm.CONCAT (dst, left, right) ->
    begin
      match expect pc regs left Bytes, expect pc regs right Bytes with
      | Ok (), Ok () -> write regs dst Bytes
      | Error error, _
      | _, Error error -> Error error
    end
  | Contract_vm.STRLEN (dst, src) ->
    begin
      match expect pc regs src Bytes with
      | Ok () -> write regs dst Int
      | Error error -> Error error
    end
  | Contract_vm.SUBSTR (dst, src, first, count) ->
    begin
      match
        expect pc regs src Bytes,
        expect pc regs first Int,
        expect pc regs count Int
      with
      | Ok (), Ok (), Ok () -> write regs dst Bytes
      | Error error, _, _
      | _, Error error, _
      | _, _, Error error -> Error error
    end
  | Contract_vm.JIF (guard, _) ->
    begin
      match expect pc regs guard Bool with
      | Ok () -> Ok regs
      | Error error -> Error error
    end
  | Contract_vm.STOP ->
    begin
      match read pc regs 0 with
      | Ok _ -> Ok regs
      | Error error -> Error error
    end
  | Contract_vm.JMP _
  | Contract_vm.JDEST _
  | Contract_vm.NOP -> Ok regs
  | _ -> Error (Opcode (pc, -1))

let merge left right =
  Array.init 64 (fun reg ->
    match left.(reg), right.(reg) with
    | Some left_kind, Some right_kind when left_kind = right_kind ->
      Some left_kind
    | _ -> None)

let flow code =
  let size = Array.length code in
  let states = Array.make size None in
  states.(0) <- Some (Array.make 64 None);
  let target pc at =
    if at <= pc then Error (Cycle (pc, at)) else Ok at
  in
  let next pc =
    if pc + 1 < size then Ok (pc + 1)
    else Error (Fallthrough pc)
  in
  let successors pc = function
    | Contract_vm.STOP -> Ok []
    | Contract_vm.JMP at ->
      begin
        match target pc at with
        | Ok at -> Ok [at]
        | Error error -> Error error
      end
    | Contract_vm.JIF (_, at) ->
      begin
        match target pc at, next pc with
        | Ok at, Ok rest -> Ok [at; rest]
        | Error error, _
        | _, Error error -> Error error
      end
    | _ ->
      begin
        match next pc with
        | Ok at -> Ok [at]
        | Error error -> Error error
      end
  in
  let add regs at =
    states.(at) <-
      Some
        (match states.(at) with
         | None -> Array.copy regs
         | Some prior -> merge prior regs)
  in
  let rec walk pc =
    if pc = size then Ok ()
    else
      match states.(pc) with
      | None -> Error (Dead_code pc)
      | Some regs ->
        begin
          match step pc (Array.copy regs) code.(pc) with
          | Error error -> Error error
          | Ok regs ->
            begin
              match successors pc code.(pc) with
              | Error error -> Error error
              | Ok targets ->
                List.iter (add regs) targets;
                walk (pc + 1)
            end
        end
  in
  walk 0

let decode raw =
  match Bytecode.decode raw with
  | Error reason -> Error (Decode reason)
  | Ok code ->
    begin
      match Contract_vm.Verifier.verify code with
      | Error error -> Error (Verify (verifier_text error))
      | Ok () ->
        begin
          match check code with
          | Error error -> Error error
          | Ok () ->
            begin
              match flow code with
              | Error error -> Error error
              | Ok () ->
                let digest = Digestif.SHA256.(digest_string raw |> to_hex) in
                Ok { code; digest }
            end
        end
    end

let run ?(limit=1_000_000) profile =
  let state =
    Contract_vm.create_state
      ~limit
      ~strict_values:true
      ~ctx:{ Contract_vm.default_ctx with int_work = Int_work.Active }
      ~byte_result:Contract_vm.Typed_bytes
      ~caller:""
      ~origin:""
      ~address:""
      ~value:Z.zero
      ~storage:(Hashtbl.create 0)
      ()
  in
  if Contract_vm.run state profile.code then
    Ok { value = state.regs.(0); effort = state.effort_used }
  else
    Error Refused

let error_text = function
  | Decode reason -> "OCTB decode refused reason = " ^ reason
  | Verify reason -> "OCTB verification refused reason = " ^ reason
  | Opcode (pc, tag) ->
    Printf.sprintf "opcode is outside AML profile pc = %d tag = %d" pc tag
  | Literal pc -> Printf.sprintf "literal is outside AML profile pc = %d" pc
  | Mark (pc, target) ->
    Printf.sprintf "jump mark differs from pc pc = %d target = %d" pc target
  | Empty_reg (pc, reg) ->
    Printf.sprintf "register is empty pc = %d reg = %d" pc reg
  | Wrong_kind (pc, reg, expected, actual) ->
    Printf.sprintf
      "register kind differs pc = %d reg = %d expected = %s actual = %s"
      pc reg (kind_text expected) (kind_text actual)
  | Opaque_reg (pc, reg) ->
    Printf.sprintf "opaque data is not executable pc = %d reg = %d" pc reg
  | Cycle (pc, target) ->
    Printf.sprintf "control edge is cyclic pc = %d target = %d" pc target
  | Fallthrough pc -> Printf.sprintf "control falls through pc = %d" pc
  | Dead_code pc -> Printf.sprintf "instruction is unreachable pc = %d" pc
  | Refused -> "AML execution refused"