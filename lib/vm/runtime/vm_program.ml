(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type scope_error =
  | Scope_start of int
  | Scope_overflow of int
  | Scope_duplicate of int
  | Scope_missing of int

let fix code =
  let labels = Hashtbl.create 10 in
  Array.iteri
    (fun pc op ->
      match op with
      | Contract_vm.JDEST label -> Hashtbl.add labels label pc
      | _ -> ())
    code;
  Array.map
    (function
      | Contract_vm.JIF (reg, label) as op ->
        begin
          match Hashtbl.find_opt labels label with
          | Some pc -> Contract_vm.JIF (reg, pc)
          | None -> op
        end
      | Contract_vm.JMP label as op ->
        begin
          match Hashtbl.find_opt labels label with
          | Some pc -> Contract_vm.JMP pc
          | None -> op
        end
      | Contract_vm.CALL_INT (reg, label) as op ->
        begin
          match Hashtbl.find_opt labels label with
          | Some pc -> Contract_vm.CALL_INT (reg, pc)
          | None -> op
        end
      | op -> op)
    code

let scope ~first code =
  if first < 0 then Error (Scope_start first)
  else
    let rec collect pc seen out =
      if pc = Array.length code then Ok (List.rev out)
      else
        match code.(pc) with
        | Contract_vm.JDEST label when List.mem label seen ->
          Error (Scope_duplicate label)
        | Contract_vm.JDEST label ->
          collect (pc + 1) (label :: seen) (label :: out)
        | _ -> collect (pc + 1) seen out
    in
    match collect 0 [] [] with
    | Error error -> Error error
    | Ok found ->
      let count = List.length found in
      if first > max_int - count then Error (Scope_overflow first)
      else
        let labels =
          List.mapi (fun index label -> label, first + index) found
        in
        let mapped label =
          match List.assoc_opt label labels with
          | Some value -> Ok value
          | None -> Error (Scope_missing label)
        in
        let rec rewrite pc out =
          if pc = Array.length code then
            Ok (Array.of_list (List.rev out), first + count)
          else
            let next value = rewrite (pc + 1) (value :: out) in
            match code.(pc) with
            | Contract_vm.JDEST label ->
              Result.bind (mapped label)
                (fun value -> next (Contract_vm.JDEST value))
            | Contract_vm.JMP label ->
              Result.bind (mapped label)
                (fun value -> next (Contract_vm.JMP value))
            | Contract_vm.JIF (reg, label) ->
              Result.bind (mapped label)
                (fun value -> next (Contract_vm.JIF (reg, value)))
            | Contract_vm.CALL_INT (reg, label) ->
              Result.bind (mapped label)
                (fun value -> next (Contract_vm.CALL_INT (reg, value)))
            | value -> next value
        in
        rewrite 0 []

let scope_text = function
  | Scope_start value ->
    Printf.sprintf "label scope start is invalid value = %d" value
  | Scope_overflow value ->
    Printf.sprintf "label scope exceeds integer range start = %d" value
  | Scope_duplicate value ->
    Printf.sprintf "label is repeated value = %d" value
  | Scope_missing value ->
    Printf.sprintf "label target is absent value = %d" value

let entry code =
  let rec find pc =
    if pc >= Array.length code then None
    else
      match code.(pc) with
      | Contract_vm.JDEST 100 -> Some pc
      | _ -> find (pc + 1)
  in
  find 0