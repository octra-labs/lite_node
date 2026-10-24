(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_vm

let sources =
  ["program Sample { public view fn read(): int { return 17 } }";
   "program Sample { public fn read(): int { let a = (1, 2) let b = (a, 1 + 1) return 3 } }";
   "program Sample { public fn read(): int { let a = true ? (1, 2) : (1, 2) return 3 } }";
   "program Sample { public fn read(): bool { return 1 } }";
   "program Sample { public fn read(): int { let a = missing return a } }"]
  @ List.init 12 (fun count ->
    let constants = List.init (count + 1) (fun index ->
      Printf.sprintf "const a%d = %s" index
        (if index = 0 then "1" else Printf.sprintf "a%d + a%d" (index - 1) (index - 1))) in
    "program Sample { " ^ String.concat " " constants ^
    Printf.sprintf " public view fn read(): int { return a%d } }" count)
  @ List.init 10 (fun count ->
    let locals = List.init (count + 1) (fun index ->
      Printf.sprintf "let a%d = %s" index
        (if index = 0 then "1" else Printf.sprintf "(a%d, 1 + 1)" (index - 1))) in
    "program Sample { public fn read(): int { " ^ String.concat " " locals ^ " return 3 } }")

let () =
  let rows = List.concat_map (fun body ->
    List.map (fun (compiler, loops) ->
      let source = Program_record.{main = "main.aml";
        files = [Program_package.{path = "main.aml"; body}]; compiler; loops} in
      let result = match Program_record.compile source with
        | Error reason -> `Assoc ["error", `String reason]
        | Ok (code, compiled) ->
          let record = Program_record.make ~source:body ~code compiled in
          `Assoc ["code", `String (Base64.encode_exn code);
            "record", `String (Program_record.raw record);
            "abi", `String compiled.abi_json;
            "report", `String compiled.verification_json;
            "certificate", `String compiled.certificate_json;
            "instructions", `Int compiled.instructions] in
      `Assoc ["source", `String body; "compiler", `String compiler;
        "loops", `Bool loops; "result", result]) Program_record.profiles) sources in
  print_endline (Yojson.Safe.to_string (`List rows))