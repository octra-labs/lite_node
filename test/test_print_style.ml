(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Parsetree

let word value =
  String.length value > 0 &&
  String.for_all (function 'a'..'z' | '0'..'9' | '_' -> true | _ -> false) value

let fields text =
  let rec check = function
    | [] -> true
    | key :: "=" :: value :: rest -> word key && word value && check rest
    | _ -> false
  in
  let parts = String.split_on_char ' ' text in
  parts <> [] && check parts

let format text =
  let size = String.length text in
  let out = Buffer.create size in
  let rec scan index =
    if index = size then fields (Buffer.contents out)
    else match text.[index] with
      | '\n' when index = size - 1 -> fields (Buffer.contents out)
      | '\n' when index + 3 = size && String.sub text (index + 1) 2 = "%!" ->
        fields (Buffer.contents out)
      | '\n' | '\r' | '\t' -> false
      | '%' -> spec (index + 1)
      | ch -> Buffer.add_char out ch; scan (index + 1)
  and spec index =
    let rec digits index =
      if index < size && text.[index] >= '0' && text.[index] <= '9'
      then digits (index + 1) else index
    in
    let index =
      if index < size && text.[index] = '.' then
        let finish = digits (index + 1) in
        if finish = index + 1 then size else finish
      else index
    in
    let index =
      if index < size && List.mem text.[index] ['L'; 'l'; 'n']
      then index + 1 else index
    in
    if index < size && String.contains "diuoxfegcbsS" text.[index] then begin
      Buffer.add_char out 'x';
      scan (index + 1)
    end else false
  in
  scan 0

let wire path text =
  match Filename.basename path, text with
  | "test_pvac_verify_worker.ml", ("malformed" | "{}") -> true
  | "test_pvac_verify_worker.ml",
    "{\"schema\":\"octra_pvac_verify\",\"request_hash\":\"0000000000000000000000000000000000000000000000000000000000000000\",\"accepted\":true,\"reason\":\"\"}" -> true
  | "test_store_lock.ml", "ready" -> true
  | _ -> false

let name value =
  String.length value <= 32 && String.for_all (fun char -> Char.code char < 128) value

let test_name value =
  String.starts_with ~prefix:"test_" value || String.starts_with ~prefix:"check_" value

let errors path tree =
  let base = Filename.remove_extension (Filename.basename path) in
  let rows = ref (if name base then [] else [0, "file_name"]) in
  let expr self value =
    (match value.pexp_desc with
     | Pexp_apply ({pexp_desc = Pexp_ident name; _}, (_, arg) :: _) ->
       let diagnostic = match name.txt with
         | Longident.Lident ("print_endline" | "prerr_endline") -> true
         | Longident.Ldot (Longident.Lident "Printf", ("printf" | "eprintf")) -> true
         | _ -> false
       in
       (match arg.pexp_desc with
        | Pexp_constant (Pconst_string (text, _, _)) when diagnostic ->
          if not (wire path text || format text) then
            rows := (arg.pexp_loc.loc_start.pos_lnum, "print_format") :: !rows
        | _ -> ())
     | _ -> ());
    Ast_iterator.default_iterator.expr self value
  in
  let pat self value =
    (match value.ppat_desc with
     | Ppat_var id when test_name id.txt && not (name id.txt) ->
       rows := (id.loc.loc_start.pos_lnum, "test_name") :: !rows
     | _ -> ());
    Ast_iterator.default_iterator.pat self value
  in
  let iter = {Ast_iterator.default_iterator with expr; pat} in
  iter.structure iter tree;
  List.rev !rows

let parse path channel =
  let input = Lexing.from_channel channel in
  Location.init input path;
  Parse.implementation input

let check_cases () =
  List.iter (fun text -> if not (format text) then failwith "valid print rejected")
    ["status = pass"; "case = %s result = %b\n%!";
     "epoch = %Ld elapsed_ms = %.3f\n"; "%s = 1\n%!";
     "reason = %S\n%!"; "hash = %s%s\n"];
  List.iter (fun text -> if format text then failwith "invalid print accepted")
    ["  PASS  %s : %b\n%!"; "=== passed ===\n"; "status=pass\n";
     "status = PASS\n"; "status = pass\ncase = test\n";
     "name = %-28s\n"; "value = %8d\n"; "value = %02x\n";
     "value = %.f\n"; "status = pass  case = test\n"];
  let ast text = Parse.implementation (Lexing.from_string text) in
  let good = ast "let () = Printf.printf \"status = pass\\n%!\"" in
  let bad = ast "let () = Printf.printf \"  PASS  result\\n%!\"" in
  let data = ast "let x = Printf.sprintf \"OG16V1\"" in
  if errors "case.ml" good <> [] || errors "case.ml" bad = [] ||
     errors "case.ml" data <> [] then failwith "print selection differs";
  let ipc = ast "let () = print_endline \"ready\"" in
  if errors "test_store_lock.ml" ipc <> [] || errors "case.ml" ipc = [] then
    failwith "ipc selection differs";
  let short = "test_" ^ String.make 27 'a' in
  let long = short ^ "a" in
  let binding id = ast ("let " ^ id ^ " () = ()") in
  if errors (short ^ ".ml") (binding short) <> [] ||
     errors (long ^ ".ml") good <> [0, "file_name"] ||
     errors "case.ml" (binding long) <> [1, "test_name"] then
    failwith "test name limit differs"

let () =
  check_cases ();
  let failed = ref false in
  Array.iteri (fun index path -> if index > 0 then
    let channel = open_in_bin path in
    let tree = Fun.protect ~finally:(fun () -> close_in channel)
      (fun () -> parse path channel) in
    List.iter (fun (line, reason) ->
      failed := true;
      Printf.eprintf "status = fail check = print_style path = %s line = %d reason = %s\n%!" path line reason)
      (errors path tree)) Sys.argv;
  if !failed then exit 1;
  Printf.printf "status = pass test = print_style files = %d\n%!" (Array.length Sys.argv - 1)