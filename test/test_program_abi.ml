(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_vm
open Yojson.Safe.Util

let require condition message =
  if not condition then failwith message

let text field json = member field json |> to_string
let list field json = member field json |> to_list

let method_named name json =
  list "functions" json
  |> List.find (fun item -> String.equal (text "name" item) name)

let source = {|
Program InterfaceData {
  state { title: string }
  event Sent(sender: indexed address, amount: u128)
  constructor(title: string, amount: u128) { self.title = title }
  private fn hidden(): int { return 1 }
  internal fn internal_value(): int { return 2 }
  pure fn widths(a: u64, b: u128, c: u256, d: bytes32): u256 { return c }
  view fn read(): string { return self.title }
  fn clear() { self.title = "" }
}
|}

let check_types () =
  let open Oct_lang in
  let cases = [
    TInt, "int"; TBool, "bool"; TString, "string"; TAddress, "address";
    TBytes, "bytes"; TBytes32, "bytes32"; TU64, "u64"; TU128, "u128";
    TU256, "u256"; TCipher, "cipher"; TPubKey, "pubkey"; TVoid, "void";
    TMap (TAddress, TList TU128), "map[address]list[u128]";
    TOption (TTuple [TString; TU64]), "option[(string,u64)]";
    TStruct "Position", "Position"; TEnum "Choice", "Choice";
  ] in
  List.iter
    (fun (kind, expected) ->
      require (String.equal expected (Source_abi.type_name kind))
        ("type differs = " ^ expected))
    cases

let check_source () =
  let ast = Oct_parse.parse source in
  let json = Source_abi.to_json ast in
  let decoded = Source_abi.encode ast |> Yojson.Safe.from_string in
  require (decoded = json) "ABI JSON round trip differs";
  let ctor = member "constructor" json in
  require (list "inputs" ctor = [`String "string"; `String "u128"])
    "constructor types differ";
  require (list "input_names" ctor = [`String "title"; `String "amount"])
    "constructor names differ";
  require (List.length (list "functions" json) = 3) "private method exposed";
  let widths = method_named "widths" json in
  require
    (list "inputs" widths
     = List.map (fun value -> `String value) ["u64"; "u128"; "u256"; "bytes32"])
    "integer widths lost";
  require (text "output" widths = "u256") "result type lost";
  require (member "view" widths = `Bool true) "pure method is not read-only";
  require (member "view" (method_named "read" json) = `Bool true)
    "view method is not read-only";
  require (text "output" (method_named "clear" json) = "void") "void changed";
  let fields = List.hd (list "events" json) |> list "fields" in
  require (List.length fields = 2) "event fields differ";
  require (member "indexed" (List.hd fields) = `Bool true) "event index lost";
  require (text "type" (List.nth fields 1) = "u128") "event width lost";
  let absent = Source_abi.to_json { ast with Oct_lang.ctor = None } in
  require (member "constructor" absent = `Null) "absent constructor changed"

let check_escaping () =
  let ast = Oct_parse.parse source in
  let fn = List.hd ast.Oct_lang.funcs in
  let name = "quoted\"\\\n" ^ String.make 1 (Char.chr 1) in
  let fn = { fn with Oct_lang.fn_name = name; fn_vis = Oct_lang.Public } in
  let json =
    Source_abi.encode { ast with Oct_lang.funcs = [fn] } |> Yojson.Safe.from_string
  in
  require (text "name" (List.hd (list "functions" json)) = name)
    "JSON name escaping differs"

let () =
  check_types ();
  check_source ();
  check_escaping ();
  Printf.printf "status = pass test = program_abi\n%!"