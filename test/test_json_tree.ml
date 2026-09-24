(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_core

let require ok reason = if not ok then failwith reason

let outcome read raw =
  try Ok (Yojson.Safe.to_string (read raw)) with
  | Yojson.Json_error reason -> Error reason

let check raw =
  let prior = outcome Yojson.Safe.from_string raw in
  let next = outcome Json_tree.read raw in
  require (prior = next) (Printf.sprintf "JSON result differs raw = %S prior = %s next = %s"
    raw (match prior with Ok s -> s | Error s -> s)
    (match next with Ok s -> s | Error s -> s));
  match next with
  | Error _ -> ()
  | Ok encoded ->
    require (Json_tree.write (Json_tree.read raw) = encoded) "JSON encoding differs"

let rec ordered = function
  | `Assoc fields -> `Assoc (List.sort (fun (a, _) (b, _) -> String.compare a b)
      (List.map (fun (key, json) -> key, ordered json) fields))
  | `List values -> `List (List.map ordered values)
  | json -> json

let check_grammar () =
  List.iter (fun raw ->
    require (try ignore (Yojson.Safe.from_string raw); false
      with Yojson.Json_error _ -> true) "JSON dependency grammar differs from lock";
    require (try ignore (Json_tree.read raw); false
      with Yojson.Json_error _ -> true) "JSON extension accepted")
    ["(0)"; "<None>"; "<Some:0>"];
  List.iter check [
    ""; " \n/* a */ "; "null"; "true"; "false"; "NaN"; "Infinity"; "-Infinity";
    "0"; "-0"; "1.5"; "-1e-3"; "999999999999999999999999"; "[1,2]";
    "{a:1,b:[null,true],a:3}"; "(1,2)"; "<Some:{z:1,a:2}>"; "<None>"; "()";
    "{\"a\\\"[}]\":\"\\\\[]{}()<>\",b:\"\\u03c9\\ud83d\\ude00\"}";
    "// before\n[/* item */ 1,// next\n2]/* after */";
    "{a:1} rubbish after data"; "1\n2"; "1 " ^ String.make 80 'x';
    "1x"; "1 x"; "{}\n{}"; "[1,]"; "(1,)"; "{a:1,}"; "<x,1>";
    "<x:1,2>"; "[}"; "{]"; "\"\\q\""; "\"\\uD800\""; "01"; "+1";
    "{a:\"x\n\"}"; "{a:/* unfinished"; "[true false]"; "<\"x\":/* a */[1]>";
  ];
  let tokens = [|"["; "]"; "{"; "}"; ":"; ","; "0"; " "; "\n"; "\"a\""; "<"; ">"; "("; ")"|] in
  let rec enumerate n raw =
    check raw;
    if n > 0 then Array.iter (fun token -> enumerate (n - 1) (raw ^ token)) tokens
  in
  enumerate 4 "";
  let rng = Random.State.make [|53; 127|] in
  let atoms = [|`Null; `Bool true; `Int (-13); `Float 0.125;
    `Intlit "123456789123456789123456789"; `String "\000\\\"\n"|] in
  let rec tree depth =
    if depth = 0 then atoms.(Random.State.int rng (Array.length atoms))
    else match Random.State.int rng 3 with
    | 0 -> `Assoc (List.init (Random.State.int rng 5)
        (fun _ -> string_of_int (Random.State.int rng 3), tree (depth - 1)))
    | 1 -> `List (List.init (Random.State.int rng 5) (fun _ -> tree (depth - 1)))
    | _ -> tree 0
  in
  for _ = 1 to 5_000 do
    let json = tree 5 in
    let raw = Yojson.Safe.to_string json in
    check raw;
    check (raw ^ "?");
    let pos = Random.State.int rng (String.length raw) in
    check (String.sub raw 0 pos ^ String.sub raw (pos + 1) (String.length raw - pos - 1));
    require (Json_tree.write ~sort:true json = Yojson.Safe.to_string (ordered json))
      "attestation ordering differs"
  done

let nested left right count =
  String.concat "" (List.init count (fun _ -> left)) ^ "0"
    ^ String.concat "" (List.init count (fun _ -> right))

let check_depth prior =
  let read raw = if prior then Yojson.Safe.from_string raw else Json_tree.read raw in
  List.iter (fun (left, right, count) ->
    let raw = nested left right count in
    let json = read raw in
    require (Json_tree.write json = raw) "deep JSON changed";
    require (Json_tree.write ~sort:true json = raw) "deep sorted JSON changed")
    ["[", "]", 250_000; "{\"a\":", "}", 80_000];
  let raw = "[" ^ String.concat "," (List.init 100_000 string_of_int) ^ "]" in
  require (Json_tree.write (read raw) = raw) "wide JSON changed";
  let raw = "{" ^ String.concat "," (List.init 100_000 (fun _ -> "\"x\":0")) ^ "}" in
  require (Json_tree.write ~sort:true (read raw) = raw) "wide object changed";
  let raw = "[" ^ String.concat "," (List.init 100_000 (fun _ -> "{\"a\":0}")) ^ "]" in
  require (Json_tree.write (read raw) = raw) "object array changed";
  let raw = nested "[" "]" 250_000 ^ "?" in
  require (try ignore (read raw); false with Yojson.Json_error _ -> true)
    "deep trailing data accepted"

let () =
  match Array.to_list Sys.argv with
  | [_; "--prior"] -> check_depth true
  | [_; "--stack"] -> check_depth false
  | _ ->
    check_grammar ();
    check_depth false;
    let pid = Unix.create_process "/bin/sh"
      [|"sh"; "-c";
        "ulimit -c 0; ulimit -s 512; export DYLD_LIBRARY_PATH=\"$1\"; if [ -n \"$2\" ]; then export DYLD_FALLBACK_LIBRARY_PATH=\"$2\"; fi; exec \"$0\" --stack";
        Sys.executable_name;
        Option.value ~default:(Sys.getcwd ()) (Sys.getenv_opt "DYLD_LIBRARY_PATH");
        Option.value ~default:"" (Sys.getenv_opt "DYLD_FALLBACK_LIBRARY_PATH")|]
      Unix.stdin Unix.stdout Unix.stderr in
    let rec wait () = try snd (Unix.waitpid [] pid) with
      | Unix.Unix_error (Unix.EINTR, _, _) -> wait () in
    require (wait () = Unix.WEXITED 0) "JSON stack process failed";
    Printf.printf "status = pass test = json_tree\n"