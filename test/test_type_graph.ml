(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_vm
open Oct_lang

let check label value = if not value then failwith label

let rec doubled count leaf =
  if count = 0 then leaf else
  let child = doubled (count - 1) leaf in
  TTuple [child; child]

let rec prior expected actual =
  expected = actual || Oct_types.numeric expected && Oct_types.numeric actual
  || match expected, actual with
  | TBytes, TString | TString, TBytes -> true
  | TStruct left, TEnum right | TEnum left, TStruct right -> String.equal left right
  | TOption _, TOption TVoid -> true
  | TOption left, TOption right | TList left, TList right -> prior left right
  | TMap (lk, lv), TMap (rk, rv) -> prior lk rk && prior lv rv
  | TTuple left, TTuple right ->
    List.length left = List.length right && List.for_all2 prior left right
  | _ -> false

let test_compat () =
  check "allocated enum compatibility changed"
    (Oct_types.compatible (TEnum (String.make 1 'A')) (TEnum (String.make 1 'A')));
  let leaves = [TInt; TBool; TString; TAddress; TBytes; TBytes32; TU64; TU128;
    TU256; TCipher; TPubKey; TVoid; TStruct "A"; TStruct "B"; TEnum "A"] in
  let values = leaves @ List.concat_map (fun value ->
    [TOption value; TList value; TMap (value, TBool); TTuple [value; value]]) leaves in
  List.iter (fun left -> List.iter (fun right ->
    check "type compatibility changed" (Oct_types.compatible left right = prior left right)) values) values;
  let random = Random.State.make [|169; 1737333|] in
  let rec nested depth =
    if depth = 0 then List.nth leaves (Random.State.int random (List.length leaves))
    else match Random.State.int random 4 with
      | 0 -> TOption (nested (depth - 1))
      | 1 -> TList (nested (depth - 1))
      | 2 -> TMap (nested (depth - 1), nested (depth - 1))
      | _ -> TTuple [nested (depth - 1); nested (depth - 1)] in
  for _ = 1 to 200 do
    let left = nested 4 and right = nested 4 in
    List.iter (fun (a, b) ->
      check "nested type compatibility changed" (Oct_types.compatible a b = prior a b))
      [left, right; right, left; left, left]
  done;
  let left = doubled 60 TInt in
  let right = doubled 60 TInt in
  check "independent shared types differ" (Oct_types.compatible left right);
  check "shared numeric types differ" (Oct_types.compatible left (doubled 60 TU64));
  check "shared unequal types accepted" (not (Oct_types.compatible left (doubled 60 TBool)));
  let wrong = TTuple [right; TBool] in
  check "second edge omitted" (not (Oct_types.compatible (TTuple [left; left]) wrong))

let test_wide () =
  let fields n final = List.init n (fun i -> if i = n - 1 then final else TInt) in
  let left = TTuple (fields 1000 TInt) in
  check "wide tuple differs" (Oct_types.compatible left (TTuple (fields 1000 TU64)));
  check "wide tuple last field omitted" (not (Oct_types.compatible left (TTuple (fields 1000 TBool))));
  check "wide tuple arity omitted" (not (Oct_types.compatible left (TTuple (fields 999 TInt))))

let test_maps () =
  let host = doubled 60 TInt in
  let core = Result.get_ok (Oct_form.typ 1 1 host) in
  check "core map lost sharing" (match core with C_syn.TPair (a, b) -> a == b | _ -> false);
  let copy = Result.get_ok (Oct_form.typ 1 1 (doubled 60 TInt)) in
  check "core comparison differs" (Oct_form.same core copy "different" = Ok ());
  check "core mismatch accepted"
    (Oct_form.same core (C_syn.TPair (copy, C_syn.TBool)) "different" = Error "different");
  let back = Option.get (Oct_form.host_typ core) in
  check "host map lost sharing" (match back with TTuple [a; b] -> a == b | _ -> false);
  check "round trip differs" (Oct_types.compatible host back);
  check "unsupported host type changed" (Oct_form.typ 2 3 (TList TInt) =
    Error "line 2 column 3: direct form type is unsupported = list");
  check "unsupported core type changed" (Oct_form.host_typ C_syn.TUnit = None)

let test_compile () =
  let locals = List.init 45 (fun i -> Printf.sprintf "let a%d = %s" i
    (if i = 0 then "1" else Printf.sprintf "(a%d, a%d)" (i - 1) (i - 1)))
    |> String.concat " " in
  let body = "program Types { public fn get(): int { " ^ locals ^ " return 1 } }" in
  List.iter (fun compiler ->
    let result = Program_package.compile_at ~loops:true ~compiler ~point_ops:true
      ~main:"main.aml" ~sources:[Program_package.{path = "main.aml"; body}] in
    check ("shared types did not reach code generation: " ^ (match result with
      | Ok _ -> "ok" | Error error -> Program_package.error_message error)) (match result with
      | Ok _ -> true
      | Error error ->
        Program_package.error_message error = "Program compile failed: source = main.aml line 1 column 1: register capacity exceeded maximum = 64"))
    [Program_package.Source; Program_package.Preview]

let test_index () =
  let node = doubled 800 TInt in
  Gc.full_major ();
  Gc.compact ();
  check "indexed scalar differs"
    (Oct_types.compatible (TTuple [node; TBool]) (TTuple [doubled 800 TInt; TBool]));
  List.iter (fun depth ->
    let value = doubled depth TInt in
    let calls = ref 0 in
    let mapped = Oct_form.HostTypes.mapper (fun walk value ->
      incr calls; Oct_form.type_step 1 1 walk value) value in
    check "indexed conversion failed" (Result.is_ok mapped);
    check "conversion repeated a shared node" (!calls = depth + 1);
    Printf.printf "event = type_scale depth = %d visited = %d\n" depth !calls)
    [200; 400; 800]

let test_saved_types () =
  let program = Oct_parse.parse "program Types { public fn get(): int { return 1 } }" in
  let measure count ternary scope =
    Gc.compact ();
    let before = Gc.allocated_bytes () in
    let rec loop remaining name previous values =
      if remaining = 0 then previous else
      let term = if ternary then ETernary (EBool true,
          ETuple [EVar name; ETuple [EInt Z.one; EInt Z.one]],
          ETuple [EVar name; ETuple [EInt Z.one; EInt Z.one]])
        else ETuple [EVar name; EBinop (Add, EInt Z.one, EInt Z.one)] in
      let next = Option.get (Oct_form.infer program 1 1 values term) in
      check "noncopy local expanded saved type"
        (match next.host with TTuple [left; _] -> left == previous.Oct_form.host | _ -> false);
      if remaining mod 17 = 0 then Gc.minor ();
      let name = "a" ^ string_of_int remaining in
      let values = (name, next) :: if scope then values else [] in
      loop (remaining - 1) name next values in
    let first = Oct_form.local TInt in
    ignore (loop count "a0" first ["a0", first]);
    Gc.allocated_bytes () -. before in
  List.iter (fun (ternary, scope) ->
    let small = measure 100 ternary scope in
    let large = measure 400 ternary scope in
    Printf.printf "event = saved_type_scale ternary = %b scope = %b small = %.0f large = %.0f\n"
      ternary scope small large;
    check "saved type conversion grew faster than linear" (large <= 5. *. small))
    [false, false; true, false; false, true; true, true];
  let host = doubled 800 TInt in
  let core = Result.get_ok (Oct_form.typ 1 1 host) in
  let count = ref 0 in
  let saved = [|core, Some host|] in
  let check_saved () =
    let found = Type_graph.Core.mapper ~saved (fun _ _ -> incr count; None) core in
    check "saved root was expanded" (!count = 0 && match found with Some value -> value == host | _ -> false) in
  check_saved (); Gc.minor (); Gc.compact (); check_saved ()

let test_locals () =
  let leaf = Oct_form.local TInt in
  let values = ["a", leaf] in
  let program = Oct_parse.parse "program Types { public fn get(): int { return 1 } }" in
  let values = Oct_form.put "a" (Oct_form.local TBool) values in
  check "local replacement retained duplicate name" (List.length values = 1);
  check "local replacement changed visible type"
    (match Oct_form.infer program 1 1 values (EVar "a") with
      | Some value -> value.host = TBool | None -> false);
  let values = Oct_form.put "a" leaf values in
  let first = Option.get (Oct_form.infer program 1 1 values (ETuple [EVar "a"; EInt Z.one])) in
  let second = Option.get (Oct_form.infer program 1 1 ["b", first] (ETuple [EVar "b"; EInt Z.one])) in
  check "local host type was copied" (match second.host with TTuple [a; _] -> a == first.host | _ -> false);
  check "local core type was copied" (match second.core, first.core with
    | Some (C_syn.TPair (a, _)), Some b -> a == b | _ -> false);
  check "void inference changed" (Oct_form.infer program 1 1 ["u", Oct_form.local TVoid] (EVar "u") = None)

let test_long_names () =
  let program = Oct_parse.parse "program Types { public fn get(): int { return 1 } }" in
  let term = ECall ("abs", [EInt Z.one]) in
  let infer values = Option.map (fun value -> value.Oct_form.host)
    (Oct_form.infer program 1 1 values term) in
  List.iter (fun size ->
    let name = String.make size 'a' in
    let values = [name, Oct_form.local TInt] in
    let expected = match Oct_form.strict program 1 1 values term with
      | Ok (_, value) -> Oct_form.host_typ value.typ
      | Error _ -> Oct_form.host_infer program values term in
    check "unused name changed historical inference" (infer values = expected);
    let values = Oct_form.put name (Oct_form.local TString) values in
    check "nonconvertible name changed inference"
      (infer values = infer [])) [64; 65];
  List.iter (fun parameter -> List.iter (fun size ->
    let name = String.make size 'l' in
    let source = "program Names { public fn get("
      ^ (if parameter then name ^ ": int" else "") ^ "): bool { "
      ^ (if parameter then "" else "let " ^ name ^ " = 1 ")
      ^ "let x = abs(5) let " ^ name ^ " = \"s\" return equal[int](x, 1) } }" in
    let result = Aml_source.compile ~metered:true ~loops:true ~syntax:Oct_gen.Source source in
    check "whole source changed long name inference"
      (if size = 64 then Result.is_ok result else match result with
       | Error reason -> String.ends_with ~suffix:"direct form variable is absent = x" reason
       | Ok _ -> false)) [64; 65]) [false; true]

let test_wrapped_types () =
  let measure count =
    Gc.compact ();
    let before = Gc.allocated_bytes () in
    let rec loop n host core =
      if n = 0 then () else
      let left = TTuple [host; TTuple [TInt; TInt]] in
      let right = TTuple [host; TTuple [TU64; TInt]] in
      check "wrapped host types differ" (Oct_types.compatible left right);
      check "wrapped incompatible leaf accepted"
        (not (Oct_types.compatible left (TTuple [host; TTuple [TBool; TInt]])));
      let a = C_syn.TPair (core, C_syn.TPair (C_syn.TInt, C_syn.TInt)) in
      let b = C_syn.TPair (core, C_syn.TPair (C_syn.TInt, C_syn.TInt)) in
      check "wrapped core types differ" (Oct_form.same a b "different" = Ok ());
      loop (n - 1) left a in
    loop count TInt C_syn.TInt;
    Gc.allocated_bytes () -. before in
  let small = measure 200 and large = measure 800 in
  check "wrapped comparison expanded shared subtrees" (large < 5. *. small);
  Printf.printf "event = wrapped_type_work small = %.0f large = %.0f\n" small large

let test_first_field () =
  let rec graph count left right =
    if count = 0 then left else graph (count - 1) (TMap (left, right)) left in
  let measure count =
    let left = TMap (TBool, graph count TInt TBool) in
    let right = TMap (TInt, graph count TBool TInt) in
    Gc.compact ();
    let before = Gc.allocated_bytes () in
    check "first field mismatch accepted" (not (Oct_types.compatible left right));
    Gc.allocated_bytes () -. before in
  let small = measure 50 and large = measure 400 in
  Printf.printf "event = first_field_work small = %.0f large = %.0f\n" small large;
  check "first field refusal traversed unused graph" (large <= small +. 10_000.);
  let left = graph 400 TInt TBool and right = graph 400 TInt TBool in
  let visits = ref 0 in
  let result = Type_graph.Host.compare (fun cursor left right ->
    incr visits;
    if !visits mod 13 = 0 then Gc.compact ();
    match left, right with
    | TMap _, TMap _ -> Some [cursor.at [0]; cursor.at [1]]
    | _ -> if left = right then Some [] else None) left right in
  check "lazy graph lost comparison during collection" result;
  check "lazy graph revisited product" (!visits < 1_000)

let test_scale () =
  List.iter (fun count ->
    let locals = List.init count (fun i -> Printf.sprintf "let a%d = %s" i
      (if i = 0 then "1" else Printf.sprintf "(a%d, 1)" (i - 1))) |> String.concat " " in
    let body = "program Types { public fn get(): int { " ^ locals ^ " return 1 } }" in
    let before = Sys.time () in
    let result = Program_package.compile_at ~loops:true ~compiler:Program_package.Preview ~point_ops:true
      ~main:"main.aml" ~sources:[Program_package.{path = "main.aml"; body}] in
    check "linear locals changed refusal" (match result with
      | Error error -> Program_package.error_message error =
        "Program compile failed: source = main.aml line 1 column 1: register capacity exceeded maximum = 64"
      | Ok _ -> false);
    Printf.printf "event = local_scale count = %d cpu_s = %.6f\n" count (Sys.time () -. before))
    [200; 500; 1000]

let test_handoff () =
  let rec wrap count value = if count = 0 then value else TOption (wrap (count - 1) value) in
  List.iter (fun depth ->
    let left = wrap depth (TMap (TBool, doubled 120 TInt)) in
    let right = wrap depth (TMap (TInt, doubled 121 TInt)) in
    let calls = ref 0 in
    let equal = Type_graph.Host.compare (fun node a b ->
      incr calls; Gc.minor ();
      if !calls = 64 then Gc.compact ();
      match a, b with
      | TOption _, TOption _ -> Some [node.at [0]]
      | TMap _, TMap _ -> Some [node.at [0]; node.at [1]]
      | _ -> if a = b then Some [] else None) left right in
    check "handoff missed first unequal leaf" (not equal);
    check "handoff visited unused sibling" (!calls <= 2 * depth + 4);
    check "handoff rejected equal chain" (Oct_types.compatible (wrap depth TInt) (wrap depth TInt)))
    [62; 63; 64; 65];
  check "asymmetric sharing differs"
    (Oct_types.compatible (doubled 8 TInt) (TTuple [doubled 7 TInt; doubled 7 TInt]));
  let rec sum depth leaf = if depth = 0 then leaf else C_syn.TSum (sum (depth - 1) leaf, C_syn.TInt) in
  let left = sum 100 C_syn.TBool in
  check "core sum tag changed" (Obj.tag (Obj.repr left) = 6);
  check "core sum comparison lost child" (Oct_form.same left (sum 100 C_syn.TInt) "different" = Error "different");
  check "core sum comparison differs" (Oct_form.same left (sum 100 C_syn.TBool) "different" = Ok ())

let run () =
  let start = Gc.allocated_bytes () in
  let alarm = Gc.create_alarm (fun () ->
    if Gc.allocated_bytes () -. start > 64_000_000. then
      failwith "shared type allocation exceeded work budget") in
  Fun.protect ~finally:(fun () -> Gc.delete_alarm alarm) (fun () ->
    test_compat ();
    test_maps ();
    test_compile ());
  test_index ();
  test_wide ();
  test_saved_types ();
  test_locals ();
  test_long_names ();
  test_wrapped_types ();
  test_first_field ();
  test_handoff ();
  test_scale ()

let () =
  let reader, writer = Unix.pipe () in
  match Unix.fork () with
  | 0 ->
    Unix.close reader;
    (try run (); Unix.close writer; exit 0
     with error -> prerr_endline (Printexc.to_string error); exit 1)
  | pid ->
    Unix.close writer;
    let ready, _, _ = Unix.select [reader] [] [] 120. in
    Unix.close reader;
    if ready = [] then Unix.kill pid Sys.sigkill;
    let _, status = Unix.waitpid [] pid in
    check "shared type work exceeded deadline" (ready <> []);
    check "type graph child failed" (status = Unix.WEXITED 0);
    print_endline "status = pass suite = type_graph"