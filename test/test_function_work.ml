(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_vm

let check label ok = if not ok then failwith label

let measure label setup =
  let emit, generate, code = setup () in
  for _ = 1 to 20_000 do emit Contract_vm.NOP done;
  Gc.full_major ();
  let before = Gc.allocated_bytes () in
  for label = 1 to 400 do generate label done;
  let allocated = Gc.allocated_bytes () -. before in
  check (label ^ " changed first instruction") (List.hd (List.rev (code ())) = Contract_vm.NOP);
  check (label ^ " omitted terminal instruction") (List.hd (code ()) = Contract_vm.STOP);
  Printf.printf "event = function_work compiler = %s allocated = %.0f\n%!" label allocated;
  check (label ^ " repeatedly scanned emitted code") (allocated < 8_000_000.)

let test_emitters () =
  let parsed = Oct_parse.parse "contract Work { public fn read(): int { return 1 } }" in
  let fn = List.hd parsed.funcs in
  measure "legacy" (fun () ->
    let env = Oct_emit.make_env ~metered:true false false Oct_lang.ContractDecl [] [] [] [] [] [] [fn] in
    Oct_emit.emit env, Oct_emit.gen_function env fn, (fun () -> env.code));
  measure "source" (fun () ->
    let env = Oct_gen.make_env ~metered:true ~loops:false Oct_gen.Source Oct_lang.ContractDecl
      [] [] [] [] [] [] [fn] [] [] in
    Oct_gen.emit env, (fun label -> Hashtbl.replace env.func_labels fn.fn_name label;
      Oct_gen.gen_function env fn label), (fun () -> env.code))

let test_duplicates () =
  let modifiers = [""; "public "; "private "; "internal "; "view "; "pure "; "payable "; "nonreentrant "] in
  List.iter (fun left -> List.iter (fun right ->
    let source = "contract Names { " ^ left ^ "fn f(): int { return 1 } "
      ^ right ^ "fn f(): int { return 2 } }" in
    let rejected = try ignore (Oct_parse.parse source); false with
      | Oct_parse.ParseError (reason, _, _) -> reason = "duplicate function name = f" in
    check "duplicate function modifier escaped check" rejected) modifiers) modifiers

let test_symbols () =
  let open Oct_lang in
  let parsed = Oct_parse.parse "contract Names { public fn read(): int { return 1 } }" in
  let fn = List.hd parsed.funcs in
  let first = {fn with fn_ret = TInt} and second = {fn with fn_ret = TBool} in
  let const n = {c_name = "c"; c_typ = TInt; c_value = EInt (Z.of_int n)} in
  let state typ = {sf_name = "s"; sf_typ = typ} in
  let struct_value typ = {sd_name = "S"; sd_fields = ["x", typ]} in
  let enum names = {en_name = "E"; en_variants = names} in
  let event typ = {ev_name = "e"; ev_fields = ["x", typ, false]} in
  let error code = {err_name = "r"; err_code = code; err_msg = "error"} in
  let structs = [struct_value TInt; struct_value TBool] in
  let enums = [enum ["a"]; enum ["b"]] in
  let consts = [const 1; const 2] and states = [state TInt; state TBool] in
  let events = [event TInt; event TBool] and errors = [error 1; error 2] in
  let funcs = [first; second] in
  let check_index symbols =
    check "function duplicate order changed" (Hashtbl.find symbols.Oct_symbols.funcs "read" == first);
    List.iter (fun ok -> check "declaration duplicate order changed" ok)
      [Hashtbl.find symbols.structs "S" == List.hd structs;
       Hashtbl.find symbols.enums "E" == List.hd enums;
       Hashtbl.find symbols.consts "c" == List.hd consts;
       Hashtbl.find symbols.state "s" == List.hd states;
       Hashtbl.find symbols.events "e" == List.hd events;
       Hashtbl.find symbols.errors "r" == List.hd errors] in
  let legacy = Oct_emit.make_env false false ContractDecl structs enums consts states events errors funcs in
  let source = Oct_gen.make_env ~loops:false Oct_gen.Source ContractDecl structs enums consts states
    events errors funcs ["read"] ["read"] in
  check_index legacy.symbols; check_index source.symbols;
  legacy.locals <- ["c", 2, TBool]; source.locals <- ["c", 2, TBool];
  List.iter (fun infer ->
    check "constant precedence changed" (infer (EVar "c") = TInt);
    check "function inference changed" (infer (ECall ("read", [])) = TInt))
    [Oct_emit.typ_of_expr legacy; Oct_gen.typ_of_expr source];
  check "form membership changed" (Oct_gen.form_call source "read" && not (Oct_gen.form_call source "absent"));
  check "direct membership changed" (Oct_gen.direct_call source "read" && not (Oct_gen.direct_call source "absent"))

let test_lookup_work () =
  let open Oct_lang in
  let parsed = Oct_parse.parse "contract Work { public fn read(): int { return 1 } }" in
  let fn = List.hd parsed.funcs in
  let measure source functions count =
    let names = List.init count (fun i -> Printf.sprintf "entry%06d" i) in
    let funcs = List.map (fun name -> {fn with fn_name = name}) names in
    let consts = List.map (fun name -> {c_name = name; c_typ = TInt; c_value = EInt Z.one}) names in
    let name = List.hd (List.rev names) in
    let term = if functions then ECall (name, []) else EVar name in
    let infer = if source then
      let env = Oct_gen.make_env ~loops:false Oct_gen.Source ContractDecl [] [] consts [] [] [] funcs [] [] in
      Oct_gen.typ_of_expr env
      else let env = Oct_emit.make_env false false ContractDecl [] [] consts [] [] [] funcs in
      Oct_emit.typ_of_expr env in
    let once () =
      Gc.full_major ();
      let before = Sys.time () in
      for _ = 1 to 200_000 do check "lookup result differs" (infer term = TInt) done;
      Sys.time () -. before in
    Float.min (once ()) (once ()) in
  List.iter (fun source -> List.iter (fun functions ->
    let small = measure source functions 32 and large = measure source functions 256 in
    Printf.printf "event = lookup_work source = %b functions = %b small = %.6f large = %.6f\n%!"
      source functions small large;
    check "declaration lookup grows with table size" (large < 3. *. small +. 0.02)) [false; true]) [false; true]

let test_caps () =
  let open Oct_lang in
  let parsed = Oct_parse.parse "contract Work { public fn read(): int { return 1 } }" in
  let fn = List.hd parsed.funcs in
  let verify ast expected =
    List.iter (fun loops ->
      let result = Oct_compile.compile_ast_first ~metered:true ~loops ~checked:false
        ~source_mode:"single" ~source_material:"" ast in
      check "protocol declaration cap missing" (result.error = Some expected);
      List.iter (fun syntax ->
        check "source declaration cap missing"
          (Aml_source.compile_ast ~metered:true ~loops ~syntax ast = Error expected))
        [Oct_gen.Source; Oct_gen.Forms]) [false; true];
    check "direct protocol cap missing" (try ignore (Oct_emit.generate ~metered:true ast); false
      with Oct_emit.GenError (reason, _) -> reason = expected);
    check "direct source cap missing" (try ignore (Oct_gen.generate ~metered:true ~syntax:Oct_gen.Source ast); false
      with Oct_gen.GenError (reason, _, _) -> reason = expected) in
  verify {parsed with funcs = List.init (Program_limits.max_functions + 1)
    (fun i -> {fn with fn_name = "f" ^ string_of_int i})} "Program function limit exceeded";
  verify {parsed with consts = List.init (Bytecode.max_consts + 1)
    (fun i -> {c_name = "c" ^ string_of_int i; c_typ = TInt; c_value = EInt Z.one})}
    "Program constant limit exceeded";
  List.iter (fun count ->
    let ast = {parsed with funcs = List.init count (fun i -> {fn with fn_name = "f" ^ string_of_int i})} in
    check "function cap refused legal count" (Result.is_ok
      (Oct_symbols.check ~metered:true ~functions:count ~consts:0));
    let before = Oct_compile.compile_ast_first ~metered:false ~checked:false ~loops:false
      ~source_mode:"single" ~source_material:"" ast in
    let after = Oct_compile.compile_ast_first ~metered:true ~checked:false ~loops:false
      ~source_mode:"single" ~source_material:"" ast in
    check "function cap changed admitted output" (before = after && before.error = None))
    [Program_limits.max_functions - 1; Program_limits.max_functions];
  List.iter (fun count ->
    let ast = {parsed with consts = List.init count (fun i ->
      {c_name = "c" ^ string_of_int i; c_typ = TInt; c_value = EInt Z.one})} in
    let result metered = Oct_compile.compile_ast_first ~metered ~checked:false ~loops:false
      ~source_mode:"single" ~source_material:"" ast in
    let before = result false in
    check "constant cap refused legal count" (before.error = None && result true = before))
    [Bytecode.max_consts - 1; Bytecode.max_consts];
  check "unmetered cap changed" (Oct_symbols.check ~metered:false
    ~functions:(Program_limits.max_functions + 1) ~consts:(Bytecode.max_consts + 1) = Ok ());
  let form = List.hd (Oct_parse.parse
    "program Mixed { form add [many x: int] (many y: int) ->[many] int marks {} = x + y }").forms in
  let ast = {parsed with forms = [form]; funcs = List.init Program_limits.max_functions
    (fun i -> {fn with fn_name = "f" ^ string_of_int i})} in
  check "forms omitted from declaration cap" (Aml_source.compile_ast ~metered:true ~loops:false
    ~syntax:Oct_gen.Source ast = Error "Program function limit exceeded")

let test_orbit_lookup () =
  let open Oct_lang in
  let term count = EOrbit (Option.get (C_nat.of_int 4_000), None, EInt Z.zero, ("x", Once, TInt),
    EBinop (Add, EVar "x", EVar ("c" ^ string_of_int (count - 1)))) in
  let measure source count =
    let consts = List.init count (fun i -> {c_name = "c" ^ string_of_int i;
      c_typ = TInt; c_value = EInt Z.one}) in
    let once () =
      Gc.full_major ();
      let emit, code = if source then
        let env = Oct_gen.make_env ~metered:true ~loops:false Oct_gen.Source ContractDecl [] [] consts [] [] [] [] [] [] in
        (fun () -> ignore (Oct_gen.gen_expr env (term count))), (fun () -> env.code)
      else let env = Oct_emit.make_env ~metered:true false false ContractDecl [] [] consts [] [] [] [] in
        (fun () -> ignore (Oct_emit.gen_expr env (term count))), (fun () -> env.code) in
      let before = Sys.time () in
      emit ();
      let elapsed = Sys.time () -. before in
      check "orbit omitted emission" (List.length (code ()) > 10_000);
      elapsed in
    Float.min (once ()) (once ()) in
  List.iter (fun source ->
    let small = measure source 32 and large = measure source 4096 in
    Printf.printf "event = orbit_lookup source = %b small = %.6f large = %.6f\n%!" source small large;
    check "emission repeatedly scanned declarations" (large < 4. *. small +. 0.02)) [false; true]

let test_alias_work () =
  let open Oct_lang in
  let consts = List.init 512 (fun i -> {c_name = "c" ^ string_of_int i; c_typ = TInt;
    c_value = if i = 0 then EInt Z.one else EVar ("c" ^ string_of_int (i - 1))}) in
  let term = EOrbit (Option.get (C_nat.of_int 600), None, EInt Z.zero, ("x", Once, TInt),
    EBinop (Add, EVar "x", EVar "c511")) in
  List.iter (fun source ->
    let generate consts term =
      if source then
        let env = Oct_gen.make_env ~metered:true ~loops:false Oct_gen.Source ProgramDecl
          [] [] consts [] [] [] [] [] [] in
        ignore (Oct_gen.gen_expr env term); env.code
      else let env = Oct_emit.make_env ~metered:true false false ProgramDecl
          [] [] consts [] [] [] [] in
        ignore (Oct_emit.gen_expr env term); env.code in
    Gc.full_major ();
    let before = Gc.allocated_bytes () in
    let code = generate consts term in
    let allocated = Gc.allocated_bytes () -. before in
    Printf.printf "event = alias_work source = %b allocated = %.0f\n%!" source allocated;
    check "alias chain work repeated per orbit" (allocated < 8_000_000.);
    let direct = EOrbit (Option.get (C_nat.of_int 600), None, EInt Z.zero, ("x", Once, TInt),
      EBinop (Add, EVar "x", EInt Z.one)) in
    check "alias compression changed emitted bytes" (code = generate [] direct)) [false; true]

let test_infer_work () =
  let open Oct_lang in
  let consts = List.init 64 (fun i -> {c_name = "c" ^ string_of_int i; c_typ = TVoid;
    c_value = if i = 0 then EVar "x" else EBinop (Add,
      EVar ("c" ^ string_of_int (i - 1)), EInt Z.one)}) in
  let env = Oct_gen.make_env ~metered:true ~loops:false Oct_gen.Source ProgramDecl
    [] [] consts [] [] [] [] [] [] in
  env.locals <- ["x", 1, TInt];
  Gc.full_major ();
  let before = Gc.allocated_bytes () in
  for _ = 1 to 4000 do
    env.locals <- ["x", 1, TInt];
    check "constant inferred type changed" (Oct_gen.typ_of_expr env (EVar "c63") = TInt)
  done;
  let allocated = Gc.allocated_bytes () -. before in
  Printf.printf "event = infer_work allocated = %.0f\n%!" allocated;
  check "constant inference repeated per expression" (allocated < 10_000_000.);
  env.locals <- ["x", 1, TString];
  check "constant memo escaped its local types" (Oct_gen.typ_of_expr env (EVar "c63") = TString);
  let refused = try ignore (Oct_gen.typ_of_expr_with ["c0"] env (EVar "c1")); false
    with Oct_gen.GenError (reason, _, _) -> reason = "constant dependency is cyclic name = c0" in
  check "constant memo bypassed active dependency" refused;
  env.locals <- [];
  check "constant memo escaped its local scope" (try
    ignore (Oct_gen.typ_of_expr env (EVar "c63")); false with Oct_gen.GenError _ -> true);
  let parsed = Oct_parse.parse "program Marks { public fn f(x: int): int { return x } }" in
  let value = EUse {ux_name = "f"; ux_caps = []; ux_arg = EInt Z.one;
    ux_mult = Once; ux_bind = "x"; ux_typ = TInt; ux_body = EVar "x"} in
  let env = Oct_gen.make_env ~loops:false Oct_gen.Source ProgramDecl [] []
    [{c_name = "c"; c_typ = TVoid; c_value = value}] [] [] [] parsed.funcs ["f"] [] in
  check "form type changed" (Oct_gen.typ_of_expr env (EVar "c") = TInt);
  env.fn_is_pure <- true;
  check "constant memo bypassed pure marks" (try
    ignore (Oct_gen.typ_of_expr env (EVar "c")); false with
    | Oct_gen.GenError (reason, _, _) -> reason = "pure function form marks are not empty = f")

let test_nested_infer () =
  let open Oct_lang in
  let measure count =
    let consts = List.init count (fun i -> {c_name = "c" ^ string_of_int i; c_typ = TVoid;
      c_value = if i = 0 then EString "address"
        else ECall ("to_address", [EVar ("c" ^ string_of_int (i - 1))])}) in
    let env = Oct_gen.make_env ~metered:true ~loops:false Oct_gen.Source ProgramDecl
      [] [] consts [] [] [] [] [] [] in
    Gc.full_major ();
    let before = Gc.allocated_bytes () in
    ignore (Oct_gen.gen_expr env (EVar ("c" ^ string_of_int (count - 1))));
    let allocated = Gc.allocated_bytes () -. before in
    check "nested inference changed emission" (env.next_reg = 2 && env.code =
      List.init (count - 1) (fun _ -> Contract_vm.ASSERT_ADDR 1)
      @ [Contract_vm.LDI (1, Contract_vm.VString "address")]);
    Printf.printf "event = nested_infer count = %d allocated = %.0f\n%!" count allocated;
    allocated in
  let small = measure 128 and medium = measure 256 and large = measure 512 in
  check "nested inference repeats dependency walks"
    (medium < 3. *. small && large < 3. *. medium);
  let source = "program Address { " ^ String.concat " " (List.init 512 (fun i ->
    "const c" ^ string_of_int i ^ " = " ^ (if i = 0 then "\"address\""
      else "to_address(c" ^ string_of_int (i - 1) ^ ")")))
    ^ " public fn read(): address { return c511 } }" in
  check "nested inference exceeds package limit" (String.length source < 65_536);
  let result = Oct_compile.compile_program_source ~metered:true
    (fun path -> if path = "main.aml" then Some source else None) "main.aml" in
  check "nested inference source route refused" (result.error = None)

let test_cycle_names () =
  let open Oct_lang in
  let const name value = {c_name = name; c_typ = TVoid; c_value = value} in
  let cases = [
    [const "a" (EVar "b"); const "b" (EBinop (Add, EVar "a", EInt Z.one))], "a", "a";
    [const "a" (EVar "b"); const "b" (EBinop (Add, EVar "a", EInt Z.one))], "b", "b";
    [const "a" (EVar "b"); const "b" (EVar "c");
     const "c" (EBinop (Add, EInt Z.one, EVar "b"))], "a", "b";
    [const "a" (EVar "b"); const "b" (EVar "c");
     const "c" (EUnop (Neg, EVar "a"))], "a", "a";
    [const "a" (EVar "b"); const "b" (EVar "a")], "a", "a";
    [const "a" (EVar "a")], "a", "a";
  ] in
  List.iter (fun syntax -> List.iter (fun loops -> List.iter (fun metered ->
    List.iter (fun (consts, name, repeated) ->
      let env = Oct_gen.make_env ~metered ~loops syntax ProgramDecl
        [] [] consts [] [] [] [] [] [] in
      env.line <- 7; env.column <- 9;
      let expected = "constant dependency is cyclic name = " ^ repeated in
      for _ = 1 to 2 do
        match Oct_gen.typ_of_expr env (EVar name) with
        | _ -> failwith "cyclic type accepted"
        | exception Oct_gen.GenError (reason, line, column) ->
          check ("cycle diagnostic differs: " ^ reason)
            (reason = expected && line = 7 && column = 9)
      done) cases) [false; true]) [false; true]) [Oct_gen.Source; Oct_gen.Forms];
  let consts = [const "a" (EVar "b");
    const "b" (ELet ("slot", Once, TInt, EInt Z.one, EVar "a"))] in
  let env = Oct_gen.make_env ~loops:false Oct_gen.Source ProgramDecl
    [] [] consts [] [] [] [] [] [] in
  env.locals <- ["outer", 1, TBool];
  begin match Oct_gen.typ_of_expr env (EVar "a") with
  | _ -> failwith "scoped cycle accepted"
  | exception Oct_gen.GenError (reason, _, _) ->
    check "scoped cycle diagnostic differs" (reason = "constant dependency is cyclic name = a");
    check "cycle replay duplicated local context"
      (env.locals = ["slot", 0, TInt; "outer", 1, TBool])
  end;
  env.locals <- ["x", 1, TInt];
  let term = ELet ("x", Once, TBool, EBinop (Lt, EVar "x", EInt Z.zero), EVar "a") in
  begin match Oct_gen.typ_of_expr env term with
  | _ -> failwith "shadowed cycle accepted"
  | exception Oct_gen.GenError (reason, _, _) ->
    check "cycle replay lost entry local type" (reason = "constant dependency is cyclic name = a")
  end;
  let module P = Program_package in
  let source = "program Cycle { const a = b const b = a + 1 public fn read(): int { return a } }" in
  let expected = "Program compile failed: source = main.aml line 1 column 69: constant dependency is cyclic name = a" in
  List.iter (fun loops -> List.iter (fun metered ->
    let verify = function
      | Error error -> check ("package cycle diagnostic differs: " ^ P.error_message error)
          (P.error_message error = expected)
      | Ok _ -> failwith "cyclic source package accepted" in
    List.iter (fun compiler ->
      verify (P.compile_sources_at ~metered ~loops ~compiler ~point_ops:true
        ~main:"main.aml" ~sources:[P.{path = "main.aml"; body = source}])) [P.Source; P.Preview];
    let good = P.compile_sources_at ~metered ~loops ~compiler:P.Source ~point_ops:true
      ~main:"main.aml" ~sources:[P.{path = "main.aml";
        body = "program Cycle { public fn read(): int { return 7 } }"}]
      |> Result.get_ok in
    let bytes = Bytes.of_string good.package in
    let count_at = 10 + Bytes.get_uint16_le bytes 8 in
    let length_at = count_at + 4 + Bytes.get_uint16_le bytes (count_at + 2) in
    let body_at = length_at + 4 in
    let after = body_at + Int32.to_int (Bytes.get_int32_le bytes length_at) in
    let prefix = Bytes.sub bytes 0 body_at in
    Bytes.set_int32_le prefix length_at (Int32.of_int (String.length source));
    let encoded = Base64.encode_exn (Bytes.to_string prefix ^ source
      ^ String.sub good.package after (String.length good.package - after)) in
    check "cycle package shape invalid" (P.validate_base64 encoded = Ok ());
    List.iter (fun compiler ->
      verify (P.admit_base64 ~metered ~loops ~consts:false ~compiler ~point_ops:true encoded))
      [P.Source; P.Preview];
    match P.admit_base64 ~metered ~loops ~consts:true ~compiler:P.Source ~point_ops:true encoded with
    | Error error -> check "active constant cycle policy changed"
        (P.error_message error = "Program compile failed: constant dependency is cyclic")
    | Ok _ -> failwith "active constant cycle policy missing") [false; true]) [false; true]

let test_alias_infer () =
  let open Oct_lang in
  let consts = List.init 2048 (fun i -> {c_name = "c" ^ string_of_int i; c_typ = TVoid;
    c_value = if i = 0 then EInt Z.one else EVar ("c" ^ string_of_int (i - 1))}) in
  let env = Oct_gen.make_env ~loops:false Oct_gen.Source ProgramDecl
    [] [] consts [] [] [] [] [] [] in
  Gc.full_major ();
  let before = Gc.allocated_bytes () in
  for _ = 1 to 4000 do
    check "alias inferred type differs" (Oct_gen.typ_of_expr env (EVar "c2047") = TInt)
  done;
  let allocated = Gc.allocated_bytes () -. before in
  Printf.printf "event = alias_infer allocated = %.0f\n%!" allocated;
  check "alias inference repeats dependency walks" (allocated < 8_000_000.);
  let before = Gc.allocated_bytes () in
  for index = 1 to 64 do
    env.locals <- ["unused", index, TBool];
    check "alias context changed type" (Oct_gen.typ_of_expr env (EVar "c2047") = TInt)
  done;
  let allocated = Gc.allocated_bytes () -. before in
  Printf.printf "event = alias_context allocated = %.0f\n%!" allocated;
  check "alias context repeats dependency walks" (allocated < 8_000_000.)

let test_alias_modes () =
  let open Oct_lang in
  let const name typ value = {c_name = name; c_typ = typ; c_value = value} in
  let consts = [const "a" TVoid (EVar "epoch"); const "epoch" TInt (EInt Z.one)] in
  let env = Oct_emit.make_env false true ProgramDecl [] [] consts [] [] [] [] in
  ignore (Oct_emit.gen_expr env (EVar "a"));
  check "builtin alias lost epoch" (env.code = [Contract_vm.EPOCH 1]);
  env.locals <- ["epoch", 2, TBool]; env.code <- []; env.next_reg <- 3;
  ignore (Oct_emit.gen_expr env (EVar "a"));
  check "alias cache crossed builtin scope" (env.code = [Contract_vm.LDI (3, Contract_vm.VInt Z.one)]);
  let symbols = Oct_symbols.create [] []
    [const "a" TVoid (EVar "b"); const "b" TBool (EVar "c"); const "c" TInt (EInt Z.one)]
    [] [] [] [] in
  check "typed alias ignored its annotation" (Oct_symbols.resolve symbols 8 "a" = Ok "b");
  check "value alias used a type endpoint" (Oct_symbols.resolve symbols 0 "a" = Ok "c");
  let cyclic = Oct_symbols.create [] []
    [const "a" TInt (EVar "b"); const "b" TInt (EVar "a")] [] [] [] [] in
  check "alias cycle accepted" (Oct_symbols.resolve cyclic 0 "a" = Error "a");
  check "alias cycle cached a value" (Hashtbl.length cyclic.aliases = 0)

let () =
  test_cycle_names ();
  test_emitters (); test_duplicates (); test_symbols (); test_lookup_work (); test_caps (); test_orbit_lookup ();
  test_alias_work (); test_infer_work (); test_alias_modes ();
  test_nested_infer (); test_alias_infer ();
  print_endline "status = pass test = function_work"