(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_vm
open Oct_lang

let check message value = if not value then failwith message

let ast consts = { (Oct_parse.parse "contract Types {}") with consts }

let generators consts =
  let gen = Oct_gen.make_env ~loops:true Oct_gen.Source ContractDecl
    [] [] consts [] [] [] [] [] [] in
  let checked = Oct_check.{ ast = ast consts; locals = []; ret = TVoid; active = [] } in
  Oct_gen.typ_of_expr gen, Oct_check.expr checked

let constant name value = {c_name = name; c_typ = TVoid; c_value = value}

let test_dag () =
  let name i = "C" ^ string_of_int i in
  let consts = List.init 28 (fun i -> constant (name i)
    (if i = 0 then EInt Z.one else EBinop (Add, EVar (name (i - 1)), EVar (name (i - 1))))) in
  let gen, checked = generators consts in
  List.iter (fun infer ->
    Gc.full_major ();
    let started = Gc.allocated_bytes () in
    let alarm = Gc.create_alarm (fun () ->
      if Gc.allocated_bytes () -. started > 32_000_000. then
        failwith "constant inference repeated the dependency graph") in
    Fun.protect ~finally:(fun () -> Gc.delete_alarm alarm) (fun () ->
      check "constant DAG type differs" (infer (EVar (name 27)) = TInt);
      check "constant inference allocation exceeds graph budget"
        (Gc.allocated_bytes () -. started < 1_000_000.))) [gen; checked]

let test_scopes () =
  let gen, checked = generators [constant "C" (EVar "x")] in
  let value = ETuple [ELet ("x", Many, TInt, EInt Z.one, EVar "C");
    ELet ("x", Many, TBool, EBool true, EVar "C")] in
  List.iter (fun infer ->
    check "cached constant crossed local scope" (infer value = TTuple [TInt; TBool]);
    check "scope cache escaped inference call" (infer value = TTuple [TInt; TBool]))
    [gen; checked]

let test_cycles () =
  let gen, checked = generators [constant "C" (EVar "D"); constant "D" (EVar "C")] in
  List.iter (fun infer ->
    check "constant cycle was accepted" (try ignore (infer (EVar "C")); false with
      | Oct_gen.GenError _ | Oct_check.Invalid _ -> true)) [gen; checked];
  let consts = [constant "A" (EVar "C");
    constant "C" (ELet ("C", Many, TInt, EInt Z.one, EVar "A"))] in
  let env = Oct_check.{ast = ast consts; locals = []; ret = TVoid; active = []} in
  check "cached dependency bypassed active cycle" (try
    ignore (Oct_check.expr env (ETuple [EVar "C"; EVar "A"])); false
    with Oct_check.Invalid _ -> true)

let () =
  test_dag ();
  test_scopes ();
  test_cycles ();
  print_endline "status = pass suite = const_types"