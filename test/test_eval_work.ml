(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_vm

module Eval = C_eval
module Term = C_term
module Type = C_type

let require condition reason =
  if not condition then failwith reason

let nat value = Option.get (C_nat.of_int value)

let bind id typ = Term.bind (nat id) Type.Many typ

let read ?(inputs = []) term =
  match Eval.run_in inputs term with
  | Ok out -> out
  | Error error -> failwith (Eval.text error)

let refused ?(inputs = []) term actual =
  match Eval.run_in inputs term with
  | Error (Eval.Arith (limit, cost)) ->
    require (Z.equal limit Eval.max_cost) "integer limit changed";
    require (Z.equal cost actual) "integer refusal total"
  | Error error -> failwith (Eval.text error)
  | Ok _ -> failwith "integer budget ignored"

let wide cells = Z.shift_left Z.one (64 * (cells - 1))

let price op left right = Int_work.cost Int_work.Active op left right

let inputs value = [bind 0 Type.Int, Eval.Int value]

let product = Term.Mul (Term.Var (nat 0), Term.Var (nat 0))

let rec repeat count term =
  if count = 1 then term
  else Term.Let (bind (10 + count) Type.Int, term, repeat (count - 1) term)

let check_sum () =
  let value = wide 512 in
  let one = price Int_work.Mul value value in
  let values = inputs value in
  let out = read ~inputs:values (repeat 3 product) in
  require (Z.equal out.int_work (Z.mul (Z.of_int 3) one)) "sum lost prior work";
  refused ~inputs:values (repeat 4 product) (Z.mul (Z.of_int 4) one);
  let again = read ~inputs:values product in
  require (Z.equal again.int_work one) "run retained prior budget"

let check_ops () =
  let left = Z.of_int (-19) in
  let right = Z.of_int 3 in
  let a = Term.Int left in
  let b = Term.Int right in
  List.iter (fun (op, term, answer) ->
    let out = read term in
    require (Eval.equal out.value (Eval.Int answer)) "arithmetic result changed";
    require (Z.equal out.int_work (price op left right)) "arithmetic price";
    require (Z.equal out.work (if op = Int_work.Neg || op = Int_work.Abs
      then Z.of_int 2 else Z.of_int 3)) "structural work changed")
    [Int_work.Add, Term.Add (a, b), Z.add left right;
     Int_work.Sub, Term.Sub (a, b), Z.sub left right;
     Int_work.Mul, Term.Mul (a, b), Z.mul left right;
     Int_work.Div, Term.Div (a, b), Z.div left right;
     Int_work.Mod, Term.Mod (a, b), Z.rem left right;
     Int_work.Neg, Term.Neg a, Z.neg left;
     Int_work.Abs, Term.Abs a, Z.abs left];
  List.iter (fun (term, expected) ->
    match Eval.run term with
    | Error actual when actual = expected -> ()
    | _ -> failwith "zero divisor changed")
    [Term.Div (a, Term.Int Z.zero), Eval.Divide_zero;
     Term.Mod (a, Term.Int Z.zero), Eval.Modulo_zero]

let check_edge () =
  let value = wide 999 in
  let last = wide 1993 in
  let values = inputs value @ [bind 1 Type.Int, Eval.Int last] in
  let term = Term.Let (bind 2 Type.Int, product,
    Term.Add (Term.Var (nat 1), Term.Int Z.zero)) in
  let out = read ~inputs:values term in
  require (Z.equal out.int_work Eval.max_cost) "exact integer budget refused";
  let values = inputs value @ [bind 1 Type.Int, Eval.Int (wide 1994)] in
  refused ~inputs:values term (Z.succ Eval.max_cost)

let check_preflight () =
  let value = wide 16_384 in
  let values = inputs value in
  let cost = price Int_work.Mul value value in
  let before = Gc.allocated_bytes () in
  refused ~inputs:values product cost;
  require (Gc.allocated_bytes () -. before < 128_000.) "product allocated before charge"

let check_fold () =
  let value = wide 512 in
  let fold = Term.fold (bind 1 Type.Unit) (bind 2 Type.Int) product in
  let term count =
    Term.Vfold (Term.Vec (Type.Unit, List.init count (fun _ -> Term.Unit)),
      Term.Int Z.zero, fold) in
  let values = inputs value in
  let one = price Int_work.Mul value value in
  let out = read ~inputs:values (term 3) in
  require (Z.equal out.int_work (Z.mul (Z.of_int 3) one)) "fold budget reset";
  refused ~inputs:values (term 4) (Z.mul (Z.of_int 4) one)

let check_branches () =
  let values = inputs (wide 512) in
  let cheap = Term.Add (Term.Int Z.one, Term.Int Z.one) in
  let expensive = repeat 4 product in
  let terms = [
    Term.If (Term.Bool true, cheap, expensive);
    Term.If (Term.Bool false, expensive, cheap);
    Term.Let (Term.bind (nat 1) Type.Zero Type.Int, expensive, cheap);
    Term.Case (Term.Inl (Term.Unit, Type.Unit), bind 1 Type.Unit, cheap,
      bind 2 Type.Unit, expensive);
  ] in
  List.iter (fun term ->
    let out = read ~inputs:values term in
    require (Eval.equal out.value (Eval.Int (Z.of_int 2))) "branch value changed";
    require (Z.equal out.int_work (Z.of_int 4)) "unchosen branch charged") terms

let check_compare () =
  let left = wide 512 in
  let right = Z.succ left in
  let values = inputs left @ [bind 1 Type.Int, Eval.Int right] in
  List.iter (fun (rel, expected) ->
    let out = read ~inputs:values (Term.Cmp (rel, Term.Var (nat 0), Term.Var (nat 1))) in
    require (Eval.equal out.value (Eval.Bool expected)) "comparison value";
    require (Z.equal out.int_work (price Int_work.Sub left right)) "comparison price")
    [Term.Lt, true; Term.Le, true; Term.Gt, false; Term.Ge, false];
  let typ = Type.Vec (nat 4, Type.Int) in
  let vector = Eval.Vec (Array.make 4 (Eval.Int left)) in
  let values = [bind 0 typ, vector] in
  let out = read ~inputs:values (Term.Eq (typ, Term.Var (nat 0), Term.Var (nat 0))) in
  require (Eval.equal out.value (Eval.Bool true)) "vector equality value";
  require (Z.equal out.int_work (Z.mul (Z.of_int 4) (price Int_work.Sub left left)))
    "vector equality price";
  let typ = Type.Vec (nat 1942, Type.Int) in
  let vector = Eval.Vec (Array.make 1942 (Eval.Int left)) in
  refused ~inputs:[bind 0 typ, vector]
    (Term.Eq (typ, Term.Var (nat 0), Term.Var (nat 0)))
    (Z.mul (Z.of_int 1942) (price Int_work.Sub left left))

let check_rule () =
  let term = Term.Add (Term.Int Z.one, Term.Int Z.one) in
  let info = Result.get_ok (C_check.check term) in
  let out = Result.get_ok (Eval.run ~fuel:info.res.steps term) in
  require (Z.equal out.int_work (Z.of_int 4)) "fuel changed integer limit";
  require (Z.equal out.work info.res.work) "certificate work changed";
  let rule = Result.get_ok (C_rule.rule ~version:1 ~activate:Z.zero C_rule.local) in
  let schedule = Result.get_ok (C_rule.schedule [rule]) in
  let selected = Result.get_ok (Eval.run_at schedule ~epoch:Z.zero term) in
  require (selected.out = out) "rule selected different evaluation"

let () =
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "sum" then check_sum ()
  else begin
    check_sum ();
    check_ops ();
    check_edge ();
    check_preflight ();
    check_fold ();
    check_branches ();
    check_compare ();
    check_rule ()
  end;
  Printf.printf "eval_work = pass\n"