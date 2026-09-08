(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let var id = "v" ^ C_nat.text id

type part =
  | Term of C_term.t
  | Text of string
  | Terms of C_term.t list

let term value =
  let out = C_text.make () in
  let rec walk = function
    | [] -> ()
    | _ when C_text.full out -> ()
    | Text value :: rest ->
      C_text.add out value;
      walk rest
    | Terms [] :: rest -> walk rest
    | Terms [value] :: rest -> walk (Term value :: rest)
    | Terms (value :: values) :: rest ->
      walk (Term value :: Text ", " :: Terms values :: rest)
    | Term C_term.Unit :: rest -> C_text.add out "()"; walk rest
    | Term (C_term.Bool true) :: rest -> C_text.add out "true"; walk rest
    | Term (C_term.Bool false) :: rest -> C_text.add out "false"; walk rest
    | Term (C_term.Int value) :: rest ->
      C_text.add out (C_eval.value_text (C_eval.Int value));
      walk rest
    | Term (C_term.Narrow (typ, value)) :: rest ->
      C_text.add out ("(narrow " ^ C_type.text typ ^ " ");
      C_text.add out (C_eval.value_text (C_eval.Int value));
      walk (Text ")" :: rest)
    | Term (C_term.Bytes value) :: rest ->
      C_text.add out (C_eval.value_text (C_eval.Bytes value));
      walk rest
    | Term (C_term.Vec (elem, values)) :: rest ->
      C_text.add out ("(vec<" ^ C_type.text elem ^ "> [");
      walk (Terms values :: Text "])" :: rest)
    | Term (C_term.Seq (cap, elem, values)) :: rest ->
      C_text.add out ("(seq<" ^ C_nat.text cap ^ "," ^ C_type.text elem ^ "> [");
      walk (Terms values :: Text "])" :: rest)
    | Term (C_term.Var id) :: rest -> C_text.add out (var id); walk rest
    | Term (C_term.Let (bind, value, body)) :: rest ->
      C_text.add out ("(let " ^ C_type.mul_text bind.mul ^ " " ^ var bind.id
        ^ " : " ^ C_type.text bind.typ ^ " = ");
      walk (Term value :: Text " in " :: Term body :: Text ")" :: rest)
    | Term (C_term.If (guard, yes, no)) :: rest ->
      C_text.add out "(if ";
      walk (Term guard :: Text " then " :: Term yes :: Text " else "
        :: Term no :: Text ")" :: rest)
    | Term (C_term.Pair (left, right)) :: rest ->
      C_text.add out "(";
      walk (Term left :: Text ", " :: Term right :: Text ")" :: rest)
    | Term (C_term.Unpair (pair, left, right, body)) :: rest ->
      C_text.add out ("(let (" ^ var left.id ^ ", " ^ var right.id ^ ") = ");
      walk (Term pair :: Text " in " :: Term body :: Text ")" :: rest)
    | Term (C_term.Fst pair) :: rest ->
      C_text.add out "(fst ";
      walk (Term pair :: Text ")" :: rest)
    | Term (C_term.Snd pair) :: rest ->
      C_text.add out "(snd ";
      walk (Term pair :: Text ")" :: rest)
    | Term (C_term.Inl (value, right)) :: rest ->
      C_text.add out "(inl ";
      walk (Term value :: Text (" : " ^ C_type.text right ^ ")") :: rest)
    | Term (C_term.Inr (left, value)) :: rest ->
      C_text.add out ("(inr " ^ C_type.text left ^ " : ");
      walk (Term value :: Text ")" :: rest)
    | Term (C_term.Case (value, left, yes, right, no)) :: rest ->
      C_text.add out "(case ";
      walk (Term value :: Text (" of inl " ^ var left.id ^ " -> ")
        :: Term yes :: Text (" | inr " ^ var right.id ^ " -> ")
        :: Term no :: Text ")" :: rest)
    | Term (C_term.Act (atom, body)) :: rest ->
      C_text.add out ("(" ^ C_eff.atom_text atom ^ "; ");
      walk (Term body :: Text ")" :: rest)
    | Term (C_term.Add (left, right)) :: rest ->
      C_text.add out "(";
      walk (Term left :: Text " + " :: Term right :: Text ")" :: rest)
    | Term (C_term.Sub (left, right)) :: rest ->
      C_text.add out "(";
      walk (Term left :: Text " - " :: Term right :: Text ")" :: rest)
    | Term (C_term.Mul (left, right)) :: rest ->
      C_text.add out "(";
      walk (Term left :: Text " * " :: Term right :: Text ")" :: rest)
    | Term (C_term.Div (left, right)) :: rest ->
      C_text.add out "(";
      walk (Term left :: Text " / " :: Term right :: Text ")" :: rest)
    | Term (C_term.Mod (left, right)) :: rest ->
      C_text.add out "(";
      walk (Term left :: Text " % " :: Term right :: Text ")" :: rest)
    | Term (C_term.Neg value) :: rest ->
      C_text.add out "(-";
      walk (Term value :: Text ")" :: rest)
    | Term (C_term.Abs value) :: rest ->
      C_text.add out "abs(";
      walk (Term value :: Text ")" :: rest)
    | Term (C_term.Fit (typ, value)) :: rest ->
      C_text.add out ("(fit " ^ C_type.text typ ^ " ");
      walk (Term value :: Text ")" :: rest)
    | Term (C_term.Wide value) :: rest ->
      C_text.add out "(wide ";
      walk (Term value :: Text ")" :: rest)
    | Term (C_term.Length value) :: rest ->
      C_text.add out "(length ";
      walk (Term value :: Text ")" :: rest)
    | Term (C_term.Eq (typ, left, right)) :: rest ->
      if C_type.equal typ C_type.Int then begin
        C_text.add out "(";
        walk (Term left :: Text " == " :: Term right :: Text ")" :: rest)
      end else begin
        C_text.add out ("equal[" ^ C_type.text typ ^ "](");
        walk (Term left :: Text ", " :: Term right :: Text ")" :: rest)
      end
    | Term (C_term.Cmp (rel, left, right)) :: rest ->
      let text =
        match rel with
        | C_term.Lt -> " < "
        | C_term.Le -> " <= "
        | C_term.Gt -> " > "
        | C_term.Ge -> " >= "
      in
      C_text.add out "(";
      walk (Term left :: Text text :: Term right :: Text ")" :: rest)
    | Term (C_term.Cat (left, right)) :: rest ->
      C_text.add out "(cat ";
      walk (Term left :: Text " " :: Term right :: Text ")" :: rest)
    | Term (C_term.Take (len, value)) :: rest ->
      C_text.add out ("(take " ^ C_nat.text len ^ " ");
      walk (Term value :: Text ")" :: rest)
    | Term (C_term.Drop (len, value)) :: rest ->
      C_text.add out ("(drop " ^ C_nat.text len ^ " ");
      walk (Term value :: Text ")" :: rest)
    | Term (C_term.Vcat (left, right)) :: rest ->
      C_text.add out "(vcat ";
      walk (Term left :: Text " " :: Term right :: Text ")" :: rest)
    | Term (C_term.At (index, value)) :: rest ->
      C_text.add out ("(at " ^ C_nat.text index ^ " ");
      walk (Term value :: Text ")" :: rest)
    | Term (C_term.Uncons value) :: rest ->
      C_text.add out "(uncons ";
      walk (Term value :: Text ")" :: rest)
    | Term (C_term.Vfold (vector, seed, fold)) :: rest ->
      C_text.add out "(vfold ";
      walk (Term vector :: Text " from " :: Term seed
        :: Text (" with " ^ C_type.mul_text fold.item.mul ^ " "
          ^ var fold.item.id ^ " : " ^ C_type.text fold.item.typ ^ ", "
          ^ C_type.mul_text fold.state.mul ^ " " ^ var fold.state.id ^ " : "
          ^ C_type.text fold.state.typ ^ " -> ")
        :: Term fold.body :: Text ")" :: rest)
    | Term (C_term.Step (cap, value)) :: rest ->
      C_text.add out "(step ";
      walk (Term cap :: Text " " :: Term value :: Text ")" :: rest)
    | Term (C_term.Close cap) :: rest ->
      C_text.add out "(close ";
      walk (Term cap :: Text ")" :: rest)
  in
  walk [Term value];
  C_text.get out