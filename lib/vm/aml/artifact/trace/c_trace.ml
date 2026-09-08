(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type op =
  | Load of C_emit.lit
  | Stop

type frame = {
  pc : int;
  op : op;
  reg : C_emit.lit option;
  effort : int;
  halted : bool;
  span : C_lex.span;
}

type t = {
  frames : frame list;
  result : C_emit.lit;
  octb : string;
}

type error =
  | Emit of C_emit.error
  | Shape
  | Result

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let equal left right =
  match left, right with
  | C_emit.Bool lhs, C_emit.Bool rhs -> Bool.equal lhs rhs
  | C_emit.Int lhs, C_emit.Int rhs -> Z.equal lhs rhs
  | C_emit.Bytes lhs, C_emit.Bytes rhs -> String.equal lhs rhs
  | C_emit.Data lhs, C_emit.Data rhs -> C_rval.equal lhs rhs
  | C_emit.Cap (lk, li), C_emit.Cap (rk, ri) ->
    C_nat.equal lk rk && C_nat.equal li ri
  | _ -> false

let replay = function
  | [
      { pc = 0; op = Load value; reg = Some loaded; effort = 1;
        halted = false; _ };
      { pc = 1; op = Stop; reg = Some final; effort = 2;
        halted = true; _ };
    ] when equal value loaded && equal loaded final -> Ok final
  | _ -> Error Shape

let make source =
  let* artifact =
    match C_emit.compile source with
    | Ok value -> Ok value
    | Error error -> Error (Emit error)
  in
  let frames = [
    {
      pc = 0;
      op = Load artifact.lit;
      reg = Some artifact.lit;
      effort = 1;
      halted = false;
      span = artifact.span;
    };
    {
      pc = 1;
      op = Stop;
      reg = Some artifact.lit;
      effort = 2;
      halted = true;
      span = artifact.span;
    };
  ] in
  let* result = replay frames in
  if equal result artifact.lit then
    Ok { frames; result; octb = artifact.octb }
  else Error Result

let text = function
  | Emit error -> C_emit.text error
  | Shape -> "trace shape is invalid"
  | Result -> "trace result differs from compiler result"