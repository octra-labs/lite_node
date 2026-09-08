(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type shape =
  | SUnit
  | SAtom
  | SCap of C_nat.t
  | SPair of shape * shape
  | SVec of C_nat.t * shape
  | SSum of shape

type emission =
  | Lowered
  | Specialized

type code =
  | Done
  | Push of C_emit.lit * code
  | Void of code
  | Get of C_term.id * shape * code
  | Plus of code
  | Minus of code
  | Times of code
  | Quot of code
  | Rem of code
  | Negate of code
  | Absolute of code
  | Same of code
  | Different of code
  | Order of C_term.rel * code
  | Join of code
  | Clip of C_nat.t * code
  | Skip of C_nat.t * code
  | Duo of code
  | First of code
  | Second of code
  | Empty of shape * code
  | Cons of code
  | Append of code
  | Pick of C_nat.t * code
  | Unhead of code
  | Left of code
  | Right of code
  | Pack of C_nat.t * C_type.t * code
  | Fit of C_type.t * code
  | Wide of code
  | Close of C_nat.t * code
  | Effect of int * C_eff.atom * code * code
  | Scope of C_term.bind * code * code
  | Scope2 of C_term.bind * C_term.bind * code * code
  | Iter of C_nat.t * C_term.bind * C_term.bind * code * code
  | Iter_seq of C_nat.t * C_term.bind * C_term.bind * code * code
  | Choice of C_term.bind * code * C_term.bind * code * shape * code
  | Fork of shape * code * code * code

type loc =
  | LDone
  | LPush of C_lex.span * loc
  | LVoid of C_lex.span * loc
  | LGet of C_lex.span * loc
  | LPlus of C_lex.span * loc
  | LMinus of C_lex.span * loc
  | LTimes of C_lex.span * loc
  | LQuot of C_lex.span * loc
  | LRem of C_lex.span * loc
  | LNegate of C_lex.span * loc
  | LAbsolute of C_lex.span * loc
  | LSame of C_lex.span * loc
  | LDifferent of C_lex.span * loc
  | LOrder of C_term.rel * C_lex.span * loc
  | LJoin of C_lex.span * loc
  | LClip of C_nat.t * C_lex.span * loc
  | LSkip of C_nat.t * C_lex.span * loc
  | LDuo of C_lex.span * loc
  | LFirst of C_lex.span * loc
  | LSecond of C_lex.span * loc
  | LEmpty of C_lex.span * loc
  | LCons of C_lex.span * loc
  | LAppend of C_lex.span * loc
  | LPick of C_nat.t * C_lex.span * loc
  | LUnhead of C_lex.span * loc
  | LLeft of C_lex.span * loc
  | LRight of C_lex.span * loc
  | LPack of C_lex.span * loc
  | LFit of C_lex.span * loc
  | LWide of C_lex.span * loc
  | LClose of C_lex.span * loc
  | LEffect of C_lex.span * loc * loc
  | LScope of C_lex.span * loc * loc
  | LScope2 of C_lex.span * loc * loc
  | LIter of C_lex.span * loc * loc
  | LChoice of C_lex.span * C_lex.span * C_lex.span * loc * loc * loc
  | LFork of C_lex.span * C_lex.span * C_lex.span * loc * loc * loc

type t = private {
  inputs : C_term.bind list;
  code : code;
  loc : loc;
  typ : C_type.t;
  result : C_emit.lit option;
  emission : emission;
  veils : int;
  veil_depth : C_nat.t;
  span : C_lex.span;
}

type error =
  | Source of C_parse.error
  | Feed of C_feed.error
  | Input of C_term.id * C_type.t
  | Inputs of Z.t
  | Layout of Z.t
  | Types of Z.t
  | Effects of C_eff.atom list
  | Term
  | Run of C_eval.error
  | Plan of C_eff.atom list
  | Value of C_type.t * C_eval.value
  | Replay
  | Map

val compile : string -> (t, error) result
val compile_feed : string -> C_feed.t -> (t, error) result
val lower : C_term.t -> code option
val lower_in : C_term.bind list -> C_term.t -> code option
val locate : C_lex.span -> code -> loc
val replay_actions : code -> (C_emit.lit list * C_eval.action list) option
val replay_plan : code -> (C_emit.lit list * C_eff.atom list) option
val replay : code -> C_emit.lit list option
val replay_actions_in : code -> (C_term.bind * C_emit.lit) list ->
  (C_emit.lit list * C_eval.action list) option
val replay_plan_in : code -> (C_term.bind * C_emit.lit) list ->
  (C_emit.lit list * C_eff.atom list) option
val replay_in : code -> (C_term.bind * C_emit.lit) list ->
  C_emit.lit list option
val replay_value_in : code -> (C_term.bind * C_emit.lit) list ->
  C_type.t -> C_emit.lit option
val effects : code -> C_eff.atom list
val equal : C_emit.lit -> C_emit.lit -> bool
val literal : C_type.t -> C_eval.value -> C_emit.lit option
val atoms : C_type.t -> C_eval.value -> C_emit.lit list option
val value : C_type.t -> C_emit.lit list ->
  (C_eval.value * C_emit.lit list) option
val shape_of : C_type.t -> shape option
val shape_width : shape -> Z.t
val shape_cells : shape -> Z.t
val same_shape : shape -> shape -> bool
val emission_text : emission -> string
val text : error -> string