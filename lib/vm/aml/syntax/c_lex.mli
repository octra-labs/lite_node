(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type pos = {
  off : int;
  line : int;
  col : int;
}

type span = {
  first : pos;
  last : pos;
}

type hold =
  | Recur
  | Store
  | Closure
  | Raise
  | Depend
  | Polymark

type token =
  | Program
  | Size
  | Measure
  | Law
  | Input
  | Term
  | Form
  | Kind
  | Marks
  | Under
  | Steps
  | Depth
  | Work
  | Use
  | Data
  | Shape
  | Permit
  | Tag
  | Make
  | Erase
  | Once
  | Many
  | Unit
  | Bool
  | Int
  | Sint
  | Uint
  | Bytes
  | Vec
  | Seq
  | Cap
  | Result
  | Res
  | Let
  | In
  | Split
  | As
  | If
  | Then
  | Else
  | True
  | False
  | Good
  | Bad
  | Case
  | Of
  | Fst
  | Snd
  | Equal
  | Fit
  | Wide
  | Length
  | Read
  | Write
  | Emit
  | Fail
  | Cat
  | Take
  | Drop
  | Vcat
  | At
  | Uncons
  | Fold
  | Every
  | Any
  | Count
  | Total
  | Weave
  | Braid
  | Loom
  | Orbit
  | Wake
  | Rift
  | From
  | With
  | Step
  | Close
  | Max
  | Abs
  | Lbrace
  | Rbrace
  | Lparen
  | Rparen
  | Lbrack
  | Rbrack
  | Colon
  | Comma
  | Eq
  | EqEq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  | Arrow
  | Thin
  | Bar
  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | Ident of string
  | Nat of Z.t
  | Hex of string
  | Hold of hold
  | Eof

type form =
  | F_program
  | F_size
  | F_measure
  | F_law
  | F_input
  | F_term
  | F_form
  | F_kind
  | F_marks
  | F_under
  | F_steps
  | F_depth
  | F_work
  | F_use
  | F_data
  | F_shape
  | F_permit
  | F_tag
  | F_make
  | F_erase
  | F_once
  | F_many
  | F_unit
  | F_bool
  | F_int
  | F_sint
  | F_uint
  | F_bytes
  | F_vec
  | F_seq
  | F_cap
  | F_result
  | F_res
  | F_let
  | F_in
  | F_split
  | F_as
  | F_if
  | F_then
  | F_else
  | F_true
  | F_false
  | F_ok
  | F_err
  | F_case
  | F_of
  | F_fst
  | F_snd
  | F_equal
  | F_fit
  | F_wide
  | F_length
  | F_read
  | F_write
  | F_emit
  | F_fail
  | F_cat
  | F_take
  | F_drop
  | F_vcat
  | F_at
  | F_uncons
  | F_fold
  | F_every
  | F_some
  | F_count
  | F_total
  | F_weave
  | F_braid
  | F_loom
  | F_orbit
  | F_wake
  | F_rift
  | F_from
  | F_with
  | F_step
  | F_close
  | F_max
  | F_abs
  | F_lbrace
  | F_rbrace
  | F_lparen
  | F_rparen
  | F_lbrack
  | F_rbrack
  | F_colon
  | F_comma
  | F_eq
  | F_eqeq
  | F_ne
  | F_lt
  | F_le
  | F_gt
  | F_ge
  | F_arrow
  | F_thin
  | F_bar
  | F_plus
  | F_minus
  | F_star
  | F_slash
  | F_percent
  | F_ident
  | F_nat
  | F_hex
  | F_hold of hold
  | F_eof

type item = {
  tok : token;
  span : span;
}

type cause =
  | Source of int * int
  | Tokens of int * int
  | Name of int * int
  | Number
  | Hex_text
  | Comment
  | Nest of int * int
  | Char of int

type error = {
  cause : cause;
  span : span;
}

val scan : string -> (item array, error) result
val form : token -> form
val hold_text : hold -> string
val form_text : form -> string
val text : error -> string