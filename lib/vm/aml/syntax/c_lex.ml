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

type state = {
  src : string;
  len : int;
  pos : pos;
  depth : int;
  count : int;
}

let source_max = C_rule.local.str
let token_max = C_rule.local.tm_nodes
let name_max = C_rule.local.name
let number_max = C_rule.local.text
let depth_max = C_rule.local.tm_depth

let point off line col = { off; line; col }
let zero = point 0 1 1
let span first last = { first; last }
let char state =
  if state.pos.off < state.len then Some (String.get state.src state.pos.off)
  else None

let char_at state step =
  let off = state.pos.off + step in
  if off < state.len then Some (String.get state.src off)
  else None

let next state =
  match char state with
  | None -> state
  | Some '\n' ->
      { state with pos = point (state.pos.off + 1) (state.pos.line + 1) 1 }
  | Some _ ->
      { state with pos = point (state.pos.off + 1) state.pos.line (state.pos.col + 1) }

let rec skip state =
  match char state with
  | Some (' ' | '\t' | '\r' | '\n') -> skip (next state)
  | _ -> state

let alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let digit = function
  | '0' .. '9' -> true
  | _ -> false

let alnum char = alpha char || digit char

let hex = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

let rec seek pred state =
  match char state with
  | Some char when pred char -> seek pred (next state)
  | _ -> state

let keyword = function
  | "program" -> Program
  | "size" -> Size
  | "measure" -> Measure
  | "law" -> Law
  | "input" -> Input
  | "term" -> Term
  | "form" -> Form
  | "kind" -> Kind
  | "marks" -> Marks
  | "under" -> Under
  | "steps" -> Steps
  | "depth" -> Depth
  | "work" -> Work
  | "use" -> Use
  | "data" -> Data
  | "shape" -> Shape
  | "permit" -> Permit
  | "tag" -> Tag
  | "make" -> Make
  | "erase" -> Erase
  | "once" -> Once
  | "many" -> Many
  | "unit" -> Unit
  | "bool" -> Bool
  | "int" -> Int
  | "sint" -> Sint
  | "uint" -> Uint
  | "bytes" -> Bytes
  | "vec" -> Vec
  | "seq" -> Seq
  | "cap" -> Cap
  | "result" -> Result
  | "res" -> Res
  | "let" -> Let
  | "in" -> In
  | "split" -> Split
  | "as" -> As
  | "if" -> If
  | "then" -> Then
  | "else" -> Else
  | "true" -> True
  | "false" -> False
  | "ok" -> Good
  | "err" -> Bad
  | "case" -> Case
  | "of" -> Of
  | "fst" -> Fst
  | "snd" -> Snd
  | "equal" -> Equal
  | "fit" -> Fit
  | "wide" -> Wide
  | "length" -> Length
  | "read" -> Read
  | "write" -> Write
  | "emit" -> Emit
  | "fail" -> Fail
  | "cat" -> Cat
  | "take" -> Take
  | "drop" -> Drop
  | "vcat" -> Vcat
  | "at" -> At
  | "uncons" -> Uncons
  | "fold" -> Fold
  | "every" -> Every
  | "some" -> Any
  | "count" -> Count
  | "total" -> Total
  | "weave" -> Weave
  | "braid" -> Braid
  | "loom" -> Loom
  | "orbit" -> Orbit
  | "wake" -> Wake
  | "rift" -> Rift
  | "from" -> From
  | "with" -> With
  | "step" -> Step
  | "close" -> Close
  | "max" -> Max
  | "abs" -> Abs
  | "recur" -> Hold Recur
  | "store" -> Hold Store
  | "closure" -> Hold Closure
  | "raise" -> Hold Raise
  | "dependent" -> Hold Depend
  | "polymark" -> Hold Polymark
  | value -> Ident value

let hold_text = function
  | Recur -> "recur"
  | Store -> "store"
  | Closure -> "closure"
  | Raise -> "raise"
  | Depend -> "dependent"
  | Polymark -> "polymark"

let form = function
  | Program -> F_program
  | Size -> F_size
  | Measure -> F_measure
  | Law -> F_law
  | Input -> F_input
  | Term -> F_term
  | Form -> F_form
  | Kind -> F_kind
  | Marks -> F_marks
  | Under -> F_under
  | Steps -> F_steps
  | Depth -> F_depth
  | Work -> F_work
  | Use -> F_use
  | Data -> F_data
  | Shape -> F_shape
  | Permit -> F_permit
  | Tag -> F_tag
  | Make -> F_make
  | Erase -> F_erase
  | Once -> F_once
  | Many -> F_many
  | Unit -> F_unit
  | Bool -> F_bool
  | Int -> F_int
  | Sint -> F_sint
  | Uint -> F_uint
  | Bytes -> F_bytes
  | Vec -> F_vec
  | Seq -> F_seq
  | Cap -> F_cap
  | Result -> F_result
  | Res -> F_res
  | Let -> F_let
  | In -> F_in
  | Split -> F_split
  | As -> F_as
  | If -> F_if
  | Then -> F_then
  | Else -> F_else
  | True -> F_true
  | False -> F_false
  | Good -> F_ok
  | Bad -> F_err
  | Case -> F_case
  | Of -> F_of
  | Fst -> F_fst
  | Snd -> F_snd
  | Equal -> F_equal
  | Fit -> F_fit
  | Wide -> F_wide
  | Length -> F_length
  | Read -> F_read
  | Write -> F_write
  | Emit -> F_emit
  | Fail -> F_fail
  | Cat -> F_cat
  | Take -> F_take
  | Drop -> F_drop
  | Vcat -> F_vcat
  | At -> F_at
  | Uncons -> F_uncons
  | Fold -> F_fold
  | Every -> F_every
  | Any -> F_some
  | Count -> F_count
  | Total -> F_total
  | Weave -> F_weave
  | Braid -> F_braid
  | Loom -> F_loom
  | Orbit -> F_orbit
  | Wake -> F_wake
  | Rift -> F_rift
  | From -> F_from
  | With -> F_with
  | Step -> F_step
  | Close -> F_close
  | Max -> F_max
  | Abs -> F_abs
  | Lbrace -> F_lbrace
  | Rbrace -> F_rbrace
  | Lparen -> F_lparen
  | Rparen -> F_rparen
  | Lbrack -> F_lbrack
  | Rbrack -> F_rbrack
  | Colon -> F_colon
  | Comma -> F_comma
  | Eq -> F_eq
  | EqEq -> F_eqeq
  | Ne -> F_ne
  | Lt -> F_lt
  | Le -> F_le
  | Gt -> F_gt
  | Ge -> F_ge
  | Arrow -> F_arrow
  | Thin -> F_thin
  | Bar -> F_bar
  | Plus -> F_plus
  | Minus -> F_minus
  | Star -> F_star
  | Slash -> F_slash
  | Percent -> F_percent
  | Ident _ -> F_ident
  | Nat _ -> F_nat
  | Hex _ -> F_hex
  | Hold item -> F_hold item
  | Eof -> F_eof

let form_text = function
  | F_program -> "program"
  | F_size -> "size"
  | F_measure -> "measure"
  | F_law -> "law"
  | F_input -> "input"
  | F_term -> "term"
  | F_form -> "form"
  | F_kind -> "kind"
  | F_marks -> "marks"
  | F_under -> "under"
  | F_steps -> "steps"
  | F_depth -> "depth"
  | F_work -> "work"
  | F_use -> "use"
  | F_data -> "data"
  | F_shape -> "shape"
  | F_permit -> "permit"
  | F_tag -> "tag"
  | F_make -> "make"
  | F_erase -> "erase"
  | F_once -> "once"
  | F_many -> "many"
  | F_unit -> "unit"
  | F_bool -> "bool"
  | F_int -> "int"
  | F_sint -> "sint"
  | F_uint -> "uint"
  | F_bytes -> "bytes"
  | F_vec -> "vec"
  | F_seq -> "seq"
  | F_cap -> "cap"
  | F_result -> "result"
  | F_res -> "res"
  | F_let -> "let"
  | F_in -> "in"
  | F_split -> "split"
  | F_as -> "as"
  | F_if -> "if"
  | F_then -> "then"
  | F_else -> "else"
  | F_true -> "true"
  | F_false -> "false"
  | F_ok -> "ok"
  | F_err -> "err"
  | F_case -> "case"
  | F_of -> "of"
  | F_fst -> "fst"
  | F_snd -> "snd"
  | F_equal -> "equal"
  | F_fit -> "fit"
  | F_wide -> "wide"
  | F_length -> "length"
  | F_read -> "read"
  | F_write -> "write"
  | F_emit -> "emit"
  | F_fail -> "fail"
  | F_cat -> "cat"
  | F_take -> "take"
  | F_drop -> "drop"
  | F_vcat -> "vcat"
  | F_at -> "at"
  | F_uncons -> "uncons"
  | F_fold -> "fold"
  | F_every -> "every"
  | F_some -> "some"
  | F_count -> "count"
  | F_total -> "total"
  | F_weave -> "weave"
  | F_braid -> "braid"
  | F_loom -> "loom"
  | F_orbit -> "orbit"
  | F_wake -> "wake"
  | F_rift -> "rift"
  | F_from -> "from"
  | F_with -> "with"
  | F_step -> "step"
  | F_close -> "close"
  | F_max -> "max"
  | F_abs -> "abs"
  | F_lbrace -> "{"
  | F_rbrace -> "}"
  | F_lparen -> "("
  | F_rparen -> ")"
  | F_lbrack -> "["
  | F_rbrack -> "]"
  | F_colon -> ":"
  | F_comma -> ","
  | F_eq -> "="
  | F_eqeq -> "=="
  | F_ne -> "!="
  | F_lt -> "<"
  | F_le -> "<="
  | F_gt -> ">"
  | F_ge -> ">="
  | F_arrow -> "=>"
  | F_thin -> "->"
  | F_bar -> "|"
  | F_plus -> "+"
  | F_minus -> "-"
  | F_star -> "*"
  | F_slash -> "/"
  | F_percent -> "%"
  | F_ident -> "name"
  | F_nat -> "number"
  | F_hex -> "hex"
  | F_hold item -> hold_text item
  | F_eof -> "end"

let item first state tok = { tok; span = span first state.pos }

let push state first tok out =
  let count = state.count + 1 in
  if count > token_max then
    Error { cause = Tokens (token_max, count); span = span first state.pos }
  else
    Ok ({ state with count }, item first state tok :: out)

let name state =
  let first = state.pos in
  let stop = seek alnum state in
  let len = stop.pos.off - first.off in
  if len > name_max then
    Error { cause = Name (name_max, len); span = span first stop.pos }
  else
    let value = String.sub state.src first.off len in
    Ok (stop, first, keyword value)

let number state =
  let first = state.pos in
  let rec walk digits under state =
    match char state with
    | Some char when digit char -> walk (digits + 1) false (next state)
    | Some '_' when digits > 0 && not under -> walk digits true (next state)
    | _ -> digits, under, state
  in
  let digits, under, stop = walk 0 false state in
  if under || digits = 0 || digits > number_max then
    Error { cause = Number; span = span first stop.pos }
  else
    let raw = String.sub state.src first.off (stop.pos.off - first.off) in
    let out = Buffer.create digits in
    String.iter (fun char -> if char <> '_' then Buffer.add_char out char) raw;
    try Ok (stop, first, Nat (Z.of_string (Buffer.contents out)))
    with Invalid_argument _ | Failure _ ->
      Error { cause = Number; span = span first stop.pos }

let hex_value = function
  | '0' .. '9' as char -> Char.code char - Char.code '0'
  | 'a' .. 'f' as char -> 10 + Char.code char - Char.code 'a'
  | 'A' .. 'F' as char -> 10 + Char.code char - Char.code 'A'
  | _ -> 0

let hex_text state =
  let first = state.pos in
  let body = next (next state) in
  let stop = seek hex body in
  let digits = stop.pos.off - body.pos.off in
  if digits mod 2 <> 0 then
    Error { cause = Hex_text; span = span first stop.pos }
  else
    let out = Bytes.create (digits / 2) in
    let rec fill at =
      if at = digits then ()
      else
        let high = hex_value (String.get state.src (body.pos.off + at)) in
        let low = hex_value (String.get state.src (body.pos.off + at + 1)) in
        Bytes.set out (at / 2) (Char.chr (high * 16 + low));
        fill (at + 2)
    in
    fill 0;
    Ok (stop, first, Hex (Bytes.unsafe_to_string out))

let punct state =
  let first = state.pos in
  match char state with
  | Some '{' -> Ok (next state, first, Lbrace, 1)
  | Some '}' -> Ok (next state, first, Rbrace, -1)
  | Some '(' -> Ok (next state, first, Lparen, 1)
  | Some ')' -> Ok (next state, first, Rparen, -1)
  | Some '[' -> Ok (next state, first, Lbrack, 1)
  | Some ']' -> Ok (next state, first, Rbrack, -1)
  | Some ':' -> Ok (next state, first, Colon, 0)
  | Some ',' -> Ok (next state, first, Comma, 0)
  | Some '|' -> Ok (next state, first, Bar, 0)
  | Some '+' -> Ok (next state, first, Plus, 0)
  | Some '-' when char_at state 1 = Some '>' ->
      Ok (next (next state), first, Thin, 0)
  | Some '-' -> Ok (next state, first, Minus, 0)
  | Some '*' -> Ok (next state, first, Star, 0)
  | Some '/' -> Ok (next state, first, Slash, 0)
  | Some '%' -> Ok (next state, first, Percent, 0)
  | Some '!' when char_at state 1 = Some '=' ->
      Ok (next (next state), first, Ne, 0)
  | Some '<' when char_at state 1 = Some '=' ->
      Ok (next (next state), first, Le, 0)
  | Some '<' -> Ok (next state, first, Lt, 0)
  | Some '>' when char_at state 1 = Some '=' ->
      Ok (next (next state), first, Ge, 0)
  | Some '>' -> Ok (next state, first, Gt, 0)
  | Some '=' when char_at state 1 = Some '>' ->
      Ok (next (next state), first, Arrow, 0)
  | Some '=' when char_at state 1 = Some '=' ->
      Ok (next (next state), first, EqEq, 0)
  | Some '=' -> Ok (next state, first, Eq, 0)
  | Some char ->
      Error { cause = Char (Char.code char); span = span first (next state).pos }
  | None -> Ok (state, first, Eof, 0)

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let rec tokens state out =
  let state = skip state in
  match char state with
  | None ->
      let first = state.pos in
      let* state, out = push state first Eof out in
      Ok (Array.of_list (List.rev out), state)
  | Some '0' when char_at state 1 = Some 'x' ->
      let* stop, first, tok = hex_text state in
      let* state, out = push { stop with depth = state.depth } first tok out in
      tokens state out
  | Some char when digit char ->
      let* stop, first, tok = number state in
      let* state, out = push { stop with depth = state.depth } first tok out in
      tokens state out
  | Some char when alpha char ->
      let* stop, first, tok = name state in
      let* state, out = push { stop with depth = state.depth } first tok out in
      tokens state out
  | Some _ ->
      let* stop, first, tok, delta = punct state in
      let depth = max 0 (state.depth + delta) in
      if depth > depth_max then
        Error { cause = Nest (depth_max, depth); span = span first stop.pos }
      else
        let* state, out = push { stop with depth } first tok out in
        tokens state out

let scan src =
  let len = String.length src in
  if len > source_max then
    Error { cause = Source (source_max, len); span = span zero zero }
  else
    match C_comments.erase src with
    | Error error ->
      let first = error.C_comments.first in
      let last = error.last in
      Error {
        cause = Comment;
        span = span
          (point first.off first.line first.col)
          (point last.off last.line last.col);
      }
    | Ok src ->
      let state = { src; len; pos = zero; depth = 0; count = 0 } in
      match tokens state [] with
      | Ok (items, _) -> Ok items
      | Error error -> Error error

let text error =
  let detail =
    match error.cause with
    | Source (limit, actual) ->
        Printf.sprintf "source size limit = %d actual = %d" limit actual
    | Tokens (limit, actual) ->
        Printf.sprintf "token count limit = %d actual = %d" limit actual
    | Name (limit, actual) ->
        Printf.sprintf "name size limit = %d actual = %d" limit actual
    | Number -> "invalid number"
    | Hex_text -> "invalid hex"
    | Comment -> "comment is not closed"
    | Nest (limit, actual) ->
        Printf.sprintf "syntax depth limit = %d actual = %d" limit actual
    | Char code -> Printf.sprintf "invalid source byte = %d" code
  in
  C_text.clip
    (Printf.sprintf "line = %d col = %d %s"
      error.span.first.line error.span.first.col detail)