(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Oct_lang

type lexer = {
  src : string;
  len : int;
  mutable pos : int;
  mutable line : int;
  mutable col : int;
  mutable depth : int;
}

exception LexError of string * int * int

let create src = {
  src;
  len = String.length src;
  pos = 0;
  line = 1;
  col = 1;
  depth = 0;
}

let peek lx = if lx.pos < lx.len then Some lx.src.[lx.pos] else None

let advance lx =
  if lx.pos < lx.len then begin
    if lx.src.[lx.pos] = '\n' then (lx.line <- lx.line + 1; lx.col <- 1)
    else lx.col <- lx.col + 1;
    lx.pos <- lx.pos + 1
  end

let peek2 lx =
  if lx.pos + 1 < lx.len then Some lx.src.[lx.pos + 1] else None

let err lx msg = raise (LexError (msg, lx.line, lx.col))

let open_delimiter lx token =
  lx.depth <- lx.depth + 1;
  if lx.depth > Program_limits.max_parser_depth then
    err lx "syntax nesting exceeds limit";
  advance lx;
  token

let close_delimiter lx token =
  if lx.depth > 0 then lx.depth <- lx.depth - 1;
  advance lx;
  token

let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_digit c = c >= '0' && c <= '9'
let is_alnum c = is_alpha c || is_digit c

let skip_ws lx =
  let rec go () =
    match peek lx with
    | Some ' ' | Some '\t' | Some '\r' -> advance lx; go ()
    | _ -> ()
  in go ()

let skip_line_comment lx =
  let rec go () =
    match peek lx with
    | None | Some '\n' -> ()
    | _ -> advance lx; go ()
  in go ()

let skip_block_comment lx =
  advance lx; advance lx;
  let rec go depth =
    match peek lx with
    | None -> err lx "unterminated block comment"
    | Some '*' ->
      advance lx;
      (match peek lx with
       | Some '/' -> advance lx; if depth > 1 then go (depth - 1)
       | _ -> go depth)
    | Some '/' ->
      advance lx;
      (match peek lx with
       | Some '*' -> advance lx; go (depth + 1)
       | _ -> go depth)
    | _ -> advance lx; go depth
  in go 1

let read_string lx =
  advance lx;
  let buf = Buffer.create 32 in
  let rec go () =
    match peek lx with
    | None -> err lx "unterminated string"
    | Some '"' -> advance lx; Buffer.contents buf
    | Some '\\' ->
      advance lx;
      (match peek lx with
       | Some 'n' -> Buffer.add_char buf '\n'; advance lx; go ()
       | Some 't' -> Buffer.add_char buf '\t'; advance lx; go ()
       | Some '\\' -> Buffer.add_char buf '\\'; advance lx; go ()
       | Some '"' -> Buffer.add_char buf '"'; advance lx; go ()
       | _ -> err lx "invalid escape")
    | Some c -> Buffer.add_char buf c; advance lx; go ()
  in go ()

let read_number lx =
  let buf = Buffer.create 16 in
  let rec go () =
    match peek lx with
    | Some c when is_digit c -> Buffer.add_char buf c; advance lx; go ()
    | Some '_' -> advance lx; go ()
    | _ -> Z.of_string (Buffer.contents buf)
  in go ()

let read_ident lx =
  let buf = Buffer.create 16 in
  let rec go () =
    match peek lx with
    | Some c when is_alnum c -> Buffer.add_char buf c; advance lx; go ()
    | _ -> Buffer.contents buf
  in go ()

let keyword_map = [
  "program", TkProgram;
  "contract", TkContract;
  "state", TkState; "event", TkEvent;
  "constructor", TkConstructor; "fn", TkFn; "view", TkView; "pure", TkPure;
  "let", TkLet; "return", TkReturn; "assert", TkAssert;
  "emit", TkEmit; "if", TkIf; "else", TkElse; "while", TkWhile;
  "self", TkSelf; "caller", TkCaller; "origin", TkOrigin;
  "epoch", TkEpoch; "epoch_time", TkEpochTime; "value", TkValue; "balance", TkBalance;
  "true", TkTrue; "false", TkFalse;
  "int", TkTyInt; "bool", TkTyBool; "string", TkTyString;
  "address", TkTyAddress; "bytes", TkTyBytes; "bytes32", TkTyBytes32;
  "u64", TkTyU64; "u128", TkTyU128; "u256", TkTyU256; "uint", TkTyU256;
  "cipher", TkTyCipher; "pubkey", TkTyPubKey;
  "map", TkMap; "tree_hash", TkTreeHash; "node_id", TkNodeId; "tx_hash", TkTxHash;
  "for", TkFor; "in", TkIn; "list", TkTyList;
  "const", TkConst; "require", TkRequire; "struct", TkStruct;
  "enum", TkEnum; "match", TkMatch; "self_addr", TkSelfAddr;
  "public", TkPublic; "private", TkPrivate; "internal", TkInternal; "payable", TkPayable;
  "error", TkError; "revert", TkRevert; "where", TkWhere;
  "Option", TkOption; "option", TkOption; "None", TkNone; "none", TkNone; "Some", TkSome; "some", TkSome;
  "unwrap", TkUnwrap; "is_some", TkIsSome;
  "interface", TkInterface; "implements", TkImplements;
  "import", TkImport;
  "nonreentrant", TkIdent "nonreentrant";
  "log", TkIdent "log";
  "indexed", TkIdent "indexed";
]

let classify_ident s =
  match List.assoc_opt s keyword_map with
  | Some tk -> tk
  | None -> TkIdent s

let next_token lx =
  skip_ws lx;
  match peek lx with
  | None -> TkEOF
  | Some '\n' -> advance lx; TkNewline
  | Some '/' ->
    (match peek2 lx with
     | Some '/' -> skip_line_comment lx; TkNewline
     | Some '*' -> skip_block_comment lx; TkNewline
     | Some '=' -> advance lx; advance lx; TkSlashEq
     | _ -> advance lx; TkSlash)
  | Some '"' -> TkStrLit (read_string lx)
  | Some c when is_digit c -> TkIntLit (read_number lx)
  | Some c when is_alpha c ->
    let id = read_ident lx in
    classify_ident id
  | Some '{' -> open_delimiter lx TkLBrace
  | Some '}' -> close_delimiter lx TkRBrace
  | Some '(' -> open_delimiter lx TkLParen
  | Some ')' -> close_delimiter lx TkRParen
  | Some '[' -> open_delimiter lx TkLBrack
  | Some ']' -> close_delimiter lx TkRBrack
  | Some ':' -> advance lx; TkColon
  | Some ',' -> advance lx; TkComma
  | Some '.' ->
    advance lx;
    (match peek lx with
     | Some '.' -> advance lx; TkDotDot
     | _ -> TkDot)
  | Some '+' ->
    advance lx;
    (match peek lx with
     | Some '=' -> advance lx; TkPlusEq
     | _ -> TkPlus)
  | Some '-' ->
    advance lx;
    (match peek lx with
     | Some '=' -> advance lx; TkMinusEq
     | _ -> TkMinus)
  | Some '*' ->
    advance lx;
    (match peek lx with
     | Some '=' -> advance lx; TkStarEq
     | _ -> TkStar)
  | Some '%' -> advance lx; TkPercent
  | Some '=' ->
    advance lx;
    (match peek lx with
     | Some '=' -> advance lx; TkEqEq
     | Some '>' -> advance lx; TkFatArrow
     | _ -> TkEq)
  | Some '!' ->
    advance lx;
    (match peek lx with
     | Some '=' -> advance lx; TkBangEq
     | _ -> TkBang)
  | Some '<' ->
    advance lx;
    (match peek lx with
     | Some '=' -> advance lx; TkLtEq
     | _ -> TkLt)
  | Some '>' ->
    advance lx;
    (match peek lx with
     | Some '=' -> advance lx; TkGtEq
     | _ -> TkGt)
  | Some '?' -> advance lx; TkQuestion
  | Some '&' ->
    advance lx;
    (match peek lx with
     | Some '&' -> advance lx; TkAmpAmp
     | _ -> err lx "expected &&")
  | Some '|' ->
    advance lx;
    (match peek lx with
     | Some '|' -> advance lx; TkPipePipe
     | _ -> err lx "expected ||")
  | Some c ->
    if Char.code c > 127 then
      (advance lx; err lx (Printf.sprintf "unexpected character: 0x%02X" (Char.code c)))
    else
      err lx (Printf.sprintf "unexpected character: %c" c)

type token_stream = {
  lx : lexer;
  mutable cur : token;
  mutable peeked : bool;
}

let make_stream src =
  let lx = create src in
  { lx; cur = TkNewline; peeked = false }

let rec skip_newlines ts =
  if ts.cur = TkNewline then begin
    ts.cur <- next_token ts.lx;
    skip_newlines ts
  end

let peek_token ts =
  if not ts.peeked then begin
    ts.cur <- next_token ts.lx;
    ts.peeked <- true;
    skip_newlines ts
  end;
  ts.cur

let eat ts =
  ts.peeked <- false

let expect ts tk =
  let got = peek_token ts in
  if got <> tk then
    err ts.lx (Printf.sprintf "expected %s, got %s"
      (match tk with
       | TkLBrace -> "{" | TkRBrace -> "}" | TkLParen -> "(" | TkRParen -> ")"
       | TkLBrack -> "[" | TkRBrack -> "]" | TkColon -> ":" | TkComma -> ","
       | TkEq -> "=" | TkDot -> "." | TkDotDot -> ".."
       | TkIn -> "in" | TkFatArrow -> "=>" | _ -> "token")
      (match got with
       | TkIdent s -> s | TkIntLit z -> Z.to_string z | TkStrLit s -> "\"" ^ s ^ "\""
       | TkEOF -> "EOF" | TkNewline -> "newline" | _ -> "token"));
  eat ts

let current_line ts = ts.lx.line