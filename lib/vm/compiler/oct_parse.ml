(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Oct_lang
open Oct_lex

exception ParseError of string * int * int

let perr ts msg =
  raise (ParseError (msg, current_line ts, current_column ts))

let eat_newlines ts =
  let rec go () =
    let t = peek_token ts in
    if t = TkNewline then (eat ts; go ())
  in go ()

let expect_ident ts =
  match peek_token ts with
  | TkIdent s -> eat ts; s
  | TkValue -> eat ts; "value"
  | TkEpoch -> eat ts; "epoch"
  | TkEpochTime -> eat ts; "epoch_time"
  | TkBalance -> eat ts; "balance"
  | _ -> perr ts "expected identifier"

let ident = function
  | TkIdent _
  | TkValue
  | TkEpoch
  | TkEpochTime
  | TkBalance -> true
  | _ -> false

let parse_ast_int ts expected =
  match peek_token ts with
  | TkIntLit value
      when Z.leq value (Z.of_int Program_limits.max_ast_int) ->
    eat ts;
    Z.to_int value
  | TkIntLit _ -> perr ts "integer exceeds portable range"
  | _ -> perr ts expected

let parse_nat ts expected =
  match peek_token ts with
  | TkIntLit value ->
    eat ts;
    begin
      match C_nat.make value with
      | Some found -> found
      | None -> perr ts "natural exceeds fixed range"
    end
  | _ -> perr ts expected

let parse_type ts =
  let rec typ () = fst (product ())
  and product () =
    let left = base () in
    match peek_token ts with
    | TkStar ->
      eat ts;
      TTuple [left; typ ()], true
    | _ -> left, false
  and base () =
    match peek_token ts with
    | TkTyInt -> eat ts; TInt
    | TkTyBool -> eat ts; TBool
    | TkTyString -> eat ts; TString
    | TkTyAddress -> eat ts; TAddress
    | TkTyBytes ->
      eat ts;
      begin
        match peek_token ts with
        | TkLBrack ->
          expect ts TkLBrack;
          let size = parse_nat ts "expected bytes width" in
          expect ts TkRBrack;
          if C_nat.to_int size = 32 then TBytes32
          else perr ts "sized bytes type has no public ABI"
        | _ -> TBytes
      end
    | TkTyBytes32 -> eat ts; TBytes32
    | TkTyU64 -> eat ts; TU64
    | TkTyU128 -> eat ts; TU128
    | TkTyU256 -> eat ts; TU256
    | TkTyCipher -> eat ts; TCipher
    | TkTyPubKey -> eat ts; TPubKey
    | TkMap ->
      eat ts;
      expect ts TkLBrack;
      let k = typ () in
      expect ts TkRBrack;
      let v = base () in
      TMap (k, v)
    | TkTyList ->
      eat ts;
      expect ts TkLBrack;
      let t = typ () in
      expect ts TkRBrack;
      TList t
    | TkOption ->
      eat ts;
      expect ts TkLBrack;
      let t = typ () in
      expect ts TkRBrack;
      TOption t
    | TkLParen ->
      eat ts;
      let t1, paired = product () in
      let rec more acc =
        match peek_token ts with
        | TkComma -> eat ts; more (typ () :: acc)
        | TkRParen ->
          eat ts;
          begin
            match List.rev acc with
            | [] when paired -> t1
            | rest -> TTuple (t1 :: rest)
          end
        | _ -> perr ts "expected , or ) in tuple type"
      in
      more []
    | TkTyUint ->
      eat ts;
      begin
        match peek_token ts with
        | TkLBrack ->
          expect ts TkLBrack;
          let bits = parse_nat ts "expected uint width" in
          expect ts TkRBrack;
          begin
            match C_nat.to_int bits with
            | 64 -> TU64
            | 128 -> TU128
            | 256 -> TU256
            | _ -> perr ts "uint width has no public ABI"
          end
        | _ -> TU256
      end
    | TkIdent "sint" ->
      eat ts;
      begin
        match peek_token ts with
        | TkLBrack ->
          expect ts TkLBrack;
          let _ = parse_nat ts "expected sint width" in
          expect ts TkRBrack;
          perr ts "sint type has no public ABI"
        | _ -> TStruct "sint"
      end
    | TkIdent ("vec" as name) | TkIdent ("seq" as name) ->
      eat ts;
      begin
        match peek_token ts with
        | TkLBrack ->
          expect ts TkLBrack;
          let _ = parse_nat ts ("expected " ^ name ^ " size") in
          expect ts TkComma;
          let _ = typ () in
          expect ts TkRBrack;
          perr ts (name ^ " type has no public ABI")
        | _ -> TStruct name
      end
    | TkIdent name -> eat ts; TStruct name
    | _ -> perr ts "expected type"
  in typ ()

let parse_refinement ts name =
  match peek_token ts with
  | TkGt -> eat ts; Some (RGt (name, parse_ast_int ts "expected integer after >"))
  | TkGtEq ->
    eat ts;
    Some (RGe (name, parse_ast_int ts "expected integer after >="))
  | TkLt -> eat ts; Some (RLt (name, parse_ast_int ts "expected integer after <"))
  | TkLtEq ->
    eat ts;
    Some (RLe (name, parse_ast_int ts "expected integer after <="))
  | TkBangEq ->
    eat ts;
    Some (RNeq (name, parse_ast_int ts "expected integer after !="))
  | _ -> perr ts "expected comparison operator in where clause"

let parse_params ts =
  expect ts TkLParen;
  let rec go acc =
    match peek_token ts with
    | TkRParen -> eat ts; List.rev acc
    | _ ->
      if acc <> [] then expect ts TkComma;
      let name = expect_ident ts in
      if List.exists (fun param -> String.equal param.p_name name) acc then
        perr ts ("duplicate parameter name = " ^ name);
      expect ts TkColon;
      let typ = parse_type ts in
      let refine = match peek_token ts with
        | TkWhere -> eat ts;
          let refine_name = expect_ident ts in
          if not (String.equal name refine_name) then
            perr ts "refinement parameter differs";
          parse_refinement ts name
        | _ -> None
      in
      go ({ p_name = name; p_typ = typ; p_refine = refine } :: acc)
  in go []

let bp = function
  | TkPipePipe -> 1
  | TkAmpAmp -> 2
  | TkEqEq | TkBangEq -> 3
  | TkLt | TkGt | TkLtEq | TkGtEq -> 4
  | TkPlus | TkMinus -> 5
  | TkStar | TkSlash | TkPercent -> 6
  | _ -> 0

let tok_to_binop = function
  | TkPlus -> Add | TkMinus -> Sub | TkStar -> Mul
  | TkSlash -> Div | TkPercent -> Mod
  | TkEqEq -> Eq | TkBangEq -> Neq
  | TkLt -> Lt | TkGt -> Gt | TkLtEq -> Le | TkGtEq -> Ge
  | TkAmpAmp -> And | TkPipePipe -> Or
  | _ -> failwith "not a binop"

let expr_start = function
  | TkLet
  | TkIf
  | TkMinus
  | TkBang
  | TkIntLit _
  | TkTrue
  | TkFalse
  | TkStrLit _
  | TkCaller
  | TkOrigin
  | TkEpoch
  | TkEpochTime
  | TkValue
  | TkTreeHash
  | TkNodeId
  | TkTxHash
  | TkSelfAddr
  | TkNone
  | TkSome
  | TkUnwrap
  | TkIsSome
  | TkBalance
  | TkSelf
  | TkLBrack
  | TkLParen
  | TkIdent _
  | TkEmit -> true
  | _ -> false

let rec parse_expr_bp ts depth min_bp =
  if depth > Program_limits.max_parser_depth then
    perr ts "expression nesting exceeds limit";
  let lhs = parse_prefix ts depth in
  parse_infix ts depth lhs min_bp

and parse_infix ts depth lhs min_bp =
  let tok = peek_token ts in
  if tok = TkQuestion && min_bp <= 1 then begin
    eat ts;
    let then_e = parse_expr_bp ts (depth + 1) 1 in
    expect ts TkColon;
    let else_e = parse_expr_bp ts (depth + 1) 1 in
    parse_infix ts depth (ETernary (lhs, then_e, else_e)) min_bp
  end
  else
  let p = bp tok in
  if p > 0 && p >= min_bp then begin
    eat ts;
    let rhs = parse_expr_bp ts (depth + 1) (p + 1) in
    let node = EBinop (tok_to_binop tok, lhs, rhs) in
    parse_infix ts depth node min_bp
  end
  else lhs

and parse_prefix ts depth =
  let rec collect depth ops =
    match peek_token ts with
    | TkMinus -> eat ts; collect (depth + 1) (Neg :: ops)
    | TkBang -> eat ts; collect (depth + 1) (Not :: ops)
    | _ ->
      let value = parse_prefix_tail ts depth in
      List.fold_left (fun value op -> EUnop (op, value)) value ops
  in
  collect depth []

and parse_prefix_tail ts depth =
  match peek_token ts with
  | TkLet -> parse_term_let ts depth
  | TkIf -> parse_term_if ts depth
  | TkIdent "split" -> parse_term_split ts depth
  | TkIdent "orbit" ->
    eat ts;
    begin
      match peek_token ts with
      | TkLBrack -> parse_term_orbit ts depth
      | _ -> parse_named_tail ts "orbit"
    end
  | TkIdent "equal" ->
    eat ts;
    begin
      match peek_token ts with
      | TkLBrack -> parse_equal ts depth
      | _ -> parse_named_tail ts "equal"
    end
  | _ -> parse_primary ts

and parse_term_mult ts =
  match peek_token ts with
  | TkIdent "many" -> eat ts; Many
  | TkIdent "once" -> eat ts; Once
  | _ -> perr ts "expected many or once"

and parse_term_bind ts =
  let mult = parse_term_mult ts in
  let name = expect_ident ts in
  expect ts TkColon;
  name, mult, parse_type ts

and parse_term_word ts value =
  match peek_token ts with
  | TkIdent found when String.equal found value -> eat ts
  | _ -> perr ts ("expected " ^ value)

and parse_term_let ts depth =
  eat ts;
  let name, mult, typ = parse_term_bind ts in
  expect ts TkEq;
  let value = parse_expr_bp ts (depth + 1) 1 in
  expect ts TkIn;
  ELet (name, mult, typ, value, parse_expr_bp ts (depth + 1) 1)

and parse_term_if ts depth =
  eat ts;
  let guard = parse_expr_bp ts (depth + 1) 1 in
  parse_term_word ts "then";
  let yes = parse_expr_bp ts (depth + 1) 1 in
  expect ts TkElse;
  ETernary (guard, yes, parse_expr_bp ts (depth + 1) 1)

and parse_term_split ts depth =
  eat ts;
  match peek_token ts with
  | TkLParen ->
    let call = parse_call_expr ts "split" in
    begin
      match peek_token ts with
      | TkIdent "as" ->
        let value =
          match call with
          | ECall (_, [value]) -> value
          | ECall (_, values) -> ETuple values
          | _ -> perr ts "split call invariant differs"
        in
        parse_term_split_tail ts depth value
      | _ -> call
    end
  | token when expr_start token ->
    let value = parse_expr_bp ts (depth + 1) 1 in
    parse_term_split_tail ts depth value
  | _ -> parse_named_tail ts "split"

and parse_term_split_tail ts depth value =
  parse_term_word ts "as";
  let left = parse_term_bind ts in
  expect ts TkComma;
  let right = parse_term_bind ts in
  expect ts TkIn;
  ESplit (value, left, right, parse_expr_bp ts (depth + 1) 1)

and parse_term_orbit ts depth =
  expect ts TkLBrack;
  let count = parse_nat ts "expected orbit cap" in
  let turns =
    match peek_token ts with
    | TkComma -> eat ts; Some (parse_expr_bp ts (depth + 1) 1)
    | _ -> None
  in
  expect ts TkRBrack;
  parse_term_word ts "from";
  let seed = parse_expr_bp ts (depth + 1) 1 in
  parse_term_word ts "with";
  let bind = parse_term_bind ts in
  expect ts TkFatArrow;
  EOrbit (count, turns, seed, bind, parse_expr_bp ts (depth + 1) 1)

and parse_equal ts depth =
  expect ts TkLBrack;
  let typ = parse_type ts in
  expect ts TkRBrack;
  expect ts TkLParen;
  let left = parse_expr_bp ts (depth + 1) 1 in
  expect ts TkComma;
  let right = parse_expr_bp ts (depth + 1) 1 in
  expect ts TkRParen;
  EEqual (typ, left, right)

and parse_primary ts =
  match peek_token ts with
  | TkIntLit z -> eat ts; EInt z
  | TkTrue -> eat ts; EBool true
  | TkFalse -> eat ts; EBool false
  | TkStrLit s -> eat ts; EString s
  | TkCaller -> eat ts; ECaller
  | TkOrigin -> eat ts; EOrigin
  | TkEpoch -> parse_named ts "epoch"
  | TkEpochTime -> parse_named ts "epoch_time"
  | TkValue -> parse_named ts "value"
  | TkTreeHash -> eat ts; ETreeHash
  | TkNodeId -> eat ts; ENodeId
  | TkTxHash -> eat ts; ETxHash
  | TkSelfAddr -> eat ts; ESelfAddr
  | TkNone ->
    eat ts;
    (match peek_token ts with TkLParen -> eat ts; expect ts TkRParen | _ -> ());
    ECall ("none", [])
  | TkSome ->
    eat ts;
    expect ts TkLParen;
    let e = parse_expr ts in
    expect ts TkRParen;
    ECall ("some", [e])
  | TkUnwrap ->
    eat ts;
    expect ts TkLParen;
    let e = parse_expr ts in
    expect ts TkRParen;
    ECall ("unwrap", [e])
  | TkIsSome ->
    eat ts;
    expect ts TkLParen;
    let e = parse_expr ts in
    expect ts TkRParen;
    ECall ("is_some_opt", [e])
  | TkBalance -> parse_named ts "balance"
  | TkSelf ->
    eat ts;
    expect ts TkDot;
    let field = expect_ident ts in
    parse_self_access ts field
  | TkLBrack ->
    eat ts;
    let rec items acc =
      match peek_token ts with
      | TkRBrack -> eat ts; List.rev acc
      | _ ->
        if acc <> [] then expect ts TkComma;
        let e = parse_expr ts in
        items (e :: acc)
    in
    EArray (items [])
  | TkLParen ->
    eat ts;
    let e = parse_expr ts in
    begin
      match peek_token ts with
      | TkComma ->
        eat ts;
        let rec more acc =
          let e2 = parse_expr ts in
          match peek_token ts with
          | TkComma -> eat ts; more (e2 :: acc)
          | TkRParen -> eat ts; List.rev (e2 :: acc)
          | _ -> perr ts "expected , or ) in tuple"
        in
        ETuple (e :: more [])
      | _ ->
        expect ts TkRParen;
        e
    end
  | TkIdent "use" ->
    eat ts;
    if ident (peek_token ts) then parse_use ts else parse_named_tail ts "use"
  | TkIdent "write" ->
    eat ts;
    begin
      match peek_token ts with
      | TkLBrack -> parse_action_tail ts (fun kind -> C_eff.Write kind)
      | _ -> parse_named_tail ts "write"
    end
  | TkIdent "read" ->
    eat ts;
    begin
      match peek_token ts with
      | TkLBrack -> parse_action_tail ts (fun kind -> C_eff.Read kind)
      | _ -> parse_named_tail ts "read"
    end
  | TkIdent "fail" ->
    eat ts;
    begin
      match peek_token ts with
      | TkLBrack -> parse_action_tail ts (fun kind -> C_eff.Fail kind)
      | _ -> parse_named_tail ts "fail"
    end
  | TkEmit -> parse_action ts
  | TkIdent name -> parse_named ts name
  | _ -> perr ts "expected expression"

and parse_action ts =
  eat ts;
  parse_action_tail ts (fun kind -> C_eff.Emit kind)

and parse_action_tail ts make =
  expect ts TkLBrack;
  let kind = parse_nat ts "expected effect atom" in
  expect ts TkRBrack;
  expect ts TkLParen;
  let value = parse_expr ts in
  expect ts TkRParen;
  EAction (make kind, value)

and parse_named ts name =
  eat ts;
  parse_named_tail ts name

and parse_named_tail ts name =
  match peek_token ts with
  | TkLParen -> parse_call_expr ts name
  | TkDot ->
    eat ts;
    let variant = expect_ident ts in
    EEnumVariant (name, variant)
  | _ -> EVar name

and parse_use ts =
  let name = expect_ident ts in
  expect ts TkLBrack;
  let rec caps out =
    match peek_token ts with
    | TkRBrack -> eat ts; List.rev out
    | _ ->
      if out <> [] then expect ts TkComma;
      caps (parse_expr ts :: out)
  in
  let captures = caps [] in
  expect ts TkLParen;
  let arg = parse_expr ts in
  expect ts TkRParen;
  begin
    match peek_token ts with
    | TkIdent "as" -> eat ts
    | _ -> perr ts "expected as"
  end;
  let mult =
    match peek_token ts with
    | TkIdent "once" -> eat ts; Once
    | TkIdent "many" -> eat ts; Many
    | _ -> perr ts "expected many or once"
  in
  let bind = expect_ident ts in
  expect ts TkColon;
  let typ = parse_type ts in
  expect ts TkIn;
  EUse {
    ux_name = name;
    ux_caps = captures;
    ux_arg = arg;
    ux_mult = mult;
    ux_bind = bind;
    ux_typ = typ;
    ux_body = parse_expr ts;
  }

and parse_self_access ts field =
  let keys =
    match peek_token ts with
    | TkLBrack -> parse_index_chain ts
    | _ -> []
  in
  match peek_token ts with
  | TkDot ->
    eat ts;
    let first = expect_ident ts in
    EStoragePath (field, keys, parse_storage_path_tail ts first)
  | _ ->
    if keys = [] then EField field else EIndex (field, keys)

and parse_index_chain ts =
  let rec go acc =
    match peek_token ts with
    | TkLBrack ->
      eat ts;
      let k = parse_expr ts in
      expect ts TkRBrack;
      go (k :: acc)
    | _ -> List.rev acc
  in go []

and parse_storage_path_tail ts first =
  let rec go acc =
    match peek_token ts with
    | TkDot ->
      eat ts;
      go (expect_ident ts :: acc)
    | _ -> List.rev acc
  in
  go [first]

and parse_call_expr ts name =
  expect ts TkLParen;
  let rec go acc =
    match peek_token ts with
    | TkRParen -> eat ts; List.rev acc
    | _ ->
      if acc <> [] then expect ts TkComma;
      let e = parse_expr ts in
      go (e :: acc)
  in
  ECall (name, go [])

and parse_expr ts = parse_expr_bp ts 0 1

let is_stmt_end ts =
  match peek_token ts with
  | TkNewline | TkRBrace | TkEOF -> true
  | _ -> false

let skip_stmt_end ts =
  let rec go () =
    match peek_token ts with
    | TkNewline -> eat ts; go ()
    | _ -> ()
  in go ()

let parse_revert ts =
  eat ts;
  let name = expect_ident ts in
  expect ts TkLParen;
  let rec go acc =
    match peek_token ts with
    | TkRParen -> eat ts; List.rev acc
    | _ ->
      if acc <> [] then expect ts TkComma;
      let e = parse_expr ts in
      go (e :: acc)
  in
  SRevertError (name, go [])

let rec parse_stmt ts =
  skip_stmt_end ts;
  let token = peek_token ts in
  let line = current_line ts in
  let column = current_column ts in
  let statement =
    match token with
    | TkLet -> parse_let ts
    | TkReturn -> parse_return ts
    | TkAssert -> parse_assert ts
    | TkRequire -> parse_require ts
    | TkEmit -> parse_emit ts
    | TkIf -> parse_if ts
    | TkWhile -> parse_while ts
    | TkFor -> parse_for ts
    | TkMatch -> parse_match ts
    | TkRevert -> parse_revert ts
    | TkSelf -> parse_self_stmt ts
    | TkIdent "log" ->
      eat ts;
      expect ts TkLParen;
      let rec args acc =
        match peek_token ts with
        | TkRParen -> eat ts; List.rev acc
        | _ ->
          if acc <> [] then expect ts TkComma;
          let e = parse_expr ts in
          args (e :: acc)
      in
      SEmit ("Log", args [])
    | TkIdent _ | TkEpoch | TkEpochTime | TkValue | TkBalance ->
      parse_ident_stmt ts
    | _ -> perr ts "expected statement"
  in
  SLocated (line, column, statement)

and parse_let ts =
  eat ts;
  match peek_token ts with
  | TkLParen ->
    eat ts;
    let rec names acc =
      let n = expect_ident ts in
      if List.exists (String.equal n) acc then
        perr ts ("duplicate tuple binding name = " ^ n);
      match peek_token ts with
      | TkComma -> eat ts; names (n :: acc)
      | TkRParen -> eat ts; List.rev (n :: acc)
      | _ -> perr ts "expected , or ) in tuple destructuring"
    in
    let ns = names [] in
    expect ts TkEq;
    let e = parse_expr ts in
    SLetTuple (ns, e)
  | _ ->
    let name = expect_ident ts in
    let typ =
      match peek_token ts with
      | TkColon -> eat ts; Some (parse_type ts)
      | _ -> None
    in
    expect ts TkEq;
    let e = parse_expr ts in
    SLet (name, typ, e)

and parse_return ts =
  eat ts;
  if is_stmt_end ts then SReturn None
  else
    let e = parse_expr ts in
    SReturn (Some e)

and parse_assert ts =
  eat ts;
  let e = parse_expr ts in
  SAssert e

and parse_require ts =
  eat ts;
  expect ts TkLParen;
  let cond = parse_expr ts in
  expect ts TkComma;
  let msg = parse_expr ts in
  expect ts TkRParen;
  SRequire (cond, msg)

and parse_emit ts =
  eat ts;
  let name = expect_ident ts in
  expect ts TkLParen;
  let rec args acc =
    match peek_token ts with
    | TkRParen -> eat ts; List.rev acc
    | _ ->
      if acc <> [] then expect ts TkComma;
      let e = parse_expr ts in
      args (e :: acc)
  in
  SEmit (name, args [])

and parse_if ts =
  eat ts;
  let cond = parse_expr ts in
  let body = parse_block ts in
  let els =
    skip_stmt_end ts;
    match peek_token ts with
    | TkElse ->
      eat ts;
      (match peek_token ts with
       | TkIf -> [parse_if ts]
       | _ -> parse_block ts)
    | _ -> []
  in
  SIf (cond, body, if els = [] then None else Some els)

and parse_while ts =
  eat ts;
  let cond = parse_expr ts in
  let body = parse_block ts in
  SWhile (cond, body)

and parse_for ts =
  eat ts;
  let name = expect_ident ts in
  expect ts TkIn;
  match peek_token ts with
  | TkSelf ->
    eat ts;
    expect ts TkDot;
    let field = expect_ident ts in
    let body = parse_block ts in
    SForEach (name, field, body)
  | _ ->
    let start_e = parse_expr ts in
    expect ts TkDotDot;
    let end_e = parse_expr ts in
    let body = parse_block ts in
    SFor (name, start_e, end_e, body)

and parse_match ts =
  eat ts;
  let e = parse_expr ts in
  expect ts TkLBrace;
  let rec go acc =
    skip_stmt_end ts;
    match peek_token ts with
    | TkRBrace -> eat ts; List.rev acc
    | _ ->
      let enum_name = expect_ident ts in
      expect ts TkDot;
      let variant = expect_ident ts in
      expect ts TkFatArrow;
      let body = match peek_token ts with
        | TkLBrace -> parse_block ts
        | _ -> [parse_stmt ts]
      in
      go ((enum_name, variant, body) :: acc)
  in
  SMatch (e, go [])

and parse_block ts =
  skip_stmt_end ts;
  expect ts TkLBrace;
  let rec go acc =
    skip_stmt_end ts;
    match peek_token ts with
    | TkRBrace -> eat ts; List.rev acc
    | _ -> go (parse_stmt ts :: acc)
  in go []

and parse_self_stmt ts =
  eat ts;
  expect ts TkDot;
  let field = expect_ident ts in
  let keys =
    match peek_token ts with
    | TkLBrack -> Some (parse_index_chain ts)
    | _ -> None
  in
  match keys, peek_token ts with
  | Some keys, TkDot ->
    eat ts;
    let first = expect_ident ts in
    let path = parse_storage_path_tail ts first in
    (match peek_token ts with
     | TkPlusEq | TkMinusEq | TkStarEq | TkSlashEq ->
       let op = match compound_op ts with Some o -> o | None -> assert false in
       let e = parse_expr ts in
       SStoragePathUpdate (field, keys, path, op, e)
     | _ ->
       expect ts TkEq;
       let e = parse_expr ts in
       SStoragePathSet (field, keys, path, e))
  | Some keys, _ ->
    (match peek_token ts with
     | TkPlusEq | TkMinusEq | TkStarEq | TkSlashEq ->
       let op = match compound_op ts with Some o -> o | None -> assert false in
       let e = parse_expr ts in
       SIndexUpdate (field, keys, op, e)
     | _ ->
       expect ts TkEq;
       let e = parse_expr ts in
       SIndexSet (field, keys, e))
  | None, TkDot ->
    eat ts;
    let first = expect_ident ts in
    (match peek_token ts with
     | TkLParen ->
       expect ts TkLParen;
       let rec go acc =
         match peek_token ts with
         | TkRParen -> eat ts; List.rev acc
         | _ ->
           if acc <> [] then expect ts TkComma;
           let e = parse_expr ts in
           go (e :: acc)
       in
       SFieldCall (field, first, go [])
     | _ ->
       let path = parse_storage_path_tail ts first in
       (match peek_token ts with
        | TkPlusEq | TkMinusEq | TkStarEq | TkSlashEq ->
          let op = match compound_op ts with Some o -> o | None -> assert false in
          let e = parse_expr ts in
          SStoragePathUpdate (field, [], path, op, e)
        | _ ->
          expect ts TkEq;
          let e = parse_expr ts in
          SStoragePathSet (field, [], path, e)))
  | None, _ ->
    (match peek_token ts with
    | TkEq ->
      eat ts;
      let e = parse_expr ts in
      SFieldSet (field, e)
    | TkPlusEq | TkMinusEq | TkStarEq | TkSlashEq ->
      let op = match compound_op ts with Some o -> o | None -> assert false in
      let e = parse_expr ts in
      SFieldSet (field, EBinop (op, EField field, e))
    | _ -> perr ts "expected =, [ or . after self.field")

and compound_op ts =
  match peek_token ts with
  | TkPlusEq -> eat ts; Some Add
  | TkMinusEq -> eat ts; Some Sub
  | TkStarEq -> eat ts; Some Mul
  | TkSlashEq -> eat ts; Some Div
  | _ -> None

and parse_ident_stmt ts =
  let name = expect_ident ts in
  match peek_token ts with
  | TkEq ->
    eat ts;
    let e = parse_expr ts in
    SAssign (name, e)
  | TkPlusEq | TkMinusEq | TkStarEq | TkSlashEq ->
    let op = match (compound_op ts) with Some o -> o | None -> assert false in
    let e = parse_expr ts in
    SAssign (name, EBinop (op, EVar name, e))
  | TkLParen ->
    let call = parse_call_expr ts name in
    SExpr call
  | _ ->
    perr ts "expected = or ( after identifier"

let parse_state ts =
  expect ts TkLBrace;
  let rec go acc =
    skip_stmt_end ts;
    match peek_token ts with
    | TkRBrace -> eat ts; List.rev acc
    | _ ->
      let name = expect_ident ts in
      if List.exists (fun field -> String.equal field.sf_name name) acc then
        perr ts ("duplicate state field name = " ^ name);
      expect ts TkColon;
      let typ = parse_type ts in
      (match peek_token ts with TkComma -> eat ts | _ -> ());
      go ({ sf_name = name; sf_typ = typ } :: acc)
  in go []

let parse_event ts =
  let name = expect_ident ts in
  expect ts TkLParen;
  let rec go acc =
    match peek_token ts with
    | TkRParen -> eat ts; List.rev acc
    | _ ->
      if acc <> [] then expect ts TkComma;
      let fname = expect_ident ts in
      if List.exists (fun (name, _, _) -> String.equal name fname) acc then
        perr ts ("duplicate event field name = " ^ fname);
      expect ts TkColon;
      let indexed = match peek_token ts with
        | TkIdent "indexed" -> eat ts; true
        | _ -> false
      in
      let typ = parse_type ts in
      go ((fname, typ, indexed) :: acc)
  in
  { ev_name = name; ev_fields = go [] }

let parse_function ts is_view is_pure is_payable ?(nonreentrant = false) vis =
  eat ts;
  let name = expect_ident ts in
  let params = parse_params ts in
  let ret =
    match peek_token ts with
    | TkColon -> eat ts; parse_type ts
    | _ -> TVoid
  in
  let body = parse_block ts in
  { fn_name = name; fn_params = params; fn_ret = ret;
    fn_view = is_view; fn_pure = is_pure; fn_payable = is_payable;
    fn_nonreentrant = nonreentrant; fn_vis = vis; fn_body = body }

let parse_mult ts =
  match peek_token ts with
  | TkIdent "many" -> eat ts; Many
  | TkIdent "once" -> eat ts; Once
  | _ -> perr ts "expected many or once"

let parse_form_param ts =
  let mult = parse_mult ts in
  let name = expect_ident ts in
  expect ts TkColon;
  { fp_name = name; fp_typ = parse_type ts; fp_mult = mult }

let parse_form_axis ts name =
  begin
    match peek_token ts with
    | TkIdent value when String.equal value name -> eat ts
    | _ -> perr ts ("expected " ^ name)
  end;
  expect ts TkLBrack;
  let value =
    match peek_token ts with
    | TkIntLit value ->
      eat ts;
      begin
        match C_nat.make value with
        | Some value -> C_nat.to_z value
        | None -> perr ts "direct form limit is invalid"
      end
    | _ -> perr ts "expected direct form limit"
  in
  expect ts TkRBrack;
  value

let parse_form_limit ts =
  match peek_token ts with
  | TkIdent "under" ->
    eat ts;
    expect ts TkLBrace;
    let steps = parse_form_axis ts "steps" in
    expect ts TkComma;
    let depth = parse_form_axis ts "depth" in
    expect ts TkComma;
    let work = parse_form_axis ts "work" in
    expect ts TkRBrace;
    Some (C_limit.make3 ~steps ~depth ~work)
  | _ -> None

let parse_form_mark ts make =
  eat ts;
  expect ts TkLBrack;
  let kind = parse_nat ts "expected effect atom" in
  expect ts TkRBrack;
  expect ts TkColon;
  let target = expect_ident ts in
  { mk_atom = make kind; mk_target = target }

let parse_form_marks ts =
  expect ts TkLBrace;
  let rec walk out =
    match peek_token ts with
    | TkRBrace -> eat ts; List.rev out
    | _ ->
      if out <> [] then expect ts TkComma;
      begin
        match peek_token ts with
        | TkEmit ->
          walk (parse_form_mark ts (fun kind -> C_eff.Emit kind) :: out)
        | TkIdent "write" ->
          walk (parse_form_mark ts (fun kind -> C_eff.Write kind) :: out)
        | TkIdent "read" ->
          walk (parse_form_mark ts (fun kind -> C_eff.Read kind) :: out)
        | TkIdent "fail" ->
          walk (parse_form_mark ts (fun kind -> C_eff.Fail kind) :: out)
        | _ -> perr ts "direct form mark is unsupported"
      end
  in
  walk []

let parse_form_tail ts line column name params public =
  expect ts TkMinus;
  expect ts TkGt;
  expect ts TkLBrack;
  let mult = parse_mult ts in
  expect ts TkRBrack;
  let ret = parse_type ts in
  begin
    match peek_token ts with
    | TkIdent "marks" -> eat ts
    | _ -> perr ts "expected marks"
  end;
  let marks = parse_form_marks ts in
  let lim = parse_form_limit ts in
  expect ts TkEq;
  {
    fm_name = name;
    fm_params = params;
    fm_ret = ret;
    fm_mult = mult;
    fm_marks = marks;
    fm_lim = lim;
    fm_body = parse_expr ts;
    fm_public = public;
    fm_line = line;
    fm_column = column;
  }

let parse_form ts =
  let line = current_line ts in
  let column = current_column ts in
  eat ts;
  let name = expect_ident ts in
  expect ts TkLBrack;
  let rec captures out =
    match peek_token ts with
    | TkRBrack -> eat ts; List.rev out
    | _ ->
      if out <> [] then expect ts TkComma;
      captures (parse_form_param ts :: out)
  in
  let caps = captures [] in
  expect ts TkLParen;
  let arg = parse_form_param ts in
  expect ts TkRParen;
  parse_form_tail ts line column name (caps @ [arg]) false

let parse_main ts =
  let line = current_line ts in
  let column = current_column ts in
  eat ts;
  expect ts TkLParen;
  let rec params out =
    match peek_token ts with
    | TkRParen -> eat ts; List.rev out
    | _ ->
      if out <> [] then expect ts TkComma;
      params (parse_form_param ts :: out)
  in
  parse_form_tail ts line column "main" (params []) true

let parse_constructor ts =
  let params = parse_params ts in
  let body = parse_block ts in
  { fn_name = "constructor"; fn_params = params; fn_ret = TVoid;
    fn_view = false; fn_pure = false; fn_payable = true; fn_nonreentrant = false;
    fn_vis = Public; fn_body = body }

let parse_const ts =
  let name = expect_ident ts in
  let typ = match peek_token ts with
    | TkColon -> eat ts; parse_type ts
    | _ -> TVoid
  in
  expect ts TkEq;
  let value = parse_expr ts in
  { c_name = name; c_typ = typ; c_value = value }

let parse_invariant ts =
  eat ts;
  let name = expect_ident ts in
  expect ts TkEq;
  let expr = parse_expr ts in
  { inv_name = name; inv_expr = expr }

let parse_struct_def ts =
  let name = expect_ident ts in
  expect ts TkLBrace;
  let rec go acc =
    skip_stmt_end ts;
    match peek_token ts with
    | TkRBrace -> eat ts; List.rev acc
    | _ ->
      let fname = expect_ident ts in
      if List.exists (fun (name, _) -> String.equal name fname) acc then
        perr ts ("duplicate struct field name = " ^ fname);
      expect ts TkColon;
      let typ = parse_type ts in
      (match peek_token ts with TkComma -> eat ts | _ -> ());
      go ((fname, typ) :: acc)
  in
  { sd_name = name; sd_fields = go [] }

let parse_error_def ts =
  let name = expect_ident ts in
  expect ts TkLParen;
  let code = parse_ast_int ts "expected error code (integer)" in
  expect ts TkComma;
  let msg = match peek_token ts with
    | TkStrLit s -> eat ts; s
    | _ -> perr ts "expected error message (string)" in
  expect ts TkRParen;
  { err_name = name; err_code = code; err_msg = msg }

let parse_enum_def ts =
  let name = expect_ident ts in
  expect ts TkLBrace;
  let rec go acc =
    skip_stmt_end ts;
    match peek_token ts with
    | TkRBrace -> eat ts; List.rev acc
    | _ ->
      if acc <> [] then
        (match peek_token ts with TkComma -> eat ts | _ -> ());
      skip_stmt_end ts;
      let v = expect_ident ts in
      if List.exists (String.equal v) acc then
        perr ts ("duplicate enum variant name = " ^ v);
      go (v :: acc)
  in
  { en_name = name; en_variants = go [] }

let parse_iface_method ts =
  eat ts;
  let name = expect_ident ts in
  let params = parse_params ts in
  let ret = match peek_token ts with
    | TkColon -> eat ts; parse_type ts
    | _ -> TVoid
  in
  { im_name = name; im_params = params; im_ret = ret }

let parse_interface ts =
  eat ts;
  let name = expect_ident ts in
  expect ts TkLBrace;
  let rec go acc =
    skip_stmt_end ts;
    match peek_token ts with
    | TkRBrace -> eat ts; List.rev acc
    | TkFn ->
      let item = parse_iface_method ts in
      if List.exists (fun prior -> String.equal prior.im_name item.im_name) acc then
        perr ts ("duplicate interface method name = " ^ item.im_name);
      go (item :: acc)
    | _ -> perr ts "expected fn or } in interface"
  in
  { if_name = name; if_methods = go [] }

let parse_implements ts =
  eat ts;
  let rec go acc =
    let name = expect_ident ts in
    match peek_token ts with
    | TkComma -> eat ts; go (name :: acc)
    | _ -> List.rev (name :: acc)
  in
  go []

let parse_import ts =
  eat ts;
  let rec parse_names acc =
    let name = expect_ident ts in
    match peek_token ts with
    | TkComma -> eat ts; parse_names (name :: acc)
    | _ -> List.rev (name :: acc)
  in
  let names = parse_names [] in
  (match peek_token ts with
   | TkIdent "from" -> eat ts
   | _ -> perr ts "expected 'from' after import names");
  let path = match peek_token ts with
    | TkStrLit s -> eat ts; s
    | _ -> perr ts "expected string path after from"
  in
  { imp_names = names; imp_path = path }

let rec parse_fn_modifiers ts ~vis ~is_view ~is_pure ~is_payable ~nonreentrant funcs =
  match peek_token ts with
  | TkView -> eat ts; parse_fn_modifiers ts ~vis ~is_view:true ~is_pure ~is_payable ~nonreentrant funcs
  | TkPure -> eat ts; parse_fn_modifiers ts ~vis ~is_view ~is_pure:true ~is_payable ~nonreentrant funcs
  | TkPayable -> eat ts; parse_fn_modifiers ts ~vis ~is_view:false ~is_pure:false ~is_payable:true ~nonreentrant funcs
  | TkIdent "nonreentrant" -> eat ts; parse_fn_modifiers ts ~vis ~is_view ~is_pure ~is_payable ~nonreentrant:true funcs
  | TkFn ->
    let fn = parse_function ts is_view is_pure is_payable ~nonreentrant vis in
    if List.exists (fun prior -> String.equal prior.fn_name fn.fn_name) !funcs then
      perr ts ("duplicate function name = " ^ fn.fn_name);
    funcs := fn :: !funcs
  | _ -> perr ts "expected fn after modifiers"

let parse_contract ts =
  skip_stmt_end ts;
  let imports = ref [] in
  let rec parse_imports () =
    skip_stmt_end ts;
    match peek_token ts with
    | TkImport -> imports := parse_import ts :: !imports; parse_imports ()
    | _ -> ()
  in
  parse_imports ();
  let ifaces = ref [] in
  let rec parse_ifaces () =
    skip_stmt_end ts;
    match peek_token ts with
    | TkInterface ->
      let item = parse_interface ts in
      ifaces := item :: !ifaces;
      parse_ifaces ()
    | _ -> ()
  in
  parse_ifaces ();
  match peek_token ts with
  | TkEOF ->
    { declaration = InterfaceDecl; name = ""; imports = List.rev !imports;
      structs = []; enums = []; consts = []; invariants_decl = []; state = [];
      events = []; errors = []; interfaces = List.rev !ifaces;
      implements = []; ctor = None; funcs = []; forms = [] }
  | _ ->
  let declaration =
    match peek_token ts with
    | TkProgram -> eat ts; ProgramDecl
    | TkIdent "Program" -> eat ts; ProgramDecl
    | TkContract -> eat ts; ContractDecl
    | TkIdent "Contract" -> eat ts; ContractDecl
    | _ -> perr ts "expected program or contract"
  in
  let name = expect_ident ts in
  let impls = match peek_token ts with
    | TkImplements -> parse_implements ts
    | _ -> []
  in
  expect ts TkLBrace;
  let structs = ref [] in
  let enums = ref [] in
  let consts = ref [] in
  let invariants_decl = ref [] in
  let state = ref [] in
  let state_seen = ref false in
  let events = ref [] in
  let errors = ref [] in
  let ctor = ref None in
  let funcs = ref [] in
  let forms = ref [] in
  let rec go () =
    skip_stmt_end ts;
    match peek_token ts with
    | TkRBrace -> eat ts
    | TkStruct ->
      eat ts;
      let item = parse_struct_def ts in
      if
        List.exists (fun prior -> String.equal prior.sd_name item.sd_name) !structs
        || List.exists (fun prior -> String.equal prior.en_name item.sd_name) !enums
      then perr ts ("duplicate type name = " ^ item.sd_name);
      structs := item :: !structs;
      go ()
    | TkEnum ->
      eat ts;
      let item = parse_enum_def ts in
      if
        List.exists (fun prior -> String.equal prior.en_name item.en_name) !enums
        || List.exists (fun prior -> String.equal prior.sd_name item.en_name) !structs
      then perr ts ("duplicate type name = " ^ item.en_name);
      enums := item :: !enums;
      go ()
    | TkConst ->
      eat ts;
      let item = parse_const ts in
      if List.exists (fun prior -> String.equal prior.c_name item.c_name) !consts then
        perr ts ("duplicate constant name = " ^ item.c_name);
      consts := item :: !consts;
      go ()
    | TkIdent "invariant" ->
      let item = parse_invariant ts in
      if
        List.exists
          (fun prior -> String.equal prior.inv_name item.inv_name)
          !invariants_decl
      then perr ts ("duplicate invariant name = " ^ item.inv_name);
      invariants_decl := item :: !invariants_decl;
      go ()
    | TkState ->
      eat ts;
      if !state_seen then perr ts "duplicate state declaration";
      state_seen := true;
      state := parse_state ts;
      go ()
    | TkError ->
      eat ts;
      let item = parse_error_def ts in
      if List.exists (fun prior -> String.equal prior.err_name item.err_name) !errors then
        perr ts ("duplicate error name = " ^ item.err_name);
      errors := item :: !errors;
      go ()
    | TkEvent ->
      eat ts;
      let item = parse_event ts in
      if List.exists (fun prior -> String.equal prior.ev_name item.ev_name) !events then
        perr ts ("duplicate event name = " ^ item.ev_name);
      events := item :: !events;
      go ()
    | TkConstructor ->
      eat ts;
      if Option.is_some !ctor then perr ts "duplicate constructor";
      ctor := Some (parse_constructor ts);
      go ()
    | TkPublic ->
      eat ts;
      if peek_token ts = TkIdent "main" then begin
        if declaration <> ProgramDecl then
          perr ts "main declaration requires Program";
        if
          List.exists (fun prior -> String.equal prior.fm_name "main") !forms
          || List.exists (fun prior -> String.equal prior.fn_name "main") !funcs
        then perr ts "duplicate main declaration";
        forms := parse_main ts :: !forms
      end else
        parse_fn_modifiers ts ~vis:Public ~is_view:false ~is_pure:false
          ~is_payable:false ~nonreentrant:false funcs;
      go ()
    | TkPrivate | TkInternal ->
      let vis = match peek_token ts with
        | TkPrivate -> eat ts; Private
        | TkInternal -> eat ts; Internal
        | _ -> Public in
      parse_fn_modifiers ts ~vis ~is_view:false ~is_pure:false ~is_payable:false ~nonreentrant:false funcs;
      go ()
    | TkView ->
      eat ts;
      parse_fn_modifiers ts ~vis:Public ~is_view:true ~is_pure:false ~is_payable:false ~nonreentrant:false funcs;
      go ()
    | TkPure ->
      eat ts;
      parse_fn_modifiers ts ~vis:Public ~is_view:false ~is_pure:true ~is_payable:false ~nonreentrant:false funcs;
      go ()
    | TkPayable ->
      eat ts;
      parse_fn_modifiers ts ~vis:Public ~is_view:false ~is_pure:false ~is_payable:true ~nonreentrant:false funcs;
      go ()
    | TkIdent "nonreentrant" ->
      eat ts;
      parse_fn_modifiers ts ~vis:Public ~is_view:false ~is_pure:false ~is_payable:false ~nonreentrant:true funcs;
      go ()
    | TkFn ->
      let fn = parse_function ts false false false Public in
      if List.exists (fun prior -> String.equal prior.fn_name fn.fn_name) !funcs then
        perr ts ("duplicate function name = " ^ fn.fn_name);
      funcs := fn :: !funcs;
      go ()
    | TkIdent "form" ->
      if declaration <> ProgramDecl then
        perr ts "form declarations require Program";
      let form = parse_form ts in
      if List.exists (fun prior -> String.equal prior.fm_name form.fm_name) !forms
      then perr ts ("duplicate form name = " ^ form.fm_name);
      forms := form :: !forms;
      go ()
    | TkIdent "input" | TkIdent "term" when declaration = ProgramDecl ->
      perr ts "stateful or callable entry requires public main"
    | _ -> perr ts "expected struct, enum, const, state, event, constructor, fn, or }"
  in
  go ();
  { declaration;
    name;
    imports = List.rev !imports;
    structs = List.rev !structs;
    enums = List.rev !enums;
    consts = List.rev !consts;
    invariants_decl = List.rev !invariants_decl;
    state = !state;
    events = List.rev !events;
    errors = List.rev !errors;
    interfaces = List.rev !ifaces;
    implements = impls;
    ctor = !ctor;
    funcs = List.rev !funcs;
    forms = List.rev !forms }

let syntax source =
  let ts = make_stream source in
  let parsed = parse_contract ts in
  begin
    match peek_token ts with
    | TkEOF -> parsed
    | _ -> perr ts "trailing source after declaration"
  end

let parse ?(resolve_import = fun _names _path -> None) source =
  let ct = syntax source in
  let merged = List.fold_left (fun acc imp ->
    match resolve_import imp.imp_names imp.imp_path with
    | Some imported ->
      { acc with
        structs = imported.structs @ acc.structs;
        enums = imported.enums @ acc.enums;
        consts = imported.consts @ acc.consts;
        events = imported.events @ acc.events;
        errors = imported.errors @ acc.errors;
        interfaces = imported.interfaces @ acc.interfaces;
        funcs = imported.funcs @ acc.funcs;
        forms = imported.forms @ acc.forms }
    | None -> acc
  ) ct ct.imports in
  Oct_scope.resolve merged