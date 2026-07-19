(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


open Oct_lang
open Oct_lex

exception ParseError of string * int

let perr ts msg = raise (ParseError (msg, current_line ts))

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

let parse_type ts =
  let rec base () =
    match peek_token ts with
    | TkTyInt -> eat ts; TInt
    | TkTyBool -> eat ts; TBool
    | TkTyString -> eat ts; TString
    | TkTyAddress -> eat ts; TAddress
    | TkTyBytes -> eat ts; TBytes
    | TkTyBytes32 -> eat ts; TBytes32
    | TkTyU64 -> eat ts; TU64
    | TkTyU128 -> eat ts; TU128
    | TkTyU256 -> eat ts; TU256
    | TkTyCipher -> eat ts; TCipher
    | TkTyPubKey -> eat ts; TPubKey
    | TkMap ->
      eat ts;
      expect ts TkLBrack;
      let k = base () in
      expect ts TkRBrack;
      let v = base () in
      TMap (k, v)
    | TkTyList ->
      eat ts;
      expect ts TkLBrack;
      let t = base () in
      expect ts TkRBrack;
      TList t
    | TkOption ->
      eat ts;
      expect ts TkLBrack;
      let t = base () in
      expect ts TkRBrack;
      TOption t
    | TkLParen ->
      eat ts;
      let t1 = base () in
      let rec more acc =
        match peek_token ts with
        | TkComma -> eat ts; more (base () :: acc)
        | TkRParen -> eat ts; List.rev acc
        | _ -> perr ts "expected , or ) in tuple type"
      in
      TTuple (t1 :: more [])
    | TkIdent name -> eat ts; TStruct name
    | _ -> perr ts "expected type"
  in base ()

let parse_refinement ts name =
  match peek_token ts with
  | TkGt -> eat ts; (match peek_token ts with TkIntLit n -> eat ts; Some (RGt (name, Z.to_int n)) | _ -> perr ts "expected integer after >")
  | TkGtEq -> eat ts; (match peek_token ts with TkIntLit n -> eat ts; Some (RGe (name, Z.to_int n)) | _ -> perr ts "expected integer after >=")
  | TkLt -> eat ts; (match peek_token ts with TkIntLit n -> eat ts; Some (RLt (name, Z.to_int n)) | _ -> perr ts "expected integer after <")
  | TkLtEq -> eat ts; (match peek_token ts with TkIntLit n -> eat ts; Some (RLe (name, Z.to_int n)) | _ -> perr ts "expected integer after <=")
  | TkBangEq -> eat ts; (match peek_token ts with TkIntLit n -> eat ts; Some (RNeq (name, Z.to_int n)) | _ -> perr ts "expected integer after !=")
  | _ -> perr ts "expected comparison operator in where clause"

let parse_params ts =
  expect ts TkLParen;
  let rec go acc =
    match peek_token ts with
    | TkRParen -> eat ts; List.rev acc
    | _ ->
      if acc <> [] then expect ts TkComma;
      let name = expect_ident ts in
      expect ts TkColon;
      let typ = parse_type ts in
      let refine = match peek_token ts with
        | TkWhere -> eat ts;
          let _ref_name = expect_ident ts in
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

let rec parse_expr_bp ts min_bp =
  let lhs = parse_prefix ts in
  parse_infix ts lhs min_bp

and parse_infix ts lhs min_bp =
  let tok = peek_token ts in

  if tok = TkQuestion && min_bp <= 1 then begin
    eat ts;
    let then_e = parse_expr_bp ts 1 in
    expect ts TkColon;
    let else_e = parse_expr_bp ts 1 in
    parse_infix ts (ETernary (lhs, then_e, else_e)) min_bp
  end
  else
  let p = bp tok in
  if p > 0 && p >= min_bp then begin
    eat ts;
    let rhs = parse_expr_bp ts (p + 1) in
    let node = EBinop (tok_to_binop tok, lhs, rhs) in
    parse_infix ts node min_bp
  end
  else lhs

and parse_prefix ts =
  match peek_token ts with
  | TkMinus ->
    eat ts;
    let e = parse_prefix ts in
    EUnop (Neg, e)
  | TkBang ->
    eat ts;
    let e = parse_prefix ts in
    EUnop (Not, e)
  | _ -> parse_primary ts

and parse_primary ts =
  match peek_token ts with
  | TkIntLit z -> eat ts; EInt z
  | TkTrue -> eat ts; EBool true
  | TkFalse -> eat ts; EBool false
  | TkStrLit s -> eat ts; EString s
  | TkCaller -> eat ts; ECaller
  | TkOrigin -> eat ts; EOrigin
  | TkEpoch -> eat ts; EEpoch
  | TkEpochTime -> eat ts; EEpochTime
  | TkValue -> eat ts; EValue
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
  | TkBalance ->
    eat ts;
    expect ts TkLParen;
    let e = parse_expr ts in
    expect ts TkRParen;
    EBalance e
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
    (match peek_token ts with
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
       e)
  | TkIdent name ->
    eat ts;
    (match peek_token ts with
     | TkLParen -> parse_call_expr ts name
     | TkDot ->
       eat ts;
       let variant = expect_ident ts in
       EEnumVariant (name, variant)
     | _ -> EVar name)
  | _ -> perr ts "expected expression"

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

and parse_expr ts = parse_expr_bp ts 1

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
  match peek_token ts with
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
  | TkIdent _ -> parse_ident_stmt ts
  | _ -> perr ts "expected statement"

and parse_let ts =
  eat ts;
  match peek_token ts with
  | TkLParen ->

    eat ts;
    let rec names acc =
      let n = expect_ident ts in
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
       SStoragePathSet (field, keys, path, EBinop (op, EStoragePath (field, keys, path), e))
     | _ ->
       expect ts TkEq;
       let e = parse_expr ts in
       SStoragePathSet (field, keys, path, e))
  | Some keys, _ ->
    (match peek_token ts with
     | TkPlusEq | TkMinusEq | TkStarEq | TkSlashEq ->
       let op = match compound_op ts with Some o -> o | None -> assert false in
       let e = parse_expr ts in
       SIndexSet (field, keys, EBinop (op, EIndex (field, keys), e))
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
          SStoragePathSet (field, [], path, EBinop (op, EStoragePath (field, [], path), e))
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
      expect ts TkColon;

      let indexed = match peek_token ts with
        | TkIdent "indexed" -> eat ts; true
        | _ -> false
      in
      let typ = parse_type ts in
      go ((fname, typ, indexed) :: acc)
  in
  { ev_name = name; ev_fields = go [] }

let parse_function ts is_view is_pure is_payable ?(nonreentrant=false) vis =
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
      expect ts TkColon;
      let typ = parse_type ts in
      (match peek_token ts with TkComma -> eat ts | _ -> ());
      go ((fname, typ) :: acc)
  in
  { sd_name = name; sd_fields = go [] }

let parse_error_def ts =
  let name = expect_ident ts in
  expect ts TkLParen;
  let code = match peek_token ts with
    | TkIntLit z -> eat ts; Z.to_int z
    | _ -> perr ts "expected error code (integer)" in
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
    | TkFn -> go (parse_iface_method ts :: acc)
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
  | TkFn -> funcs := parse_function ts is_view is_pure is_payable ~nonreentrant vis :: !funcs
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
    | TkInterface -> ifaces := parse_interface ts :: !ifaces; parse_ifaces ()
    | _ -> ()
  in
  parse_ifaces ();

  match peek_token ts with
  | TkEOF ->
    { declaration = InterfaceDecl; name = ""; imports = List.rev !imports;
      structs = []; enums = []; consts = []; invariants_decl = []; state = [];
      events = []; errors = []; interfaces = List.rev !ifaces;
      implements = []; ctor = None; funcs = [] }
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
  let events = ref [] in
  let errors = ref [] in
  let ctor = ref None in
  let funcs = ref [] in
  let rec go () =
    skip_stmt_end ts;
    match peek_token ts with
    | TkRBrace -> eat ts
    | TkStruct -> eat ts; structs := parse_struct_def ts :: !structs; go ()
    | TkEnum -> eat ts; enums := parse_enum_def ts :: !enums; go ()
    | TkConst -> eat ts; consts := parse_const ts :: !consts; go ()
    | TkIdent "invariant" -> invariants_decl := parse_invariant ts :: !invariants_decl; go ()
    | TkState -> eat ts; state := parse_state ts; go ()
    | TkError -> eat ts; errors := parse_error_def ts :: !errors; go ()
    | TkEvent -> eat ts; events := parse_event ts :: !events; go ()
    | TkConstructor -> eat ts; ctor := Some (parse_constructor ts); go ()
    | TkPublic | TkPrivate | TkInternal ->
      let vis = match peek_token ts with
        | TkPublic -> eat ts; Public
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
    | TkFn -> funcs := parse_function ts false false false Public :: !funcs; go ()
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
    funcs = List.rev !funcs }

let parse ?(resolve_import = fun _names _path -> None) source =
  let ts = make_stream source in
  let ct = parse_contract ts in

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
        funcs = imported.funcs @ acc.funcs }
    | None -> acc
  ) ct ct.imports in
  merged