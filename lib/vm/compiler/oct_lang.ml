(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type visibility = Public | Private | Internal

type declaration = ProgramDecl | ContractDecl | InterfaceDecl

let declaration_to_string = function
  | ProgramDecl -> "program"
  | ContractDecl -> "contract"
  | InterfaceDecl -> "interface"

type typ =
  | TInt
  | TBool
  | TString
  | TAddress
  | TBytes
  | TBytes32
  | TU64
  | TU128
  | TU256
  | TCipher
  | TPubKey
  | TMap of typ * typ
  | TList of typ
  | TStruct of string
  | TEnum of string
  | TOption of typ
  | TTuple of typ list
  | TVoid

type binop =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Gt | Le | Ge
  | And | Or

type unop = Neg | Not

type expr =
  | EInt of Z.t
  | EBool of bool
  | EString of string
  | EVar of string
  | EField of string
  | EIndex of string * expr list
  | EBinop of binop * expr * expr
  | EUnop of unop * expr
  | ECaller
  | EOrigin
  | ESelfAddr
  | EEpoch
  | EEpochTime
  | EValue
  | EBalance of expr
  | ETreeHash
  | ENodeId
  | ETxHash
  | ECall of string * expr list
  | EArray of expr list
  | ETuple of expr list
  | EStoragePath of string * expr list * string list
  | EFieldProp of string * string
  | EIndexField of string * expr list * string
  | EEnumVariant of string * string
  | ETernary of expr * expr * expr

type stmt =
  | SLet of string * typ option * expr
  | SAssign of string * expr
  | SFieldSet of string * expr
  | SIndexSet of string * expr list * expr
  | SReturn of expr option
  | SAssert of expr
  | SRequire of expr * expr
  | SEmit of string * expr list
  | SIf of expr * stmt list * (stmt list) option
  | SWhile of expr * stmt list
  | SFor of string * expr * expr * stmt list
  | SFieldCall of string * string * expr list
  | SStoragePathSet of string * expr list * string list * expr
  | SIndexFieldSet of string * expr list * string * expr
  | SForEach of string * string * stmt list
  | SMatch of expr * (string * string * stmt list) list
  | SLetTuple of string list * expr
  | SExpr of expr
  | SRevertError of string * expr list

type event_def = {
  ev_name : string;
  ev_fields : (string * typ * bool) list;
}

type error_def = {
  err_name : string;
  err_code : int;
  err_msg : string;
}

type refinement =
  | RGt of string * int
  | RGe of string * int
  | RLt of string * int
  | RLe of string * int
  | RNeq of string * int
  | RNonZero of string

type param = { p_name : string; p_typ : typ; p_refine : refinement option }

type effect =
  | EffStorageWrite
  | EffStorageRead
  | EffEmit
  | EffTransfer
  | EffCall
  | EffRevert

type func_def = {
  fn_name : string;
  fn_params : param list;
  fn_ret : typ;
  fn_view : bool;
  fn_pure : bool;
  fn_payable : bool;
  fn_nonreentrant : bool;
  fn_vis : visibility;
  fn_body : stmt list;
}

type state_field = {
  sf_name : string;
  sf_typ : typ;
}

type const_def = {
  c_name : string;
  c_typ : typ;
  c_value : expr;
}

type invariant_decl = {
  inv_name : string;
  inv_expr : expr;
}

type struct_def = {
  sd_name : string;
  sd_fields : (string * typ) list;
}

type enum_def = {
  en_name : string;
  en_variants : string list;
}

type import_decl = {
  imp_names : string list;
  imp_path : string;
}

type iface_method = {
  im_name : string;
  im_params : param list;
  im_ret : typ;
}

type interface_def = {
  if_name : string;
  if_methods : iface_method list;
}

type contract = {
  declaration : declaration;
  name : string;
  imports : import_decl list;
  structs : struct_def list;
  enums : enum_def list;
  consts : const_def list;
  invariants_decl : invariant_decl list;
  state : state_field list;
  events : event_def list;
  errors : error_def list;
  interfaces : interface_def list;
  implements : string list;
  ctor : func_def option;
  funcs : func_def list;
}

let typ_to_string = function
  | TInt -> "int" | TBool -> "bool" | TString -> "string"
  | TAddress -> "address" | TBytes -> "bytes" | TBytes32 -> "bytes32"
  | TU64 -> "u64" | TU128 -> "u128" | TU256 -> "u256"
  | TCipher -> "cipher" | TPubKey -> "pubkey"
  | TMap _ -> "map" | TList _ -> "list" | TStruct n -> n | TEnum n -> n
  | TOption _ -> "option" | TTuple _ -> "tuple" | TVoid -> "void"

type token =
  | TkProgram | TkContract | TkState | TkEvent | TkConstructor | TkFn | TkView | TkPure
  | TkLet | TkReturn | TkAssert | TkEmit | TkIf | TkElse | TkWhile
  | TkSelf | TkCaller | TkOrigin | TkEpoch | TkEpochTime | TkValue | TkBalance
  | TkTrue | TkFalse
  | TkTyInt | TkTyBool | TkTyString | TkTyAddress | TkTyBytes | TkTyBytes32
  | TkTyU64 | TkTyU128 | TkTyU256
  | TkTyCipher | TkTyPubKey | TkMap
  | TkTreeHash | TkNodeId | TkTxHash
  | TkFor | TkIn | TkDotDot | TkTyList
  | TkConst | TkRequire | TkStruct | TkEnum | TkMatch | TkFatArrow | TkSelfAddr
  | TkPublic | TkPrivate | TkInternal | TkPayable
  | TkError | TkRevert
  | TkOption | TkNone | TkSome | TkUnwrap | TkIsSome
  | TkInterface | TkImplements
  | TkImport
  | TkPlusEq | TkMinusEq | TkStarEq | TkSlashEq
  | TkQuestion | TkWhere
  | TkIdent of string
  | TkIntLit of Z.t
  | TkStrLit of string
  | TkLBrace | TkRBrace | TkLParen | TkRParen | TkLBrack | TkRBrack
  | TkColon | TkComma | TkDot | TkEq
  | TkPlus | TkMinus | TkStar | TkSlash | TkPercent
  | TkEqEq | TkBangEq | TkLt | TkGt | TkLtEq | TkGtEq
  | TkAmpAmp | TkPipePipe | TkBang
  | TkNewline | TkEOF