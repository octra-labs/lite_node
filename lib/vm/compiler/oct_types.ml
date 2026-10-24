(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Oct_lang

let numeric = function
  | TInt | TU64 | TU128 | TU256 | TEnum _ -> true
  | _ -> false

let text = function
  | TString | TAddress -> true
  | _ -> false

let opaque = function
  | TCipher | TPubKey -> true
  | _ -> false

let numeric_result left right =
  if left = TU256 || right = TU256 then TU256
  else if left = TU128 || right = TU128 then TU128
  else if left = TU64 || right = TU64 then TU64
  else TInt

module Graph = Type_graph.Host

let compatible expected actual =
  Graph.compare (fun node expected actual ->
    if numeric expected && numeric actual then Some []
    else match expected, actual with
     | TBytes, TString | TString, TBytes -> Some []
     | TStruct left, TStruct right
     | TStruct left, TEnum right | TEnum left, TStruct right ->
       if String.equal left right then Some [] else None
     | TOption _, TOption TVoid -> Some []
     | TOption _, TOption _
     | TList _, TList _ -> Some [node.Type_graph.at [0]]
     | TMap _, TMap _ -> Some [node.at [0]; node.at [1]]
     | TTuple _, TTuple _ -> node.items [0]
     | _ -> None) expected actual

let rec statement_returns = function
  | SLocated (_, _, statement) -> statement_returns statement
  | SReturn _ | SRevertError _ -> true
  | SIf (_, yes, Some no) -> block_returns yes && block_returns no
  | SMatch (_, arms) ->
    arms <> [] && List.for_all (fun (_, _, body) -> block_returns body) arms
  | SLet _
  | SLetTuple _
  | SAssign _
  | SFieldSet _
  | SIndexSet _
  | SIndexUpdate _
  | SAssert _
  | SRequire _
  | SEmit _
  | SIf (_, _, None)
  | SWhile _
  | SFor _
  | SFieldCall _
  | SStoragePathSet _
  | SStoragePathUpdate _
  | SIndexFieldSet _
  | SForEach _
  | SExpr _ -> false

and block_returns = function
  | [] -> false
  | statement :: rest -> statement_returns statement || block_returns rest

let statement_line = function
  | SLocated (line, _, _) -> line
  | _ -> 0

let block_line = function
  | statement :: _ -> statement_line statement
  | [] -> 0