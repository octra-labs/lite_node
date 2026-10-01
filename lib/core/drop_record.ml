(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  hash : string;
  from_addr : string;
  to_addr : string;
  nonce : int;
  ou : Z.t;
  op_type : Transaction.op_type;
  reason : string;
  detail : string;
  dropped_at : float;
}

let validate row =
  if String.length row.hash <> 64
     || not (String.for_all (function '0'..'9' | 'a'..'f' -> true | _ -> false) row.hash)
     || String.length row.from_addr > 256
     || String.length row.to_addr > 256
     || row.nonce < 0 || Z.sign row.ou < 0
     || String.length row.reason > 65_536 || String.length row.detail > 65_536
     || not (Float.is_finite row.dropped_at) || row.dropped_at < 0.
  then invalid_arg "invalid local drop record"

let encode row =
  validate row;
  let bytes = Yojson.Safe.to_string (`List [
    `String row.hash; `String row.from_addr; `String row.to_addr;
    `Int row.nonce; `String (Z.to_string row.ou);
    `String (Transaction.op_type_to_string row.op_type);
    `String row.reason; `String row.detail; `Float row.dropped_at
  ]) in
  if String.length bytes > 65_536 then invalid_arg "local drop record exceeds size limit";
  bytes

let decode bytes =
  if String.length bytes > 65_536 then invalid_arg "local drop record exceeds size limit";
  match Yojson.Safe.from_string bytes with
  | `List [`String hash; `String from_addr; `String to_addr; `Int nonce;
           `String ou; `String op; `String reason; `String detail; `Float dropped_at] ->
    let op_type = match Transaction.op_type_of_string op with
      | Ok value -> value
      | Error _ -> invalid_arg "invalid local drop operation" in
    let row = {hash; from_addr; to_addr; nonce; ou = Z.of_string ou;
      op_type; reason; detail; dropped_at} in
    validate row;
    row
  | _ -> invalid_arg "invalid local drop encoding"

let order_key row =
  let time = if row.dropped_at = 0. then 0. else row.dropped_at in
  Printf.sprintf "%016Lx:%s" (Int64.bits_of_float time) row.hash

let addresses row =
  List.filter (fun addr -> addr <> "")
    (if row.from_addr = row.to_addr then [row.from_addr]
     else [row.from_addr; row.to_addr])

let newest ~limit rows =
  List.iter validate rows;
  let ordered = List.stable_sort (fun left right ->
    let time = Float.compare left.dropped_at right.dropped_at in
    if time <> 0 then time else String.compare left.hash right.hash) rows in
  let rec skip count rows =
    match rows with
    | _ :: rest when count > 0 -> skip (count - 1) rest
    | _ -> rows in
  skip (max 0 (List.length ordered - limit)) ordered