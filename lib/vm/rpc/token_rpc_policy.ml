(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type meta = {
  address : string;
  owner : string;
  symbol : string;
  name : string;
  decimals : string;
  total_supply : string;
}

type page = {
  address : string;
  offset : int;
  limit : int;
  skip : int;
  rows_rev : Yojson.Safe.t list;
  count : int;
  row_bytes : int;
  limited : bool;
}

type add_result =
  | Continue of page
  | Complete of page

let max_scan_programs = 1_024
let max_page_rows = 128
let max_page_bytes = 131_072
let page_overhead_bytes = 512
let max_u128 = Z.sub (Z.shift_left Z.one 128) Z.one

let trim limit value =
  if String.length value <= limit then value
  else String.sub value 0 limit

let digits value =
  String.length value > 0
  && String.for_all (function '0' .. '9' -> true | _ -> false) value

let unsigned ~digits_max ~value_max value =
  if String.length value > digits_max || not (digits value) then None
  else
    try
      let parsed = Z.of_string value in
      if Z.compare parsed value_max <= 0 then Some value else None
    with _ ->
      None

let u128 value =
  unsigned ~digits_max:39 ~value_max:max_u128 value

let encrypted_balance value =
  if not (Octra_core.Pvac_verify_policy.ciphertext_allowed value) then false
  else
    try
      ignore (Base64.decode_exn value);
      true
    with _ ->
      false

let decimal_places value =
  match unsigned ~digits_max:2 ~value_max:(Z.of_int 18) value with
  | Some valid -> Some valid
  | None -> None

let meta ~address ~owner ~symbol ~name ~decimals ~total_supply =
  let symbol = trim 32 symbol in
  let name = trim 64 (Option.value ~default:symbol name) in
  match
    decimal_places (Option.value ~default:"0" decimals),
    u128 (Option.value ~default:"0" total_supply)
  with
  | Some decimals, Some total_supply
    when not (String.equal symbol "") && not (String.equal symbol "0") ->
    Some {
      address;
      owner = trim 64 owner;
      symbol;
      name;
      decimals;
      total_supply;
    }
  | _ ->
    None

let row (token : meta) ~balance =
  let balance_kind =
    match u128 balance with
    | Some value when String.equal value "0" -> None
    | Some value -> Some (value, "plain", false)
    | None when encrypted_balance balance -> Some (balance, "encrypted", true)
    | None -> None
  in
  match balance_kind with
  | None -> None
  | Some (balance, kind, encrypted) ->
    Some (`Assoc [
      "address", `String token.address;
      "name", `String token.name;
      "symbol", `String token.symbol;
      "total_supply", `String token.total_supply;
      "balance", `String balance;
      "balance_kind", `String kind;
      "balance_is_encrypted", `Bool encrypted;
      "decimals", `String token.decimals;
      "owner", `String token.owner;
    ])

let address (token : meta) =
  token.address

let empty_page ~address ~offset ~limit =
  {
    address;
    offset;
    limit = min max_page_rows (max 1 limit);
    skip = max 0 offset;
    rows_rev = [];
    count = 0;
    row_bytes = 0;
    limited = false;
  }

let add page row =
  if page.skip > 0 then
    Continue { page with skip = page.skip - 1 }
  else if page.count >= page.limit then
    Complete { page with limited = true }
  else
    let bytes = String.length (Yojson.Safe.to_string row) in
    let next_bytes = page.row_bytes + bytes + 1 in
    if next_bytes + page_overhead_bytes > max_page_bytes then
      Complete { page with limited = true }
    else
      Continue {
        page with
        rows_rev = row :: page.rows_rev;
        count = page.count + 1;
        row_bytes = next_bytes;
      }

let payload limited page =
  let limited = limited || page.limited in
  let next_offset =
    if limited then `Int (page.offset + page.count) else `Null
  in
  `Assoc [
    "address", `String page.address;
    "tokens", `List (List.rev page.rows_rev);
    "count", `Int page.count;
    "offset", `Int page.offset;
    "next_offset", next_offset;
    "limited", `Bool limited;
  ]

let rec fit limited page =
  let value = payload limited page in
  if String.length (Yojson.Safe.to_string value) <= max_page_bytes then value
  else
    match page.rows_rev with
    | [] -> payload true page
    | row :: rows_rev ->
      let bytes = String.length (Yojson.Safe.to_string row) + 1 in
      fit true {
        page with
        rows_rev;
        count = page.count - 1;
        row_bytes = max 0 (page.row_bytes - bytes);
      }

let finish ~limited page =
  fit limited page

let page_bytes value =
  String.length (Yojson.Safe.to_string value)