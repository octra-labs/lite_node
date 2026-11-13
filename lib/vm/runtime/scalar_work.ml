(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let bit_limit = 65_536
let text_limit = 65_536

let consensus_id =
  "scalar_work:bits_65536:parse_chars_65536:scan_32:parse_square:decimal_square:load_16:minimum_1000000:wasm_integer:receipt_decimal"

let units length = Z.cdiv (Z.of_int length) (Z.of_int 32)

let scan length = units length

let parse length =
  let cells = units length in
  Z.add cells (Z.mul cells cells)

let decimal number =
  let cells = Z.of_int (1 + Z.numbits number / 64) in
  Z.add cells (Z.mul cells cells)

let valid number = Z.numbits number <= bit_limit

let text_ok text = String.length text <= text_limit

let load length = Z.mul (Z.of_int length) (Z.of_int 16)

let allowance active declared prior =
  if not active then prior
  else if Z.sign declared < 0 then 0
  else if Z.fits_int declared then min prior (max 1_000_000 (Z.to_int declared))
  else prior

let json_cost values =
  let rec loop total = function
    | [] -> Some total
    | `Intlit text :: rest ->
      if text_ok text then loop (Z.add total (parse (String.length text))) rest
      else None
    | `String text :: rest -> loop (Z.add total (scan (String.length text))) rest
    | (`Int _ | `Bool _ | `Null | `Float _) :: rest -> loop (Z.succ total) rest
    | _ -> None in
  loop Z.zero values

let decimal_equal number text =
  let length = String.length text in
  let bits = Z.numbits number in
  let digits = length - if Z.sign number < 0 then 1 else 0 in
  if digits <= 0 || digits > bits + 1 || bits > digits * 4 then false
  else String.equal (Z.to_string number) text