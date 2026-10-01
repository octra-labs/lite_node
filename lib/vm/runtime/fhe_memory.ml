(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = { mutable spent : Z.t }

let limit = Z.of_int 536_870_912
let image_factor = Z.of_int 16
let key_factor = Z.of_int 8
let key_scratch = Z.of_int 270_336
let public_cells = Z.of_int 65_536
let cell_bytes = Z.of_int 64
let key_io_bytes = Z.of_int 64
let consensus_id = String.concat ":" [
  "fhe_retained_volume"; Z.to_string limit; Z.to_string image_factor;
  Z.to_string key_factor; Z.to_string key_scratch; Z.to_string public_cells; Z.to_string cell_bytes;
  "key_io"; Z.to_string key_io_bytes;
]

let create () = {spent = Z.zero}
let used budget = budget.spent

let reserve budget amount =
  if Z.sign amount < 0 || Z.gt amount (Z.sub limit budget.spent) then false
  else (budget.spent <- Z.add budget.spent amount; true)

let image size = Z.mul image_factor (Z.of_int size)

let key_image raw =
  let length = String.length raw in
  if length = 0 then None
  else
    if Char.code raw.[0] <> 0xec then Some (Z.of_int length)
      else if length < 5 then None
      else
        let size = ref Z.zero in
        for index = 1 to 4 do
          size := Z.add (Z.shift_left !size 8) (Z.of_int (Char.code raw.[index]))
        done;
        if Z.gt !size (Z.of_int 33_554_432)
           || Z.gt !size (Z.mul (Z.of_int 64) (Z.of_int (length - 5))) then None
        else Some !size

let key_decode raw =
  Option.map (fun size -> Z.add key_scratch
    (Z.mul key_factor (Z.add (Z.of_int (String.length raw)) size))) (key_image raw)

let key_effort volume =
  let cost = Z.cdiv volume key_io_bytes in
  if Z.sign cost < 0 || not (Z.fits_int cost) then None else Some (Z.to_int cost)

let key_read_effort raw =
  Option.bind (key_image raw) (fun size ->
    key_effort (Z.add (Z.of_int (String.length raw)) size))

let key_write_effort key =
  key_effort (Z.of_int (Pvac_ffi.pubkey_image_size key))

let key_value key =
  Z.add key_scratch (Z.mul key_factor (Z.of_int (Pvac_ffi.pubkey_image_size key)))

let cipher_decode raw =
  Z.add (Z.of_int 4096)
    (Z.add (image (String.length raw)) (Z.mul public_cells cell_bytes))