(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type key = {
  data : string;
  sampling : Pvac_ffi.sampling;
  words : int option;
  size : int option;
}

type cipher = {
  data : string;
  shape : Pvac_ffi.cipher_shape option;
  words : int option;
}

let key_size (key : key) =
  match key.size with Some size -> size | None -> failwith "fhe key size"

let key_words (key : key) =
  match key.words with Some words -> words | None -> failwith "fhe key words"

let cipher_words (cipher : cipher) =
  match cipher.words with Some words -> words | None -> failwith "fhe cipher words"

let cipher_shape (cipher : cipher) =
  match cipher.shape with Some shape -> shape | None -> failwith "fhe cipher shape"

let numbers values =
  let bytes = Bytes.create (List.length values * 8) in
  List.iteri (fun index value ->
    Bytes.set_int64_be bytes (index * 8) (Int64.of_int value)) values;
  Bytes.unsafe_to_string bytes

let integers count raw =
  if String.length raw <> count * 8 then Error "fhe metadata size"
  else
    let rec read index values =
      if index = count then Ok (List.rev values)
      else
        let value = String.get_int64_be raw (index * 8) in
        if value < Int64.of_int min_int || value > Int64.of_int max_int then
          Error "fhe metadata integer"
        else read (index + 1) (Int64.to_int value :: values) in
    read 0 []

let optional = function
  | -1 -> None
  | value when value >= 0 -> Some value
  | _ -> invalid_arg "fhe metadata value"

let key_meta (key : key) =
  let shape = key.sampling in
  numbers [shape.rows; shape.columns; shape.weight; shape.noise; shape.branches;
    Option.value key.words ~default:(-1); Option.value key.size ~default:(-1)]

let key_of_meta data raw =
  Result.bind (integers 7 raw) (function
    | [rows; columns; weight; noise; branches; words; size] ->
      begin try
        Ok {data; sampling = Pvac_ffi.{rows; columns; weight; noise; branches};
          words = optional words; size = optional size}
      with Invalid_argument _ -> Error "fhe key metadata" end
    | _ -> Error "fhe key metadata")

let cipher_meta (cipher : cipher) =
  let shape = match cipher.shape with
    | None -> [0; 0; 0; 0; 0; 0]
    | Some shape ->
      [1; shape.slots; shape.layers; shape.edges; shape.c0; shape.base_layers] in
  numbers (Option.value cipher.words ~default:(-1) :: shape)

let cipher_of_meta data raw =
  Result.bind (integers 7 raw) (function
    | [words; present; slots; layers; edges; c0; base_layers] ->
      begin try
        let shape = match present with
          | 0 when slots = 0 && layers = 0 && edges = 0 && c0 = 0 && base_layers = 0 -> None
          | 1 when slots >= 0 && layers >= 0 && edges >= 0 && c0 >= 0
              && base_layers >= 0 && base_layers <= layers ->
            Some Pvac_ffi.{slots; layers; edges; c0; base_layers}
          | _ -> invalid_arg "fhe cipher shape" in
        Ok {data; shape; words = optional words}
      with Invalid_argument _ -> Error "fhe cipher metadata" end
    | _ -> Error "fhe cipher metadata")

let describe read value =
  try Some (read value) with Invalid_argument _ | Failure _ -> None

let of_key key = {
  data = Pvac_ffi.serialize_pubkey key |> Bytes.to_string;
  sampling = Pvac_ffi.pubkey_sampling key;
  words = describe Pvac_ffi.pubkey_bit_words key;
  size = describe Pvac_ffi.pubkey_image_size key;
}

let of_cipher cipher = {
  data = Pvac_ffi.serialize_cipher cipher |> Bytes.to_string;
  shape = describe Pvac_ffi.cipher_shape cipher;
  words = describe Pvac_ffi.cipher_bit_words cipher;
}