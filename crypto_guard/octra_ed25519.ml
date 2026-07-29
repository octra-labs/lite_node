(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let p = Z.(sub (shift_left one 255) (of_int 19))

let z_of_le raw =
  let rec loop i acc =
    if i < 0 then acc
    else
      loop
        (i - 1)
        Z.(add (shift_left acc 8) (of_int (Char.code raw.[i])))
  in
  loop 31 Z.zero

let x_raw raw =
  if String.length raw <> 32 then None
  else
    let y_bytes = Bytes.of_string raw in
    Bytes.set_uint8 y_bytes 31 (Bytes.get_uint8 y_bytes 31 land 0x7f);
    let y = z_of_le (Bytes.unsafe_to_string y_bytes) in
    if Z.compare y p >= 0 then None
    else
      let den = Z.(erem (sub (add p one) y) p) in
      if Z.equal den Z.zero then None
      else
        let inv = Z.powm den Z.(sub p (of_int 2)) p in
        let u = Z.(erem (mul (erem (add one y) p) inv) p) in
        Some
          (String.init 32 (fun i ->
             Z.extract u (i * 8) 8 |> Z.to_int |> Char.chr))

let probe =
  match Mirage_crypto_ec.X25519.secret_of_octets (String.make 32 '\x42') with
  | Ok (secret, _) -> secret
  | Error _ -> failwith "invalid x25519 probe key"

let safe raw =
  try
    match Mirage_crypto_ec.Ed25519.pub_of_octets raw, x_raw raw with
    | Ok _, Some x ->
      begin
        match Mirage_crypto_ec.X25519.key_exchange probe x with
        | Ok _ -> true
        | Error _ -> false
      end
    | _ -> false
  with _ -> false

let verify ~pub ~msg signature =
  safe pub
  &&
  match Mirage_crypto_ec.Ed25519.pub_of_octets pub with
  | Ok key -> Mirage_crypto_ec.Ed25519.verify ~key ~msg signature
  | Error _ -> false