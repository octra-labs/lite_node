(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let hash tag payload =
  let h = Digestif.SHA256.init () in
  let h = Digestif.SHA256.feed_string h tag in
  let h = Digestif.SHA256.feed_string h "\x00" in
  let h = Digestif.SHA256.feed_string h payload in
  Digestif.SHA256.get h |> Digestif.SHA256.to_raw_string

let hash_hex tag payload =
  let raw = hash tag payload in
  let buf = Buffer.create 64 in
  String.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))) raw;
  Buffer.contents buf

let hash_encoded tag encode_fn =
  let payload = Oce1.encode encode_fn in
  hash tag payload

let hash_encoded_hex tag encode_fn =
  let payload = Oce1.encode encode_fn in
  hash_hex tag payload

let nil_hash = String.make 32 '\x00'

let is_nil h = h = nil_hash