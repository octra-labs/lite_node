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