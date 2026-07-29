(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  chain_id : string;
  node_id : string;
  node_addr : string;
  pubkey : string;
  binary_hash : string;
  config_hash : string;
  host : string;
  port : int;
  features : string list;
  ts : int64;
  signature : string;
}

type verdict =
  | Valid
  | Invalid of string

let proto = 1

let max_chain_id_bytes = 128
let max_node_id_bytes = 64
let max_node_addr_bytes = 128
let max_host_bytes = 255
let max_features = 32
let max_feature_bytes = 64
let max_records = 32

let node_id_of_pubkey pubkey =
  Hash_domain.hash_hex "octra:node_id:v1" pubkey

let transport_config_hash ~config_hash ~binary_hash =
  Hash_domain.hash_encoded "octra:p2p_transport_config:v1" (fun buf ->
    Oce1.put_hash32 buf config_hash;
    Oce1.put_hash32 buf binary_hash)

let sign_bytes r =
  Hash_domain.hash_encoded "octra:p2p_peer_record:v1" (fun buf ->
    Oce1.put_u16 buf proto;
    Oce1.put_string buf r.chain_id;
    Oce1.put_string buf r.node_id;
    Oce1.put_string buf r.node_addr;
    Oce1.put_raw buf r.pubkey;
    Oce1.put_hash32 buf r.binary_hash;
    Oce1.put_hash32 buf r.config_hash;
    Oce1.put_string buf r.host;
    Oce1.put_u16 buf r.port;
    Oce1.put_list Oce1.put_string buf r.features;
    Oce1.put_u64 buf r.ts)

let make ~chain_id ~node_addr ~pubkey ~binary_hash ~config_hash ~host ~port ~features ~ts ~sign_fn =
  let node_id = node_id_of_pubkey pubkey in
  let base = {
    chain_id;
    node_id;
    node_addr;
    pubkey;
    binary_hash;
    config_hash;
    host;
    port;
    features;
    ts;
    signature = String.make 64 '\x00';
  } in
  { base with signature = sign_fn (sign_bytes base) }

let put buf r =
  Oce1.put_u16 buf proto;
  Oce1.put_string buf r.chain_id;
  Oce1.put_string buf r.node_id;
  Oce1.put_string buf r.node_addr;
  Oce1.put_raw buf r.pubkey;
  Oce1.put_hash32 buf r.binary_hash;
  Oce1.put_hash32 buf r.config_hash;
  Oce1.put_string buf r.host;
  Oce1.put_u16 buf r.port;
  Oce1.put_list Oce1.put_string buf r.features;
  Oce1.put_u64 buf r.ts;
  Oce1.put_sig64 buf r.signature

let get c =
  let got_proto = Oce1.get_u16 c in
  if got_proto <> proto then failwith "peer record proto mismatch";
  let chain_id = Oce1.get_string_bounded ~max:max_chain_id_bytes c in
  let node_id = Oce1.get_string_bounded ~max:max_node_id_bytes c in
  let node_addr = Oce1.get_string_bounded ~max:max_node_addr_bytes c in
  let pubkey = Oce1.get_raw c 32 in
  let binary_hash = Oce1.get_hash32 c in
  let config_hash = Oce1.get_hash32 c in
  let host = Oce1.get_string_bounded ~max:max_host_bytes c in
  let port = Oce1.get_u16 c in
  let features = Oce1.get_list_bounded ~max:max_features
    (Oce1.get_string_bounded ~max:max_feature_bytes) c in
  let ts = Oce1.get_u64 c in
  let signature = Oce1.get_sig64 c in
  if String.length node_id <> max_node_id_bytes then
    failwith "peer record node id length";
  { chain_id; node_id; node_addr; pubkey; binary_hash; config_hash; host; port; features; ts; signature }

let encode r =
  Oce1.encode (fun buf -> put buf r)

let decode payload =
  Oce1.decode get payload

let encode_list records =
  Oce1.encode (fun buf -> Oce1.put_list put buf records)

let decode_list payload =
  Oce1.decode (fun c -> Oce1.get_list_bounded ~max:max_records get c) payload

let endpoint r =
  Printf.sprintf "%s:%d" r.host r.port

let verify_signature r =
  Octra_ed25519.verify ~pub:r.pubkey ~msg:(sign_bytes r) r.signature

let valid_host host =
  let n = String.length host in
  n > 0 && n <= 255 && not (String.contains host '\x00')

let valid_features xs =
  List.length xs <= 32
  && List.for_all (fun s -> String.length s <= 64 && not (String.contains s '\x00')) xs

let validate ?binary_hash ~chain_id ~config_hash ~allowed_pubkeys r =
  if r.chain_id <> chain_id then Invalid "chain_id"
  else if r.config_hash <> config_hash then Invalid "config_hash"
  else if (match binary_hash with Some h -> h <> r.binary_hash | None -> false) then Invalid "binary_hash"
  else if r.port <= 0 || r.port > 65535 then Invalid "port"
  else if not (valid_host r.host) then Invalid "host"
  else if r.node_id <> node_id_of_pubkey r.pubkey then Invalid "node_id"
  else if allowed_pubkeys <> [] && not (List.mem r.pubkey allowed_pubkeys) then Invalid "pubkey"
  else if not (valid_features r.features) then Invalid "features"
  else if not (verify_signature r) then Invalid "signature"
  else Valid

let split_dot host =
  String.split_on_char '.' host

let private_v4 = function
  | ["127"; _; _; _] -> true
  | ["10"; _; _; _] -> true
  | ["192"; "168"; _; _] -> true
  | ["169"; "254"; _; _] -> true
  | ["172"; b; _; _] ->
    (try
      let n = int_of_string b in
      n >= 16 && n <= 31
    with _ -> false)
  | _ -> false

let v4_bucket = function
  | [a; b; c; d] ->
    (try
      ignore (int_of_string a);
      ignore (int_of_string b);
      ignore (int_of_string c);
      ignore (int_of_string d);
      Some (a ^ "." ^ b ^ "." ^ c)
    with _ -> None)
  | _ -> None

let overlay_v4 = function
  | ["100"; b; _; _] ->
    (try
      let n = int_of_string b in
      n >= 64 && n <= 127
    with _ -> false)
  | _ -> false

let bucket_of_host host =
  if host = "localhost" || host = "::1" || private_v4 (split_dot host) then
    "local"
  else if overlay_v4 (split_dot host) then
    match v4_bucket (split_dot host) with
    | Some b -> "overlay:" ^ b
    | None -> "overlay"
  else
    match v4_bucket (split_dot host) with
    | Some b -> "v4:" ^ b
    | None -> "dns:" ^ host

let bucket r =
  bucket_of_host r.host

let public_bucket bucket =
  String.starts_with ~prefix:"v4:" bucket
  || String.starts_with ~prefix:"dns:" bucket

let allow_bucket ~max_per_bucket records r =
  let b = bucket r in
  if not (public_bucket b) then true
  else
    let count =
      List.fold_left
        (fun n x -> if bucket x = b then n + 1 else n)
        0
        records
    in
    count < max_per_bucket