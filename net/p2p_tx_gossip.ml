(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type msg =
  | Inv of string list
  | Get of string list
  | Tx of { hash : string; tx_json : string }

let version = 1
let kind_inv = 1
let kind_get = 2
let kind_tx = 3
let max_hashes = 512
let max_tx_json = 5_000_000

let is_hex c =
  ('0' <= c && c <= '9')
  || ('a' <= c && c <= 'f')
  || ('A' <= c && c <= 'F')

let check_hash h =
  if String.length h <> 64 || not (String.for_all is_hex h) then
    invalid_arg "p2p_tx_gossip: bad hash";
  String.lowercase_ascii h

let check_hashes hs =
  if List.length hs > max_hashes then
    invalid_arg "p2p_tx_gossip: too many hashes";
  List.map check_hash hs

let put_hashes buf hs =
  let hs = check_hashes hs in
  Oce1.put_u16 buf (List.length hs);
  List.iter (Oce1.put_string buf) hs

let get_hashes c =
  let n = Oce1.get_u16 c in
  if n > max_hashes then failwith "p2p_tx_gossip: too many hashes";
  List.init n (fun _ -> check_hash (Oce1.get_string c))

let encode = function
  | Inv hs ->
    Oce1.encode (fun buf ->
      Oce1.put_u8 buf version;
      Oce1.put_u8 buf kind_inv;
      put_hashes buf hs)
  | Get hs ->
    Oce1.encode (fun buf ->
      Oce1.put_u8 buf version;
      Oce1.put_u8 buf kind_get;
      put_hashes buf hs)
  | Tx { hash; tx_json } ->
    let hash = check_hash hash in
    if String.length tx_json > max_tx_json then
      invalid_arg "p2p_tx_gossip: tx json too large";
    Oce1.encode (fun buf ->
      Oce1.put_u8 buf version;
      Oce1.put_u8 buf kind_tx;
      Oce1.put_string buf hash;
      Oce1.put_string buf tx_json)

let decode payload =
  Oce1.decode
    (fun c ->
      let v = Oce1.get_u8 c in
      if v <> version then failwith "p2p_tx_gossip: bad version";
      match Oce1.get_u8 c with
      | k when k = kind_inv -> Inv (get_hashes c)
      | k when k = kind_get -> Get (get_hashes c)
      | k when k = kind_tx ->
        let hash = check_hash (Oce1.get_string c) in
        let tx_json = Oce1.get_string c in
        if String.length tx_json > max_tx_json then
          failwith "p2p_tx_gossip: tx json too large";
        Tx { hash; tx_json }
      | _ -> failwith "p2p_tx_gossip: bad kind")
    payload