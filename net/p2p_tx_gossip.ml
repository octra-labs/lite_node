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

let is_control payload =
  String.length payload <= 4 + max_hashes * 68
  && try match decode payload with Inv _ | Get _ -> true | Tx _ -> false
    with Failure _ | Invalid_argument _ -> false

type parse_kind = Direct | Encrypted | Program

let parse_kind = function
  | "encrypt" | "decrypt" | "key_switch" | "stealth" | "claim" -> Some Encrypted
  | "program_deploy" -> Some Program
  | "standard" | "private" | "deploy" | "call" | "program_exec" | "multi_exec"
  | "upgrade" | "deploy_circle" | "circle_program_update" | "circle_asset_put"
  | "circle_asset_put_encrypted" | "circle_sealed_slot_put" | "circle_slot_policy_put"
  | "circle_state_descriptor_put" | "circle_balance_cell_put" | "circle_register_cell_put"
  | "circle_transport_policy_put" | "circle_hfhe_policy_put" | "circle_key_policy_put"
  | "circle_key_grant" | "circle_key_extend" | "circle_key_revoke" | "circle_key_erase"
  | "circle_outbox_open" | "circle_relay_claim" | "circle_relay_cancel" | "circle_ingress_commit"
  | "circle_call" | "validator_set_update" | "validator_ready" | "validator_bond"
  | "validator_exit" | "validator_withdraw" | "validator_evidence" | "recrypt"
  | "op01_burn" | "compute_fund" | "compute_return" | "compute_settle" -> Some Direct
  | _ -> None

let is_light payload =
  String.length payload <= 16_384 &&
  try
    let raw = match decode payload with
      | Tx {tx_json; _} -> tx_json
      | Inv _ | Get _ -> "null"
      | exception (Failure _ | Invalid_argument _) ->
        Oce1.decode Oce1.get_string payload in
    match Yojson.Safe.from_string raw with
    | `Assoc fields ->
      let names = List.map fst fields in
      List.length names = List.length (List.sort_uniq String.compare names)
      && (match List.assoc_opt "op_type" fields with
        | None -> true
        | Some (`String op) -> parse_kind op = Some Direct
        | _ -> false)
    | _ -> false
  with Failure _ | Invalid_argument _ | Yojson.Json_error _ -> false