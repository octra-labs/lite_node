(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module T = Octra_core.Transaction
module S = Octra_core.Tx_signature
module V = Octra_node_runtime.Tx_view
module P = Octra_node_runtime.P2p_tx_admit

let check name ok = if not ok then failwith name

let keys byte =
  let key = Mirage_crypto_ec.Ed25519.priv_of_octets (String.make 32 byte) |> Result.get_ok in
  let pub = Mirage_crypto_ec.Ed25519.pub_of_priv key
    |> Mirage_crypto_ec.Ed25519.pub_to_octets |> Base64.encode_exn in
  Octra_core.Crypto.Address.address_from_pubkey pub,
  Mirage_crypto_ec.Ed25519.priv_to_octets key |> Base64.encode_exn, pub

let draft () =
  let address, key, pub = keys 'a' in
  let recipient, _, _ = keys 'b' in
  T.{from = address; to_ = recipient; amount = Z.one; nonce = 1;
    ou = Z.of_int 10; timestamp = 100.; signature = ""; public_key = Some pub;
    message = None; op_type = Standard; encrypted_data = None}, key, pub

let accepted signing pub tx =
  check "signature verification differs" (S.verify signing tx pub);
  check "network envelope differs" (S.check_active signing [tx] = Ok ());
  check "RPC signature differs" (V.signature_check ~signing ~account_public_key:(Some pub) tx = Ok ());
  check "P2P signature differs" (P.sig_valid ~signing tx (Some pub));
  check "P2P receive differs" (P.admit ~signing ~now:100. ~max_drift:300.
    ~sender_pk:(Some pub) tx = P.Accept)

let refused signing pub tx =
  check "signature accepted wrong network" (not (S.verify signing tx pub));
  check "RPC accepted wrong network" (Result.is_error (V.signature_check ~signing ~account_public_key:(Some pub) tx));
  check "P2P accepted wrong network" (not (P.sig_valid ~signing tx (Some pub)));
  check "P2P receive accepted wrong network" (P.admit ~signing ~now:100. ~max_drift:300.
    ~sender_pk:(Some pub) tx <> P.Accept);
  match signing with S.Historical -> () | S.Network _ ->
    check "network envelope accepted wrong network" (Result.is_error (S.check_active signing [tx]))

let test_network () =
  let tx, key, pub = draft () in
  let history = T.sign_with_privkey tx key in
  check "historical signature changed" (S.sign S.Historical tx key = history);
  check "historical bytes changed" (S.bytes S.Historical tx = T.serialize_for_signing tx);
  accepted S.Historical pub history;
  let networks = ["octra-devnet-9871-cluster"; "octra-mainnet"; "a\000b"] in
  List.iter (fun chain ->
    let mode = S.Network chain in
    let signed = S.sign mode tx key in
    accepted mode pub signed;
    refused mode pub history;
    refused S.Historical pub signed;
    List.iter (fun other -> if other <> chain then refused (S.Network other) pub signed) networks;
    refused mode pub {signed with nonce = signed.nonce + 1};
    refused mode pub {signed with amount = Z.succ signed.amount};
    refused mode pub {signed with message = Some chain};
    check "signed transaction lost network separation" (T.hash history <> T.hash signed);
    let cursor = Octra_net.Oce1.make_cursor (S.bytes mode tx) in
    check "network tag differs" (Octra_net.Oce1.get_string cursor = "octra:transaction:network");
    check "network field differs" (Octra_net.Oce1.get_string cursor = chain);
    check "signed body differs" (Octra_net.Oce1.get_string cursor = T.serialize_for_signing tx);
    check "unsigned bytes appended" (Octra_net.Oce1.remaining cursor = 0);
    check "carried key missing accepted" (Result.is_error (S.check mode {signed with public_key = None}))) networks;
  let mode epoch = S.at_plan ~chain_id:"test" ~activation_epoch:(Some 50L) ~epoch in
  check "early network rule" (mode 49L = S.Historical);
  check "late network rule" (mode 50L = S.Network "test" && mode 51L = S.Network "test");
  check "unset network rule" (S.at_plan ~chain_id:"test" ~activation_epoch:None ~epoch:Int64.max_int = S.Historical);
  check "production activation assigned" (S.at ~chain_id:"octra-devnet-9871-cluster" ~epoch:Int64.max_int = S.Historical);
  let message = Yojson.Safe.to_string (`Assoc ["consensus_pubkey", `String pub;
    "head_epoch", `String "49"; "state_root", `String (String.make 64 'a')]) in
  let ready = {tx with op_type = T.ValidatorReady; to_ = tx.from; amount = Z.zero; message = Some message} in
  let signed = match Octra_node_runtime.Set_delivery.sign ~signing:(mode 50L) ~key ready with
    | Ok signed -> signed | Error reason -> failwith reason in
  check "duty uses historical signature after transition" (S.verify (mode 50L) signed pub);
  check "duty signature crossed network" (not (S.verify (S.Network "other") signed pub))

let () = test_network (); print_endline "status = pass test = tx_network"