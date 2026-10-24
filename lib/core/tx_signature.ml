(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = Historical | Network of string

let at_plan ~chain_id ~activation_epoch ~epoch =
  match activation_epoch with
  | Some first when epoch >= first -> Network chain_id
  | Some _ | None -> Historical

let at ~chain_id ~epoch =
  at_plan ~chain_id ~epoch
    ~activation_epoch:(Option.map (fun plan -> Int64.of_int plan.Rule_graph.activation_epoch)
      (Rule_graph.tx_network_activation_for_chain chain_id))

let bytes mode tx =
  let body = Transaction.serialize_for_signing tx in
  match mode with
  | Historical -> body
  | Network chain_id ->
    if chain_id = "" then invalid_arg "transaction network is empty";
    let buffer = Buffer.create (String.length body + String.length chain_id + 64) in
    List.iter (Octra_net.Oce1.put_string buffer) ["octra:transaction:network"; chain_id; body];
    Buffer.contents buffer

let verify mode tx pubkey =
  match mode with
  | Historical -> Transaction.verify tx pubkey
  | Network _ ->
    try Octra_ed25519.verify ~pub:(Base64.decode_exn pubkey)
      ~msg:(bytes mode tx) (Base64.decode_exn tx.Transaction.signature)
    with _ -> false

let sign mode tx key =
  match mode with
  | Historical -> Transaction.sign_with_privkey tx key
  | Network _ ->
    match Mirage_crypto_ec.Ed25519.priv_of_octets (Base64.decode_exn key) with
    | Error _ -> invalid_arg "transaction signing key is invalid"
    | Ok secret ->
      let signature = Mirage_crypto_ec.Ed25519.sign ~key:secret (bytes mode tx) in
      {tx with Transaction.signature = Base64.encode_exn signature}

let check mode tx =
  match tx.Transaction.public_key with
  | Some key when Crypto.Address.verify_address_pubkey tx.from key
      && verify mode tx key -> Ok ()
  | _ -> Error "transaction network signature is invalid"

let check_active mode txs =
  match mode with
  | Historical -> Ok ()
  | Network _ ->
    let rec loop = function
      | [] -> Ok ()
      | tx :: rest -> Result.bind (check mode tx) (fun () -> loop rest) in
    loop txs

let consensus_id = "ed25519:oce1:transaction_network:chain_id:signing_json"