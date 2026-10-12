(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Tx = Transaction

let ( let* ) = Result.bind

let decode ~size ~field text =
  if String.length text > ((size + 2) / 3) * 4 then
    Error ("tx_too_large", field ^ " encoding is too long")
  else
    match Base64.decode text with
    | Ok raw when String.length raw = size -> Ok raw
    | _ -> Error ("invalid_signature", field ^ " encoding is invalid")

let scalar_order = Z.(add (shift_left one 252)
  (of_string "27742317777372353535851937790883648493"))

let normalize ~sender_pk (tx : Tx.t) =
  let* () = match tx.public_key with
    | Some key when String.length key > 44 ->
      Error ("tx_too_large", "public key encoding is too long")
    | _ -> Ok () in
  let* key = match sender_pk with
    | None -> Error ("invalid_signature", "sender public key is missing")
    | Some key -> decode ~size:32 ~field:"public key" key in
  let public_key = Base64.encode_exn key in
  let* () =
    if Crypto.Address.verify_address_pubkey tx.from public_key then Ok ()
    else Error ("invalid_address", "sender public key does not match address") in
  let* () = match tx.op_type, tx.public_key with
    | Tx.ValidatorBond, Some carried ->
      let* raw = decode ~size:32 ~field:"bond public key" carried in
      if String.equal raw key then Ok ()
      else Error ("invalid_signature", "bond public key differs from sender key")
    | Tx.ValidatorBond, None ->
      Error ("invalid_signature", "bond public key is missing")
    | _ -> Ok () in
  let* raw = decode ~size:64 ~field:"signature" tx.signature in
  let scalar = Z.of_bits (String.sub raw 32 32) in
  if Z.compare scalar scalar_order >= 0 then
    Error ("invalid_signature", "signature scalar is out of range")
  else Ok {tx with signature = Base64.encode_exn raw; public_key = Some public_key}

let consensus_id = "sender_key32_signature64_base64_scalar_unique_nonce"

module Nonces = Set.Make (struct
  type t = string * int
  let compare = Stdlib.compare
end)

let check tx =
  let* prepared = normalize ~sender_pk:tx.Tx.public_key tx
    |> Result.map_error snd in
  if tx.signature <> prepared.signature then Error "signature encoding is not normalized"
  else if tx.public_key <> prepared.public_key then Error "public key encoding is not normalized"
  else Ok ()

let check_mode mode txs =
  match mode with
  | Rule_graph.Prior -> Ok ()
  | Rule_graph.Active ->
    let rec loop seen = function
      | [] -> Ok ()
      | tx :: rest ->
        let* () = check tx in
        let key = tx.Tx.from, tx.nonce in
        if Nonces.mem key seen then Error "duplicate sender nonce"
        else loop (Nonces.add key seen) rest in
    loop Nonces.empty txs

let check_compute ~settle mode txs =
  if mode = Rule_graph.Prior && List.exists (fun tx ->
    match tx.Tx.op_type with Tx.ComputeFund | Tx.ComputeReturn | Tx.ComputeSettle -> true | _ -> false) txs
  then Error "compute transfers not active"
  else if settle = Rule_graph.Prior
      && List.exists (fun tx -> tx.Tx.op_type = Tx.ComputeSettle) txs then
    Error "compute settlement not active"
  else Ok ()

let check_epoch ~chain_id ~epoch txs =
  let height = Int64.to_int (Int64.max (Int64.of_int min_int)
    (Int64.min (Int64.of_int max_int) epoch)) in
  let* () = check_compute ~settle:(Rule_graph.settle_at ~chain_id ~epoch:height)
    (Rule_graph.vm_bytes_at ~chain_id ~epoch:height) txs in
  check_mode (Rule_graph.tx_envelope_at ~chain_id ~epoch) txs

let check_rule rules ~epoch txs =
  let* compute = Rule_graph.vm_bytes rules ~epoch |> Result.map_error Rule_graph.fault_message in
  let* settle = Rule_graph.settle rules ~epoch |> Result.map_error Rule_graph.fault_message in
  let* () = check_compute ~settle compute txs in
  let* mode = Rule_graph.tx_envelope rules ~epoch
    |> Result.map_error Rule_graph.fault_message in
  check_mode mode txs

let check_charged mode rejections =
  if List.for_all (fun (item : Tx_outcome.rejection) ->
    not item.charged || (mode = Rule_graph.Active
      && match item.tx.op_type with
        | Tx.ComputeFund | Tx.ComputeReturn | Tx.ComputeSettle -> true
        | _ -> false)) rejections then Ok ()
  else Error "charged outcome not active"

let check_outcome ~chain_id ~epoch ~receipts txs =
  match Rule_graph.tx_envelope_at ~chain_id ~epoch with
  | Rule_graph.Prior -> Ok ()
  | Rule_graph.Active ->
    let* partition = Tx_outcome.decode_final ~confirmed:txs receipts in
    let height = Int64.to_int (Int64.min (Int64.of_int max_int) epoch) in
    let* () = check_charged (Rule_graph.vm_bytes_at ~chain_id ~epoch:height)
      partition.rejections in
    let* inputs = Tx_outcome.merge ~confirmed:txs ~rejections:partition.rejections in
    check_epoch ~chain_id ~epoch inputs