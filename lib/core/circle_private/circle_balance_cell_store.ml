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


open Lwt.Syntax

let load_opt store circle_id path_key =
  let* ciphertext_commitment_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_balance_cell.ciphertext_commitment_key path_key] in
  let* amount_commitment_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_balance_cell.amount_commitment_key path_key] in
  let* proof_kind_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_balance_cell.proof_kind_key path_key] in
  let* proof_receipt_hash_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_balance_cell.proof_receipt_hash_key path_key] in
  let cell =
    Circle_balance_cell.of_stored_values
      ~ciphertext_commitment_raw
      ~amount_commitment_raw
      ~proof_kind_raw
      ~proof_receipt_hash_raw in
  if Circle_balance_cell.materialized cell then
    Lwt.return (Some cell)
  else
    Lwt.return_none

let load store circle_id path_key =
  let* cell_opt = load_opt store circle_id path_key in
  match cell_opt with
  | Some cell -> Lwt.return cell
  | None -> Lwt.return Circle_balance_cell.empty