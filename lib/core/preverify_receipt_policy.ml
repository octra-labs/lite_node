(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let activation_epoch_of getenv =
  match getenv "OCTRA_PREVERIFY_RECEIPT_ACTIVATION_EPOCH" with
  | Some raw ->
    (try
       let epoch = int_of_string raw in
       if epoch < 0 then max_int else epoch
     with _ -> max_int)
  | None -> max_int

let activation_epoch () =
  activation_epoch_of Sys.getenv_opt

let active ~epoch_id =
  epoch_id >= activation_epoch ()

let should_check ~epoch_id ~receipts =
  receipts <> [] || active ~epoch_id

let check ~rejections ~epoch_id ~receipts txs =
  let charged = List.exists (fun (item : Tx_outcome.rejection) ->
    item.charged && Preverify_commit.heavy item.tx) rejections in
  if should_check ~epoch_id ~receipts || charged then
    Preverify_commit.check_strings receipts (Tx_outcome.executed ~confirmed:txs rejections)
  else
    Ok ()