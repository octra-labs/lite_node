(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module T = Octra_core.Transaction
module C = Octra_consensus.C_types
module H = Octra_consensus.C_hash
module Ready = Octra_core.Validator_ready_policy

let tx_root txs =
  Octra_net.Hash_domain.hash "octra:tx_list:v1"
    (String.concat "" (List.map T.hash txs))

let previous ~chain_id ~epoch ~lookup parent =
  match parent with
  | None -> if epoch = 0L then Some false else None
  | Some (parent : C.parent_commit) ->
    let cert = parent.certificate in
    let header = cert.header in
    if epoch <= 0L || cert.epoch_id <> Int64.pred epoch
       || header.epoch_id <> cert.epoch_id || cert.chain_id <> chain_id
       || header.chain_id <> chain_id || H.proposal_id header <> cert.proposal_id
    then None
    else if header.tx_list_hash = tx_root [] then Some false
    else match lookup cert.proposal_id with
      | Some txs when tx_root txs = header.tx_list_hash ->
        Some (List.exists Octra_core.Preverify_worker.snapshot_transition txs)
      | Some _ | None -> None

let urgent ~epoch tx =
  tx.T.op_type = T.ValidatorReady
  && match Octra_core.Validator_registry.ready_payload_of_message tx.message with
     | Error _ -> false
     | Ok ready ->
       Ready.delivery ~epoch ~head:ready.head_epoch
       && Int64.sub (Int64.pred epoch) ready.head_epoch = Ready.window

let allow ~epoch ~previous ordinary =
  previous = Some false && not (List.exists (urgent ~epoch) ordinary)

let select ~ordered ~epoch ~previous ~ordinary inputs =
  let isolated = List.exists (fun tx ->
    Octra_core.Preverify_worker.snapshot_transition tx
    && (not ordered || tx.T.op_type <> T.CircleCall)) inputs in
  if ordered && not isolated && not (List.exists (urgent ~epoch) ordinary)
  then inputs
  else if allow ~epoch ~previous ordinary then inputs else ordinary