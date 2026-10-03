(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module T = Octra_core.Transaction
module Senders = Map.Make (String)

let before ~excluded inputs =
  let cutoffs = List.fold_left (fun values tx ->
    Senders.update tx.T.from (fun prior ->
      Some (min tx.nonce (Option.value prior ~default:tx.nonce))) values)
    Senders.empty excluded in
  List.filter (fun tx ->
    match Senders.find_opt tx.T.from cutoffs with
    | None -> true
    | Some nonce -> tx.nonce < nonce) inputs

let without inputs =
  before ~excluded:(List.filter Octra_core.Preverify_worker.snapshot_transition inputs) inputs

let select ~selected ~confirmed ~rejected inputs =
  match selected, confirmed, rejected with
  | [tx], [], 1 when Octra_core.Preverify_worker.snapshot_transition tx ->
    let remaining = without inputs in
    if remaining = [] then None else Some remaining
  | _ -> None