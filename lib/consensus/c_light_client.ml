(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type root_quorum = {
  root : C_light_root.t;
  signers : string list;
}

let raw_to_hex s =
  String.concat "" (List.init (String.length s) (fun i ->
    Printf.sprintf "%02x" (Char.code s.[i])))

let same_root a b =
  a.C_light_root.chain_id = b.C_light_root.chain_id
  && a.head_epoch = b.head_epoch
  && a.state_root = b.state_root
  && a.ledger_state_root = b.ledger_state_root
  && a.epoch_index_root = b.epoch_index_root
  && a.txid_hi = b.txid_hi
  && a.config_hash = b.config_hash

let uniq sorted =
  let rec go last acc = function
    | [] -> List.rev acc
    | x :: xs ->
      match last with
      | Some prev when prev = x -> go last acc xs
      | _ -> go (Some x) (x :: acc) xs in
  go None [] sorted

let root_signers roots =
  roots
  |> List.map (fun r -> r.C_light_root.validator_addr)
  |> List.sort String.compare
  |> uniq

let verify_root_quorum ~validator_set_proof roots =
  if not (C_light_validator_set.verify validator_set_proof) then None
  else
    match roots with
    | [] -> None
    | root :: _ ->
      match
        C_light_validator_set.validator_set
          ~weighted:validator_set_proof.weighted
          validator_set_proof.validators
      with
      | Error _ -> None
      | Ok vs ->
        let chain_id = validator_set_proof.chain_id in
        let config_hash = validator_set_proof.config_hash in
        let root_ok r =
          same_root root r
          && C_light_root.verify ~validator_set:vs ~chain_id ~config_hash r in
        if not (List.for_all root_ok roots) then None
        else
          let signers = root_signers roots in
          if not
            (C_types.has_quorum_at
               ~chain_id:root.chain_id
               ~epoch_id:root.head_epoch
               vs
               signers)
          then None
          else Some { root; signers }

let rec verify_chain = function
  | [] | [_] -> true
  | prev :: next :: rest ->
    C_light_epoch.verify_chain_step ~prev next && verify_chain (next :: rest)

let last = function
  | [] -> None
  | x :: xs -> Some (List.fold_left (fun _ y -> y) x xs)

let verify_epoch_chain ~root epochs =
  match last epochs with
  | None -> false
  | Some tip ->
    begin
      match root.C_light_root.epoch_index_root with
      | None -> false
      | Some epoch_index_root ->
        List.for_all C_light_epoch.verify epochs
        && verify_chain epochs
        && tip.chain_id = root.chain_id
        && tip.epoch_id = root.head_epoch
        && String.lowercase_ascii tip.epoch_index_root =
           raw_to_hex epoch_index_root
    end

let verify_tx ~root ~epochs tx =
  verify_epoch_chain ~root epochs
  && C_light_epoch.tx_position_ok tx
  && List.exists (C_light_epoch.same tx.epoch) epochs