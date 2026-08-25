(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Rpc = Octra_core.Rpc

let node_version () =
  Rpc_view.node_version ()

let runtime_version ~source_commit ~binary_hash ~consensus_profile
    ~consensus_rules_id ~runtime_profile_hash ~config_hash ~chain_id
    ~validator =
  Rpc_view.runtime_version
    ~source_commit
    ~binary_hash
    ~consensus_profile
    ~consensus_rules_id
    ~runtime_profile_hash
    ~config_hash
    ~chain_id
    ~validator

let validator_view_pubkey ~validator_view_pub ~validator_address =
  Rpc_view.validator_view_pubkey
    ~validator_view_pub
    ~validator_address

let epoch_tags ~count ~min_epoch ~max_epoch ~keep_epochs =
  Rpc_view.epoch_tags ~count ~min_epoch ~max_epoch ~keep_epochs

let enrollment_epoch = function
  | None -> `Null
  | Some epoch -> `String (Int64.to_string epoch)

let validator_enrollment ~head_epoch ~address ~pubkey
    (candidate : Octra_core.Validator_admission.candidate option) =
  match candidate with
  | None ->
    Ok
      (`Assoc [
        "head_epoch", `Int head_epoch;
        "address", `String address;
        "consensus_pubkey", `String pubkey;
        "identity", `String "self_reported";
        "state", `String "absent";
        "bond", `Null;
        "bonded_epoch", `Null;
        "ready_epoch", `Null;
        "exit_epoch", `Null;
      ])
  | Some candidate ->
    let candidate_pubkey = Base64.encode_exn candidate.pubkey in
    if not (String.equal candidate.address address) then
      Error (Rpc.err (-32000) "validator enrollment address differs" None)
    else if not (String.equal candidate_pubkey pubkey) then
      Error (Rpc.err (-32000) "validator enrollment public key differs" None)
    else
      let state =
        match candidate.exit_epoch, candidate.ready_epoch with
        | Some _, _ -> "exiting"
        | None, Some _ -> "ready"
        | None, None -> "bonded"
      in
      Ok
        (`Assoc [
          "head_epoch", `Int head_epoch;
          "address", `String address;
          "consensus_pubkey", `String pubkey;
          "identity", `String "self_reported";
          "state", `String state;
          "bond", `String (Z.to_string candidate.bond);
          "bonded_epoch", `String (Int64.to_string candidate.bonded_epoch);
          "ready_epoch", enrollment_epoch candidate.ready_epoch;
          "exit_epoch", enrollment_epoch candidate.exit_epoch;
        ])

let consensus_peer_states
    ~now ~diag ~peer_records ~voting ~voting_reason ~round_state ~round_peers
    ~round_votes ~round_agreed =
  let diag_json, score_rows = Rpc_view.peer_diag diag ~now in
  match peer_records with
  | None ->
    Rpc_view.consensus_peer_states
      ~enabled:false
      ~voting
      ~voting_reason
      ~round_state
      ~round_peers
      ~round_votes
      ~round_agreed
      ~peers:[]
      ~scores:score_rows
      ~diag:diag_json
  | Some records ->
    let peers =
      records
      |> List.sort (fun a b ->
        String.compare a.Octra_consensus.C_driver.responder_addr
          b.Octra_consensus.C_driver.responder_addr)
      |> List.map (Rpc_view.consensus_peer ~now)
    in
    Rpc_view.consensus_peer_states
      ~enabled:true
      ~voting
      ~voting_reason
      ~round_state
      ~round_peers
      ~round_votes
      ~round_agreed
      ~peers
      ~scores:score_rows
      ~diag:diag_json

let node_status ~epoch ~validator ~roots ~timestamp ~head =
  Rpc_view.node_status
    ~epoch
    ~validator
    ~roots
    ~timestamp
    ~head

let node_stats ~current_epoch ~total_accounts ~active_accounts ~true_total
    ~encrypted ~max_supply ~total_confirmed ~staging ~recent_tx_count
    ~latest_epochs ~head =
  let display_supply = Z.add true_total encrypted in
  Rpc_view.node_stats
    ~current_epoch
    ~total_accounts
    ~active_accounts
    ~display_supply
    ~encrypted_supply:encrypted
    ~max_supply
    ~total_confirmed
    ~staging
    ~recent_tx_count
    ~latest_epochs
    ~head

let validator_set_proof ~chain_id ~config_hash ?program_trust_hash
    ?runtime_profile_hash ?scheduled validator_set =
  let proof =
    Octra_consensus.C_light_validator_set.of_validator_set
      ~chain_id
      ~config_hash
      ?program_trust_hash
      ?runtime_profile_hash
      ?scheduled
      validator_set
  in
  if Octra_consensus.C_light_validator_set.verify proof then
    Ok (Rpc_view.validator_set_proof proof)
  else
    Error (Rpc.err (-32000) "invalid validator set proof" None)