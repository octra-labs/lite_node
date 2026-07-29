(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open C_types

let proposal_id_v3 (h : epoch_header) =
  Octra_net.Hash_domain.hash_encoded "octra:epoch_proposal_id:v3" (fun buf ->
    Octra_net.Oce1.put_u16 buf h.proto_version;
    Octra_net.Oce1.put_string buf h.chain_id;
    Octra_net.Oce1.put_u64 buf h.epoch_id;
    Octra_net.Oce1.put_hash32 buf h.prev_state_root;
    Octra_net.Oce1.put_hash32 buf h.tx_list_hash;
    Octra_net.Oce1.put_hash32 buf h.receipt_root;
    Octra_net.Oce1.put_hash32 buf h.proposed_state_root;
    Octra_net.Oce1.put_addr buf h.creator_addr;
    Octra_net.Oce1.put_u64 buf h.txid_hi;
    Octra_net.Oce1.put_u64 buf (Int64.bits_of_float h.ts))

let proposal_id_v4 (h : epoch_header) =
  Octra_net.Hash_domain.hash_encoded "octra:epoch_proposal_id:v4" (fun buf ->
    Octra_net.Oce1.put_u16 buf h.proto_version;
    Octra_net.Oce1.put_string buf h.chain_id;
    Octra_net.Oce1.put_u64 buf h.epoch_id;
    Octra_net.Oce1.put_hash32 buf h.prev_state_root;
    Octra_net.Oce1.put_hash32 buf h.tx_list_hash;
    Octra_net.Oce1.put_hash32 buf h.receipt_root;
    Octra_net.Oce1.put_hash32 buf h.proposed_state_root;
    Octra_net.Oce1.put_hash32 buf h.parent_commit_hash;
    Octra_net.Oce1.put_addr buf h.creator_addr;
    Octra_net.Oce1.put_u64 buf h.txid_hi;
    Octra_net.Oce1.put_u64 buf (Int64.bits_of_float h.ts))

let proposal_id (h : epoch_header) =
  match h.proto_version with
  | version when version = proto_version_parent_legacy ->
    proposal_id_v3 h
  | version when version = proto_version_current ->
    proposal_id_v4 h
  | _ -> invalid_arg "unsupported epoch header protocol"

let vote_sign_bytes (v : vote) =
  Octra_net.Hash_domain.hash_encoded "octra:vote:v2" (fun buf ->
    Octra_net.Oce1.put_string buf v.chain_id;
    Octra_net.Oce1.put_u64 buf v.epoch_id;
    Octra_net.Oce1.put_u32_int buf v.round;
    Octra_net.Oce1.put_u8 buf (vote_type_to_u8 v.vote_type);
    Octra_net.Oce1.put_hash32 buf v.proposal_id)

let round_sync_sign_bytes (sync : C_codec.round_sync) =
  Octra_net.Hash_domain.hash_encoded "octra:round_sync:v1" (fun buf ->
    Octra_net.Oce1.put_string buf sync.chain_id;
    Octra_net.Oce1.put_u64 buf sync.epoch_id;
    Octra_net.Oce1.put_u32_int buf sync.round;
    Octra_net.Oce1.put_u8 buf (round_step_to_u8 sync.step);
    Octra_net.Oce1.put_bool buf sync.request;
    Octra_net.Oce1.put_addr buf sync.validator)

let propose_sign_bytes (p : propose) =
  let pid = proposal_id p.header in
  let domain =
    match p.header.proto_version with
    | version when version = proto_version_parent_legacy ->
      "octra:propose:v3"
    | version when version = proto_version_current ->
      "octra:propose:v4"
    | _ -> invalid_arg "unsupported proposal protocol"
  in
  Octra_net.Hash_domain.hash_encoded domain (fun buf ->
    Octra_net.Oce1.put_string buf p.chain_id;
    Octra_net.Oce1.put_u64 buf p.epoch_id;
    Octra_net.Oce1.put_u32_int buf p.round;
    Octra_net.Oce1.put_option Octra_net.Oce1.put_u32_int buf p.valid_round;
    Octra_net.Oce1.put_hash32 buf pid)

let canonical_parent_commit parent =
  let certificate = parent.certificate in
  {
    parent with
    certificate = {
      certificate with
      precommits =
        List.sort
          (fun (left : vote) (right : vote) ->
            String.compare left.validator right.validator)
          certificate.precommits;
    };
  }

let parent_commit_hash parent =
  parent
  |> canonical_parent_commit
  |> C_codec.encode_parent_commit
  |> Octra_net.Hash_domain.hash "octra:parent_commit:v1"

let parent_commit_hash_opt = function
  | None -> Octra_net.Hash_domain.nil_hash
  | Some parent -> parent_commit_hash parent

let tx_list_hash (tx_hashes : string list) =
  Octra_net.Hash_domain.hash_encoded "octra:tx_list_hash:v1" (fun buf ->
    Octra_net.Oce1.put_list Octra_net.Oce1.put_hash32 buf tx_hashes)

let receipt_root (receipts_json : string list) =
  Octra_net.Hash_domain.hash_encoded "octra:preverify_receipt_root:v1" (fun buf ->
    Octra_net.Oce1.put_list Octra_net.Oce1.put_string buf receipts_json)

let proof_cert_sign_bytes ~chain_id (pc : proof_cert) =
  Octra_net.Hash_domain.hash_encoded "octra:proof_cert:v1" (fun buf ->
    Octra_net.Oce1.put_string buf chain_id;
    Octra_net.Oce1.put_hash32 buf pc.task_id;
    Octra_net.Oce1.put_bool buf pc.result)

let proof_task_id ~tx_hash ~proof_kind ~inputs_hash =
  Octra_net.Hash_domain.hash_encoded "octra:proof_task:v1" (fun buf ->
    Octra_net.Oce1.put_hash32 buf tx_hash;
    Octra_net.Oce1.put_u8 buf (proof_kind_to_u8 proof_kind);
    Octra_net.Oce1.put_hash32 buf inputs_hash)

let proofqc_hash (qc : proof_qc) =
  let signers = List.map fst qc.certs |> List.sort String.compare in
  Octra_net.Hash_domain.hash_encoded "octra:proof_qc_hash:v1" (fun buf ->
    Octra_net.Oce1.put_hash32 buf qc.task_id;
    Octra_net.Oce1.put_bool buf qc.result;
    Octra_net.Oce1.put_list Octra_net.Oce1.put_addr buf signers)

let epoch_root_response_sign_bytes
    ~chain_id ~epoch_id ~state_root ~responder_addr ~responder_head_epoch =
  Octra_net.Hash_domain.hash_encoded "octra:epoch_root_response:v1" (fun buf ->
    Octra_net.Oce1.put_string buf chain_id;
    Octra_net.Oce1.put_u64 buf epoch_id;
    Octra_net.Oce1.put_option Octra_net.Oce1.put_hash32 buf state_root;
    Octra_net.Oce1.put_addr buf responder_addr;
    Octra_net.Oce1.put_u64 buf responder_head_epoch)

let bundle_response_sign_bytes (r : C_codec.bundle_response) =
  Octra_net.Hash_domain.hash_encoded "octra:bundle_response:v1" (fun buf ->
    Octra_net.Oce1.put_string buf r.chain_id;
    Octra_net.Oce1.put_u64 buf r.epoch_id;
    Octra_net.Oce1.put_hash32 buf r.proposal_id;
    Octra_net.Oce1.put_addr buf r.responder_addr;
    Octra_net.Oce1.put_list Octra_net.Oce1.put_hash32 buf r.tx_hashes;
    Octra_net.Oce1.put_list Octra_net.Oce1.put_string buf r.txs_json;
    Octra_net.Oce1.put_list Octra_net.Oce1.put_string buf r.receipts_json)

let catchup_range_response_sign_bytes
    ~chain_id ~request_id ~responder_addr ~from_epoch ~records_root =
  Octra_net.Hash_domain.hash_encoded "octra:catchup_range_response:v1" (fun buf ->
    Octra_net.Oce1.put_string buf chain_id;
    Octra_net.Oce1.put_string buf request_id;
    Octra_net.Oce1.put_addr buf responder_addr;
    Octra_net.Oce1.put_u64 buf from_epoch;
    Octra_net.Oce1.put_hash32 buf records_root)

let catchup_records_root (records : C_codec.catchup_epoch_record list) =
  Octra_net.Hash_domain.hash_encoded "octra:catchup_records_root:v5" (fun buf ->
    Octra_net.Oce1.put_list (fun b r ->
      Octra_net.Oce1.put_u64 b r.C_codec.epoch_id;
      Octra_net.Oce1.put_hash32 b r.prev_state_root;
      Octra_net.Oce1.put_hash32 b r.state_root;
      Octra_net.Oce1.put_hash32 b r.tx_list_hash;
      Octra_net.Oce1.put_list Octra_net.Oce1.put_hash32 b r.tx_hashes;
      Octra_net.Oce1.put_hash32 b r.receipt_root;
      Octra_net.Oce1.put_list Octra_net.Oce1.put_string b r.receipts_json;
      Octra_net.Oce1.put_u64 b (Int64.bits_of_float r.epoch_ts);
      Octra_net.Oce1.put_addr b r.creator_addr;
      Octra_net.Oce1.put_u32_int b r.commit_round;
      begin
        match r.reward_source with
        | Some source -> C_reward_source.encode_into b source
        | None -> invalid_arg "catchup reward source is missing"
      end;
      match r.finality with
      | Some finality ->
        Octra_net.Oce1.put_hash32 b finality.finalize.proposal_id;
        Octra_net.Oce1.put_hash32 b
          (C_config.validator_set_hash finality.validator_set)
      | None -> invalid_arg "catchup finality is missing") buf records)

let catchup_records_root_v1_wire (records : C_codec.catchup_epoch_record list) =
  Octra_net.Hash_domain.hash_encoded "octra:catchup_records_root:v2" (fun buf ->
    Octra_net.Oce1.put_list (fun b r ->
      Octra_net.Oce1.put_u64 b r.C_codec.epoch_id;
      Octra_net.Oce1.put_hash32 b r.prev_state_root;
      Octra_net.Oce1.put_hash32 b r.state_root;
      Octra_net.Oce1.put_hash32 b r.tx_list_hash;
      Octra_net.Oce1.put_list Octra_net.Oce1.put_hash32 b r.tx_hashes;
      Octra_net.Oce1.put_addr b "";
      Octra_net.Oce1.put_u32_int b 0) buf records)

let verify_ed25519 ~pubkey_raw ~msg ~signature =
  Octra_ed25519.verify ~pub:pubkey_raw ~msg signature

let sign_ed25519 ~priv_raw ~msg =
  match Mirage_crypto_ec.Ed25519.priv_of_octets priv_raw with
  | Ok sk -> Mirage_crypto_ec.Ed25519.sign ~key:sk msg
  | Error _ -> failwith "Ed25519 sign: invalid private key"

let verify_vote ~pubkey_raw (v : vote) =
  let msg = vote_sign_bytes v in
  verify_ed25519 ~pubkey_raw ~msg ~signature:v.signature

let verify_round_sync ~pubkey_raw (sync : C_codec.round_sync) =
  let msg = round_sync_sign_bytes sync in
  verify_ed25519 ~pubkey_raw ~msg ~signature:sync.signature

let verify_propose ~pubkey_raw (p : propose) =
  let msg = propose_sign_bytes p in
  verify_ed25519 ~pubkey_raw ~msg ~signature:p.signature

let verify_proof_cert ~chain_id ~pubkey_raw (pc : proof_cert) =
  let msg = proof_cert_sign_bytes ~chain_id pc in
  verify_ed25519 ~pubkey_raw ~msg ~signature:pc.signature