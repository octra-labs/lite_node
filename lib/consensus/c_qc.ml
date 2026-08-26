(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open C_types

type verdict =
  | Valid
  | Invalid of string

let invalid s = Invalid s

let unique addrs =
  let sorted = List.sort String.compare addrs in
  let rec loop = function
    | a :: b :: _ when a = b -> false
    | _ :: rest -> loop rest
    | [] -> true
  in
  loop sorted

let vote_ok ~chain_id ~epoch_id ~round ~proposal_id (vote : vote) =
  vote.chain_id = chain_id
  && vote.epoch_id = epoch_id
  && vote.round = round
  && vote.vote_type = Precommit
  && vote.proposal_id = proposal_id

let validate_certificate_with_policy
    ~harden_quorum
    ~chain_id
    ~validator_set
    ~verify_vote
    (certificate : commit_certificate) =
  if certificate.chain_id <> chain_id then invalid "chain_id"
  else
    let count_floor =
      harden_quorum
      && C_quorum_policy.count_floor_required
           ~chain_id
           ~epoch_id:certificate.epoch_id
    in
    let validator_set =
      if count_floor then resilient_validator_set validator_set
      else
        validator_set_for_epoch
          ~chain_id
          ~epoch_id:certificate.epoch_id
          validator_set
    in
    if certificate.header.chain_id <> certificate.chain_id then
      invalid "header_chain_id"
    else if certificate.header.epoch_id <> certificate.epoch_id then
      invalid "header_epoch"
    else if
      certificate.header.proto_version <> proto_version_parent_legacy
      && certificate.header.proto_version <> proto_version_current
    then
      invalid "header_proto_version"
    else if not (is_validator validator_set certificate.header.creator_addr) then
      invalid "header_creator"
    else if Octra_net.Hash_domain.is_nil certificate.proposal_id then
      invalid "nil_proposal"
    else if
      certificate.proposal_id <> C_hash.proposal_id certificate.header
    then
      invalid "proposal_id"
    else begin
      let addrs =
        List.map
          (fun (vote : vote) -> vote.validator)
          certificate.precommits
      in
      if not (unique addrs) then invalid "duplicate_validator"
      else if not
        (if count_floor then C_types.has_quorum validator_set addrs
         else
           C_types.has_quorum_at
             ~chain_id
             ~epoch_id:certificate.epoch_id
             validator_set
             addrs)
      then invalid "quorum"
      else begin
        let bad_vote =
          List.find_opt
            (fun vote ->
              not (vote_ok
                ~chain_id:certificate.chain_id
                ~epoch_id:certificate.epoch_id
                ~round:certificate.commit_round
                ~proposal_id:certificate.proposal_id
                vote))
            certificate.precommits
        in
        match bad_vote with
        | Some _ -> invalid "vote_fields"
        | None ->
          let bad_validator =
            List.find_opt
              (fun (vote : vote) -> not (C_types.is_validator validator_set vote.validator))
              certificate.precommits
          in
          match bad_validator with
          | Some _ -> invalid "validator"
          | None ->
            let bad_sig =
              List.find_opt
                (fun (vote : vote) -> not (verify_vote vote))
                certificate.precommits
            in
            match bad_sig with
            | Some _ -> invalid "signature"
            | None -> Valid
      end
    end

let validate_certificate ~chain_id ~validator_set ~verify_vote certificate =
  validate_certificate_with_policy
    ~harden_quorum:false
    ~chain_id
    ~validator_set
    ~verify_vote
    certificate

let validate_finalize_with ~version_valid ~harden_quorum ~chain_id ~validator_set
    ~verify_vote (finalize : finalize) =
  if not (version_valid finalize) then
    invalid "header_proto_version"
  else if
    C_hash.parent_commit_hash_opt finalize.parent_commit
    <> finalize.header.parent_commit_hash
  then
    invalid "parent_commit_hash"
  else
    validate_certificate_with_policy
      ~harden_quorum
      ~chain_id
      ~validator_set
      ~verify_vote
      (C_types.certificate_of_finalize finalize)

let validate_finalize ~chain_id ~validator_set ~verify_vote finalize =
  validate_finalize_with
    ~harden_quorum:false
    ~version_valid:(fun finalize ->
      finalize.header.proto_version
      = C_protocol.version_for_epoch finalize.epoch_id)
    ~chain_id
    ~validator_set
    ~verify_vote
    finalize

let validate_persisted_finalize ~chain_id ~validator_set ~verify_vote finalize =
  validate_finalize_with
    ~harden_quorum:false
    ~version_valid:(fun finalize ->
      C_protocol.supported_version finalize.header.proto_version)
    ~chain_id
    ~validator_set
    ~verify_vote
    finalize

let validate_persisted_finalize_hardened
    ~chain_id
    ~validator_set
    ~verify_vote
    finalize =
  validate_finalize_with
    ~harden_quorum:true
    ~version_valid:(fun finalize ->
      C_protocol.supported_version finalize.header.proto_version)
    ~chain_id
    ~validator_set
    ~verify_vote
    finalize