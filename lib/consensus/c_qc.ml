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

let validate_finalize ~chain_id ~validator_set ~verify_vote f =
  if f.chain_id <> chain_id then invalid "chain_id"
  else if f.header.chain_id <> f.chain_id then invalid "header_chain_id"
  else if f.header.epoch_id <> f.epoch_id then invalid "header_epoch"
  else if Octra_net.Hash_domain.is_nil f.proposal_id then invalid "nil_proposal"
  else if f.proposal_id <> C_hash.proposal_id f.header then invalid "proposal_id"
  else if List.length f.precommits < validator_set.quorum then invalid "quorum"
  else
    let addrs = List.map (fun (v : vote) -> v.validator) f.precommits in
    if not (unique addrs) then invalid "duplicate_validator"
    else
      let bad_vote =
        List.find_opt
          (fun vote ->
            not (vote_ok
              ~chain_id:f.chain_id
              ~epoch_id:f.epoch_id
              ~round:f.commit_round
              ~proposal_id:f.proposal_id
              vote))
          f.precommits
      in
      match bad_vote with
      | Some _ -> invalid "vote_fields"
      | None ->
        let bad_validator =
          List.find_opt
            (fun (vote : vote) -> not (C_types.is_validator validator_set vote.validator))
            f.precommits
        in
        match bad_validator with
        | Some _ -> invalid "validator"
        | None ->
          let bad_sig = List.find_opt (fun (vote : vote) -> not (verify_vote vote)) f.precommits in
          match bad_sig with
          | Some _ -> invalid "signature"
          | None -> Valid