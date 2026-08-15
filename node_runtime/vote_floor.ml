(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let ( let* ) result next =
  Result.bind result next

let vote_hash value =
  Digestif.SHA256.digest_string value
  |> Digestif.SHA256.to_hex

let matching_head data_dir through_epoch =
  match Octra_core.Head_manifest.load_result data_dir with
  | Octra_core.Head_manifest.Present head
    when Int64.equal (Int64.of_int head.epoch_id) through_epoch ->
    Ok ()
  | Octra_core.Head_manifest.Present _ ->
    Error "head epoch does not match local finalized head"
  | Octra_core.Head_manifest.Missing ->
    Error "local finalized head is missing"
  | Octra_core.Head_manifest.Corrupt _ ->
    Error "local finalized head is invalid"

let valid_identity validator pubkey =
  if String.length pubkey <> 32 then
    Error "validator public key is invalid"
  else
    let encoded = Base64.encode_exn pubkey in
    if Octra_core.Crypto.Address.verify_address_pubkey validator encoded then Ok ()
    else Error "validator public key does not match address"

let vote_of_wire wire =
  try
    let vote = Octra_consensus.C_codec.decode_vote wire in
    if String.equal (Octra_consensus.C_codec.encode_vote vote) wire then Ok vote
    else Error "pending vote wire is not exact"
  with exn ->
    Error ("pending vote wire decode failed: " ^ Printexc.to_string exn)

let sync_of_wire ~chain_id ~validator ~pubkey ~epoch_id encoded =
  let* wire =
    try Ok (Base64.decode_exn encoded)
    with exn -> Error ("round sync base64 decode failed: " ^ Printexc.to_string exn)
  in
  let* sync =
    try
      let sync = Octra_consensus.C_codec.decode_round_sync wire in
      if String.equal (Octra_consensus.C_codec.encode_round_sync sync) wire then
        Ok sync
      else Error "round sync wire is not exact"
    with exn ->
      Error ("round sync wire decode failed: " ^ Printexc.to_string exn)
  in
  if not (String.equal sync.chain_id chain_id) then
    Error "round sync chain does not match"
  else if not (Int64.equal sync.epoch_id epoch_id) then
    Error "round sync epoch does not match"
  else if sync.request then
    Error "round sync is a request"
  else if not (String.equal sync.validator validator) then
    Error "round sync validator does not match"
  else if not (Octra_consensus.C_hash.verify_round_sync ~pubkey_raw:pubkey sync) then
    Error "round sync signature is invalid"
  else Ok sync.round

let current_vote ~chain_id ~validator ~pubkey ~epoch_id entry =
  if entry.Octra_core.Wal.epoch_id < 0 then
    Error "pending vote epoch is invalid"
  else if Int64.compare (Int64.of_int entry.epoch_id) epoch_id > 0 then
    Error "pending consensus vote exceeds current epoch"
  else if not (Int64.equal (Int64.of_int entry.epoch_id) epoch_id) then
    Ok None
  else if entry.validator_addr <> validator then
    Error "pending vote validator does not match"
  else if entry.round < 0 || entry.round > Octra_consensus.C_codec.max_round then
    Error "pending vote round is invalid"
  else
    match entry.vote_b64 with
    | None -> Error "pending vote has no stored precommit"
    | Some encoded ->
      let* vote =
        try vote_of_wire (Base64.decode_exn encoded)
        with exn ->
          Error ("pending vote base64 decode failed: " ^ Printexc.to_string exn)
      in
      if not (String.equal vote.chain_id chain_id) then
        Error "pending vote chain does not match"
      else if not (String.equal vote.validator validator) then
        Error "pending vote signer does not match"
      else if not (Int64.equal vote.epoch_id epoch_id) then
        Error "pending vote epoch does not match"
      else if vote.round <> entry.round then
        Error "pending vote round does not match"
      else if vote.vote_type <> Octra_consensus.C_types.Precommit then
        Error "pending vote type is invalid"
      else if not (String.equal (vote_hash vote.proposal_id) entry.proposal_id) then
        Error "pending vote proposal does not match"
      else if not (Octra_consensus.C_hash.verify_vote ~pubkey_raw:pubkey vote) then
        Error "pending vote signature is invalid"
      else
        Ok (Some vote)

let pending_votes ~data_dir ~chain_id ~validator ~pubkey ~epoch_id =
  try
    let entries = Octra_core.Wal.read_pending_commits data_dir in
    let memory = Octra_consensus.C_vote_log.memory () in
    List.fold_left
      (fun result entry ->
        let* votes = result in
        let* vote =
          current_vote ~chain_id ~validator ~pubkey ~epoch_id entry
        in
        match vote with
        | None -> Ok votes
        | Some vote ->
          let* _ = Octra_consensus.C_vote_log.keep memory vote in
          Ok (vote :: votes))
      (Ok [])
      entries
  with exn ->
    Error ("pending vote read failed: " ^ Printexc.to_string exn)

let existing_round store ~chain_id ~validator ~epoch_id =
  Octra_consensus.C_vote_log.max_round store ~chain_id ~validator ~epoch_id

let maximum_round initial votes =
  List.fold_left
    (fun highest (vote : Octra_consensus.C_types.vote) ->
      max highest vote.round)
    initial
    votes

let plan ~data_dir ~chain_id ~validator ~pubkey ~through_epoch ~round =
  if Int64.compare through_epoch 0L < 0
     || Int64.equal through_epoch Int64.max_int then
    Error "vote floor epoch is invalid"
  else if round < 0 || round > Octra_consensus.C_codec.max_round then
    Error "vote floor round is invalid"
  else
    let epoch_id = Int64.succ through_epoch in
    let store = Octra_consensus.C_vote_log.disk ~data_dir in
    let* () = matching_head data_dir through_epoch in
    let* () = valid_identity validator pubkey in
    let* votes = pending_votes ~data_dir ~chain_id ~validator ~pubkey ~epoch_id in
    let* () =
      List.fold_left
        (fun result vote ->
          let* () = result in
          Octra_consensus.C_vote_log.check store vote)
        (Ok ())
        votes
    in
    let* prior = existing_round store ~chain_id ~validator ~epoch_id in
    let marked =
      match prior with
      | None -> maximum_round round votes
      | Some value -> maximum_round (max round value) votes
    in
    Ok (store, votes, epoch_id, marked)

let check ~data_dir ~chain_id ~validator ~pubkey ~through_epoch ~round =
  let* (_, _, _, marked) =
    plan ~data_dir ~chain_id ~validator ~pubkey ~through_epoch ~round
  in
  Ok marked

let prepared ~data_dir ~chain_id ~validator ~pubkey ~through_epoch ~round =
  let* (store, _, epoch_id, marked) =
    plan ~data_dir ~chain_id ~validator ~pubkey ~through_epoch ~round
  in
  if marked <> round then
    Error "vote floor round does not match staged round"
  else
    let* mark = Octra_consensus.C_vote_log.round_mark store in
    match mark with
    | Some (marked_epoch, marked_round)
      when Int64.equal marked_epoch epoch_id && marked_round = round ->
      Ok round
    | _ -> Error "vote floor round is not prepared"

let staged ~data_dir ~chain_id ~validator ~pubkey ~through_epoch ~round =
  let* marked =
    prepared ~data_dir ~chain_id ~validator ~pubkey ~through_epoch ~round
  in
  let store = Octra_consensus.C_vote_log.disk ~data_dir in
  let* floor = Octra_consensus.C_vote_log.floor store in
  match floor with
  | Some value when Int64.equal value through_epoch -> Ok marked
  | _ -> Error "vote floor is not staged"

let set ~data_dir ~chain_id ~validator ~pubkey ~through_epoch ~round =
  let* (store, votes, epoch_id, marked) =
    plan ~data_dir ~chain_id ~validator ~pubkey ~through_epoch ~round
  in
  let* () =
    List.fold_left
        (fun result vote ->
          let* () = result in
          Octra_consensus.C_vote_log.keep store vote |> Result.map (fun _ -> ()))
      (Ok ())
      votes
  in
  let* () =
    Octra_consensus.C_vote_log.set_round_mark store ~epoch_id ~round:marked
  in
  let* () = Octra_consensus.C_vote_log.set_floor store ~through_epoch in
  Ok marked

let sync_round ~chain_id ~validator ~pubkey ~through_epoch ~round_min ~sync =
  if round_min < 0 || round_min > Octra_consensus.C_codec.max_round then
    Error "round minimum is invalid"
  else
  let* round =
    sync_of_wire
      ~chain_id
      ~validator
      ~pubkey
      ~epoch_id:(Int64.succ through_epoch)
      sync
  in
  Ok (max round round_min)

let check_sync ~data_dir ~chain_id ~validator ~pubkey ~through_epoch
    ~round_min ~sync =
  let* round =
    sync_round
      ~chain_id
      ~validator
      ~pubkey
      ~through_epoch
      ~round_min
      ~sync
  in
  check ~data_dir ~chain_id ~validator ~pubkey ~through_epoch ~round

let set_sync ~data_dir ~chain_id ~validator ~pubkey ~through_epoch
    ~round_min ~sync =
  let* round =
    sync_round
      ~chain_id
      ~validator
      ~pubkey
      ~through_epoch
      ~round_min
      ~sync
  in
  set ~data_dir ~chain_id ~validator ~pubkey ~through_epoch ~round