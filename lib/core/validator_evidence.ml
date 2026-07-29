(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type proof = {
  conflict : Octra_consensus.C_evidence.vote_conflict;
}

type verified = {
  id : string;
  offender : string;
  evidence_epoch : int64;
}

let max_message_bytes = 8_192

let proof_of_raw raw =
  try
    let conflict =
      Octra_consensus.C_evidence.decode_vote_conflict raw
    in
    let canonical =
      Octra_consensus.C_evidence.encode_vote_conflict conflict
    in
    if String.equal canonical raw then Ok { conflict }
    else Error "validator evidence is not canonical"
  with _ ->
    Error "invalid validator evidence"

let proof_of_message = function
  | None -> Error "validator evidence requires message payload"
  | Some raw when String.length raw > max_message_bytes ->
    Error "validator evidence payload is too large"
  | Some raw ->
    begin
      try
        match Yojson.Safe.from_string raw with
        | `Assoc ["evidence", `String encoded] ->
          let decoded = Base64.decode_exn encoded in
          if not (String.equal (Base64.encode_exn decoded) encoded) then
            Error "validator evidence base64 is not canonical"
          else
            proof_of_raw decoded
        | _ ->
          Error "validator evidence requires one evidence field"
      with _ ->
        Error "invalid validator evidence payload"
    end

let message conflict =
  `Assoc [
    "evidence",
    `String
      (conflict
       |> Octra_consensus.C_evidence.encode_vote_conflict
       |> Base64.encode_exn);
  ]
  |> Yojson.Safe.to_string

let verify ~chain_id ~current_epoch ~evidence_epochs ~bonded_epoch
    ~address ~pubkey proof =
  let conflict = proof.conflict in
  let vote = conflict.Octra_consensus.C_evidence.first in
  let evidence_epoch = vote.Octra_consensus.C_types.epoch_id in
  if Int64.compare evidence_epochs 0L <= 0 then
    Error "validator evidence window is not positive"
  else if not (String.equal vote.chain_id chain_id) then
    Error "validator evidence chain mismatch"
  else if not (String.equal vote.validator address) then
    Error "validator evidence offender mismatch"
  else if Int64.compare evidence_epoch bonded_epoch < 0 then
    Error "validator evidence predates current bond"
  else if Int64.compare evidence_epoch current_epoch > 0 then
    Error "validator evidence epoch is in the future"
  else if
    Int64.compare
      (Int64.sub current_epoch evidence_epoch)
      evidence_epochs >= 0
  then
    Error "validator evidence expired"
  else if
    not
      (Octra_consensus.C_evidence.verify_vote_conflict
         ~pubkey_raw:pubkey
         conflict)
  then
    Error "invalid validator evidence signatures"
  else
    Ok {
      id =
        conflict
        |> Octra_consensus.C_evidence.vote_conflict_id
        |> Base64.encode_exn;
      offender = address;
      evidence_epoch;
    }