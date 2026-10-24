(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Tx = Octra_core.Transaction
module Rest = Node_rest_facade
module Fold = Octra_core.Set_fold

let proof_fields (proof : Fold.proof) = [
  "duty_vote", `String (Base64.encode_exn (Octra_consensus.C_codec.encode_vote proof.vote));
  "duty_commit", `String (Base64.encode_exn
    (Octra_consensus.C_parent_commit.encode proof.commit));
]

let mark_fields ?(base = []) ~epoch ~chain_id ~address ~state proofs =
  let window = Octra_core.Validator_ready_policy.window in
  let last = if epoch > Int64.sub Int64.max_int window then Int64.max_int
    else Int64.add epoch window in
  let available at = at >= 0L && at <= last
    && Int64.sub last at <= Fold.participating.challenge in
  let proofs = List.sort (fun (left : Fold.proof) right ->
    let order = Int64.compare left.vote.epoch_id right.vote.epoch_id in
    if order <> 0 then order else String.compare left.vote.signature right.vote.signature) proofs in
  let rec collect previous claims = function
    | [] -> List.rev claims
    | (proof : Fold.proof) :: rest ->
      let claim = Fold.{epoch = proof.vote.epoch_id; signature = proof.vote.signature} in
      let sets = [claim.epoch, chain_id, proof.commit.validator_set] in
      if not (available claim.epoch) || Some claim.epoch = previous
          || Fold.verify_mark_claim Fold.participating ~chain_id ~epoch ~address ~sets claim state <> Ok () then
        collect previous claims rest
      else collect (Some claim.epoch) (claim :: claims) rest in
  match collect None [] proofs with
  | _ :: _ as claims -> Ok ["duty_marks", `String (Fold.encode_marks claims)]
  | [] ->
    match List.find_opt (fun (proof : Fold.proof) ->
      match Fold.find_final proof.vote.epoch_id state with
      | Some {round = None; _} when available proof.vote.epoch_id ->
        String.length (Yojson.Safe.to_string (`Assoc (base @ proof_fields proof)))
        <= Octra_core.Tx_payload.standard.message_validator_len
      | _ -> false) proofs with
    | Some proof -> Ok (proof_fields proof)
    | None -> Error "validator duty saved marks are unavailable"

let mark_fields_at ?(base = []) ~store ~head ~epoch ~state_root ~chain_id ~address proofs =
  let open Lwt.Syntax in
  match head () with
  | Some current when epoch > 0L
      && Int64.of_int current.Octra_core.Head_manifest.epoch_id = Int64.pred epoch
      && current.state_root = state_root ->
    let* state = Status_read_rpc.duty_state ~store ~head:current in
    if head () <> Some current then Lwt.return_error "committed validator duty head changed"
    else Lwt.return (Result.bind state (fun state ->
      mark_fields ~base ~epoch ~chain_id ~address ~state proofs))
  | _ -> Lwt.return_error "committed validator duty head changed"

type staged = { hash : string; tx : Tx.t }

type network = {
  broadcast : Octra_net.P2p_frame.frame -> unit;
  post : Tx.t -> (unit, Set_post.failure) result Lwt.t;
}

let sign ~signing ~key tx =
  let ( let* ) = Result.bind in
  let* () = Octra_core.Tx_payload.admit tx in
  let tx = {tx with Tx.ou = Tx.ou_cost tx} in
  Ok (Octra_core.Tx_signature.sign signing tx key)

let stage ~bft_mode runtime ledger tx =
  let ( let* ) = Result.bind in
  if tx.Tx.op_type <> Tx.ValidatorReady then Error "validator duty operation required"
  else
    let* tx = Rest.prepare_tx ledger tx |> Result.map_error snd in
    let* hash = Rest.add_tx_to_staging ~relay:false ~bft_mode runtime ledger tx in
    if hash <> Tx.hash tx then Error "validator duty staging hash mismatch"
    else Ok { hash; tx }

let put ~retry post staged =
  Set_post.put ~retry post ~hash:staged.hash staged.tx

let retry ~bft_mode runtime ledger network tx =
  match Rest.add_tx_to_staging ~relay:false ~bft_mode runtime ledger tx with
  | Error reason -> Lwt.return_error (Set_post.Wait reason)
  | Ok hash when hash <> Tx.hash tx ->
    Lwt.return_error (Set_post.Refused "validator duty staging hash mismatch")
  | Ok hash ->
    let payload = Octra_net.P2p_tx_gossip.encode
      (Octra_net.P2p_tx_gossip.Tx {
        hash;
        tx_json = Yojson.Safe.to_string (Tx.to_yojson tx);
      }) in
    network.broadcast Octra_net.P2p_frame.{ msg_type = msg_tx_gossip; payload };
    Lwt.map (function
      | Error (Set_post.Refused reason) -> Error (Set_post.Retry reason)
      | result -> result) (network.post tx)