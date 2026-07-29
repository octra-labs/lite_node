(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type proof

type verified = {
  id : string;
  offender : string;
  evidence_epoch : int64;
}

val proof_of_message : string option -> (proof, string) result
val message : Octra_consensus.C_evidence.vote_conflict -> string
val verify :
  chain_id:string ->
  current_epoch:int64 ->
  evidence_epochs:int64 ->
  bonded_epoch:int64 ->
  address:string ->
  pubkey:string ->
  proof ->
  (verified, string) result