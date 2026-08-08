(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type request = {
  chain_id : string;
  epoch_id : int64;
  expires_epoch : int64;
  validator_set_root : string;
  selection_root : string;
  circle_id : string;
  state_root : string;
  model_epoch : int64;
  model_state_root : string;
  graph_root : string;
  model_root : string;
  program_root : string;
  executor_root : string;
  caller : string;
  session_id : string;
  sequence : int64;
  input_hash : string;
  input_bytes : int;
  max_output_bytes : int;
}

type vote = {
  request_id : string;
  node_id : string;
  output_hash : string;
  trace_root : string;
  output_bytes : int;
  steps : int64;
  operations : int64;
  signature : string;
}

type signer = {
  node_id : string;
  weight : Z.t;
  vote_id : string;
}

type certificate = {
  request_id : string;
  output_hash : string;
  trace_root : string;
  output_bytes : int;
  steps : int64;
  operations : int64;
  votes : vote list;
  signers : signer list;
  signed_weight : Z.t;
  root : string;
}

type reject =
  | Request_malformed
  | Chain_mismatch
  | Epoch_mismatch
  | Selection_malformed
  | Threshold_malformed

type decision =
  | Rejected of reject
  | Insufficient
  | Conflicting
  | Agreed of certificate

val request_id : request -> string
val vote_sign_bytes : vote -> string
val vote_id : vote -> string
val decide :
  chain_id:string ->
  current_epoch:int64 ->
  max_ttl:int64 ->
  selected:string list ->
  signer_floor:int ->
  weight_floor:Z.t ->
  weight_of:(string -> Z.t option) ->
  verify_signature:(vote -> bool) ->
  request ->
  vote list ->
  decision