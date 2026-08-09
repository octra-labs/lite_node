(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type head = {
  epoch_id : int64;
  state_root : string;
}

type provider = {
  limits : Resource_compute_provider_config.limits;
  engine : Resource_compute_provider_engine.t;
}

type deps = {
  chain_id : string;
  node_id : string;
  sign : string -> string;
  validator_set : unit -> Octra_consensus.C_types.validator_set;
  head : unit -> head option;
  state_root_at : int64 -> string option;
  self_test :
    unit ->
    (Resource_compute_provider_rpc.self_test, string) result Lwt.t;
  nonce : unit -> string;
  broadcast : string -> unit Lwt.t;
  relay : source:string -> string -> unit Lwt.t;
  sleep : float -> unit Lwt.t;
}

type request = {
  model_epoch : int64;
  model_state_root : string;
  circle_id : string;
  graph_root : string;
  model_root : string;
  program_root : string;
  executor_root : string;
  caller : string;
  caller_public_key : string;
  caller_signature : string;
  session_id : string;
  sequence : int64;
  method_name : string;
  params_json : string;
  max_output_bytes : int;
}

type response = {
  output_json : string;
  request : Octra_consensus.Resource_compute_certificate.request;
  certificate : Octra_consensus.Resource_compute_certificate.certificate;
  selection : Octra_consensus.Resource_compute_selection.selection;
}

type job_status =
  | Waiting
  | Finished of response
  | Refused of string

type cancellation = {
  caller : string;
  caller_public_key : string;
  caller_signature : string;
  session_id : string;
}

type t

val create :
  deps:deps ->
  provider:provider option ->
  t

val on_message :
  t ->
  source:string ->
  string ->
  unit Lwt.t

val compute :
  t ->
  request ->
  (response, string) result Lwt.t

val submit :
  t ->
  request ->
  (string, string) result Lwt.t

val status :
  t ->
  caller:string ->
  job_id:string ->
  job_status option Lwt.t

val cancellation_sign_bytes :
  chain_id:string ->
  caller:string ->
  caller_public_key:string ->
  session_id:string ->
  string

val cancel :
  t ->
  cancellation ->
  (int, string) result Lwt.t

val active_jobs :
  t ->
  int Lwt.t

val provider_enabled :
  t ->
  bool

val shutdown :
  t ->
  unit Lwt.t