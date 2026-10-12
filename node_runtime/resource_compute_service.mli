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
  funding : head -> Octra_consensus.Resource_compute_protocol.header ->
    (Octra_core.Compute_funding.funded option, string) result Lwt.t;
  self_test :
    unit ->
    (Resource_compute_provider_rpc.self_test, string) result Lwt.t;
  nonce : unit -> string;
  broadcast : string -> unit Lwt.t;
  send : peer:string -> string -> unit Lwt.t;
  cancel_rounds : unit -> bool;
  now : unit -> int64;
  relay : source:string -> string -> unit Lwt.t;
  sleep : float -> unit Lwt.t;
}

type request = {
  unit_price : int64 option;
  fee_cap : int64 option;
  memory_bytes : int option;
  guest_bytes : int option;
  fuel_limit : int option;
  valid_until : int64 option;
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

type cancel_status = {
  selection_root : string;
  members : string list;
  missing : string list;
  receipts : Octra_consensus.Resource_compute_protocol.cancel_ack list;
  expires : int64 option;
}

type job_status =
  | Waiting
  | Funding of Octra_consensus.Resource_compute_protocol.header
  | Finished of response
  | Refused of string
  | Cancelled of cancel_status

type cancellation = {
  caller : string;
  caller_public_key : string;
  caller_signature : string;
  session_id : string;
  until : int64 option;
}

type t

val create :
  deps:deps ->
  provider:provider option ->
  history:(Octra_core.Compute_history.t, string) result ->
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
  (string * string, string) result Lwt.t

val status :
  t ->
  Compute_read.t ->
  (job_status option, string) result Lwt.t

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

val cancel_receipts :
  t -> cancellation ->
  (Octra_consensus.Resource_compute_protocol.cancel_ack list, string) result Lwt.t

val active_jobs :
  t ->
  int Lwt.t

val provider_enabled :
  t ->
  bool

val shutdown :
  t ->
  unit Lwt.t