(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Protocol = Octra_consensus.Resource_compute_protocol
module Certificate = Octra_consensus.Resource_compute_certificate

type outcome = Complete | Stopped of int | Exhausted

type measured = private {
  request_id : string;
  output_hash : string;
  output_bytes : int;
  trace_root : string;
  steps : int64;
  operations : int64;
  outcome : outcome;
}

val measure :
  request:Certificate.request -> output:string -> trace_root:string ->
  steps:int64 -> operations:int64 -> outcome:outcome -> (measured, string) result

type t = private {
  header : Protocol.header;
  measured : measured;
  root : string;
  charged : Z.t;
  refund : Z.t;
  payments : (string * Z.t) list;
}

val verify :
  epoch:int64 -> header:Protocol.header ->
  validators:Octra_consensus.C_types.validator_set -> selected:string list ->
  votes:Certificate.vote list -> measured:measured -> (t, string) result

type fault = Rejected of string | Unavailable of string
type funding = Due of int64 | Paid of int64
type verifier = store:Store_irmin.t -> epoch:int64 ->
  funded:(Protocol.header -> (funding, fault) result Lwt.t) ->
  string -> (t list, fault) result Lwt.t

val max_wire : int
val max_fuel : int
val model_limit : int64
val resource_limits : fuel:int option -> memory:int option -> guest:int option -> (unit, string) result
val resources : Certificate.request -> (unit, string) result
val consensus_id : string
val encode_request : fuel:int -> string list -> string
val decode_request : string -> (int * string list, string) result
val fee : string -> Z.t