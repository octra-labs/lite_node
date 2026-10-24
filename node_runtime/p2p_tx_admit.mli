(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type verdict =
  | Accept
  | Invalid_address
  | Invalid_payload
  | Timestamp_drift of float
  | Invalid_signature

val to_valid : Octra_core.Transaction.t -> bool

val addr_valid : Octra_core.Transaction.t -> bool

val sig_valid : signing:Octra_core.Tx_signature.t -> Octra_core.Transaction.t -> string option -> bool

val admit :
  ?duty:(int64 * Octra_core.Rule_graph.mode) option ->
  ?bft_mode:bool ->
  signing:Octra_core.Tx_signature.t ->
  now:float ->
  max_drift:float ->
  sender_pk:string option ->
  Octra_core.Transaction.t ->
  verdict