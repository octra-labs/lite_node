(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type action =
  | Request of string list
  | Serve of string list
  | Receive of {
      hash : string;
      tx_json : string;
    }
  | Legacy

type tx_check =
  | Decoded of Octra_core.Transaction.t
  | Decode_error of string
  | Hash_mismatch of {
      advertised : string;
      actual : string;
    }

val plan_inv :
  ?cfg:Octra_net.P2p_tx_gossip_guard.cfg ->
  has:(string -> bool) ->
  string list ->
  string list

val plan_get :
  ?cfg:Octra_net.P2p_tx_gossip_guard.cfg ->
  has:(string -> bool) ->
  string list ->
  string list

val plan :
  ?cfg:Octra_net.P2p_tx_gossip_guard.cfg ->
  has:(string -> bool) ->
  string ->
  action

val check_tx :
  hash:string ->
  tx_json:string ->
  tx_check