(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type entry = {
  epoch : int;
  index : int64;
  hash : string;
  json : string;
}

type key = {
  blob : string;
  hash : string;
  math : bool;
}

type checked = private {
  id : int;
  sender : string;
  receiver : string;
  send_hash : string;
  claim_hash : string;
  commitment : string;
}

val verify :
  send:entry ->
  claim:entry ->
  before_send:Yojson.Safe.t option ->
  before_claim:Yojson.Safe.t option ->
  output:Yojson.Safe.t ->
  sender_key:key ->
  receiver_key:key ->
  (checked, string) result

val sent : entry -> key -> (string, string) result

val amount : op:Transaction.op_type -> entry -> key -> (Z.t, string) result