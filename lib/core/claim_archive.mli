(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type pin = {
  epoch : int;
  state_root : string;
  index_root : string;
  next_txid : int64;
}

type t

val cipher_at : Store_irmin.t -> pin -> string -> (string option, string) result

val find : t -> string -> (int64, string) result

val source : t -> int64 -> ((int * string), string) result

val fold :
  t -> init:'a ->
  f:('a -> Claim_history.entry -> Transaction.t -> ('a, string) result) ->
  ('a, string) result

val sent : t -> index:int64 -> math:bool -> (string, string) result

val read :
  Store_irmin.t ->
  Store_chaindata.t ->
  before:pin ->
  after:pin ->
  max_txs:int ->
  (t, string) result

val key :
  t ->
  address:string ->
  index:int64 ->
  math:bool ->
  (Claim_history.key, string) result

val verify :
  send:t ->
  claim:t ->
  send_index:int64 ->
  claim_index:int64 ->
  sender_math:bool ->
  receiver_math:bool ->
  (Claim_history.checked, string) result