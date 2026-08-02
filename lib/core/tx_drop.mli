(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type row = {
  hash : string;
  from_addr : string;
  to_addr : string;
  nonce : int;
  ou : Z.t;
  op_type : Transaction.op_type;
  reason : string;
  detail : string;
  dropped_at : float;
}

type t

val open_db :
  ?max_rows:int ->
  string ->
  t

val save_many :
  t ->
  row list ->
  (unit, string) result

val find :
  t ->
  string ->
  row option

val by_addr :
  t ->
  string ->
  limit:int ->
  offset:int ->
  row list

val close : t -> unit