(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type rejection = {
  position : int;
  tx : Transaction.t;
  error_type : string;
  reason : string;
}

type partition = {
  preverify : string list;
  rejections : rejection list;
}

val encode_rejection : rejection -> string
val split_admit : string list -> (partition, string) result
val build :
  candidates:Transaction.t list ->
  (Transaction.t * string * string) list ->
  (rejection list, string) result
val merge :
  confirmed:Transaction.t list ->
  rejections:rejection list ->
  (Transaction.t list, string) result
val decode_admit :
  confirmed:Transaction.t list ->
  string list ->
  (partition, string) result
val decode_final :
  confirmed:Transaction.t list ->
  string list ->
  (partition, string) result
val encode : string list -> rejection list -> string list
val equal : rejection list -> rejection list -> bool