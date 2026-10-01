(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
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

val encode : t -> string
val decode : string -> t
val order_key : t -> string
val addresses : t -> string list
val newest : limit:int -> t list -> t list