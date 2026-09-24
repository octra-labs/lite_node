(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type identity = {
  chain_id : string;
  address : string;
  pubkey : string;
  bonded_epoch : int64;
}

type t

val create : identity -> privkey:string -> (t, string) result
val encode : t -> string
val decode : string -> (t, string) result
val applies : identity -> t -> (bool, string) result
val id : t -> string