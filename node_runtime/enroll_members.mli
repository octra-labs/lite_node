(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  epoch : int64;
  active : bool;
  scheduled : bool;
  activate_epoch : int64 option;
  next_set_epoch : int64 option;
  set_hash : string;
}

val of_values :
  head_epoch:int64 ->
  address:string ->
  pubkey:string ->
  string option * string option ->
  (t option, string) result