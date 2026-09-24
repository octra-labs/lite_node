(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type change =
  | Keep
  | Deposit of Z.t
  | Withdraw of Z.t
  | Send of string
  | Claim of Claim_history.checked

type t

type summary = {
  commitment : string;
  records : int;
  changes : int;
  received : int;
}

val create : address:string -> t
val apply : t -> hash:string -> change -> (t, string) result
val finish : t -> summary