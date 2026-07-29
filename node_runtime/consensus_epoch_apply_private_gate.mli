(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type reject = {
  tag : string;
  reason : string;
}

type notify_reject = {
  tag : string;
  reason : string;
  notify_reason : string;
}

type kat_action =
  | Kat_apply
  | Kat_backfill_then_apply
  | Kat_reject of notify_reject

val fhe_cap :
  current:int ->
  max:int ->
  reject option

val encrypted_debit_limit :
  had_debit:bool ->
  reject option

val stealth_target :
  target:string ->
  reject option

val aes_kat_mismatch :
  op:string ->
  reject

val aes_kat_notify : op:string -> string

val aes_kat_notify_reject :
  op:string ->
  notify_reject

val kat_action :
  op:string ->
  Octra_core.Private_ledger.kat ->
  kat_action

val first_reject : reject option list -> reject option