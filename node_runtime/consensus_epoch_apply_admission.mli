(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type catchup = {
  head : int;
  reason : string;
  target_epoch : int64;
  queue_reason : string;
}

type already_applied = {
  head : int;
  next_epoch : int;
}

type plan =
  | Apply_now
  | Already_applied of already_applied
  | Need_catchup of catchup

val plan :
  consensus_mode:bool ->
  head:int ->
  current_epoch:int ->
  plan