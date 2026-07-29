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

let finality_gap_reason ~current_epoch ~head =
  Printf.sprintf "finality_gap height = %d head = %d" current_epoch head

let plan ~consensus_mode ~head ~current_epoch =
  if not consensus_mode then
    Apply_now
  else
    match Octra_consensus.Finality_log.decide ~head current_epoch with
    | Octra_consensus.Finality_log.Already_applied ->
      Already_applied { head; next_epoch = head + 1 }
    | Octra_consensus.Finality_log.Need_catchup ->
      Need_catchup {
        head;
        reason = finality_gap_reason ~current_epoch ~head;
        target_epoch = Int64.of_int current_epoch;
        queue_reason = "finality_gap_apply";
      }
    | Octra_consensus.Finality_log.Apply_now ->
      Apply_now