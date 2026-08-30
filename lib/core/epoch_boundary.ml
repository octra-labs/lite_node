(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type head = {
  epoch_id : int;
  ledger_root : string;
}

type kind =
  | Aligned
  | Root_mismatch
  | Missing_head
  | Unexpected_head

type plan = {
  kind : kind;
  irmin_last_before : int;
  rollback_to_head : bool;
}

let string_of_kind = function
  | Aligned -> "aligned"
  | Root_mismatch -> "root_mismatch"
  | Missing_head -> "missing_head"
  | Unexpected_head -> "unexpected_head"

let plan ~commit_epoch ~pre_root ~meta_epoch head =
  match head with
  | Some h when h.epoch_id = commit_epoch - 1 && String.equal h.ledger_root pre_root ->
    {
      kind = Aligned;
      irmin_last_before = h.epoch_id;
      rollback_to_head = true;
    }
  | Some h when h.epoch_id = commit_epoch - 1 ->
    {
      kind = Root_mismatch;
      irmin_last_before = h.epoch_id;
      rollback_to_head = false;
    }
  | Some _ ->
    {
      kind = Unexpected_head;
      irmin_last_before = meta_epoch;
      rollback_to_head = false;
    }
  | None ->
    {
      kind = Missing_head;
      irmin_last_before = meta_epoch;
      rollback_to_head = false;
    }