(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


type head = {
  epoch_id : int;
  ledger_root : string;
}

type kind =
  | Aligned
  | Direct_head_write
  | Missing_head
  | Unexpected_head

type plan = {
  kind : kind;
  irmin_last_before : int;
  rollback_to_head : bool;
}

let string_of_kind = function
  | Aligned -> "aligned"
  | Direct_head_write -> "direct_head_write"
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
      kind = Direct_head_write;
      irmin_last_before = h.epoch_id;
      rollback_to_head = true;
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