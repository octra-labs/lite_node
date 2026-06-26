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


type t = {
  mutable queued_target : int64 option;
  mutable queued_reason : string;
  mutable queued_gap_active : bool;
}

type queued = {
  target_epoch : int64;
  reason : string;
}

type snapshot = {
  target_epoch : int64 option;
  reason : string;
  gap_active : bool;
}

let default_reason = "coalesced"

let create () = {
  queued_target = None;
  queued_reason = default_reason;
  queued_gap_active = false;
}

let snapshot t = {
  target_epoch = t.queued_target;
  reason = t.queued_reason;
  gap_active = t.queued_gap_active;
}

let target t =
  t.queued_target

let gap_active t =
  t.queued_gap_active

let activate_gap t =
  t.queued_gap_active <- true

let deactivate_gap t =
  t.queued_gap_active <- false

let clear_target t =
  t.queued_target <- None;
  t.queued_reason <- default_reason

let clear_all t =
  clear_target t;
  t.queued_gap_active <- false

let should_replace t target_epoch =
  match t.queued_target with
  | None -> true
  | Some old -> Int64.compare target_epoch old > 0

let queue t ~target_epoch ~reason =
  if should_replace t target_epoch then begin
    t.queued_target <- Some target_epoch;
    t.queued_reason <- reason
  end;
  snapshot t

let queue_gap t ~target_epoch ~reason =
  let result = queue t ~target_epoch ~reason in
  t.queued_gap_active <- true;
  { result with gap_active = true }

let take_if_after t ~head =
  match t.queued_target with
  | Some target_epoch when Int64.compare target_epoch head > 0 ->
    let reason = t.queued_reason in
    clear_target t;
    Some { target_epoch; reason }
  | _ ->
    clear_target t;
    None

let target_label snapshot =
  match snapshot.target_epoch with
  | Some t -> Int64.to_string t
  | None -> "-"