(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  state_attested : bool ref;
  state_attested_head : int option ref;
  state_attested_root : string option ref;
  quarantine_active : bool ref;
  quarantine_reason : string ref;
  quarantine_since_epoch : int ref;
  prev_root_mismatch_streak : int ref;
  state_root_mismatch_streak : int ref;
  ahead_of_target_streak : int ref;
}

let create () = {
  state_attested = ref false;
  state_attested_head = ref None;
  state_attested_root = ref None;
  quarantine_active = ref false;
  quarantine_reason = ref "";
  quarantine_since_epoch = ref (-1);
  prev_root_mismatch_streak = ref 0;
  state_root_mismatch_streak = ref 0;
  ahead_of_target_streak = ref 0;
}

let state_attested t =
  !(t.state_attested)

let quarantine_active t =
  !(t.quarantine_active)

let quarantine_active_ref t =
  t.quarantine_active

let quarantine_reason t =
  !(t.quarantine_reason)

let quarantine_since_epoch t =
  !(t.quarantine_since_epoch)

let prev_root_streak t =
  !(t.prev_root_mismatch_streak)

let set_prev_root_streak t streak =
  t.prev_root_mismatch_streak := streak

let state_root_streak t =
  !(t.state_root_mismatch_streak)

let set_state_root_streak t streak =
  t.state_root_mismatch_streak := streak

let ahead_streak t =
  !(t.ahead_of_target_streak)

let incr_ahead_streak t =
  incr t.ahead_of_target_streak

let clear_state_attested t =
  t.state_attested := false;
  t.state_attested_head := None;
  t.state_attested_root := None

let set_state_attested t ~head ~root =
  t.state_attested := true;
  t.state_attested_head := Some head;
  t.state_attested_root := Some root

let attested_head t head =
  state_attested t
  && (match !(t.state_attested_head) with
      | Some attested -> attested = head
      | None -> false)

let enter_quarantine t ~epoch ~reason =
  let entered = not (quarantine_active t) in
  if entered then begin
    t.quarantine_active := true;
    clear_state_attested t;
    t.quarantine_since_epoch := epoch
  end;
  t.quarantine_reason := reason;
  entered

let clear_quarantine t =
  t.quarantine_active := false;
  t.quarantine_reason := "";
  t.quarantine_since_epoch := -1;
  t.prev_root_mismatch_streak := 0;
  t.state_root_mismatch_streak := 0;
  t.ahead_of_target_streak := 0