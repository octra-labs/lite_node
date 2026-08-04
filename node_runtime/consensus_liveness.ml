(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type state = {
  key : string;
  key_started_at : float;
  height_key : string;
  height_started_at : float;
  resets : int;
}

type sample = {
  height : int64;
  round : int;
  step : string;
  expected : int64;
  now : float;
  source : string;
  stall_sec : float;
  step_timeout_sec : float option;
  observer : bool;
  voting : bool;
  catchup_active : bool;
  quarantine_active : bool;
  state_attested : bool;
  pending_finalized : bool;
  proposal_active : bool;
}

type reset = {
  height : int64;
  round : int;
  step : string;
  expected : int64;
  source : string;
  stall_sec : float;
  step_timeout_sec : float option;
  state_age : float;
  height_age : float;
  resets : int;
}

type result = {
  state : state;
  reset : reset option;
}

type driver_snapshot = {
  height : int64;
  round : int;
  step : string;
  step_timeout_sec : float option;
}

let protocol_timeout_grace_sec = 30.0

let protocol_realign_sec (sample : sample) =
  Option.map
    (fun timeout_sec -> timeout_sec +. protocol_timeout_grace_sec)
    sample.step_timeout_sec

let effective_stall_sec (sample : sample) =
  match protocol_realign_sec sample with
  | None -> sample.stall_sec
  | Some protocol_sec -> max sample.stall_sec protocol_sec

let create ~now = {
  key = "";
  key_started_at = now;
  height_key = "";
  height_started_at = now;
  resets = 0;
}

let state_key (sample : sample) =
  Printf.sprintf "%Ld:%d:%s" sample.height sample.round sample.step

let height_key (sample : sample) =
  Int64.to_string sample.height

let refresh_state state (sample : sample) =
  let key = state_key sample in
  let height_key = height_key sample in
  let key_started_at =
    if key = state.key then state.key_started_at else sample.now
  in
  let height_started_at =
    if height_key = state.height_key then state.height_started_at else sample.now
  in
  {
    state with
    key;
    key_started_at;
    height_key;
    height_started_at;
  }

let blocked (sample : sample) ~stalled =
  sample.observer
  || not sample.voting
  || sample.catchup_active
  || sample.quarantine_active
  || not sample.state_attested
  || sample.proposal_active
  || not stalled
  || sample.height <> sample.expected
  || sample.pending_finalized

let record state (sample : sample) =
  let state = refresh_state state sample in
  let state =
    if sample.proposal_active then
      {
        state with
        key_started_at = sample.now;
        height_started_at = sample.now;
      }
    else
      state
  in
  let state_age = sample.now -. state.key_started_at in
  let height_age = sample.now -. state.height_started_at in
  let stall_sec = effective_stall_sec sample in
  let state_stalled = state_age >= stall_sec in
  let height_stalled = height_age >= sample.stall_sec in
  let protocol_realign_allowed =
    match protocol_realign_sec sample with
    | None -> true
    | Some protocol_sec -> state_age >= protocol_sec
  in
  let height_realign_allowed =
    height_stalled
    && protocol_realign_allowed
    && sample.step = "propose"
    && sample.round > 0
  in
  let stalled = state_stalled || height_realign_allowed in
  if blocked sample ~stalled then
    { state; reset = None }
  else
    let resets = state.resets + 1 in
    let state = {
      state with
      key_started_at = sample.now;
      height_started_at = sample.now;
      resets;
    } in
    {
      state;
      reset = Some {
        height = sample.height;
        round = sample.round;
        step = sample.step;
        expected = sample.expected;
        source = sample.source;
        stall_sec;
        step_timeout_sec = sample.step_timeout_sec;
        state_age;
        height_age;
        resets;
      };
    }

let step_label = function
  | Octra_consensus.C_types.ProposeStep -> "propose"
  | Octra_consensus.C_types.PrevoteStep -> "prevote"
  | Octra_consensus.C_types.PrecommitStep -> "precommit"

let driver_snapshot driver =
  let step = Octra_consensus.C_driver.current_step driver in
  let round = Octra_consensus.C_driver.current_round driver in
  {
    height = Octra_consensus.C_driver.current_height driver;
    round;
    step = step_label step;
    step_timeout_sec =
      Some
        (float_of_int (Octra_consensus.C_engine.timeout_ms ~round ~step)
         /. 1000.0);
  }

let record_snapshot state snapshot ~expected ~now ~source ~stall_sec ~observer
    ~voting ~catchup_active ~quarantine_active ~state_attested
    ~pending_finalized ~proposal_active =
  record state {
    height = snapshot.height;
    round = snapshot.round;
    step = snapshot.step;
    expected;
    now;
    source;
    stall_sec;
    step_timeout_sec = snapshot.step_timeout_sec;
    observer;
    voting;
    catchup_active;
    quarantine_active;
    state_attested;
    pending_finalized;
    proposal_active;
  }