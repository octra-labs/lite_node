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
  observer : bool;
  voting : bool;
  catchup_active : bool;
  quarantine_active : bool;
  state_attested : bool;
  pending_finalized : bool;
}

type reset = {
  height : int64;
  round : int;
  step : string;
  expected : int64;
  source : string;
  stall_sec : float;
  state_age : float;
  height_age : float;
  resets : int;
}

type result = {
  state : state;
  reset : reset option;
}

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
  || not stalled
  || sample.height <> sample.expected
  || sample.pending_finalized

let record state (sample : sample) =
  let state = refresh_state state sample in
  let state_age = sample.now -. state.key_started_at in
  let height_age = sample.now -. state.height_started_at in
  let state_stalled = state_age >= sample.stall_sec in
  let height_stalled = height_age >= sample.stall_sec in
  let height_realign_allowed =
    height_stalled && sample.step = "propose" && sample.round > 0
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
        stall_sec = sample.stall_sec;
        state_age;
        height_age;
        resets;
      };
    }