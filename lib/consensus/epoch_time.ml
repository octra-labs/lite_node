(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = int64

type proposal_kind = Fresh | Reproposal

type rule = Historical | Uniform

let interval_ms = 10_000L
let interval_seconds = 10.0
let initial_drift_ms = 300_000L
let max_future_ms = 5_000L
let max_seconds = 9.22e15

let of_seconds seconds =
  match classify_float seconds with
  | FP_nan | FP_infinite -> Error "epoch time is not finite"
  | FP_normal | FP_subnormal | FP_zero ->
    if seconds < 0. || seconds > max_seconds then
      Error "epoch time is out of range"
    else
      Ok (Int64.of_float (Float.floor (seconds *. 1000.)))

let to_z value = Z.of_int64 value

let next_delay_ms ~now ~previous =
  match of_seconds now, of_seconds previous with
  | Ok now_ms, Ok previous_ms ->
    let target_ms = Int64.add previous_ms interval_ms in
    if now_ms >= target_ms then 0L
    else Int64.sub target_ms now_ms
  | Error _, _ | _, Error _ ->
    interval_ms

let check_candidate ~previous ~candidate =
  match of_seconds candidate with
  | Error error -> Error error
  | Ok candidate_ms ->
    match previous with
    | Some previous_ms when candidate_ms < previous_ms ->
      Error "epoch time is not monotonic"
    | Some previous_ms
      when Int64.sub candidate_ms previous_ms < interval_ms ->
      Error "epoch time interval is below protocol minimum"
    | _ -> Ok candidate_ms

let check ~now ~previous ~candidate =
  match of_seconds now, check_candidate ~previous ~candidate with
  | Error error, _ | _, Error error -> Error error
  | Ok now_ms, Ok candidate_ms ->
    begin
      match previous with
      | None ->
        let drift = Int64.abs (Int64.sub candidate_ms now_ms) in
        if drift > initial_drift_ms then
          Error "initial epoch time drift exceeds limit"
        else
          Ok candidate_ms
      | Some _ ->
        let future =
          if candidate_ms > now_ms then
            Int64.sub candidate_ms now_ms
          else
            0L
        in
        if future > max_future_ms then
          Error "epoch time is too far in the future"
        else
          Ok candidate_ms
    end

let check_reproposal ~previous ~candidate =
  check_candidate ~previous ~candidate

let check_proposal ~rule ~kind ~now ~previous ~candidate =
  match rule, kind with
  | Historical, Reproposal -> check_reproposal ~previous ~candidate
  | Historical, Fresh
  | Uniform, Fresh
  | Uniform, Reproposal -> check ~now ~previous ~candidate