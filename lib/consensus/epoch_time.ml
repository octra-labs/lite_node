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


type t = int64

let max_drift_ms = 300_000L
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

let check ~now ~previous ~candidate =
  match of_seconds now, of_seconds candidate with
  | Error error, _ | _, Error error -> Error error
  | Ok now_ms, Ok candidate_ms ->
    let drift = Int64.abs (Int64.sub candidate_ms now_ms) in
    if drift > max_drift_ms then
      Error "epoch time drift exceeds limit"
    else
      match previous with
      | Some previous_ms when candidate_ms < previous_ms ->
        Error "epoch time is not monotonic"
      | _ -> Ok candidate_ms