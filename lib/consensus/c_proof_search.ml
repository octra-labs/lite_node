(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  height : int64;
  target : int;
  sent_at : int64;
  round : int;
  attempts : int;
  requests : int;
  recent : (int64 * int) list;
}

let interval = 2_100_000_000L
let target_limit = 4
let round_limit = 8
let window_ns = 60_000_000_000L
let window_limit = 1024

let step ?(validators = 31) prior ~height ~round ~known ~enabled ~now =
  let prior = match prior with
    | Some prior when prior.height = height -> Some prior
    | _ -> None in
  if round <= 0 then None, None
  else if not enabled then prior, None
  else
    let target = match known with
      | Some target when target >= 0 && target < round -> target
      | _ -> match prior with
        | Some prior when prior.target + 1 < round -> prior.target + 1
        | _ -> 0 in
    let attempts, requests = match prior with
      | Some prior when prior.round = round ->
        (if prior.target = target then prior.attempts else 0), prior.requests
      | _ -> 0, 0 in
    let delay = Int64.mul interval (Int64.of_int (1 lsl max 0 (attempts - 1))) in
    let recent = match prior with
      | Some prior -> List.filter (fun (sent, _) -> now < sent || Int64.sub now sent < window_ns) prior.recent
      | None -> [] in
    let used = List.fold_left (fun used (_, cost) -> used + cost) 0 recent in
    let cost = 4 * min 255 (max 1 validators) + 4 in
    let ready = match prior with
      | Some prior -> now >= prior.sent_at && Int64.sub now prior.sent_at >= delay
      | None -> true in
    if not ready || attempts >= target_limit || requests >= round_limit
      || used + cost > window_limit then prior, None
    else Some {height; target; sent_at = now; round;
      attempts = attempts + 1; requests = requests + 1;
      recent = (now, cost) :: recent}, Some target