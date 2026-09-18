(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  height : int64;
  generation : int;
  until : int64;
}

let make ~height ~generation ~now ~started ~interval ~delay =
  let remaining = max delay (Int64.sub (Int64.add started interval) now) in
  if remaining <= 100_000_000L then None
  else Some { height; generation; until = Int64.add now remaining }

let current t ~height ~generation =
  t.height = height && t.generation = generation

let remaining t ~height ~generation ~now =
  match t with
  | Some plan when current plan ~height ~generation ->
    max 0L (Int64.sub plan.until now)
  | Some _ | None -> 0L