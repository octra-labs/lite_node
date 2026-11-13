(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type ask = Quorum | Skip | Source | Pair | Ahead | Start | Promise

type gap = Few_seats | Low_weight | No_proof

type shape = {
  n : int;
  f : int;
  quorum : int;
  total_weight : Z.t;
  quorum_weight : Z.t;
}

type space = {
  rule : C_quorum_policy.rule;
  seatless : bool;
  declared : bool;
  shape : shape;
}

let quota total =
  Z.(add (div (mul total (of_int 2)) (of_int 3)) one)

let shape ~seats ~total =
  let f = (seats - 1) / 3 in
  let quorum =
    if seats <= 0 then 0
    else if seats < 4 then seats
    else seats - f in
  let quorum_weight =
    if Z.sign total <= 0 then Z.zero
    else quota total
  in
  { n = seats; f; quorum; total_weight = total; quorum_weight }

let live ~total ~lost =
  Z.geq (Z.sub total lost) (quota total)

let sum weights =
  List.fold_left (fun total (_, weight) -> Z.add total weight) Z.zero weights

let ruled rule ~declared weights =
  { rule; seatless = false; declared; shape = shape ~seats:(List.length weights) ~total:(sum weights) }

let space policy epoch_id ~declared weights =
  let prior = ruled (C_quorum_policy.select policy epoch_id) ~declared weights in
  {prior with seatless = C_quorum_policy.seatless policy epoch_id}

let valid space =
  match space.rule with
  | C_quorum_policy.Prior | C_quorum_policy.Capped -> true
  | C_quorum_policy.Stake -> space.declared

let minority shape =
  Z.(add (sub shape.total_weight shape.quorum_weight) one)

let start_seats space =
  if space.seatless then 1 else
  match space.rule with
  | C_quorum_policy.Prior -> 1
  | C_quorum_policy.Capped -> max 1 (space.shape.quorum - 1)
  | C_quorum_policy.Stake -> max 1 (space.shape.quorum - 1)

let promise_seats space =
  match space.rule with
  | C_quorum_policy.Prior | C_quorum_policy.Capped | C_quorum_policy.Stake ->
    max 0 (space.shape.quorum - 1)

let meets space ask ~seats ~weight =
  let seated = seats >= space.shape.quorum in
  let present = seats >= space.shape.f + 1 in
  let heavy = Z.geq weight space.shape.quorum_weight in
  let blocking = Z.geq weight (minority space.shape) in
  let open_minority = Z.sign (minority space.shape) > 0 in
  if space.seatless then
    let funded = space.declared && Z.sign space.shape.total_weight > 0 in
    match ask with
    | Quorum | Pair -> funded && heavy
    | Skip | Source | Ahead | Promise -> funded && blocking
    | Start -> seats >= 1
  else
  match ask, space.rule with
  | Quorum, C_quorum_policy.Prior -> heavy
  | Quorum, C_quorum_policy.Capped -> seated && heavy
  | Quorum, C_quorum_policy.Stake -> valid space && seated && heavy
  | Skip, C_quorum_policy.Prior -> blocking
  | Skip, C_quorum_policy.Capped -> present && blocking
  | Skip, C_quorum_policy.Stake -> valid space && present && blocking
  | Source, C_quorum_policy.Prior -> blocking
  | Source, C_quorum_policy.Capped -> present && blocking
  | Source, C_quorum_policy.Stake -> present && blocking
  | Pair, C_quorum_policy.Prior -> seated && heavy
  | Pair, C_quorum_policy.Capped -> seated && heavy
  | Pair, C_quorum_policy.Stake -> seated && heavy
  | Ahead, C_quorum_policy.Prior -> open_minority && blocking
  | Ahead, C_quorum_policy.Capped -> open_minority && blocking
  | Ahead, C_quorum_policy.Stake -> open_minority && blocking
  | Start, C_quorum_policy.Prior -> seats >= start_seats space
  | Start, C_quorum_policy.Capped -> seats >= start_seats space
  | Start, C_quorum_policy.Stake -> seats >= start_seats space
  | Promise, C_quorum_policy.Prior -> seats >= promise_seats space
  | Promise, C_quorum_policy.Capped -> seats >= promise_seats space
  | Promise, C_quorum_policy.Stake -> seats >= promise_seats space

let attested space ~required ~seats ~weight =
  if space.seatless then
    space.declared && Z.sign space.shape.total_weight > 0
      && Z.geq weight (minority space.shape)
  else
  match space.rule with
  | C_quorum_policy.Prior | C_quorum_policy.Capped | C_quorum_policy.Stake ->
    seats >= required

let replies space ~configured =
  if space.seatless then (if space.shape.n <= 1 then 0 else 1) else
  match space.rule with
  | C_quorum_policy.Prior | C_quorum_policy.Capped | C_quorum_policy.Stake ->
    let limit = max 0 (space.shape.n - 1) in
    if limit = 0 then 0 else min limit (max (space.shape.f + 1) configured)

let wave space =
  match space.rule with
  | C_quorum_policy.Prior | C_quorum_policy.Capped | C_quorum_policy.Stake ->
    max 1 (space.shape.f + 2)

let fanout space =
  if space.seatless then space.shape.n else
  match space.rule with
  | C_quorum_policy.Prior | C_quorum_policy.Capped | C_quorum_policy.Stake ->
    max 1 (space.shape.f + 2)

let lowest count weights =
  let rec take left total = function
    | _ when left <= 0 -> total
    | [] -> total
    | weight :: rest -> take (left - 1) (Z.add total weight) rest
  in
  take count Z.zero (List.sort Z.compare (List.map snd weights))

let overlap space target =
  if space.seatless then
    let current = space.shape in
    let total = sum target in
    if not space.declared || Z.sign current.total_weight <= 0
       || Z.sign total <= 0 || Z.gt total current.total_weight
       || List.exists (fun (_, weight) -> Z.sign weight <= 0) target
       || List.length target <> List.length (List.sort_uniq String.compare (List.map fst target))
    then Error No_proof
    else if Z.leq (Z.sub (Z.add current.quorum_weight (quota total)) current.total_weight)
      (Z.sub current.total_weight current.quorum_weight)
    then Error Low_weight else Ok ()
  else
  match space.rule with
  | C_quorum_policy.Prior | C_quorum_policy.Capped | C_quorum_policy.Stake ->
    let current = space.shape in
    let next = shape ~seats:(List.length target) ~total:(sum target) in
    let shared = current.quorum + next.quorum - current.n in
    if shared <= current.f then Error Few_seats
    else
      let by_count = lowest shared target in
      let by_weight =
        Z.sub (Z.add current.quorum_weight next.quorum_weight) current.total_weight
      in
      let faults = Z.sub current.total_weight current.quorum_weight in
      if Z.leq (Z.max by_count by_weight) faults then Error Low_weight
      else Ok ()

let label space =
  if space.seatless then "seatless" else
  match space.rule with
  | C_quorum_policy.Prior -> "prior"
  | C_quorum_policy.Capped -> "capped"
  | C_quorum_policy.Stake -> "stake"

let start_text space = string_of_int (start_seats space)

let promise_text space =
  if space.seatless then Z.to_string (minority space.shape)
  else string_of_int (promise_seats space)