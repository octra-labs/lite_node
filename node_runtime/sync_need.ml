(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type cause =
  | Root
  | Journal
  | Conflict
  | Range

type t = {
  cause : cause;
  epoch : int;
  head : int;
  target : int64 option;
}

let label = function
  | Root -> "root"
  | Journal -> "journal"
  | Conflict -> "conflict"
  | Range -> "range"

let cause = function
  | "root" -> Some Root
  | "journal" -> Some Journal
  | "conflict" -> Some Conflict
  | "range" -> Some Range
  | _ -> None

let root ~epoch ~head =
  { cause = Root; epoch; head; target = None }

let journal ~epoch ~head =
  { cause = Journal; epoch; head; target = None }

let conflict ~epoch ~head =
  { cause = Conflict; epoch; head; target = None }

let lost ~head ~target =
  if head >= 0
     && head < max_int
     && Int64.compare target (Int64.of_int head) > 0 then
    Some {
      cause = Range;
      epoch = head + 1;
      head;
      target = Some target;
    }
  else
    None

let range ~head ~target ~limit =
  let gap = Z.sub (Z.of_int64 target) (Z.of_int head) in
  if Z.gt gap (Z.of_int (max 0 limit)) then lost ~head ~target
  else None

let valid value =
  value.head >= 0
  && value.head < max_int
  && value.epoch = value.head + 1
  &&
  match value.cause, value.target with
  | Root, None
  | Conflict, None
  | Journal, None -> true
  | (Range | Root), Some target -> Int64.compare target (Int64.of_int value.head) > 0
  | _ -> false

let equal left right =
  left.cause = right.cause
  && left.epoch = right.epoch
  && left.head = right.head
  && left.target = right.target

let covers prior next =
  valid prior && valid next && prior.epoch <= next.head
  && (prior.cause = Root || prior.cause = Range)
  && (match prior.target with
      | None -> true
      | Some target ->
        let through = Option.fold ~none:(Int64.of_int next.head)
          ~some:(Int64.max (Int64.of_int next.head)) next.target in
        Int64.compare target through <= 0)

let history_limit ~retained text =
  match text with
  | None -> retained
  | Some text ->
    let size = String.length text in
    let start = if size > 0 && (text.[0] = '-' || text.[0] = '+') then 1 else 0 in
    let digits = size - start in
    if digits < 1 || digits > 19
       || not (String.for_all (fun char -> char >= '0' && char <= '9')
         (String.sub text start digits)) then retained
    else Z.(to_int (min (of_int retained) (max one (of_string text))))

let snapshot_ready ~head ~limit value =
  valid value && (value.cause = Root || value.cause = Range)
  && head >= value.epoch && head < max_int && limit >= 0
  && match value.target with
     | None -> true
     | Some target ->
       Z.leq (Z.sub (Z.of_int64 target) (Z.of_int head)) (Z.of_int limit)

let range_ready ~head ~limit value =
  value.cause = Range && snapshot_ready ~head ~limit value

let carry prior next =
  match prior.target, next.cause with
  | Some target, Root when Int64.compare target (Int64.of_int next.head) > 0 ->
    {next with cause = Range; target = Some (Option.fold ~none:target ~some:(Int64.max target) next.target)}
  | _ -> next