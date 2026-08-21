(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type cause =
  | Root
  | Journal
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
  | Range -> "range"

let cause = function
  | "root" -> Some Root
  | "journal" -> Some Journal
  | "range" -> Some Range
  | _ -> None

let root ~epoch ~head =
  { cause = Root; epoch; head; target = None }

let journal ~epoch ~head =
  { cause = Journal; epoch; head; target = None }

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
  | Journal, None -> true
  | Range, Some target -> Int64.compare target (Int64.of_int value.head) > 0
  | _ -> false

let equal left right =
  left.cause = right.cause
  && left.epoch = right.epoch
  && left.head = right.head
  && left.target = right.target