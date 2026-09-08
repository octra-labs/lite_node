(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type axis = Steps | Depth | Work

type t = {
  steps : Z.t;
  depth : Z.t;
  work : Z.t;
}

let at axis value =
  match axis with
  | Steps -> value.steps
  | Depth -> value.depth
  | Work -> value.work

let all = [Steps; Depth; Work]
let max_z = Z.of_int C_nat.max

let valid value =
  List.for_all (fun axis ->
    let item = at axis value in
    Z.sign item >= 0 && Z.leq item max_z) all

let text value =
  "{steps[" ^ Z.to_string value.steps ^ "],depth["
  ^ Z.to_string value.depth ^ "],work[" ^ Z.to_string value.work ^ "]}"

let zero = { steps = Z.zero; depth = Z.zero; work = Z.zero }
let one = { steps = Z.one; depth = Z.zero; work = Z.one }
let make steps = { steps; depth = Z.zero; work = steps }
let make3 ~steps ~depth ~work = { steps; depth; work }
let level depth = { steps = Z.zero; depth; work = Z.zero }
let effort work = { steps = Z.zero; depth = Z.zero; work }
let used ~steps ~work = { steps; depth = Z.zero; work }

let add left right =
  {
    steps = Z.add left.steps right.steps;
    depth = Z.add left.depth right.depth;
    work = Z.add left.work right.work;
  }

let succ value = add one value

let max left right =
  {
    steps = Z.max left.steps right.steps;
    depth = Z.max left.depth right.depth;
    work = Z.max left.work right.work;
  }

let scale count value =
  {
    steps = Z.mul count value.steps;
    depth = Z.mul count value.depth;
    work = Z.mul count value.work;
  }

let le left right =
  List.for_all (fun axis -> Z.leq (at axis left) (at axis right)) all

let equal left right =
  List.for_all (fun axis -> Z.equal (at axis left) (at axis right)) all