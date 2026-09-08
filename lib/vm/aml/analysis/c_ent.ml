(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type rate = {
  num : Z.t;
  shift : C_nat.t;
}

type profile = {
  basis : C_nat.t;
  max_depth : C_nat.t;
  base : Z.t;
  slope : Z.t;
  r2 : rate;
  r3 : rate;
}

type plan = {
  depth : C_nat.t;
  cap : Z.t;
  n2 : Z.t;
  n3 : Z.t;
  groups : Z.t;
  edges : Z.t;
}

type mode =
  | Binary64
  | Integer

type limit = {
  rule : C_rule.id;
  mode : mode;
  plan : plan;
}

type error =
  | Profile
  | Depth of Z.t * Z.t
  | Rule of C_rule.error

let fixed value =
  match C_nat.of_int value with
  | Some value -> value
  | None -> invalid_arg "entropy profile natural"

let prod = {
  basis = fixed 337;
  max_depth = fixed 1_000_000;
  base = Z.of_int 120;
  slope = Z.of_int 16;
  r2 = { num = Z.of_string "4719964527767381"; shift = fixed 57 };
  r3 = { num = Z.of_string "5149052212109869"; shift = fixed 58 };
}

let rate_ok value =
  Z.sign value.num > 0 && C_nat.to_int value.shift <= 127

let valid value =
  C_nat.valid value.basis
  && not (C_nat.equal value.basis C_nat.zero)
  && C_nat.valid value.max_depth
  && Z.sign value.base > 0
  && Z.sign value.slope >= 0
  && rate_ok value.r2
  && rate_ok value.r3

let scale value amount =
  let den = Z.shift_left Z.one (C_nat.to_int value.shift) in
  Z.ediv (Z.mul amount value.num) den

let plan value depth =
  if not (valid value) then Error Profile
  else if not (C_nat.le depth value.max_depth) then
    Error (Depth (C_nat.to_z depth, C_nat.to_z value.max_depth))
  else
    let cap =
      Z.add value.base (Z.mul value.slope (C_nat.to_z depth))
    in
    let n2 = scale value.r2 cap in
    let n3 = scale value.r3 cap in
    Ok {
      depth;
      cap;
      n2;
      n3;
      groups = Z.add n2 n3;
      edges = Z.add (Z.of_int 8)
          (Z.add (Z.mul (Z.of_int 2) n2) (Z.mul (Z.of_int 3) n3));
    }

let mode rule =
  if C_rule.version rule >= 2 then Integer else Binary64

let bind rule value depth =
  match plan value depth with
  | Error error -> Error error
  | Ok plan -> Ok { rule; mode = mode rule; plan }

let bind_at schedule ~epoch value depth =
  match C_rule.select schedule ~epoch with
  | Error error -> Error (Rule error)
  | Ok selected -> bind (C_rule.id selected) value depth

let text = function
  | Profile -> "entropy profile is invalid"
  | Depth (actual, limit) ->
      "entropy depth exceeds profile actual = " ^ Z.to_string actual
      ^ " limit = " ^ Z.to_string limit
  | Rule error -> C_rule.text error