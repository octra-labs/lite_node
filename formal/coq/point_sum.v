(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import ZArith Ring.
Open Scope Z_scope.

Record point := {
  xp : Z;
  yp : Z;
  zp : Z;
  tp : Z
}.

Definition add_ext d p q :=
  let a := (yp p - xp p) * (yp q - xp q) in
  let b := (yp p + xp p) * (yp q + xp q) in
  let c := tp p * tp q * (2 * d) in
  let v := 2 * zp p * zp q in
  {| xp := (b - a) * (v - c);
     yp := (v + c) * (b + a);
     zp := (v - c) * (v + c);
     tp := (b - a) * (b + a) |}.

Definition add_saved d p x y t :=
  let a := (yp p - xp p) * (y - x) in
  let b := (yp p + xp p) * (y + x) in
  let c := tp p * (t * (2 * d)) in
  let v := 2 * zp p in
  {| xp := (b - a) * (v - c);
     yp := (v + c) * (b + a);
     zp := (v - c) * (v + c);
     tp := (b - a) * (b + a) |}.

Theorem saved_exact : forall d p x y t,
  add_saved d p x y t =
  add_ext d p {| xp := x; yp := y; zp := 1; tp := t |}.
Proof.
  intros. unfold add_saved, add_ext. simpl. f_equal; ring.
Qed.

Print Assumptions saved_exact.