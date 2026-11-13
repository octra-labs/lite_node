(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import ZArith Ring List.
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

Record prepared := {
  minus : Z;
  plus : Z;
  product : Z
}.

Definition prepare d q :=
  {| minus := yp q - xp q;
     plus := yp q + xp q;
     product := tp q * (2 * d) |}.

Definition add_prepared p q :=
  let a := (yp p - xp p) * minus q in
  let b := (yp p + xp p) * plus q in
  let c := tp p * product q in
  let v := 2 * zp p in
  {| xp := (b - a) * (v - c);
     yp := (v + c) * (b + a);
     zp := (v - c) * (v + c);
     tp := (b - a) * (b + a) |}.

Theorem prepared_step : forall d p q,
  zp q = 1 -> add_prepared p (prepare d q) = add_ext d p q.
Proof.
  intros d p q H. unfold add_prepared, prepare, add_ext.
  simpl. rewrite H. f_equal; ring.
Qed.

Theorem prepared_sequence : forall d points p,
  Forall (fun q => zp q = 1) points ->
  fold_left add_prepared (map (prepare d) points) p =
  fold_left (add_ext d) points p.
Proof.
  intros d points. induction points as [|q rest IH]; intros p H.
  - reflexivity.
  - inversion H; subst. simpl. rewrite prepared_step by assumption.
    apply IH. assumption.
Qed.

Print Assumptions prepared_step.
Print Assumptions prepared_sequence.