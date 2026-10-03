(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import ZArith Bool Lia.
Open Scope Z_scope.

Definition span limit base count :=
  (0 <=? base) && (0 <=? count) &&
  ((count =? 0) || (base <=? limit - (count - 1))).

Definition sized limit base count :=
  (0 <=? base) && (0 <=? count) && (base <=? limit - count).

Definition shift limit base index width :=
  let displacement := index * width in
  if (0 <=? index) && (0 <? width) && (displacement <=? limit) &&
    (base <=? limit - displacement) && span limit (base + displacement) width
  then Some (base + displacement)
  else None.

Theorem span_cells : forall limit base count cell,
  span limit base count = true ->
  0 <= cell < count ->
  0 <= base + cell <= limit.
Proof.
  intros limit base count cell accepted inside.
  unfold span in accepted.
  repeat rewrite andb_true_iff in accepted.
  destruct accepted as [[positive length] extent].
  apply Z.leb_le in positive. apply Z.leb_le in length.
  apply orb_true_iff in extent. destruct extent as [empty | fits].
  - apply Z.eqb_eq in empty. lia.
  - apply Z.leb_le in fits. lia.
Qed.

Theorem sized_end : forall limit base count,
  sized limit base count = true ->
  0 <= base + count <= limit.
Proof.
  intros limit base count accepted.
  unfold sized in accepted.
  repeat rewrite andb_true_iff in accepted.
  destruct accepted as [[positive length] fits].
  apply Z.leb_le in positive. apply Z.leb_le in length.
  apply Z.leb_le in fits. lia.
Qed.

Theorem shift_exact : forall limit base index width target,
  shift limit base index width = Some target ->
  target = base + index * width /\
  0 <= index * width <= limit /\
  0 <= target <= limit.
Proof.
  intros limit base index width target accepted.
  unfold shift in accepted.
  destruct (((((0 <=? index) && (0 <? width)) && (index * width <=? limit)) &&
    (base <=? limit - index * width)) &&
    span limit (base + index * width) width) eqn:checks; [|discriminate].
  inversion accepted; subst.
  repeat rewrite andb_true_iff in checks.
  destruct checks as [[[[position length] product] sum] cells].
  apply Z.leb_le in position. apply Z.ltb_lt in length.
  apply Z.leb_le in product. apply Z.leb_le in sum.
  pose proof (span_cells limit (base + index * width) width 0 cells).
  nia.
Qed.

Theorem shift_cells : forall limit base index width target cell,
  shift limit base index width = Some target ->
  0 <= cell < width ->
  0 <= target + cell <= limit.
Proof.
  intros limit base index width target cell accepted inside.
  unfold shift in accepted.
  destruct (((((0 <=? index) && (0 <? width)) && (index * width <=? limit)) &&
    (base <=? limit - index * width)) &&
    span limit (base + index * width) width) eqn:checks; [|discriminate].
  inversion accepted; subst.
  apply andb_true_iff in checks. destruct checks as [_ cells].
  exact (span_cells limit (base + index * width) width cell cells inside).
Qed.

Definition publish {A : Type} limit base count (before after : A) :=
  if sized limit base count then after else before.

Theorem refused_unchanged : forall (A : Type) limit base count (before after : A),
  sized limit base count = false ->
  publish limit base count before after = before.
Proof. intros. unfold publish. rewrite H. reflexivity. Qed.