(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import ZArith Lia Bool.
Open Scope Z_scope.

Definition clip floor ceiling price := Z.min ceiling (Z.max floor price).

Definition change price target speed used :=
  price * Z.abs (used - target) / (target * speed).

Definition step price target speed used :=
  if target <? used
  then price + Z.max 1 (change price target speed used)
  else price - change price target speed used.

Definition advance floor ceiling price target speed used :=
  clip floor ceiling (step price target speed used).

Theorem clip_range : forall floor ceiling price,
  floor <= ceiling -> floor <= clip floor ceiling price <= ceiling.
Proof.
  intros. unfold clip.
  pose proof (Z.le_max_l floor price).
  destruct (Z.min_spec ceiling (Z.max floor price)) as [[? ->]|[? ->]]; lia.
Qed.

Theorem clip_same : forall floor ceiling price,
  floor <= price <= ceiling -> clip floor ceiling price = price.
Proof.
  intros. unfold clip. rewrite Z.max_r, Z.min_r; lia.
Qed.

Theorem clip_order : forall floor ceiling left right,
  left <= right -> clip floor ceiling left <= clip floor ceiling right.
Proof.
  intros. unfold clip. apply Z.min_le_compat; [lia|].
  apply Z.max_le_compat; lia.
Qed.

Theorem change_positive : forall price target speed used,
  0 <= price -> 0 < target -> 0 < speed ->
  0 <= change price target speed used.
Proof.
  intros. unfold change. apply Z.div_pos.
  - pose proof (Z.abs_nonneg (used - target)). nia.
  - nia.
Qed.

Theorem target_same : forall floor ceiling price target speed,
  floor <= price <= ceiling -> 0 < target -> 0 < speed ->
  advance floor ceiling price target speed target = price.
Proof.
  intros. unfold advance, step, change. rewrite Z.ltb_irrefl.
  replace (target - target) with 0 by lia. simpl.
  rewrite Z.mul_0_r, Z.div_0_l by nia.
  rewrite Z.sub_0_r. apply clip_same. assumption.
Qed.

Theorem step_order : forall price target speed left right,
  0 <= price -> 0 < target -> 0 < speed -> left <= right ->
  step price target speed left <= step price target speed right.
Proof.
  intros price target speed left right positive width rate ordered.
  pose proof (change_positive price target speed left positive width rate) as low.
  pose proof (change_positive price target speed right positive width rate) as high.
  unfold step.
  destruct (target <? left) eqn:first; destruct (target <? right) eqn:second;
    try apply Z.ltb_lt in first; try apply Z.ltb_ge in first;
    try apply Z.ltb_lt in second; try apply Z.ltb_ge in second.
  - assert (cost : change price target speed left <= change price target speed right).
    { unfold change. rewrite !Z.abs_eq by lia. apply Z.div_le_mono; nia. }
    pose proof (Z.max_le_compat 1 1 _ _ (Z.le_refl 1) cost). lia.
  - lia.
  - pose proof (Z.le_max_l 1 (change price target speed right)). lia.
  - assert (cost : change price target speed right <= change price target speed left).
    { unfold change. rewrite !Z.abs_neq by lia. apply Z.div_le_mono; nia. }
    lia.
Qed.

Theorem price_order : forall floor ceiling price target speed left right,
  0 <= price -> 0 < target -> 0 < speed -> left <= right ->
  advance floor ceiling price target speed left <=
    advance floor ceiling price target speed right.
Proof. intros. apply clip_order. apply step_order; assumption. Qed.

Theorem price_direction : forall floor ceiling price target speed used,
  0 <= price -> 0 < target -> 0 < speed -> floor <= price <= ceiling ->
  (used <= target -> advance floor ceiling price target speed used <= price) /\
  (target <= used -> price <= advance floor ceiling price target speed used).
Proof.
  intros. pose proof (target_same floor ceiling price target speed H2 H0 H1) as same.
  split; intro load.
  - rewrite <- same at 2. apply price_order; assumption.
  - rewrite <- same at 1. apply price_order; assumption.
Qed.

Theorem overload_increases : forall floor ceiling price target speed used,
  floor <= price -> price < ceiling -> target < used ->
  price < advance floor ceiling price target speed used.
Proof.
  intros. unfold advance, step.
  assert (load : (target <? used) = true) by (apply Z.ltb_lt; lia).
  rewrite load. unfold clip.
  pose proof (Z.le_max_l 1 (change price target speed used)).
  pose proof (Z.le_max_r floor (price + Z.max 1 (change price target speed used))).
  destruct (Z.min_spec ceiling
    (Z.max floor (price + Z.max 1 (change price target speed used))))
    as [[? ->]|[? ->]]; lia.
Qed.

Definition reserve price work cap :=
  if (0 <? price) && (0 <=? work) && (0 <=? cap) && (price * work <=? cap)
  then Some (price * work) else None.

Definition settle price work used :=
  if (0 <=? used) && (used <=? work)
  then Some (price * used, price * work - price * used) else None.

Theorem reserve_cap : forall price work cap held,
  reserve price work cap = Some held -> 0 <= held <= cap /\ held = price * work.
Proof.
  intros. unfold reserve in H.
  destruct (((0 <? price) && (0 <=? work) && (0 <=? cap)) && (price * work <=? cap))
    eqn:accepted; [|discriminate].
  repeat rewrite andb_true_iff in accepted.
  destruct accepted as [[[positive work_ok] cap_ok] fits].
  apply Z.ltb_lt in positive. apply Z.leb_le in work_ok.
  apply Z.leb_le in cap_ok. apply Z.leb_le in fits.
  inversion H; subst. nia.
Qed.

Theorem payment_conservation : forall price work cap held used charged refund,
  reserve price work cap = Some held ->
  settle price work used = Some (charged, refund) ->
  0 <= charged <= cap /\ 0 <= refund /\ charged + refund = held /\ used <= work.
Proof.
  intros price work cap held used charged refund offer payment.
  pose proof (reserve_cap price work cap held offer) as [fits amount].
  unfold reserve in offer.
  destruct (((0 <? price) && (0 <=? work) && (0 <=? cap)) && (price * work <=? cap))
    eqn:accepted; [|discriminate].
  repeat rewrite andb_true_iff in accepted.
  destruct accepted as [[[positive work_ok] cap_ok] reserved].
  apply Z.ltb_lt in positive.
  unfold settle in payment.
  destruct ((0 <=? used) && (used <=? work)) eqn:ran; [|discriminate].
  rewrite andb_true_iff in ran. destruct ran as [used_ok complete].
  apply Z.leb_le in used_ok. apply Z.leb_le in complete.
  inversion payment; subst. nia.
Qed.

Theorem work_not_price : forall price work used,
  work < used -> settle price work used = None.
Proof.
  intros. unfold settle.
  assert (excess : (used <=? work) = false) by (apply Z.leb_gt; assumption).
  rewrite excess, andb_false_r. reflexivity.
Qed.

Theorem partial_refund : forall price work used,
  0 <= used <= work ->
  settle price work used = Some (price * used, price * (work - used)).
Proof.
  intros. unfold settle.
  assert (positive : (0 <=? used) = true) by (apply Z.leb_le; lia).
  assert (fits : (used <=? work) = true) by (apply Z.leb_le; lia).
  rewrite positive, fits. simpl. f_equal. f_equal. ring.
Qed.