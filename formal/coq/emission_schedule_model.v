(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import ZArith.
From Stdlib Require Import Lia.
From Stdlib Require Import Ring.

Open Scope Z_scope.

Definition curve_weight duration elapsed :=
  17 * duration * elapsed - 7 * elapsed * elapsed.

Definition curve_denominator duration :=
  10 * duration * duration.

Definition curve_cumulative initial duration elapsed :=
  initial * curve_weight duration elapsed
  / curve_denominator duration.

Definition curve_reward issued elapsed :=
  issued (elapsed + 1) - issued elapsed.

Definition next_remaining remaining reward :=
  remaining - reward.

Definition next_supply supply reward :=
  supply + reward.

Definition fee_burn fees :=
  fees / 5.

Definition fee_reward fees :=
  fees - fee_burn fees.

Definition next_supply_with_burn supply reward burn :=
  supply + reward - burn.

Definition next_retired retired burn :=
  retired + burn.

Theorem curve_weight_zero :
  forall duration,
    curve_weight duration 0 = 0.
Proof.
  intros duration.
  unfold curve_weight.
  ring.
Qed.

Theorem curve_weight_end :
  forall duration,
    curve_weight duration duration = curve_denominator duration.
Proof.
  intros duration.
  unfold curve_weight, curve_denominator.
  ring.
Qed.

Theorem curve_weight_increment :
  forall duration elapsed,
    curve_weight duration (elapsed + 1)
    - curve_weight duration elapsed
    = 17 * duration - 14 * elapsed - 7.
Proof.
  intros duration elapsed.
  unfold curve_weight.
  ring.
Qed.

Theorem curve_weight_strictly_increases :
  forall duration elapsed,
    0 < duration ->
    0 <= elapsed ->
    elapsed < duration ->
    curve_weight duration elapsed
    < curve_weight duration (elapsed + 1).
Proof.
  intros duration elapsed duration_positive elapsed_nonnegative elapsed_bound.
  pose proof (curve_weight_increment duration elapsed).
  lia.
Qed.

Theorem curve_denominator_positive :
  forall duration,
    0 < duration ->
    0 < curve_denominator duration.
Proof.
  intros duration duration_positive.
  unfold curve_denominator.
  apply Z.mul_pos_pos.
  apply Z.mul_pos_pos.
  lia.
  exact duration_positive.
  exact duration_positive.
Qed.

Theorem curve_cumulative_zero :
  forall initial duration,
    0 < duration ->
    curve_cumulative initial duration 0 = 0.
Proof.
  intros initial duration duration_positive.
  unfold curve_cumulative.
  rewrite curve_weight_zero.
  rewrite Z.mul_0_r.
  apply Z.div_0_l.
  pose proof (curve_denominator_positive duration duration_positive).
  lia.
Qed.

Theorem curve_cumulative_end :
  forall initial duration,
    0 < duration ->
    curve_cumulative initial duration duration = initial.
Proof.
  intros initial duration duration_positive.
  unfold curve_cumulative.
  rewrite curve_weight_end.
  apply Z.div_mul.
  pose proof (curve_denominator_positive duration duration_positive).
  lia.
Qed.

Theorem curve_reward_nonnegative :
  forall issued elapsed,
    issued elapsed <= issued (elapsed + 1) ->
    0 <= curve_reward issued elapsed.
Proof.
  intros issued elapsed monotone.
  unfold curve_reward.
  lia.
Qed.

Theorem curve_reward_bounded :
  forall initial issued elapsed,
    issued elapsed <= issued (elapsed + 1) ->
    issued (elapsed + 1) <= initial ->
    curve_reward issued elapsed <= initial - issued elapsed.
Proof.
  intros initial issued elapsed monotone bounded.
  unfold curve_reward.
  lia.
Qed.

Theorem reserve_conserved :
  forall supply remaining reward,
    0 <= reward ->
    reward <= remaining ->
    next_supply supply reward + next_remaining remaining reward
    = supply + remaining.
Proof.
  intros supply remaining reward reward_nonnegative reward_bounded.
  unfold next_supply, next_remaining.
  lia.
Qed.

Theorem hard_cap_preserved :
  forall supply remaining reward cap,
    supply + remaining <= cap ->
    0 <= reward ->
    reward <= remaining ->
    next_supply supply reward <= cap.
Proof.
  intros supply remaining reward cap bounded reward_nonnegative reward_bounded.
  unfold next_supply.
  lia.
Qed.

Theorem exhausted_pool_is_absorbing :
  forall supply,
    next_remaining 0 0 = 0 /\
    next_supply supply 0 = supply.
Proof.
  intros supply.
  unfold next_remaining, next_supply.
  lia.
Qed.

Theorem fee_split_conserved :
  forall fees,
    fee_burn fees + fee_reward fees = fees.
Proof.
  intros fees.
  unfold fee_reward.
  lia.
Qed.

Theorem supply_envelope_conserved :
  forall supply remaining retired reward burn,
    next_supply_with_burn supply reward burn
    + next_remaining remaining reward
    + next_retired retired burn
    = supply + remaining + retired.
Proof.
  intros supply remaining retired reward burn.
  unfold next_supply_with_burn, next_remaining, next_retired.
  ring.
Qed.

Theorem fee_burn_matches_unrewarded_fees :
  forall public fees,
    public - (public - fees + fee_reward fees) = fee_burn fees.
Proof.
  intros public fees.
  unfold fee_reward.
  ring.
Qed.