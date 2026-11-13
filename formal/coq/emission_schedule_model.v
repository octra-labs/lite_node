(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import ZArith.
From Stdlib Require Import Lia.
From Stdlib Require Import Ring.
From Stdlib Require Import List.

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

Module Weighted_pay.

Definition total := fold_right Z.add 0.

Definition shares pool weights :=
  map (fun weight => pool * weight / total weights) weights.

Definition dust pool weights := pool - total (shares pool weights).

Lemma divided_sum pool den weights :
  den <> 0 ->
  den * total (map (fun weight => pool * weight / den) weights)
  + total (map (fun weight => (pool * weight) mod den) weights)
  = pool * total weights.
Proof.
  intros nonzero. induction weights as [|weight more ih]; simpl; [ring|].
  pose proof (Z.div_mod (pool * weight) den nonzero). nia.
Qed.

Lemma residual_range pool den weights :
  0 < den ->
  0 <= total (map (fun weight => (pool * weight) mod den) weights)
  /\ total (map (fun weight => (pool * weight) mod den) weights)
     <= den * Z.of_nat (length weights) - 1 +
        (if Nat.eqb (length weights) 0 then 1 else 0).
Proof.
  intros positive. induction weights as [|weight more ih]; simpl; [lia|].
  pose proof (Z.mod_pos_bound (pool * weight) den positive).
  destruct more; simpl in *; nia.
Qed.

Theorem dust_range pool weights :
  0 < total weights ->
  0 <= dust pool weights < Z.of_nat (length weights).
Proof.
  intros positive.
  pose proof (divided_sum pool (total weights) weights ltac:(lia)) as exact.
  pose proof (residual_range pool (total weights) weights positive) as range.
  assert (Nat.eqb (length weights) 0 = false) as nonempty.
  { apply Nat.eqb_neq. destruct weights; simpl in *; lia. }
  rewrite nonempty in range. unfold dust, shares. nia.
Qed.

Theorem shares_nonnegative pool weights :
  0 <= pool -> Forall (fun weight => 0 < weight) weights ->
  0 < total weights -> Forall (fun amount => 0 <= amount) (shares pool weights).
Proof.
  intros amount positive den. unfold shares. rewrite Forall_map.
  rewrite Forall_forall in *. intros weight inside.
  specialize (positive weight inside). apply Z.div_pos; nia.
Qed.

Theorem full_distribution base pool weights :
  0 <= base -> 0 <= pool ->
  Forall (fun weight => 0 < weight) weights -> 0 < total weights ->
  Forall (fun amount => 0 <= amount) (shares pool weights)
  /\ 0 <= base + dust pool weights
  /\ total (shares pool weights) + (base + dust pool weights) = base + pool
  /\ dust pool weights < Z.of_nat (length weights).
Proof.
  intros base_positive pool_positive weights_positive den.
  pose proof (shares_nonnegative pool weights pool_positive weights_positive den).
  pose proof (dust_range pool weights den).
  unfold dust in *. repeat split; try assumption; lia.
Qed.

Definition payout (rows : list (bool * Z)) := total (map snd rows).

Definition retained (rows : list (bool * Z)) := filter (fun row => negb (fst row)) rows.

Definition retired (rows : list (bool * Z)) := filter fst rows.

Theorem payout_partition rows :
  payout (retained rows) + payout (retired rows) = payout rows.
Proof.
  induction rows as [|[excluded amount] more ih]; simpl; [reflexivity|].
  destruct excluded; simpl in *; unfold payout, retained, retired in *; simpl in *; lia.
Qed.

Lemma payout_positive rows :
  Forall (fun row => 0 <= snd row) rows -> 0 <= payout rows.
Proof.
  intros positive. unfold payout, total.
  induction positive; simpl in *; lia.
Qed.

Theorem retirement_range rows :
  Forall (fun row => 0 <= snd row) rows ->
  0 <= payout (retired rows) <= payout rows.
Proof.
  intros positive.
  assert (Forall (fun row => 0 <= snd row) (retained rows)) as kept.
  { unfold retained. apply Forall_forall. intros row inside.
    apply filter_In in inside. apply (proj1 (Forall_forall _ _) positive row (proj1 inside)). }
  assert (Forall (fun row => 0 <= snd row) (retired rows)) as lost.
  { unfold retired. apply Forall_forall. intros row inside.
    apply filter_In in inside. apply (proj1 (Forall_forall _ _) positive row (proj1 inside)). }
  pose proof (payout_positive _ kept).
  pose proof (payout_positive _ lost).
  pose proof (payout_partition rows). lia.
Qed.

Theorem retirement_envelope rows supply reserve burned cap :
  supply + reserve + burned = cap ->
  payout (retired rows) <= supply ->
  0 <= reserve -> 0 <= burned ->
  Forall (fun row => 0 <= snd row) rows ->
  0 <= supply - payout (retired rows) /\
  0 <= burned + payout (retired rows) /\
  (supply - payout (retired rows)) + reserve
    + (burned + payout (retired rows)) = cap /\
  payout (retained rows) + payout (retired rows) = payout rows.
Proof.
  intros envelope enough reserve_positive burned_positive positive.
  pose proof (retirement_range rows positive).
  pose proof (payout_partition rows). repeat split; lia.
Qed.

End Weighted_pay.

Print Assumptions Weighted_pay.dust_range.
Print Assumptions Weighted_pay.shares_nonnegative.
Print Assumptions Weighted_pay.full_distribution.
Print Assumptions Weighted_pay.payout_partition.
Print Assumptions Weighted_pay.retirement_range.
Print Assumptions Weighted_pay.retirement_envelope.