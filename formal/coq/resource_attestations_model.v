(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import Arith.PeanoNat.
From Stdlib Require Import Lists.List.
From Stdlib Require Import Lia.
From Stdlib Require Import ZArith.

Import ListNotations.

Fixpoint path_ok (index : nat) (sides : list bool) : bool :=
  match sides with
  | [] => Nat.eqb index 0
  | bit :: rest =>
      andb (Nat.eqb (index mod 2) (if bit then 1 else 0))
        (path_ok (index / 2) rest)
  end.

Theorem path_index_unique :
  forall sides left right,
    path_ok left sides = true ->
    path_ok right sides = true ->
    left = right.
Proof.
  induction sides as [|side rest ih]; intros left right hl hr; simpl in *.
  - apply Nat.eqb_eq in hl. apply Nat.eqb_eq in hr. lia.
  - apply Bool.andb_true_iff in hl as [hl pl].
    apply Bool.andb_true_iff in hr as [hr pr].
    apply Nat.eqb_eq in hl. apply Nat.eqb_eq in hr.
    change (left mod 2 = (if side then 1 else 0)) in hl.
    change (right mod 2 = (if side then 1 else 0)) in hr.
    pose proof (ih (left / 2) (right / 2) pl pr).
    pose proof (Nat.div_mod left 2 ltac:(lia)).
    pose proof (Nat.div_mod right 2 ltac:(lia)).
    lia.
Qed.

Theorem path_relabel_rejected :
  forall sides left right,
    path_ok left sides = true -> left <> right ->
    path_ok right sides = false.
Proof.
  intros sides left right hl ne.
  destruct (path_ok right sides) eqn:hr; [|reflexivity].
  exfalso. apply ne. eapply path_index_unique; eauto.
Qed.

Definition quorum_safe total quorum byzantine :=
  3 * quorum > 2 * total /\ 3 * byzantine < total.

Theorem valid_qc_intersection :
  forall total quorum byzantine,
    quorum_safe total quorum byzantine ->
    quorum + quorum > total + byzantine.
Proof.
  unfold quorum_safe.
  intros total quorum byzantine assumptions.
  destruct assumptions.
  lia.
Qed.

Theorem conflicting_qc_impossible :
  forall total quorum byzantine,
    quorum_safe total quorum byzantine ->
    ~(quorum + quorum <= total + byzantine).
Proof.
  intros total quorum byzantine assumptions contradiction.
  pose proof (valid_qc_intersection total quorum byzantine assumptions).
  lia.
Qed.

Module Shares.
Local Open Scope Z_scope.

Definition quorum weight := 2 * weight / 3 + 1.

Lemma quorum_range : forall weight,
  0 < weight -> 2 * weight < 3 * quorum weight /\ quorum weight <= weight.
Proof.
  intros weight positive. unfold quorum.
  pose proof (Z.div_mod (2 * weight) 3 ltac:(lia)).
  pose proof (Z.mod_pos_bound (2 * weight) 3 ltac:(lia)).
  nia.
Qed.

Theorem quorum_overlap : forall weight faulty,
  0 < weight -> 0 <= faulty -> 3 * faulty < weight ->
  faulty < Z.max 0 (2 * quorum weight - weight).
Proof.
  intros weight faulty positive nonnegative minority.
  pose proof (quorum_range weight positive).
  pose proof (Z.le_max_r 0 (2 * quorum weight - weight)).
  nia.
Qed.

Fixpoint total (values : list Z) : Z :=
  match values with [] => 0 | x :: xs => x + total xs end.

Definition parts budget weight values :=
  map (fun value => budget * value / weight) values.

Lemma part_error : forall budget weight value,
  0 < weight ->
  0 <= budget * value - (budget * value / weight) * weight <= weight - 1.
Proof.
  intros budget weight value positive.
  pose proof (Z.div_mod (budget * value) weight ltac:(lia)).
  pose proof (Z.mod_pos_bound (budget * value) weight positive).
  nia.
Qed.

Lemma total_error : forall values budget weight,
  0 < weight ->
  0 <= budget * total values - total (parts budget weight values) * weight
    <= Z.of_nat (length values) * (weight - 1).
Proof.
  induction values as [|value rest ih]; intros budget weight positive.
  - simpl. unfold parts. simpl. lia.
  - specialize (ih budget weight positive).
    pose proof (part_error budget weight value positive).
    unfold parts in *.
    change (0 <= budget * (value + total rest) -
      (budget * value / weight + total (map (fun x => budget * x / weight) rest)) * weight
      <= Z.of_nat (S (length rest)) * (weight - 1)).
    rewrite Nat2Z.inj_succ. nia.
Qed.

Lemma residue_range : forall values budget,
  0 < total values ->
  let residue := budget - total (parts budget (total values) values) in
  0 <= residue < Z.of_nat (length values).
Proof.
  intros values budget positive.
  pose proof (total_error values budget (total values) positive).
  assert (0 < Z.of_nat (length values)).
  { destruct values; simpl in *; lia. }
  cbn zeta. nia.
Qed.

Lemma base_budget : forall values budget,
  0 < total values ->
  total (parts budget (total values) values) <= budget.
Proof.
  intros values budget positive.
  pose proof (residue_range values budget positive).
  cbn zeta in *. lia.
Qed.

Fixpoint dust count values :=
  match count, values with
  | S rest, value :: tail => value + 1 :: dust rest tail
  | _, _ => values
  end.

Lemma dust_sum : forall count values,
  (count <= length values)%nat ->
  total (dust count values) = total values + Z.of_nat count.
Proof.
  induction count as [|count ih]; intros values fits.
  - simpl. lia.
  - destruct values as [|value rest]; simpl in fits; try lia.
    simpl dust. simpl total.
    rewrite ih by lia. rewrite Nat2Z.inj_succ. lia.
Qed.

Theorem payout_sum : forall values budget,
  0 < total values ->
  let base := parts budget (total values) values in
  total (dust (Z.to_nat (budget - total base)) base) = budget.
Proof.
  intros values budget positive. cbn zeta.
  pose proof (residue_range values budget positive) as range.
  cbn zeta in range.
  rewrite dust_sum.
  - rewrite Z2Nat.id by lia. lia.
  - unfold parts in *. rewrite length_map.
    apply Nat2Z.inj_le. rewrite Z2Nat.id by lia. lia.
Qed.

Lemma parts_pos : forall values budget weight,
  Forall (fun x => 0 <= x) values -> 0 <= budget -> 0 < weight ->
  Forall (fun x => 0 <= x) (parts budget weight values).
Proof.
  intros values budget weight positive money units.
  unfold parts. induction positive; simpl; constructor.
  - apply Z.div_pos; [apply Z.mul_nonneg_nonneg; assumption|exact units].
  - assumption.
Qed.

Lemma dust_pos : forall count values,
  Forall (fun x => 0 <= x) values ->
  Forall (fun x => 0 <= x) (dust count values).
Proof.
  induction count as [|count ih]; intros values positive; [exact positive|].
  destruct values as [|value rest]; [constructor|].
  inversion positive; subst. simpl dust. constructor; [lia|].
  apply ih. assumption.
Qed.

Lemma total_pos : forall values,
  Forall (fun x => 0 <= x) values -> 0 <= total values.
Proof.
  intros values positive. induction positive; simpl; lia.
Qed.

Lemma member_total : forall values value,
  Forall (fun x => 0 <= x) values -> In value values -> value <= total values.
Proof.
  induction values as [|head rest ih]; intros value positive member.
  - inversion member.
  - inversion positive; subst. simpl in member. simpl total.
    pose proof (total_pos rest ltac:(assumption)).
    destruct member as [same|member]; [subst; lia|].
    specialize (ih value ltac:(assumption) member). lia.
Qed.

Theorem payout_range : forall values budget,
  Forall (fun x => 0 <= x) values -> 0 < total values ->
  0 <= budget <= 9223372036854775807 ->
  let base := parts budget (total values) values in
  Forall (fun x => 0 <= x <= 9223372036854775807)
    (dust (Z.to_nat (budget - total base)) base).
Proof.
  intros values budget positive units money. cbn zeta.
  pose proof (payout_sum values budget units) as paid. cbn zeta in paid.
  pose proof (parts_pos values budget (total values) positive ltac:(lia) units) as base.
  pose proof (dust_pos (Z.to_nat (budget - total (parts budget (total values) values)))
    (parts budget (total values) values) base) as nonnegative.
  rewrite Forall_forall. intros value member.
  pose proof (member_total _ value nonnegative member).
  rewrite Forall_forall in nonnegative. specialize (nonnegative value member).
  lia.
Qed.

Print Assumptions payout_sum.
Print Assumptions payout_range.
End Shares.

Record attestation := {
  attestation_weight : nat;
  attestation_valid : bool
}.

Definition attestation_influence value :=
  match attestation_valid value with
  | true => attestation_weight value
  | false => 0
  end.

Fixpoint influence values :=
  match values with
  | nil => 0
  | cons head tail => attestation_influence head + influence tail
  end.

Theorem attestation_sybil_invariance :
  forall left right rest,
    influence ({| attestation_weight := left + right; attestation_valid := true |} :: rest)
    =
    influence
      ({| attestation_weight := left; attestation_valid := true |}
       :: {| attestation_weight := right; attestation_valid := true |}
       :: rest).
Proof.
  intros left right rest.
  simpl.
  rewrite Nat.add_assoc.
  reflexivity.
Qed.

Fixpoint pow_nat base exponent :=
  match exponent with
  | 0 => 1
  | S previous => base * pow_nat base previous
  end.

Fixpoint binom total selected :=
  match total, selected with
  | _, 0 => 1
  | 0, S _ => 0
  | S total_previous, S selected_previous =>
      binom total_previous selected_previous + binom total_previous (S selected_previous)
  end.

Definition weighted_binomial_term total selected adversarial honest :=
  binom total selected
  * pow_nat adversarial selected
  * pow_nat honest (total - selected).

Fixpoint weighted_tail_count total selected count adversarial honest :=
  match count with
  | 0 => 0
  | S rest =>
      weighted_binomial_term total selected adversarial honest
      + weighted_tail_count total (S selected) rest adversarial honest
  end.

Definition capture_threshold committee_size :=
  committee_size / 3 + 1.

Definition weighted_capture_numerator committee_size threshold adversarial honest :=
  if threshold <=? committee_size then
    weighted_tail_count
      committee_size
      threshold
      (S (committee_size - threshold))
      adversarial
      honest
  else 0.

Definition probability_denominator committee_size adversarial honest :=
  pow_nat (adversarial + honest) committee_size.

Definition rational_limit_holds committee_size threshold adversarial honest limit_num limit_den :=
  weighted_capture_numerator committee_size threshold adversarial honest * limit_den
  <= limit_num * probability_denominator committee_size adversarial honest.

Definition committee_captured committee_size adversarial_selected :=
  3 * adversarial_selected > committee_size.

Theorem capture_tail_zero_above_size :
  forall committee_size threshold adversarial honest,
    threshold > committee_size ->
    weighted_capture_numerator committee_size threshold adversarial honest = 0.
Proof.
  intros committee_size threshold adversarial honest greater.
  unfold weighted_capture_numerator.
  apply Nat.leb_gt in greater.
  rewrite greater.
  reflexivity.
Qed.

Theorem capture_false_below_threshold :
  forall committee_size adversarial_selected,
    3 * adversarial_selected <= committee_size ->
    ~ committee_captured committee_size adversarial_selected.
Proof.
  intros committee_size adversarial_selected within_limit captured.
  unfold committee_captured in captured.
  lia.
Qed.

Example binom_5_2 :
  binom 5 2 = 10.
Proof.
  reflexivity.
Qed.

Example capture_tail_half_5_threshold_2 :
  weighted_capture_numerator 5 2 1 1 = 26.
Proof.
  reflexivity.
Qed.

Example capture_denominator_half_5 :
  probability_denominator 5 1 1 = 32.
Proof.
  reflexivity.
Qed.

Theorem capture_limit_half_5 :
  rational_limit_holds 5 2 1 1 13 16.
Proof.
  unfold rational_limit_holds.
  simpl.
  lia.
Qed.

Theorem capture_limit_fifth_5 :
  rational_limit_holds 5 2 1 4 1 3.
Proof.
  unfold rational_limit_holds.
  simpl.
  lia.
Qed.

Record catchup_state := {
  catchup_verified : bool;
  catchup_roots_match : bool;
  catchup_ranges_complete : bool
}.

Definition ready_to_vote state :=
  catchup_verified state = true
  /\ catchup_roots_match state = true
  /\ catchup_ranges_complete state = true.

Theorem ready_to_vote_requires_verified_catchup :
  forall state,
    ready_to_vote state ->
    catchup_verified state = true.
Proof.
  intros state ready.
  unfold ready_to_vote in ready.
  destruct ready as [verified _].
  exact verified.
Qed.