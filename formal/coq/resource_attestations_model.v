(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import Arith.PeanoNat.
From Stdlib Require Import Lists.List.
From Stdlib Require Import Lia.

Import ListNotations.

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

Fixpoint sum_nat (values : list nat) : nat :=
  match values with
  | nil => 0
  | cons head tail => head + sum_nat tail
  end.

Definition reward_conservation budget payouts :=
  sum_nat payouts <= budget.

Theorem reward_conservation_holds :
  forall budget payouts,
    sum_nat payouts <= budget ->
    reward_conservation budget payouts.
Proof.
  intros budget payouts bounded.
  unfold reward_conservation.
  exact bounded.
Qed.

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

Definition rational_bound_holds committee_size threshold adversarial honest bound_num bound_den :=
  weighted_capture_numerator committee_size threshold adversarial honest * bound_den
  <= bound_num * probability_denominator committee_size adversarial honest.

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
  intros committee_size adversarial_selected bounded captured.
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

Theorem capture_bound_half_5_threshold_2 :
  rational_bound_holds 5 2 1 1 13 16.
Proof.
  unfold rational_bound_holds.
  simpl.
  lia.
Qed.

Theorem capture_bound_alpha_20_committee_5 :
  rational_bound_holds 5 2 1 4 1 3.
Proof.
  unfold rational_bound_holds.
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