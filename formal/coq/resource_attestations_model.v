(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import Arith.PeanoNat.
From Stdlib Require Import Bool.Bool.
From Stdlib Require Import Lists.List.
From Stdlib Require Import Lia.
From Stdlib Require Import ZArith.

Import ListNotations.

Module Expiry.
Section Entries.
Context {A : Type}.
Variable deadline : A -> Z.

Definition retain now (entries : list A) :=
  filter (fun entry => Z.leb now (deadline entry)) entries.

Definition remove now (entries : list A) :=
  filter (fun entry => negb (Z.leb now (deadline entry))) entries.

Theorem retain_exact : forall now entries entry,
  In entry (retain now entries) <->
  In entry entries /\ (now <= deadline entry)%Z.
Proof.
  intros now entries entry. unfold retain.
  rewrite filter_In, Z.leb_le. reflexivity.
Qed.

Theorem expiry_count : forall now entries,
  length (retain now entries) + length (remove now entries) = length entries.
Proof.
  intros now entries. induction entries as [|entry rest ih].
  - reflexivity.
  - unfold retain, remove in *. simpl.
    destruct (Z.leb now (deadline entry)); simpl; lia.
Qed.

Theorem room_after_expiry : forall now entries cap,
  length entries <= cap ->
  (exists entry, In entry entries /\ (deadline entry < now)%Z) ->
  length (retain now entries) < cap.
Proof.
  intros now entries cap fits [entry [member expired]].
  assert (In entry (remove now entries)) as removed.
  { unfold remove. apply filter_In. split; [exact member|].
    apply Bool.negb_true_iff, Z.leb_gt. exact expired. }
  pose proof (expiry_count now entries).
  destruct (remove now entries); simpl in *; [contradiction|lia].
Qed.

Theorem no_live_eviction : forall now entries,
  Forall (fun entry => (now <= deadline entry)%Z) entries ->
  retain now entries = entries.
Proof.
  intros now entries live. induction live as [|entry rest valid live ih].
  - reflexivity.
  - unfold retain in *. simpl.
    apply Z.leb_le in valid. rewrite valid, ih. reflexivity.
Qed.

Theorem retain_advance : forall entries earlier later,
  (earlier <= later)%Z ->
  retain later (retain earlier entries) = retain later entries.
Proof.
  induction entries as [|entry rest ih]; intros earlier later monotone.
  - reflexivity.
  - unfold retain in *. simpl.
    destruct (Z.leb earlier (deadline entry)) eqn:old;
      destruct (Z.leb later (deadline entry)) eqn:current; simpl.
    + rewrite current, ih by exact monotone. reflexivity.
    + rewrite current, ih by exact monotone. reflexivity.
    + apply Z.leb_gt in old. apply Z.leb_le in current. lia.
    + apply ih. exact monotone.
Qed.

Print Assumptions retain_exact.
Print Assumptions expiry_count.
Print Assumptions room_after_expiry.
Print Assumptions no_live_eviction.
Print Assumptions retain_advance.
End Entries.
End Expiry.

Module Cancellation.
Definition entries := list (nat * Z).

Fixpoint contains key (state : entries) :=
  match state with
  | [] => false
  | (current, _) :: rest => Nat.eqb key current || contains key rest
  end.

Fixpoint put key expiry (state : entries) :=
  match state with
  | [] => [(key, expiry)]
  | (current, previous) :: rest =>
    if Nat.eqb key current then (key, expiry) :: rest
    else (current, previous) :: put key expiry rest
  end.

Definition record cap now ttl key state :=
  let live := Expiry.retain snd now state in
  if contains key live || Nat.ltb (length live) cap
  then (put key (now + ttl)%Z live, true)
  else (live, false).

Lemma put_size : forall state key expiry,
  length (put key expiry state) =
    if contains key state then length state else S (length state).
Proof.
  induction state as [|[current previous] rest ih]; intros key expiry.
  - reflexivity.
  - simpl. destruct (Nat.eqb key current); simpl; [reflexivity|].
    rewrite ih. destruct (contains key rest); reflexivity.
Qed.

Lemma put_present : forall state key expiry,
  In (key, expiry) (put key expiry state).
Proof.
  induction state as [|[current previous] rest ih]; intros key expiry.
  - simpl. auto.
  - simpl. destruct (Nat.eqb key current); simpl; auto.
Qed.

Lemma put_other : forall state key expiry old deadline,
  old <> key -> In (old, deadline) state ->
  In (old, deadline) (put key expiry state).
Proof.
  induction state as [|[current previous] rest ih]; intros key expiry old deadline ne member.
  - contradiction.
  - simpl in *. destruct (Nat.eqb key current) eqn:same.
    + apply Nat.eqb_eq in same. simpl.
      destruct member as [equal|member]; [inversion equal; subst; contradiction|auto].
    + simpl. destruct member as [equal|member]; [auto|].
      right. eapply ih; eauto.
Qed.

Theorem capacity_kept : forall cap now ttl key state,
  length state <= cap ->
  length (fst (record cap now ttl key state)) <= cap.
Proof.
  intros cap now ttl key state fits.
  pose proof (Expiry.expiry_count snd now state) as count.
  unfold record. destruct (contains key (Expiry.retain snd now state)) eqn:found;
    destruct (Nat.ltb (length (Expiry.retain snd now state)) cap) eqn:room;
    simpl; try rewrite put_size, found; try lia.
  apply Nat.ltb_lt in room. lia.
Qed.

Theorem acknowledged_present : forall cap now ttl key state,
  snd (record cap now ttl key state) = true ->
  In (key, (now + ttl)%Z) (fst (record cap now ttl key state)).
Proof.
  intros cap now ttl key state accepted. unfold record in *.
  destruct (contains key (Expiry.retain snd now state)
    || Nat.ltb (length (Expiry.retain snd now state)) cap);
    simpl in *; [apply put_present|discriminate].
Qed.

Theorem full_refused : forall cap now ttl key state,
  contains key (Expiry.retain snd now state) = false ->
  cap <= length (Expiry.retain snd now state) ->
  record cap now ttl key state = (Expiry.retain snd now state, false).
Proof.
  intros cap now ttl key state absent full. unfold record.
  rewrite absent. apply Nat.ltb_ge in full. rewrite full. reflexivity.
Qed.

Theorem live_cancel_kept : forall cap now ttl key old deadline state,
  old <> key -> In (old, deadline) state -> (now <= deadline)%Z ->
  In (old, deadline) (fst (record cap now ttl key state)).
Proof.
  intros cap now ttl key old deadline state ne member live.
  assert (In (old, deadline) (Expiry.retain snd now state)) as retained.
  { apply Expiry.retain_exact. simpl. auto. }
  unfold record. destruct (contains key (Expiry.retain snd now state)
    || Nat.ltb (length (Expiry.retain snd now state)) cap); simpl.
  - apply put_other; assumption.
  - exact retained.
Qed.

Theorem flood_preserves_cancel : forall keys cap now ttl old deadline state,
  Forall (fun key => old <> key) keys ->
  In (old, deadline) state -> (now <= deadline)%Z ->
  In (old, deadline)
    (fold_left (fun state key => fst (record cap now ttl key state)) keys state).
Proof.
  induction keys as [|key rest ih]; intros cap now ttl old deadline state distinct member live.
  - exact member.
  - inversion distinct; subst. simpl. apply ih; try assumption.
    apply live_cancel_kept; assumption.
Qed.

Print Assumptions capacity_kept.
Print Assumptions acknowledged_present.
Print Assumptions full_refused.
Print Assumptions live_cancel_kept.
Print Assumptions flood_preserves_cancel.
End Cancellation.

Module Slots.
Definition owned (owner : nat -> nat) caller (slots : list nat) :=
  length (filter (fun key => Nat.eqb (owner key) caller) slots).

Definition claim cap quota owner key slots :=
  if existsb (Nat.eqb key) slots then (slots, true)
  else if Nat.ltb (length slots) cap
    && Nat.ltb (owned owner (owner key) slots) quota
  then (key :: slots, true)
  else (slots, false).

Lemma present : forall key slots,
  existsb (Nat.eqb key) slots = true <-> In key slots.
Proof.
  intros key slots. rewrite existsb_exists. split.
  - intros [value [member equal]]. apply Nat.eqb_eq in equal. subst. exact member.
  - intros member. exists key. split; [exact member|apply Nat.eqb_refl].
Qed.

Theorem quotas_kept : forall cap quota owner key slots,
  length slots <= cap ->
  (forall caller, owned owner caller slots <= quota) ->
  length (fst (claim cap quota owner key slots)) <= cap
  /\ (forall caller, owned owner caller (fst (claim cap quota owner key slots)) <= quota).
Proof.
  intros cap quota owner key slots global local. unfold claim.
  destruct (existsb (Nat.eqb key) slots); simpl; [auto|].
  destruct (Nat.ltb (length slots) cap
    && Nat.ltb (owned owner (owner key) slots) quota) eqn:room; simpl; [|auto].
  apply andb_true_iff in room. destruct room as [space own].
  apply Nat.ltb_lt in space. apply Nat.ltb_lt in own. split; [lia|].
  intros caller. unfold owned. simpl.
  destruct (Nat.eqb (owner key) caller) eqn:same; simpl.
  - apply Nat.eqb_eq in same. subst caller. unfold owned in own. lia.
  - apply local.
Qed.

Theorem reserved_cancel : forall cap quota owner key slots,
  In key slots -> claim cap quota owner key slots = (slots, true).
Proof.
  intros cap quota owner key slots member. unfold claim.
  apply present in member. rewrite member. reflexivity.
Qed.

Theorem acknowledged_present : forall cap quota owner key slots,
  snd (claim cap quota owner key slots) = true ->
  In key (fst (claim cap quota owner key slots)).
Proof.
  intros cap quota owner key slots accepted. unfold claim in *.
  destruct (existsb (Nat.eqb key) slots) eqn:member; simpl; [apply present; exact member|].
  destruct (Nat.ltb (length slots) cap
    && Nat.ltb (owned owner (owner key) slots) quota); simpl in *; [auto|discriminate].
Qed.

Theorem caller_full : forall cap quota owner key slots,
  ~ In key slots -> quota <= owned owner (owner key) slots ->
  claim cap quota owner key slots = (slots, false).
Proof.
  intros cap quota owner key slots absent full. unfold claim.
  destruct (existsb (Nat.eqb key) slots) eqn:member.
  - apply present in member. contradiction.
  - apply Nat.ltb_ge in full. rewrite full, andb_false_r. reflexivity.
Qed.

Theorem distinct_kept : forall cap quota owner key slots,
  NoDup slots -> NoDup (fst (claim cap quota owner key slots)).
Proof.
  intros cap quota owner key slots unique. unfold claim.
  destruct (existsb (Nat.eqb key) slots) eqn:member; simpl; [exact unique|].
  destruct (Nat.ltb (length slots) cap
    && Nat.ltb (owned owner (owner key) slots) quota); simpl; [|exact unique].
  constructor; [|exact unique]. intro found. apply present in found. congruence.
Qed.

Theorem reservation_kept : forall cap quota owner key saved slots,
  In saved slots -> In saved (fst (claim cap quota owner key slots)).
Proof.
  intros cap quota owner key saved slots member. unfold claim.
  destruct (existsb (Nat.eqb key) slots); simpl; [exact member|].
  destruct (Nat.ltb (length slots) cap
    && Nat.ltb (owned owner (owner key) slots) quota); simpl; auto.
Qed.

Theorem flood_preserves_slot : forall attempts cap quota owner saved slots,
  In saved slots ->
  In saved (fold_left (fun state key => fst (claim cap quota owner key state)) attempts slots).
Proof.
  induction attempts as [|key rest ih]; intros cap quota owner saved slots member.
  - exact member.
  - simpl. apply ih. apply reservation_kept. exact member.
Qed.

Print Assumptions quotas_kept.
Print Assumptions reserved_cancel.
Print Assumptions acknowledged_present.
Print Assumptions caller_full.
Print Assumptions distinct_kept.
Print Assumptions reservation_kept.
Print Assumptions flood_preserves_slot.
End Slots.

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
  | S rest, value :: suffix => value + 1 :: dust rest suffix
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
  | cons head suffix => attestation_influence head + influence suffix
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

Fixpoint weighted_threshold_count total selected count adversarial honest :=
  match count with
  | 0 => 0
  | S rest =>
      weighted_binomial_term total selected adversarial honest
      + weighted_threshold_count total (S selected) rest adversarial honest
  end.

Definition capture_threshold committee_size :=
  committee_size / 3 + 1.

Definition weighted_capture_numerator committee_size threshold adversarial honest :=
  if threshold <=? committee_size then
    weighted_threshold_count
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

Theorem capture_zero_above_size :
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

Example capture_half_5_threshold_2 :
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