(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import ZArith Bool Lia List.
Open Scope Z_scope.

Definition meets (quorum members : Z) : bool :=
  (0 <=? quorum) && (0 <=? members) && (quorum <=? members).

Theorem meets_exact : forall quorum members,
  meets quorum members = true <-> 0 <= quorum /\ quorum <= members.
Proof.
  intros quorum members. unfold meets.
  repeat rewrite andb_true_iff.
  repeat rewrite Z.leb_le. lia.
Qed.

Module Stake.

Fixpoint score (weight : nat -> Z) (yes : nat -> bool) (ids : list nat) : Z :=
  match ids with
  | nil => 0
  | cons id rest => (if yes id then weight id else 0) + score weight yes rest
  end.

Definition quota (total : Z) : Z := 2 * total / 3 + 1.

Definition accepts weight floor yes ids :=
  (floor <=? score (fun _ => 1) yes ids)
  && (quota (score weight (fun _ => true) ids) <=? score weight yes ids).

Lemma quota_range total :
  0 < total -> 2 * total < 3 * quota total /\ quota total <= total.
Proof.
  intros positive. unfold quota.
  pose proof (Z.div_mod (2 * total) 3 ltac:(lia)).
  pose proof (Z.mod_pos_bound (2 * total) 3 ltac:(lia)).
  nia.
Qed.

Lemma score_cap weight yes ids :
  (forall id, In id ids -> 0 <= weight id) ->
  score weight yes ids <= score weight (fun _ => true) ids.
Proof.
  induction ids as [|id rest ih]; intros positive; simpl; [lia|].
  assert (0 <= weight id) by (apply positive; simpl; auto).
  assert (score weight yes rest <= score weight (fun _ => true) rest).
  { apply ih. intros other present. apply positive. simpl. auto. }
  destruct (yes id); lia.
Qed.

Lemma score_join weight left right ids :
  score weight left ids + score weight right ids =
  score weight (fun id => left id && right id) ids +
  score weight (fun id => left id || right id) ids.
Proof.
  induction ids as [|id rest ih]; simpl; [lia|].
  destruct (left id), (right id); simpl; lia.
Qed.

Theorem signed_overlap weight floor left right ids faulty :
  (forall id, In id ids -> 0 <= weight id) ->
  0 < score weight (fun _ => true) ids ->
  3 * faulty < score weight (fun _ => true) ids ->
  accepts weight floor left ids = true ->
  accepts weight floor right ids = true ->
  faulty < score weight (fun id => left id && right id) ids.
Proof.
  intros positive total minority first second.
  unfold accepts in first, second.
  apply andb_true_iff in first as [_ first].
  apply andb_true_iff in second as [_ second].
  apply Z.leb_le in first, second.
  pose proof (score_join weight left right ids) as joined.
  pose proof (score_cap weight (fun id => left id || right id) ids positive) as capped.
  pose proof (quota_range _ total) as [strict _].
  nia.
Qed.

Theorem seats_required weight floor yes ids :
  score (fun _ => 1) yes ids < floor -> accepts weight floor yes ids = false.
Proof.
  intros insufficient. unfold accepts.
  assert (floor <=? score (fun _ => 1) yes ids = false) as refused
    by (apply Z.leb_gt; exact insufficient).
  rewrite refused. reflexivity.
Qed.

Theorem weight_required weight floor yes ids :
  score weight yes ids < quota (score weight (fun _ => true) ids) ->
  accepts weight floor yes ids = false.
Proof.
  intros insufficient. unfold accepts.
  assert (quota (score weight (fun _ => true) ids) <=? score weight yes ids = false)
    as refused by (apply Z.leb_gt; exact insufficient).
  rewrite refused, andb_false_r. reflexivity.
Qed.

End Stake.

Theorem excess_refused : forall quorum members,
  members < quorum -> meets quorum members = false.
Proof.
  intros quorum members excess.
  destruct (meets quorum members) eqn:accepted; [|reflexivity].
  apply meets_exact in accepted. lia.
Qed.

Theorem negative_refused : forall quorum members,
  quorum < 0 \/ members < 0 -> meets quorum members = false.
Proof.
  intros quorum members negative.
  destruct (meets quorum members) eqn:accepted; [|reflexivity].
  apply meets_exact in accepted. lia.
Qed.

Theorem members_increase : forall quorum first next,
  meets quorum first = true -> first <= next -> meets quorum next = true.
Proof.
  intros quorum first next accepted order.
  apply meets_exact in accepted. apply meets_exact. lia.
Qed.

Theorem zero_members : forall quorum,
  meets quorum 0 = true <-> quorum = 0.
Proof.
  intro quorum. rewrite meets_exact. lia.
Qed.

Definition members (bootstrap : bool) (current next : Z) : Z :=
  if bootstrap then next else current.

Theorem bootstrap_counts_next : forall quorum current next,
  meets quorum (members true current next) = meets quorum next.
Proof. reflexivity. Qed.

Theorem existing_counts_current : forall quorum current next,
  meets quorum (members false current next) = meets quorum current.
Proof. reflexivity. Qed.

Definition apply_if {state : Type} (quorum count : Z) (before after : state) : state :=
  if meets quorum count then after else before.

Theorem refusal_preserves_state : forall (state : Type) quorum count (before after : state),
  count < quorum -> apply_if quorum count before after = before.
Proof.
  intros state quorum count before after excess. unfold apply_if.
  rewrite (excess_refused quorum count excess). reflexivity.
Qed.

Module Health.

Import ListNotations.

Definition count_floor (prior : bool) (f : Z) : Z :=
  if prior then 0 else f + 1.

Definition source_threshold (total : Z) : Z := total - Stake.quota total + 1.

Definition agrees weight prior f threshold yes ids :=
  (count_floor prior f <=? Stake.score (fun _ => 1) yes ids)
  && (threshold <=? Stake.score weight yes ids).

Definition head_mask (replied : nat -> bool) (head : nat -> Z) (target : Z) id :=
  replied id && (target <=? head id).

Definition low_mask (replied : nat -> bool) (head : nat -> Z) (local : Z) id :=
  replied id && (head id <? local).

Definition ahead weight threshold replied head local ids :=
  (threshold <=? Stake.score weight (low_mask replied head local) ids)
  && (Stake.score weight (head_mask replied head local) ids <? threshold).

Definition root_mask (replied : nat -> bool) (roots : nat -> option Z) (root : Z) id :=
  replied id && match roots id with
  | Some value => value =? root
  | None => false
  end.

Definition eligible_foreign weight prior f threshold replied roots local foreign ids :=
  negb (local =? foreign)
  && agrees weight prior f threshold (root_mask replied roots foreign) ids.

Lemma source_threshold_range total :
  0 < total ->
  total <= 3 * source_threshold total /\ 3 * source_threshold total < total + 3.
Proof.
  intros positive. unfold source_threshold, Stake.quota.
  pose proof (Z.div_mod (2 * total) 3 ltac:(lia)).
  pose proof (Z.mod_pos_bound (2 * total) 3 ltac:(lia)).
  nia.
Qed.

Lemma source_fault_budget total faulty :
  0 < total -> 3 * faulty < total -> faulty < source_threshold total.
Proof.
  intros positive minority.
  pose proof (source_threshold_range total positive) as [lower upper]. lia.
Qed.

Lemma score_nonnegative weight yes ids :
  (forall id, In id ids -> 0 <= weight id) ->
  0 <= Stake.score weight yes ids.
Proof.
  induction ids as [|id rest ih]; intros positive; simpl; [lia|].
  assert (0 <= weight id) by (apply positive; simpl; auto).
  assert (0 <= Stake.score weight yes rest).
  { apply ih. intros other present. apply positive. simpl. auto. }
  destruct (yes id); lia.
Qed.

Lemma score_mask weight left right ids :
  (forall id, In id ids -> 0 <= weight id) ->
  (forall id, In id ids -> left id = true -> right id = true) ->
  Stake.score weight left ids <= Stake.score weight right ids.
Proof.
  induction ids as [|id rest ih]; intros positive included; simpl; [lia|].
  assert (0 <= weight id) by (apply positive; simpl; auto).
  assert (Stake.score weight left rest <= Stake.score weight right rest).
  { apply ih.
    - intros other present. apply positive. simpl. auto.
    - intros other present accepted. apply included; simpl; auto. }
  assert (left id = true -> right id = true) as step.
  { apply included. simpl. auto. }
  destruct (left id), (right id); simpl in *.
  - lia.
  - specialize (step eq_refl). discriminate.
  - lia.
  - lia.
Qed.

Theorem prior_weight_only weight f threshold yes ids :
  agrees weight true f threshold yes ids = (threshold <=? Stake.score weight yes ids).
Proof.
  unfold agrees, count_floor.
  assert (0 <= Stake.score (fun _ => 1) yes ids) as nonnegative.
  { apply score_nonnegative. intros id present. lia. }
  assert (0 <=? Stake.score (fun _ => 1) yes ids = true) as counted
    by (apply Z.leb_le; exact nonnegative).
  rewrite counted. reflexivity.
Qed.

Theorem stake_count_floor weight f threshold yes ids :
  agrees weight false f threshold yes ids = true <->
  f + 1 <= Stake.score (fun _ => 1) yes ids /\ threshold <= Stake.score weight yes ids.
Proof.
  unfold agrees, count_floor. rewrite andb_true_iff.
  repeat rewrite Z.leb_le. reflexivity.
Qed.

Lemma insufficient weight prior f threshold yes ids :
  Stake.score weight yes ids < threshold ->
  agrees weight prior f threshold yes ids = false.
Proof.
  intros low. unfold agrees.
  assert (threshold <=? Stake.score weight yes ids = false) as refused
    by (apply Z.leb_gt; exact low).
  rewrite refused, andb_false_r. reflexivity.
Qed.

Lemma low_score weight faulty replied head local ids :
  (forall id, In id ids -> 0 <= weight id) ->
  (forall id, In id ids -> replied id = true -> faulty id = false -> local <= head id) ->
  Stake.score weight (low_mask replied head local) ids <= Stake.score weight faulty ids.
Proof.
  intros positive current. apply score_mask; [exact positive|].
  intros id present supported. unfold low_mask in supported.
  apply andb_true_iff in supported as [responded low].
  apply Z.ltb_lt in low.
  destruct (faulty id) eqn:bad; [reflexivity|].
  pose proof (current id present responded bad). lia.
Qed.

Theorem false_ahead_refused weight faulty replied head local ids threshold :
  (forall id, In id ids -> 0 <= weight id) ->
  Stake.score weight faulty ids < threshold ->
  (forall id, In id ids -> replied id = true -> faulty id = false -> local <= head id) ->
  ahead weight threshold replied head local ids = false.
Proof.
  intros positive minority current. unfold ahead.
  pose proof (low_score weight faulty replied head local ids positive current).
  assert (threshold <=? Stake.score weight (low_mask replied head local) ids = false)
    as refused by (apply Z.leb_gt; lia).
  rewrite refused. reflexivity.
Qed.

Corollary source_ahead_limit weight faulty replied head local ids :
  (forall id, In id ids -> 0 <= weight id) ->
  0 < Stake.score weight (fun _ => true) ids ->
  3 * Stake.score weight faulty ids < Stake.score weight (fun _ => true) ids ->
  (forall id, In id ids -> replied id = true -> faulty id = false -> local <= head id) ->
  ahead weight (source_threshold (Stake.score weight (fun _ => true) ids))
    replied head local ids = false.
Proof.
  intros positive total minority current.
  apply (false_ahead_refused weight faulty replied head local ids).
  - exact positive.
  - apply source_fault_budget; assumption.
  - exact current.
Qed.

Theorem current_weight_protects weight replied head local ids threshold :
  threshold <= Stake.score weight (head_mask replied head local) ids ->
  ahead weight threshold replied head local ids = false.
Proof.
  intros enough. unfold ahead.
  assert (Stake.score weight (head_mask replied head local) ids <? threshold = false)
    as refused by (apply Z.ltb_ge; exact enough).
  rewrite refused, andb_false_r. reflexivity.
Qed.

Lemma head_score weight faulty replied head target ids :
  (forall id, In id ids -> 0 <= weight id) ->
  (forall id, In id ids -> replied id = true -> faulty id = false -> head id < target) ->
  Stake.score weight (head_mask replied head target) ids <= Stake.score weight faulty ids.
Proof.
  intros positive above. apply score_mask; [exact positive|].
  intros id present supported. unfold head_mask in supported.
  apply andb_true_iff in supported as [responded high].
  apply Z.leb_le in high.
  destruct (faulty id) eqn:bad; [reflexivity|].
  pose proof (above id present responded bad). lia.
Qed.

Theorem head_source_refused weight faulty replied head target ids prior f threshold :
  (forall id, In id ids -> 0 <= weight id) ->
  Stake.score weight faulty ids < threshold ->
  (forall id, In id ids -> replied id = true -> faulty id = false -> head id < target) ->
  agrees weight prior f threshold (head_mask replied head target) ids = false.
Proof.
  intros positive minority above. apply insufficient.
  pose proof (head_score weight faulty replied head target ids positive above). lia.
Qed.

Corollary source_head_limit weight faulty replied head target ids prior f :
  (forall id, In id ids -> 0 <= weight id) ->
  0 < Stake.score weight (fun _ => true) ids ->
  3 * Stake.score weight faulty ids < Stake.score weight (fun _ => true) ids ->
  (forall id, In id ids -> replied id = true -> faulty id = false -> head id < target) ->
  agrees weight prior f (source_threshold (Stake.score weight (fun _ => true) ids))
    (head_mask replied head target) ids = false.
Proof.
  intros positive total minority above.
  apply (head_source_refused weight faulty replied head target ids prior f).
  - exact positive.
  - apply source_fault_budget; assumption.
  - exact above.
Qed.

Lemma root_score weight faulty replied roots foreign ids :
  (forall id, In id ids -> 0 <= weight id) ->
  (forall id, In id ids -> replied id = true -> faulty id = false -> roots id <> Some foreign) ->
  Stake.score weight (root_mask replied roots foreign) ids <= Stake.score weight faulty ids.
Proof.
  intros positive separate. apply score_mask; [exact positive|].
  intros id present supported. unfold root_mask in supported.
  apply andb_true_iff in supported as [responded matches].
  destruct (roots id) as [value|] eqn:reported; simpl in matches; [|discriminate].
  apply Z.eqb_eq in matches.
  destruct (faulty id) eqn:bad; [reflexivity|].
  exfalso. apply (separate id present responded bad).
  rewrite reported, matches. reflexivity.
Qed.

Theorem faulty_root_ineligible weight faulty replied roots local foreign ids prior f threshold :
  (forall id, In id ids -> 0 <= weight id) ->
  Stake.score weight faulty ids < threshold ->
  (forall id, In id ids -> replied id = true -> faulty id = false -> roots id <> Some foreign) ->
  eligible_foreign weight prior f threshold replied roots local foreign ids = false.
Proof.
  intros positive minority separate. unfold eligible_foreign.
  assert (agrees weight prior f threshold (root_mask replied roots foreign) ids = false)
    as refused.
  { apply insufficient.
    pose proof (root_score weight faulty replied roots foreign ids positive separate). lia. }
  rewrite refused, andb_false_r. reflexivity.
Qed.

Corollary source_root_limit weight faulty replied roots local foreign ids prior f :
  (forall id, In id ids -> 0 <= weight id) ->
  0 < Stake.score weight (fun _ => true) ids ->
  3 * Stake.score weight faulty ids < Stake.score weight (fun _ => true) ids ->
  (forall id, In id ids -> replied id = true -> faulty id = false -> roots id <> Some foreign) ->
  eligible_foreign weight prior f (source_threshold (Stake.score weight (fun _ => true) ids))
    replied roots local foreign ids = false.
Proof.
  intros positive total minority separate.
  apply (faulty_root_ineligible weight faulty replied roots local foreign ids prior f).
  - exact positive.
  - apply source_fault_budget; assumption.
  - exact separate.
Qed.

Fixpoint insert_head (head : Z) (heads : list Z) : list Z :=
  match heads with
  | [] => [head]
  | value :: rest =>
    if value <=? head then head :: heads else value :: insert_head head rest
  end.

Definition count_target (f : nat) (head : nat -> Z) (replied : nat -> bool) (ids : list nat) :=
  nth_error (fold_right insert_head [] (map head (filter replied ids))) f.

Definition sample_ids : list nat := [0%nat; 1%nat; 2%nat; 3%nat].
Definition sample_faulty (id : nat) := Nat.ltb id 2%nat.
Definition sample_weight id : Z := if sample_faulty id then 1 else 100.
Definition sample_head id : Z := if sample_faulty id then 5000 else 10.
Definition sample_roots id : option Z := if sample_faulty id then Some 1 else Some 0.

Example count_only_target :
  count_target 1%nat sample_head (fun _ => true) sample_ids = Some 5000 /\
  map sample_head (filter (fun id => negb (sample_faulty id)) sample_ids) = [10; 10] /\
  Stake.score sample_weight sample_faulty sample_ids = 2 /\
  source_threshold (Stake.score sample_weight (fun _ => true) sample_ids) = 68 /\
  agrees sample_weight false 1 68 (head_mask (fun _ => true) sample_head 5000) sample_ids = false.
Proof. vm_compute. repeat split; reflexivity. Qed.

Example faulty_root_case :
  Stake.score (fun _ => 1) (root_mask (fun _ => true) sample_roots 1) sample_ids = 2 /\
  eligible_foreign sample_weight false 1 68 (fun _ => true) sample_roots 0 1 sample_ids = false.
Proof. vm_compute. split; reflexivity. Qed.

Example policy_count_case :
  agrees sample_weight true 1 68 (fun id => Nat.eqb id 2%nat) sample_ids = true /\
  agrees sample_weight false 1 68 (fun id => Nat.eqb id 2%nat) sample_ids = false.
Proof. vm_compute. split; reflexivity. Qed.

Example source_not_finality :
  source_threshold 3 = 1 /\
  agrees (fun _ => 1) true 0 (source_threshold 3)
    (fun id => Nat.eqb id 0%nat) [0%nat; 1%nat; 2%nat] = true /\
  Stake.accepts (fun _ => 1) 0
    (fun id => Nat.eqb id 0%nat) [0%nat; 1%nat; 2%nat] = false.
Proof. vm_compute. repeat split; reflexivity. Qed.

Example low_head_case :
  let heads := fun id => if sample_faulty id then 7 else 10 in
  agrees sample_weight false 3 68 (head_mask (fun _ => true) heads 7) sample_ids = true /\
  agrees sample_weight false 3 68 (head_mask (fun _ => true) heads 10) sample_ids = false /\
  ahead sample_weight 68 (fun _ => true) heads 10 sample_ids = false.
Proof. vm_compute. repeat split; reflexivity. Qed.

Example heavy_low_case :
  let heads := fun id => if sample_faulty id then 10 else 7 in
  ahead sample_weight 68 (fun _ => true) heads 10 sample_ids = true.
Proof. vm_compute. reflexivity. Qed.

End Health.