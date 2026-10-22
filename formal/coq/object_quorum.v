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