(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import List Arith Bool Lia.
Import ListNotations.

Fixpoint accepts (limits counts : list nat) : bool :=
  match limits, counts with
  | [], [] => true
  | cap :: caps, count :: rest => (count <=? cap) && accepts caps rest
  | _, _ => false
  end.

Theorem accepts_spec : forall limits counts,
  accepts limits counts = true <-> Forall2 (fun cap count => count <= cap) limits counts.
Proof.
  induction limits as [|cap caps IH]; intros [|count rest]; simpl.
  - split; intros; constructor.
  - split; intros H; [discriminate|inversion H].
  - split; intros H; [discriminate|inversion H].
  - rewrite andb_true_iff, Nat.leb_le, IH.
    split; intros H; [destruct H; constructor; assumption|inversion H; auto].
Qed.

Definition add cap word total count :=
  if (total + count <=? word) && (total + count <=? cap)
  then Some (total + count) else None.

Theorem add_checked : forall cap word total count result,
  add cap word total count = Some result ->
  result = total + count /\ result <= word /\ result <= cap.
Proof.
  intros. unfold add in H.
  destruct ((total + count <=? word) && (total + count <=? cap)) eqn:ok; [|discriminate].
  inversion H; subst. rewrite andb_true_iff, !Nat.leb_le in ok. tauto.
Qed.

Theorem split_count_same : forall cap word total left right middle result,
  add cap word total left = Some middle ->
  add cap word middle right = Some result ->
  add cap word total (left + right) = Some result.
Proof.
  intros. apply add_checked in H. apply add_checked in H0.
  destruct H as [Hm [Hw Hc]], H0 as [Hr [Hw' Hc']].
  subst. unfold add. replace (total + (left + right)) with (total + left + right) by lia.
  rewrite (proj2 (Nat.leb_le _ _) Hw'), (proj2 (Nat.leb_le _ _) Hc'). reflexivity.
Qed.

Record entry := Entry { active : bool; id : nat; counts : list nat }.

Fixpoint lookup mode name rows :=
  match rows with
  | [] => None
  | row :: rest =>
    if Bool.eqb mode (active row) && (name =? id row)
    then Some (counts row) else lookup mode name rest
  end.

Definition sound limits rows :=
  Forall (fun row => active row = true -> accepts limits (counts row) = true) rows.

Theorem active_lookup_checked : forall limits rows name value,
  sound limits rows -> lookup true name rows = Some value ->
  Forall2 (fun cap count => count <= cap) limits value.
Proof.
  intros limits rows. induction rows as [|[mode key data] rest IH]; intros name value Hsound found;
    simpl in found; [discriminate|].
  inversion Hsound as [|first rest' valid others]; subst.
  destruct mode; simpl in found, valid.
  - destruct (name =? key).
    + inversion found; subst. apply accepts_spec. now apply valid.
    + eapply IH; eauto.
  - eapply IH; eauto.
Qed.

Definition publish limits mode name value rows :=
  if negb mode || accepts limits value then Entry mode name value :: rows else rows.

Theorem publish_sound : forall limits mode name value rows,
  sound limits rows -> sound limits (publish limits mode name value rows).
Proof.
  intros. unfold publish.
  destruct mode; simpl.
  - destruct (accepts limits value) eqn:ok; [constructor; simpl; auto|assumption].
  - constructor; [simpl; discriminate|assumption].
Qed.

Theorem prior_cache_no_active_hit : forall rows name,
  Forall (fun row => active row = false) rows -> lookup true name rows = None.
Proof.
  induction rows as [|row rest IH]; intros name all; [reflexivity|].
  inversion all; subst. simpl. rewrite H1. simpl. now apply IH.
Qed.

Theorem active_failure_keeps_cache : forall limits name value rows,
  accepts limits value = false -> publish limits true name value rows = rows.
Proof. intros. unfold publish. simpl. now rewrite H. Qed.